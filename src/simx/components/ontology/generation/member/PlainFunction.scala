/*
 * Copyright 2015 The SIRIS Project
 *
 *    Licensed under the Apache License, Version 2.0 (the "License");
 *    you may not use this file except in compliance with the License.
 *    You may obtain a copy of the License at
 *
 *        http://www.apache.org/licenses/LICENSE-2.0
 *
 *    Unless required by applicable law or agreed to in writing, software
 *    distributed under the License is distributed on an "AS IS" BASIS,
 *    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *    See the License for the specific language governing permissions and
 *    limitations under the License.
 *
 * The SIRIS Project is a cooperation between Beuth University, Berlin and the
 * HCI Group at the University of Würzburg. The project is funded by the German
 * Federal Ministry of Education and Research (grant no. 17N4409).
 */

package simx.components.ontology.generation.member

import java.io.Serializable
import java.util

import org.semanticweb.owlapi.model._
import simx.components.ontology.generation.OntoGenTwo
import simx.components.ontology.generation.helper._
import uk.ac.manchester.cs.owl.owlapi.OWLObjectSomeValuesFromImpl
import collection.JavaConversions._

/**
 * Created by dwiebusch on 19.02.15
 */
case class PlainFunction(ind : OWLIndividual)(implicit o : OntoGenTwo) extends Writable with WritableForPackage {

  val returnValue = getReturnValue
  val parameters = getParameters

  def getComment = o.getAnnotationFor(ind)(o.getComment).headOption

  def getOperatorType: OperatorType =
    OperatorType(o.getObjectProperties(ind)(o.hasOperatorTypeProp.get).headOption.map(OWLFunctions.getName).
      getOrElse(throw generateException("Could not determine operator type for individual '" + getName + "'")))

  def getOperatorName =
    o.getDataProperties(ind)(o.hasNameProp.get).headOption.map(_.getLiteral).getOrElse("of")

  def getImplementation =
    o.getDataProperties(ind)(o.implementedByProp.get).headOption.map(_.getLiteral)

  private def getImplementationCode =
    getImplementation.map(i => " = " + i + "(" + getParameters.map(_._1).mkString(", ") + ")").getOrElse("")

  def getVerboseFunctionName = getOperatorType match {
    case _: Prefix => deCap(getReturnValueString) + cap(getOperatorName)
    case _: Infix => getFunctionName
  }

  def getFunctionName = deCap(getOperatorName)

  def getName =
    OWLFunctions.getName(ind)

  def getReturnValueString =
    returnValue.map(r => OWLFunctions.getName(r.asOWLClass())).getOrElse("Unit")

  def getParamStringFor(params: List[(String, OWLClass)]) =
    params.map(idx => idx._1 + " : " + OWLFunctions.getName(idx._2)).mkString(", ")

  def getParamString = getParamStringFor(getParameters)

  def getFunctionsCode =
    getComment.map(c => "/**\n" + " * " + c + "\n */\n").getOrElse("") +
    "def " + getVerboseFunctionName + "(" + getParamString + ") : " + getReturnValueString + getImplementationCode

  override def toString: String = getFunctionsCode

  def getForComponent: String = {

    def getTargetPackages(oWLClass: OWLClass): String = {
      val dataFactory = o.manager.getOWLDataFactory

      val classExpression: OWLObjectSomeValuesFrom =
        dataFactory.getOWLObjectSomeValuesFrom(o.getDescribesProp, oWLClass)

      val ontoSvarDesc = new OntologySVarDescription(o.getSVarDescriptionClass)(o)

      val allIndividuals = o.getIndividuals(classExpression)

      //val allBases = allIndividuals.map(ontoSvarDesc.getMostGeneralBase)
      val allPackages = allIndividuals.map(ontoSvarDesc.getTargetComponent).toList.distinct

      val mostGeneralIndividualPackage =
        if(allPackages.contains("simx.core")) "simx.core" else allPackages.head

      mostGeneralIndividualPackage
    }

    val targetPackages: List[String] =
      getTargetPackages(returnValue.get.asOWLClass()) :: parameters.map(_._2).map(getTargetPackages)

    targetPackages.find(_ != "simx.core").getOrElse("simx.core")
  }

  def getSVarDescriptionCode = {
    getComment.map(c => "/**\n" + " * " + c + "\n */\n").getOrElse("") + (getOperatorType match {
      case _: Prefix =>
        "def " + getFunctionName + "(" + getParamString + ")(implicit functions: Functions) = functions." +
          getVerboseFunctionName + "(" + getParameters.map(_._1).mkString(", ") + ")"
      case _: Infix =>
        "def " + getFunctionName + "(" + getParamStringFor(getParameters.tail) + ")(implicit functions: Functions) = functions." +
          getVerboseFunctionName + "(this, " + getParameters.tail.map(_._1).mkString(", ") + ")"
    })
  }

  protected def parse(f : Set[OWLObjectProperty] => Boolean) =
    o.getTypes(ind).filter(x => f(x.getObjectPropertiesInSignature.toSet)).flatMap(PlainVisitor().parse).toList.
      sortBy(_._1.asOWLObjectProperty().toStringID).map(_._2)


  protected def createParameterTuple(cls : OWLClassExpression) : (String, OWLClass) = {
    val result = PlainVisitor().parse(cls)
    OWLFunctions.getName(result(o.getHasRoleProp).asOWLClass) -> result(o.getHasSubjectProp).asOWLClass
  }

  def getParameters: List[(String, OWLClass)] = {
    var counter = 0
    parse( _.map(_.toStringID).exists(x => x.matches(".*#hasParameter[0-5]?") || x.matches(".*#hasRole"))).map { cls =>
      counter += 1
      if (cls.isAnonymous) createParameterTuple(cls) else "param" + counter -> cls.asOWLClass()
    }
  }

  def getReturnValue: Option[OWLClassExpression] =
    parse( _.map(_.toStringID).exists( _.matches(".*#hasReturnValue") ) ).headOption

  override def packageName: String = getForComponent

  override def toScalaCode: String = toString
}

private case class PlainVisitor(implicit o : OntoGenTwo)
  extends VisitorAdapter(Map[OWLObjectPropertyExpression, OWLClassExpression]())
{
  override def visit(owlObjectSomeValuesFrom: OWLObjectSomeValuesFrom) {
    result += owlObjectSomeValuesFrom.getProperty -> owlObjectSomeValuesFrom.getFiller
  }

  override def visit(owlObjectIntersectionOf: OWLObjectIntersectionOf): Unit ={
    owlObjectIntersectionOf.getOperands.foreach(_ accept this)
  }

  override def visit(owlClass: OWLClass){
    result += o.getHasSubjectProp -> owlClass
  }
}
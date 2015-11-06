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

import org.semanticweb.owlapi.model._
import org.semanticweb.owlapi.reasoner.OWLReasoner
import simx.components.ontology.generation.OntoGenTwo
import simx.components.ontology.generation.helper._
import simx.ontology.{SimxOwlFunctions, SimxOwlInterface}

import scala.collection.JavaConversions._

case class OntologyFunctionClass(override val owlClass : OWLClass)(implicit o : OntoGenTwo)
  extends OntologyMember(owlClass)(o) {
  var functions : Set[OntologyFunction] = Set()

  getIndividuals.collect{
    case n: OWLNamedIndividual => functions += OntologyFunction(n)(o)
  }

  override def getFunctions: List[OntologyFunction] = functions.toList
}

case class OntologyFunction(ind : OWLNamedIndividual)(implicit o : OntoGenTwo) extends Writable with WritableForPackage {

  val utils = new SimxOwlFunctions(new SimxOwlInterface {
    override def reasonerFor(ontology: OWLOntology): OWLReasoner = o.reasoner.get
    override def owlDataFactory(): OWLDataFactory = o.manager.getOWLDataFactory
    override def owlOntologyManager(): OWLOntologyManager = o.manager
  })

  val returnTypeClass: OWLClass = getReturnTypeClass
  val parameterClasses: List[(String, OWLClass)] = getParameterClasses

  def getComment = {
    //o.getAnnotationFor(ind)(o.getComment).headOption
    val comment = utils.getComment(ind, o.getLoadedOntology)
    if(comment.isEmpty) None else Some(comment)
  }

  def getOperatorType: OperatorType = {
//    OperatorType(o.getObjectProperties(ind)(o.hasOperatorTypeProp.get).headOption.map(OWLFunctions.getName).
//      getOrElse(throw generateException("Could not determine operator type for individual '" + getName + "'")))
    if(utils.isInfix(ind, o.getLoadedOntology)) Infix() else Prefix()
  }
  
  def getOperatorName: String =
//    o.getDataProperties(ind)(o.hasNameProp.get).headOption.map(_.getLiteral).getOrElse("of")
    utils.getName(ind, o.getLoadedOntology)
  
  def getImplementation: Option[String] =
//    o.getDataProperties(ind)(o.implementedByProp.get).headOption.map(_.getLiteral)
    Some(utils.getImplementation(ind, o.getLoadedOntology))
  
  
  private def getImplementationCode =
    getImplementation.map(i => " = " + i + "(" + getParameterClasses.map(_._1).mkString(", ") + ")").getOrElse("")

  def getVerboseFunctionName = getOperatorType match {
    case _: Prefix => deCap(getReturnTypeString) + cap(getOperatorName)
    case _: Infix => getFunctionName
  }

  def getFunctionName = deCap(getOperatorName)

  def getName =
    OWLFunctions.getName(ind.getIRI)

  def getReturnTypeString: String =
    returnTypeClass.getIRI.getFragment
//    returnType.map(r => OWLFunctions.getName(r.asOWLClass())).getOrElse("Unit")

  def getParamStringFor(params: List[(String, OWLClass)]) =
    params.map(idx => idx._1 + " : " + OWLFunctions.getName(idx._2)).mkString(", ")

  def getParamString = getParamStringFor(getParameterClasses)

  def getFunctionsCode =
    getComment.map(c => "/**\n" + " * " + c + "\n */\n").getOrElse("") +
    "def " + getVerboseFunctionName + "(" + getParamString + ") : " + getReturnTypeString //+ getImplementationCode

  override def toString: String = ind match {
    case namedInd : OWLNamedIndividual => namedInd.getIRI.getFragment
    case _ => ind.toString
  }

  def getForComponent: String = {

    def getTargetComponent( i : OWLNamedIndividual ) : String =
      o.reasoner.get.getObjectPropertyValues(i, o.getForComponentProp).getFlattened.headOption.collect{
        case component => o.getDataProperties(component)(o.getInPackageProp).headOption.collect{
          case literal => literal.getLiteral
        }.getOrElse("simx.core")
      }.getOrElse("simx.core")

//    def getTargetPackages(oWLClass: OWLClass): String = {
//      val dataFactory = o.manager.getOWLDataFactory
//
//      val classExpression: OWLObjectSomeValuesFrom =
//        dataFactory.getOWLObjectSomeValuesFrom(o.getDescribesProp, oWLClass)
//
//      val ontoSvarDesc = new OntologySVarDescription(o.getSVarDescriptionClass)(o)
//
//      val allIndividuals = o.getIndividuals(classExpression)
//
//      //val allBases = allIndividuals.map(ontoSvarDesc.getMostGeneralBase)
//      val allPackages = allIndividuals.map(ontoSvarDesc.getTargetComponent).toList.distinct
//
//      val mostGeneralIndividualPackage =
//        if(allPackages.contains("simx.core")) "simx.core" else allPackages.head
//
//      mostGeneralIndividualPackage
//    }

    val targetPackages: List[String] =
//      getTargetPackages(returnTypeClass.asOWLClass()) :: parameterClasses.map(_._2).map(getTargetPackages)
      getTargetComponent(getReturnType) :: getParameterTypes.map(getTargetComponent).toList

    val mostSpecificPackage =
      targetPackages.find(_.startsWith("simx.applications")).
        getOrElse(targetPackages.find(_.startsWith("simx.components")).
        getOrElse("simx.core"))

    mostSpecificPackage
  }

  def getSVarDescriptionCode = {
    getComment.map(c => "/**\n" + " * " + c + "\n */\n").getOrElse("") + (getOperatorType match {
      case _: Prefix =>
        "def " + getFunctionName + "(" + getParamString + ")(implicit functions: Functions) = functions." +
          getVerboseFunctionName + "(" + getParameterClasses.map(_._1).mkString(", ") + ")"
      case _: Infix =>
        "def " + getFunctionName + "(" + getParamStringFor(getParameterClasses.tail) + ")(implicit functions: Functions) = functions." +
          getVerboseFunctionName + "(this, " + getParameterClasses.tail.map(_._1).mkString(", ") + ")"
    })
  }

  protected def parse(f : Set[OWLObjectProperty] => Boolean) =
    o.getTypes(ind).filter(x => f(x.getObjectPropertiesInSignature.toSet)).flatMap(PlainVisitor().parse).toList.
      sortBy(_._1.asOWLObjectProperty().toStringID).map(_._2)


//  protected def createParameterTuple(cls : OWLClassExpression) : (String, OWLClass) = {
//    val result = PlainVisitor().parse(cls)
//    OWLFunctions.getName(result(o.getHasRoleProp).asOWLClass) -> result(o.getHasSubjectProp).asOWLClass
//  }

  def getParameterTypes =
    utils.getParameterTypes(ind, o.getLoadedOntology)

  def getParameterClasses: List[(String, OWLClass)] = {
//    var counter = 0
//    parse( _.map(_.toStringID).exists(x => x.matches(".*#hasParameter[0-5]?") || x.matches(".*#hasRole"))).map { cls =>
//      counter += 1
//      if (cls.isAnonymous) createParameterTuple(cls) else "param" + counter -> cls.asOWLClass()
//    }

    val parameterTypes = getParameterTypes.map(
      utils.getDescribedPropertyFor(_, o.getLoadedOntology))
    val parameterRoles = utils.getParameterRoles(ind, o.getLoadedOntology)

    parameterRoles.zip(parameterTypes).toList
  }

  def getReturnType: OWLNamedIndividual = utils.getReturnType(ind, o.getLoadedOntology)

  def getReturnTypeClass: OWLClass = {
    //    parse( _.map(_.toStringID).exists( _.matches(".*#hasReturnValue") ) ).headOption
    utils.getDescribedPropertyFor(getReturnType, o.getLoadedOntology)
  }

  override def packageName: String = getForComponent

  override def toScalaCode: String = getFunctionsCode
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
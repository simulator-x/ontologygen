/*
 * Copyright 2014 The SIRIS Project
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
import simx.components.ontology.generation.OntoGenTwo
import simx.components.ontology.generation.helper.OWLFunctions
import collection.JavaConversions._

/**
 * Created by dwiebusch on 01.09.14
 */
case class OntologyActionClass(override val owlClass : OWLClass)(implicit o : OntoGenTwo)
  extends OntologyMember(owlClass)(o)
{
//  var functions : Set[PlainFunction] = Set()
  var actions : Set[OntologyAction] = Set()


  getIndividuals.foreach{ actionIndividual =>
    val potRet = new OntologyAction(actionIndividual)(o)
//    if(potRet.effects.isEmpty && potRet.preconditions.isEmpty)
//      functions += PlainFunction(actionIndividual)(o)
//    else
      actions += potRet
  }

//  override def getFunctionDescriptions: List[String] =
//    functions.map(_.toString).toList

//  override def getFunctions : List[PlainFunction] = functions.toList

  override def getActionDescriptions: List[String] =
    actions.map(_.toString).toList
}

class OntologyAction (val individual : OWLIndividual)(implicit o : OntoGenTwo ) extends Writable {
  override def getName: String =
    OWLFunctions.getName(individual)

  val preconditions =
    o.getTypes(individual).filter(_.getObjectPropertiesInSignature.contains(o.getHasPreconditionProp)).
      map(PreconditionIndividual.apply)

  val effects =
    o.getTypes(individual).filter(_.getObjectPropertiesInSignature.contains(o.getHasEffectProp)).
      map(EffectIndividual.apply)

  val parameters =
    effects.flatMap(_.parameters) ++ preconditions.flatMap(_.parameters)

  override def toString: String =
    "abstract class " + getName + "(staticFunctions : Functions){\n\timport staticFunctions._\n\t\t" +
      "def apply("+ parameters.map(t => OWLFunctions.getName(t._1) + " : " + OWLFunctions.getName(t._2)).mkString(", ") +") : Unit" +
      (if (preconditions.nonEmpty) "\n\t\tval preconditions = Set(" + preconditions.mkString("\n\t\t") + ")" else "") +
      (if (effects.nonEmpty) "\n\t\tval effects = " + effects.mkString("\n\t\t") else "") +
      "\n}"
}


protected abstract class PreconOrEffect(prop : OWLClassExpression)(implicit o : OntoGenTwo){
  private val parsed = ActionParser(o).parse(prop)
  val pObject = parsed.get(o.getHasObjectProp).map(makeObject)
  val pSubject = parsed.get(o.getHasSubjectProp).map(makeSubject)
  val pPredicate = parsed.get(o.getHasPredicateProp).map( p => Predicate(makePredicate(p)) )
  val parameters = pSubject.get.value.getParameters ++ pObject.get.value.getParameters

  if (parameters.exists(p1 => parameters.exists(p2 => p1._1 == p2._1 && p1._2 != p2._2 )))
    println("!!! INVALID PARAMETERS !!!" + this)

  protected def makePredicate(t : TreeNode) : OWLIndividual =
    t.values.head

  protected def makeFunction(t : TreeNode) : Function = {
    val pred = t.get(o.getHasPredicateProp)
    val parameters = getParameters(t)
    val funcParams = parameters.map{ p =>
      val lastChar = p._1.asOWLObjectProperty().toStringID.last
      val idx = if (lastChar.isDigit) lastChar.toString.toInt else 0
      if (p._2.values.nonEmpty)
        FunctionParameter(idx, makeFunction(p._2.values.head))
      else
        FunctionParameter(idx, makeFunction(p._2))
    }
    val func = OntologyFunction(makePredicate(pred.get).asOWLNamedIndividual())
    val mappedParams = funcParams.zip(func.parameterClasses).map( t => t._1.setType(t._2._2) )
    new Function(makePredicate(pred.get), mappedParams.toSeq :_*)
  }

  protected def makeFunction(ind : OWLIndividual) : Function = {
    new Function(ind)
  }

  protected def makeObject(t : TreeNode) : Object = {
    if (t.values.nonEmpty)
      Object(makeFunction(t.values.head))
    else
      Object(makeFunction(t))
  }

  protected def getParameters(t : TreeNode) = {
    t.children.filter(_._1.asOWLObjectProperty().toStringID.matches(".*#hasParameter[0-5]?"))
  }

  protected def makeSubject(t : TreeNode) : Subject = {
    if (t.values.nonEmpty)
      Subject(makeFunction(t.values.head))
    else
      Subject(makeFunction(t))
  }
}

case class PreconditionIndividual(prop : OWLClassExpression)(implicit o : OntoGenTwo ) extends PreconOrEffect(prop){
  override def toString: String =
    "Precondition(" + pSubject.get + ", " + pPredicate.get + ", " + pObject.get + ")"
}

case class EffectIndividual(prop : OWLClassExpression)(implicit o : OntoGenTwo ) extends PreconOrEffect(prop){
  override def toString: String =
    "Effect(" + pSubject.get + ", " + pPredicate.get + ", " + pObject.get + ")"
}

case class Object(value : Function){
  override def toString: String = value.toString
}

case class Subject(value : Function){
  override def toString: String = value.toString
}

case class Predicate(value : OWLIndividual){
  override def toString: String =
    OWLFunctions.getName(value)
}

case class FunctionParameter(idx : Int, parameterName: Function, parameterType : Option[OWLClass] = None){
  override def toString: String =
    parameterName.toString //+ parameterType.map(x => " : " + OWLFunctions.getName(x)).getOrElse(" : Any ")

  def fullString =
    parameterName.toString + parameterType.map(x => " : " + OWLFunctions.getName(x)).getOrElse(" : Any ")

  def getParameters : Set[(OWLIndividual, OWLClass)] =
    if (parameterType.nonEmpty && parameterName.isParameterless)
      parameterName.getParameters + (parameterName.predicate-> parameterType.get)
    else
      parameterName.getParameters

  def setType(tpe : OWLClass) =
    FunctionParameter(idx, parameterName, Some(tpe))
}

case class Function(predicate : OWLIndividual, parameters : FunctionParameter*)(implicit o : OntoGenTwo){
  def isParameterless =
    parameters.isEmpty

  def function =
    if (isParameterless) None else Some(OntologyFunction(predicate.asOWLNamedIndividual())(o))

  def getParameters : Set[(OWLIndividual, OWLClass)] =
    parameters.flatMap(_.getParameters).toSet

  override def toString: String = OWLFunctions.getName(predicate) +
    (if (isParameterless) "" else "(" + parameters.sortBy(_.idx).mkString(", ") + ")" )
}





/******************************************
  *
  * Helpers
  *
  **************************************** */

case class TreeNode(key : Option[OWLObjectPropertyExpression] = None, children : Map[OWLObjectPropertyExpression, TreeNode] = Map(), values : Set[OWLIndividual] = Set()){
  def ++(that : Map[OWLObjectPropertyExpression, TreeNode]) =
    TreeNode(key, children ++ that, values)

  def +(that : OWLIndividual) =
    TreeNode(key, children, values + that)

  def get(p : OWLObjectPropertyExpression) =
    children.get(p)

  override def toString: String =
    toString(1)

  protected def toString(depth : Int): String = {
    OWLFunctions.getName(key.get.asOWLObjectProperty()) +
      (if (values.nonEmpty) ": " + values.map(OWLFunctions.getName) else "") +
      (if (children.nonEmpty) "\n" + "\t" * depth + children.map(_._2.toString(depth + 1)).mkString("\n" + "\t" * depth) else "")
  }
}

private case class ActionParser(o : OntoGenTwo) extends VisitorAdapter(TreeNode()){
  private def _parse(that : OWLClassExpression) = {
    val parser = ActionParser(o)
    that accept parser
    parser.result match {
      case TreeNode(None, children, _) => children
      case treeNode => Map(treeNode.key.get -> treeNode)
    }
  }

  override def visit(owlObjectSomeValuesFrom: OWLObjectSomeValuesFrom) {
    result = TreeNode(Some(owlObjectSomeValuesFrom.getProperty), _parse(owlObjectSomeValuesFrom.getFiller))
  }

  override def visit(owlObjectIntersectionOf: OWLObjectIntersectionOf){
    result = owlObjectIntersectionOf.getOperands.foldLeft(result)(_ ++ _parse(_))
  }

  override def visit(owlObjectHasValue: OWLObjectHasValue){
    result = TreeNode(Some(owlObjectHasValue.getProperty)) + owlObjectHasValue.getFiller
  }
}





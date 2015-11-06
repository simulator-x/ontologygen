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
import simx.components.ontology.generation.OntoGenTwo
import simx.components.ontology.generation.helper.OWLFunctions._
import collection.JavaConversions._

/**
 * Created by dwiebusch on 18.02.15
 */

case class OntologyAspectIndividual(i : OWLIndividual)(implicit o : OntoGenTwo){
  val defaults =
    o.getObjectPropertyAnnotationsFor(i)(o.getHasDefaultValue).
      flatMap{ x => PropertyDescriptionIndividual(x._1.asOWLNamedIndividual, Set()).getProperties.map( _ -> x._2.headOption ) }.
      filterNot(_._2.isEmpty).map(t => t._1 -> (t._2.get.getValue match {
      case v : OWLLiteral => Some(v.getLiteral)
      case _ => None
    })).filter(_._2.nonEmpty).map(t => t._1 -> t._2.get)

  val forComponent = getForComponent(i)
  val aspectClass = getMostSpecificType(o.getTypes(i))
  val aspectName = getName(i).split('_').last
  val providings = getAnnotatedParams(o.getProvidesPropertyProp)
  val requirings = getAnnotatedParams(o.getRequiresPropertyProp)
  val createParams = getAnnotatedParams(o.getHasCreateParameterProp)
  val pkg = forComponent.collect{ case p => p.getPackage }.getOrElse("simx.core") + ".ontology.aspects"

  val parameters = (providings.filterNot(createParams.keys.contains) ++ createParams).
    map( v => new Parameter(v._1, v._2, defaults) ).toList.
    sortWith((a, b) => (a.defaultValue.isEmpty && b.defaultValue.nonEmpty) || a.name < b.name )

  override def toString: String =
    makeAspectString(aspectClass, forComponent, providings, requirings, createParams )

  private def getAnnotatedParams(prop : OWLObjectProperty) = {
    val createParameters = o.getObjectPropertyExpressions(i)(prop)
    createParameters.foldLeft(Map[PropertyDescriptionIndividual, Set[String]]()){
      (map, propertyAssertion) =>
        if (propertyAssertion.getObject.isNamed) {
          val annotations = propertyAssertion.getAnnotations.filter(_.getProperty == o.getHasAnnotation).map {
            annotation =>
              val visitor = new AnnotationValueIRIVisitor
              annotation.getValue.accept(visitor)
              visitor.returnValue.map(getName(_).split("#").last)
          }.filterNot(_.isEmpty).map(_.get)
          val individual = PropertyDescriptionIndividual(propertyAssertion.getObject.asOWLNamedIndividual(), annotations.toSet)
          val ret = map.updated(individual, map.getOrElse(individual, Set()) ++ annotations)
          if (ret.count(v => v._1.i.getIRI == individual.i.getIRI) > 1)
            ret.filterNot(v => v._1.i.getIRI == individual.i.getIRI && v._2.isEmpty)
          else
            ret
        } else
          map
    }
  }

  private def prepareAnnotated(tuple : (PropertyDescriptionIndividual, Set[String])) = {
    val name = tuple._1.getName
    if (name.isEmpty)
      name
    else if (tuple._2.isEmpty)
      name
    else
      tuple._1.getPropertyNames.head + ".withAnnotations(Symbols." + tuple._2.map(deCap).mkString(", Symbols.") + ")"
  }

  private def makeAspectString(name : Option[OWLClassExpression], forComponent : Option[ComponentIndividual],
                               providings : Map[PropertyDescriptionIndividual, Set[String]], requirings : Map[PropertyDescriptionIndividual, Set[String]],
                               createParams : Map[PropertyDescriptionIndividual, Set[String]]) = name match {
    case Some(n) if !n.isAnonymous && forComponent.isDefined =>
      val preparedProvs = providings.map(prepareAnnotated).filterNot(_.isEmpty)
      val preparedReqs  = requirings.map(prepareAnnotated).filterNot(_.isEmpty)
      val combined = preparedProvs ++ preparedReqs
      val aspectType = getName(n.asOWLClass())
      "package " + pkg + " {\n" +
        "case class " + aspectName + "(\n\t" +
        (if (parameters.nonEmpty)  parameters.map(_.optionString).mkString(",\n\t") + ",\n"  else "") +
        "\ttargetComponents : List[Symbol] = Nil\n) " +
        "extends EntityAspect(Symbols." + deCap(getName(forComponent.get.getComponentType.get.asOWLClass())) +
        ", Symbols." + deCap(aspectType) +", targetComponents) with Serializable \n{\n" +
        "\tdef getFeatures = Set(" + combined.mkString(", ")+ ")\n\n" +
        "\tdef getProvidings = Set(" + preparedProvs.mkString(", ") + ")\n\n " +
        "\tdef getCreateParams = { \n\t\tval cParams = NamedSValSet(aspectType)\n\t\t" +
        createParams.map( x => "if ("+deCap(x._1.getName)+".isLeft) cParams += " + prepareAnnotated(x) + "(" + deCap(x._1.getName) + ".left.get)" ).mkString("\n\t\t") +
        "\n\t\tcParams\n\t}\n}\n}"
    case _ => ""
  }


  protected def getMostSpecificType(in : Set[OWLClassExpression]) : Option[OWLClassExpression] =
    in.find( p => !in.exists{ that => !that.isAnonymous && o.getSuperClasses(that.asOWLClass()).contains(p) })

  protected def getForComponent(aspectIndividual : OWLIndividual) =
    o.getObjectProperties(aspectIndividual)(o.getForComponentProp).map(ComponentIndividual(_)(o)).headOption
}

case class PropertyDescriptionIndividual(i : OWLNamedIndividual, annotations : Set[String])(implicit o : OntoGenTwo){
  import simx.components.ontology.generation.helper.OWLFunctions

  protected def getDescribedProperty(in : OWLNamedIndividual)  =
    o.getTypes(in).filter(_.getObjectPropertiesInSignature.contains(o.getDescribesProp)).flatMap(_.getClassesInSignature)

  def getProperties : Set[OWLClass] =
    getDescribedProperty(i)

  def getName =
    if (annotations.nonEmpty) deCap(annotations.mkString("_")) else  getProperties.map(OWLFunctions.getName).head

  def getPropertyNames =
    getProperties.map(OWLFunctions.getName)

  def getDataTypes = {
    getPropertyNames.map("simx.core.ontology.types." + _ + ".dataType")
  }

  def getAnnotatedParams(prop : OWLObjectProperty) = {
    o.getObjectPropertyExpressions(i)(prop).foldLeft(Set[PropertyDescriptionIndividual]()){
      (set, propertyAssertion) =>
        if (propertyAssertion.getObject.isNamed) {
          val annotations = propertyAssertion.getAnnotations.filter(_.getProperty == o.getHasAnnotation).map {
            annotation =>
              val visitor = new AnnotationValueIRIVisitor
              annotation.getValue.accept(visitor)
              visitor.returnValue.map(OWLFunctions.getName(_).split("#").last)
          }.filterNot(_.isEmpty).map(_.get)
          val individual = PropertyDescriptionIndividual(propertyAssertion.getObject.asOWLNamedIndividual(), annotations.toSet)
          val ret = set + individual
          if (ret.count(v => v.i.getIRI == individual.i.getIRI) > 1)
            ret.filterNot(v => v.i.getIRI == individual.i.getIRI && v.annotations.isEmpty)
          else
            ret
        } else
          set
    }
  }

}

case class Parameter(name : String, dataType : String, defaultValue : Option[String], annotations : Set[String]){
  def this(p : PropertyDescriptionIndividual, annotations : Set[String], defaults : Map[OWLClass, String] = Map()) =
    this(deCap(p.getPropertyNames.head), p.getDataTypes.head, defaults.get(p.getProperties.head), annotations )

  override def toString: String =
    getName + " : " + dataType + defaultValue.collect{ case dv => " = " + dv }.getOrElse("")

  def optionString : String =
    getName + " : Either[" + dataType + ", Null] " + defaultValue.collect{ case dv => " = Left(" + dv + ")" }.getOrElse("")

  private def getName =
    if (annotations.isEmpty) name else deCap(annotations.mkString("_"))
}

case class ComponentIndividual(i : OWLIndividual)(implicit o : OntoGenTwo){
  def getComponentType : Option[OWLClassExpression] =
    o.getTypes(i).find(c => !c.isAnonymous && o.getSuperClasses(c.asOWLClass(), recurse = false).contains(o.getComponentClass))

  def getPackage : String =
    o.getDataProperties(i)(o.getInPackageProp).headOption.collect{
      case x => x.getLiteral
    }.getOrElse("simx.core")
}

class OntologyAspectMember(owlClass : OWLClass)(implicit o : OntoGenTwo) extends OntologyMember(owlClass)(o){
  override def getAspectDescriptions: List[String] =
    getIndividuals.map{ new OntologyAspectIndividual(_).toString }.toList
}


class AnnotationValueIRIVisitor extends OWLAnnotationValueVisitor{
  var returnValue : Option[IRI] = None
  override def visit(iri: IRI): Unit = returnValue = Some(iri)

  override def visit(owlAnonymousIndividual: OWLAnonymousIndividual): Unit = {}

  override def visit(owlLiteral: OWLLiteral): Unit = {}
}
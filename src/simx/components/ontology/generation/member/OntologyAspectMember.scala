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
      flatMap{ x => PropertyDescriptionIndividual(x._1.asOWLNamedIndividual).getProperties.map( _ -> x._2.headOption ) }.
      filterNot(_._2.isEmpty).map(t => t._1 -> (t._2.get.getValue match {
      case v : OWLLiteral => Some(v.getLiteral)
      case _ => None
    })).filter(_._2.nonEmpty).map(t => t._1 -> t._2.get)

  val forComponent = getForComponent(i)
  val name = getMostSpecificType(o.getTypes(i))
  val providings = toPropDescIndividual(o.getProvidesParameterProp)
  val requirings = toPropDescIndividual(o.getRequiresParameterProp)
  val parameters = providings.map( new Parameter(_, defaults) ).toList.
    sortWith((a, b) => (a.defaultValue.isEmpty && b.defaultValue.nonEmpty) || a.name < b.name )

  override def toString: String =
    makeAspectString(name, forComponent, providings, requirings )

  private def toPropDescIndividual(prop : OWLObjectProperty) =
    o.getObjectProperties(i)(prop).flatMap{ _.getIndividualsInSignature.map(PropertyDescriptionIndividual(_)(o)) }

  private def makeAspectString(name : Option[OWLClassExpression], forComponent : Option[ComponentIndividual],
                               providings : Set[PropertyDescriptionIndividual], requirings : Set[PropertyDescriptionIndividual]) = name match {
    case Some(n) if !n.isAnonymous && forComponent.isDefined =>
      val preparedProvs = providings.map(_.getPropertyNames.head)
      val preparedReqs  = requirings.map(_.getPropertyNames.head)
      val combined = preparedProvs ++ preparedReqs
      val aspectName = getName(n.asOWLClass())

      "case class " + aspectName + "(\n\t" + parameters.map(_.optionString).mkString(",\n\t") + ",\n\ttargetComponents : List[Symbol] = Nil\n) " +
        "extends EntityAspect(Symbols." + deCap(getName(forComponent.get.getComponentType.get.asOWLClass())) +
        ", Symbols." + deCap(aspectName) +", targetComponents) with Serializable \n{\n" +
        "\tdef getFeatures = Set(" + combined.mkString(", ")+ ")\n\n" +
        "\tdef getProvidings = Set(" + preparedProvs.mkString(", ") + ")\n\n " +
        "\tdef getCreateParams = { \n\t\tval cParams = NamedSValSet(aspectType)\n\t\t" +
        preparedProvs.map( x => "if ("+deCap(x)+".isLeft) cParams += " + x + "(" + deCap(x) + ".left.get)" ).mkString("\n\t\t") +
        "\n\t\tcParams\n\t}\n}"
    case _ => ""
  }


  protected def getMostSpecificType(in : Set[OWLClassExpression]) : Option[OWLClassExpression] =
    in.find( p => !in.exists{ that => !that.isAnonymous && o.getSuperClasses(that.asOWLClass()).contains(p) })

  protected def getForComponent(aspectIndividual : OWLIndividual) =
    o.getObjectProperties(aspectIndividual)(o.getForComponentProp).map(ComponentIndividual(_)(o)).headOption
}

case class PropertyDescriptionIndividual(i : OWLNamedIndividual)(implicit o : OntoGenTwo){
  protected def getDescribedProperty(in : OWLNamedIndividual)  =
    o.getTypes(in).filter(_.getObjectPropertiesInSignature.contains(o.getDescribesProp)).flatMap(_.getClassesInSignature)

  def getProperties : Set[OWLClass] =
    getDescribedProperty(i)

  def getPropertyNames =
    getProperties.map(getName)

  def getDataTypes = {
    getPropertyNames.map("simx.core.ontology.types." + _ + ".dataType")
  }
}

case class Parameter(name : String, dataType : String, defaultValue : Option[String]){
  def this(p : PropertyDescriptionIndividual, defaults : Map[OWLClass, String] = Map()) =
    this(deCap(p.getPropertyNames.head), p.getDataTypes.head, defaults.get(p.getProperties.head) )

  override def toString: String =
    name + " : " + dataType + defaultValue.collect{ case dv => " = " + dv }.getOrElse("")

  def optionString : String =
    name + " : Either[" + dataType + ", Null] " + defaultValue.collect{ case dv => " = Left(" + dv + ")" }.getOrElse("")
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

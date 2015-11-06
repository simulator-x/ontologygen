/*
 * Copyright 2012 The SIRIS Project
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
import simx.components.ontology.generation.helper.{OWLFunctions, DescriptionStub}

import scala.collection.JavaConversions.asScalaSet


/**
 * User: dwiebusch
 * Date: 22.11.11
 * Time: 16:14
 */


object OntologyMember{
  private var registry = Map[OWLIndividual, OntologyMember]()
  def register( key : OWLIndividual, value : OntologyMember ) {
    registry = registry.updated(key, value)
  }

  def apply( key : OWLClass ) : Option[OntologyMember] =
    registry.find(_._2.owlClass equals key).collect{ case pair => pair._2 }

  def apply( key : OWLIndividual) : Option[OntologyMember] =
    registry.get(key)

  def apply(owlClass : OWLClass, o : OntoGenTwo ) = {
    import o._
    if (owlClass == getEntityDescClass || getSuperClasses(owlClass).contains(getEntityDescClass))
      new OntologyEntityDescription(owlClass)(o)
    else if (getSuperClasses(owlClass).contains(getEntityClass))
      new OntologySemTrait(owlClass)(o)
    else if (owlClass == getActionClass || getSuperClasses(owlClass).contains(getActionClass))
      new OntologyActionClass(owlClass)(o)
    else if (owlClass == getFunctionClass || getSuperClasses(owlClass).contains(getFunctionClass))
      new OntologyFunctionClass(owlClass)(o)
    else if (getSuperClasses(owlClass).contains(getRelationClass))
      new OntologyRelationDescription(owlClass)(o)
    else if (getSuperClasses(owlClass).contains(getAspectClass))
      new OntologyAspectMember(owlClass)(o)
    else if (owlClass == getSVarDescriptionClass)
      new OntologySVarDescription(owlClass)(o)
    else if (owlClass == getEventDescriptionClass || getSuperClasses(owlClass).contains(getEventDescriptionClass))
      new OntologyEventDescription(owlClass)(o)
    else
      PlainOntologyMember(owlClass)(o)
  }

  val sVarDescClassName = "simx.core.ontology.SValDescription"
  val nullTypeClassName = "simx.core.ontology.types.NullType"
}

abstract class OntologyMember( val owlClass : OWLClass)(implicit o : OntoGenTwo ) extends Writable {
  getIndividuals.foreach( OntologyMember.register(_, this) )

  def isEntity : Boolean = false

  def getName : String =
    owlClass.toStringID.replaceAll(".*#", "")

  protected def getDescriptionStub : Option[DescriptionStub] =
    o.getAnonymousSuperClasses(owlClass).map( handleAnonymousSuperClass ).foldLeft(None : Option[DescriptionStub]){
      (a, b) => b.merge(a.getOrElse(DescriptionStub()))
    }

  protected def getEntityName : String =
    "simx.core.ontology.entities." + getName

  private def createStub(prop : OWLObjectPropertyExpression, value : OWLClass, in : DescriptionStub) =
    if (prop equals o.getHasAProp)
      DescriptionStub(value :: in.has, in.hasAspect, in.oneOf)
    else if (prop equals o.getHasAspectObjectProp)
      DescriptionStub(in.has, value :: in.hasAspect, in.oneOf)
    else
      in

  protected def handleAnonymousSuperClass( c : OWLClassExpression ) : DescriptionStub = c match {
    case cardinality : OWLObjectExactCardinality =>
      createStub(cardinality.getProperty, cardinality.getFiller.asOWLClass(), DescriptionStub())
    case someValuesFrom : OWLObjectSomeValuesFrom =>
      createStub(someValuesFrom.getProperty, someValuesFrom.getFiller.asOWLClass(), DescriptionStub())
    case unionOf : OWLObjectUnionOf =>
      val union = unionOf.getOperands.map(handleAnonymousSuperClass)
      DescriptionStub(Nil, Nil, if (union.nonEmpty && !union.head.isEmpty) union.toList else Nil)
    case expr =>
      println("unexpected type: " + expr)
      DescriptionStub()
  }

  def getIndividuals : Set[OWLIndividual] =
    o.getIndividuals(owlClass)

  protected def getEntitySVarDescription : String =
    "object "+ getName +" extends simx.core.ontology.EntitySValDescription(BaseValueDescription(simx.core.ontology.Symbols."+deCap(getName) + ")" +
      ", new simx.core.ontology.entities." + getName + "(_, _), \"" + owlClass.toStringID + "\" )"

  protected def getDescribedClasses(i : OWLIndividual) =
    o.getTypes(i).filter{ _.getObjectPropertiesInSignature.contains(o.getDescribesProp) }.
      map(_.getClassesInSignature.headOption).
      filter(_.isDefined).
      map(_.get)

  protected def typeString( i : OWLIndividual ) : Option[String] = getDataType(i).map { dt =>
    "classOf[" + getTypeString(dt).getOrElse(OWLFunctions.getName(dt)) + "]"
  }

  protected def getBaseDataType( i : OWLIndividual ) : Option[OWLIndividual] =
    getBase(i).collect{
      case base if base != i => getDataType(base) match {
        case None => getBaseDataType(base)
        case data => data
      }
    }.getOrElse(None)

  def getTargetComponent( i : OWLIndividual ) : String =
    o.getObjectProperties(i)(o.getForComponentProp).headOption.collect{
      case x => o.getDataProperties(x)(o.getInPackageProp).headOption.collect{
        case literal => literal.getLiteral
      }.getOrElse("simx.core")
    }.getOrElse("simx.core")

  protected def getAnnotations( i : OWLIndividual ) : Set[String] =
    o.getObjectProperties(i)(o.getHasAProp).map{
      x => OntologyMember(x).collect{ case something => something.getName }.getOrElse("")
    }

  def getBase( i : OWLIndividual ) : Option[OWLIndividual] =
    o.getObjectProperties(i)(o.getBasedOnProp).headOption

  def getMostGeneralBase( i : OWLIndividual ) : OWLIndividual = getBase(i) match {
    case Some(base) => getMostGeneralBase(base)
    case None => i
  }

  protected def getTypeString( i : OWLIndividual ) : Option[String] =
    o.getDataProperties(i)(o.getHasTypeStringProp).headOption.map( _.getLiteral )

  protected def getDataType( i : OWLIndividual ) : Option[OWLIndividual] =
    o.getObjectProperties(i)(o.getDataTypeProp).headOption

  protected def isRelation : Boolean = false

  protected def generatesSVar( i : OWLIndividual ) : Boolean =
    getIndividuals.nonEmpty

  override def equals(p1: Any) = p1 match {
    case that : OntologyMember => that.owlClass.equals(owlClass)
    case _ => false
  }

  override def hashCode() =
    owlClass.hashCode()
}
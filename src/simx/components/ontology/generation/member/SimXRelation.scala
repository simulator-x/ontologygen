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
import simx.components.ontology.generation.helper.OWLFunctions

import scala.collection.JavaConversions._

/**
 *
 * Created by dennis on 25.06.14.
 */
object SimXRelation{
  val baseDataProp = "SimX_UnaryRelation"
  val baseObjectProp = "SimX_BinaryRelation"
  val typesPrefix = "simx.core.ontology.types."

  def parse(ontology : OWLOntology) : Set[SimXRelation] = {
    val manager = ontology.getOWLOntologyManager
    //val dataProps =
    ontology.getDataPropertiesInSignature(true).find( _.toStringID.endsWith("#"+baseDataProp)).collect{
      case prop => create (prop, ontology.getOWLOntologyManager.getOntologies)
    }.getOrElse(Set[SimXRelation]()).toSet ++
    //val objectProps =
      ontology.getObjectPropertiesInSignature(true).find( _.toStringID.endsWith("#"+baseObjectProp)).collect{
      case prop => create(prop, ontology.getOWLOntologyManager.getOntologies)
    }.getOrElse(Set[SimXRelation]())
  }


  protected def create(baseProp : OWLDataPropertyExpression, ontologies : java.util.Set[OWLOntology]) : Set[SimXRelation] =
    baseProp.getSubProperties(ontologies).foldLeft(Set[SimXRelation]()) {
      (set, owlLink) => set + SimXDataRelation(owlLink, owlLink.getRanges(ontologies).toSet)
    }

  protected def create(baseProp : OWLObjectPropertyExpression, ontologies : java.util.Set[OWLOntology]) : Set[SimXRelation] =
    baseProp.getSubProperties(ontologies).foldLeft(Set[SimXRelation]()) {
      (set, owlLink) => set + SimXObjectRelation(owlLink, owlLink.getDomains(ontologies).toSet, owlLink.getRanges(ontologies).toSet)
  }
}

protected abstract class SimXRelation(expr : OWLProperty[_, _]) extends Writable{
  protected def toTypeString : String

  def getName : String =
    OWLFunctions.getName(expr)

  def getSVarDescriptions : Map[String, String] =
    Map("simx.core.ontology" -> toTypeString)

  override def getEntityDescription =
    None

  override def getEntityString =
    None

  override def toString: String =
    "SimXRelation " + getName + ":\n--------------------------------------------\n" +
      "Symbol:\n\t" + getSymbolString + "\n" + "Descriptions:\n\t" + getSVarDescriptions.mkString("\n\t")
}

case class SimXDataRelation(owlLink : OWLDataPropertyExpression,
                            ranges : Set[OWLDataRange])
  extends SimXRelation(owlLink.asOWLDataProperty())
{
  protected def toTypeString: String =
    "object " + getName.capitalize + " extends SVarDescription[Boolean, Boolean](" +
      SimXRelation.typesPrefix + "Boolean as Symbols." + deCap(getName) + " definedAt " + owlLink.toString.replaceAll("[<>]", "\"") +")"
}

case class SimXObjectRelation(owlLink : OWLObjectPropertyExpression,
                              domains : Set[OWLClassExpression],
                              ranges : Set[OWLClassExpression])
  extends SimXRelation(owlLink.asOWLObjectProperty())
{
  protected def toTypeString: String = {
    val subjType = SimXRelation.typesPrefix + domains.headOption.collect { case domain => OWLFunctions.getName(domain.asOWLClass())}.getOrElse("Entity")
    val objType = SimXRelation.typesPrefix + ranges.headOption.collect { case range => OWLFunctions.getName(range.asOWLClass())}.getOrElse("Entity")

    "object " + getName.capitalize + " extends simx.core.svaractor.unifiedaccess.RelationDescription[" + subjType + ".dataType, " + objType + ".dataType](" +
      subjType + ", Symbols." + getName + ", " + objType + ", " + owlLink.toString.replaceAll("[<>]", "\"") +  ")"
  }
}



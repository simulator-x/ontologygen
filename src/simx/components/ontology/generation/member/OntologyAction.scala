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

import org.semanticweb.owlapi.model.{OWLNamedIndividual, OWLIndividual}
import simx.components.ontology.generation.OntoGenTwo

/**
 * Created by dwiebusch on 01.09.14
 */
class OntologyAction (val individual : OWLIndividual)(implicit o : OntoGenTwo ) extends Writable{
  override def getName: String =
    individual.toStringID.replaceAll(".*#", "")

  override def getEntityDescription: Option[String] =
    None

  override def getEntityString: Option[String] =
    None

  override def getSVarDescriptions: Map[String, String] =
    Map()

  override def toString: String =
    "OntologyAction " + getName + " precond: " + preconditions

  private var preconditions = o.getObjectProperties(individual)(o.getHasPreconditionProp).map{
    preconditionIndividual => preconditionIndividual.asOWLNamedIndividual() -> (None, None)
  }.toMap[OWLNamedIndividual, (Option[OWLNamedIndividual], Option[OWLNamedIndividual])]

  private var positiveEffects = o.getObjectProperties(individual)(o.getHasEffectProp).map{
    effectIndividual => effectIndividual.asOWLNamedIndividual() -> (None, None)
  }.toMap[OWLNamedIndividual, (Option[OWLNamedIndividual], Option[OWLNamedIndividual])]

  o.getObjectProperties(individual)(o.getHasParameterProp).foreach{ param =>
    o.getObjectProperties(param)(o.getIsObjectOfProp).foreach{ objectOf =>
      if (preconditions.contains(objectOf.asOWLNamedIndividual()))
        preconditions = preconditions.updated(objectOf.asOWLNamedIndividual(),
          preconditions.get(objectOf.asOWLNamedIndividual()).get._1 -> Some(param.asOWLNamedIndividual()))
      else if (positiveEffects.contains(objectOf.asOWLNamedIndividual()))
        positiveEffects = positiveEffects.updated(objectOf.asOWLNamedIndividual(),
          positiveEffects.get(objectOf.asOWLNamedIndividual()).get._1 -> Some(param.asOWLNamedIndividual()))
    }
    o.getObjectProperties(param)(o.getIsSubjectOfProp).foreach{ subjectOf =>
      if (preconditions.contains(subjectOf.asOWLNamedIndividual()))
        preconditions = preconditions.updated(subjectOf.asOWLNamedIndividual(),
          Some(param.asOWLNamedIndividual()) -> preconditions.get(subjectOf.asOWLNamedIndividual()).get._2)
      else if (positiveEffects.contains(subjectOf.asOWLNamedIndividual()))
        positiveEffects = positiveEffects.updated(subjectOf.asOWLNamedIndividual(),
          Some(param.asOWLNamedIndividual()) -> positiveEffects.get(subjectOf.asOWLNamedIndividual()).get._2)
    }
  }

  private var negativeEffects =
    Map[OWLNamedIndividual, (Option[OWLNamedIndividual], Option[OWLNamedIndividual])]()


  def getPreconditions : Option[String] = {
    //println(o.getObjectProperties(individual)(o.getHasPreconditionProp).map(getPreconditionString))
    None
  }

  protected def getPreconditionString(preconIndividual : OWLIndividual) : String =
    preconIndividual.asOWLNamedIndividual().toStringID.replaceAll(".*#", "")

  def getPositiveEffects : Option[String] = {
    None
  }

  def getNegativeEffects : Option[String] = {
    None
  }
}

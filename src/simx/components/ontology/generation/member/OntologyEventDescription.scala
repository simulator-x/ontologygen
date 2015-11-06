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

import org.semanticweb.owlapi.model.{OWLObjectSomeValuesFrom, OWLClass}
import simx.components.ontology.generation.OntoGenTwo
import simx.components.ontology.generation.helper.OWLFunctions
import collection.JavaConversions._

/**
 * Created by dwiebusch on 11.04.15
 */
class OntologyEventDescription(override val owlClass : OWLClass)(implicit o : OntoGenTwo )
  extends OntologyMember(owlClass)(o) {
  override def getEventDescriptions: List[String] =
    o.getIndividuals(owlClass).flatMap{
      eventDesc =>
        val properties = o.getObjectProperties(eventDesc)(o.getProvidesPropertyProp)
        val types = properties.filter(_.isNamed).flatMap{
          propertyIndividual => PropertyDescriptionIndividual(propertyIndividual.asOWLNamedIndividual(), Set()).getPropertyNames
        }
        val symbols = eventDesc.getTypes(o.manager.getOntologies).filter(_.isAnonymous).map{
          superClass =>
            val ad = new VisitorAdapter[String](""){
              override def visit(owlObjectSomeValuesFrom: OWLObjectSomeValuesFrom): Unit = {
                if ( owlObjectSomeValuesFrom.getProperty == o.getDescribesEventProp && !owlObjectSomeValuesFrom.getFiller.isAnonymous)
                  result = OWLFunctions.getName(owlObjectSomeValuesFrom.getFiller.asOWLClass())
              }
            }
            ad.parse(superClass)
            ad.result
        }
        symbols.filterNot(_.isEmpty).map {
          symbol =>
            "object " + OWLFunctions.getName(eventDesc) + " " +
              "extends simx.core.worldinterface.eventhandling.EventDescription(" +
              "simx.core.ontology.Symbols." + deCap(symbol) +
              (if (types.nonEmpty) ", simx.core.ontology.types." + types.mkString(":: simx.core.ontology.types.") + " :: Nil "else "") +
              ")"
        }
    }.toList
}

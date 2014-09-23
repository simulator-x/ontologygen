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

import org.semanticweb.owlapi.model.OWLClass
import simx.components.ontology.generation.OntoGenTwo

/**
 * Created by dwiebusch on 01.09.14
 */
class OntologyEntityDescription(override val owlClass : OWLClass)(implicit o : OntoGenTwo )
  extends OntologyMember(owlClass)(o){
     def getEntityString : Option[String] =
       Some("class " + getName + "( e : Entity, a : SVarActor ) extends Entity(e)(a) ")

     override def isEntity: Boolean =
       true

     override protected def isRelation: Boolean =
       false

     def getSVarDescriptions : Map[String, String] =
       Map[String, String]("simx.core.ontology" -> getEntitySVarDescription)

     def getEntityDescription : Option[String] =
         Some("class " + getName + "EntityDescription( val name: Symbol, aspects : List[EntityAspect] ) " +
           "extends SpecificEntityDescription(ontology.types." + getName + ", aspects, name, Nil" +
           getDescriptionStub.collect{ case stub => ", Seq(" + stub.getFeatureString +")" }.getOrElse("") +
           ")")

     override def toString =
       "OntologyMember " + getName + ":\n--------------------------------------------\n" +
         "Symbol:\n\t" + getSymbolString + "\n" + "Descriptions:\n\t" + getSVarDescriptions.mkString("\n\t") +
         "\nAssocEntity:\n\t" + getEntityString.get + "\nEntityDescription:\n\t" + getEntityDescription.get

   }

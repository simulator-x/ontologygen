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

import org.semanticweb.owlapi.model.{OWLEntity, OWLObjectProperty, OWLIndividual}
import simx.components.ontology.generation.OntoGenTwo
import collection.JavaConversions._

/**
 * Created by dennis on 13.02.15.
 */
class FunctionDescription(ind : OWLIndividual)(implicit o : OntoGenTwo ) {
  val name =  o.getDataProperties(ind)(o.hasNameProp.get).headOption.map(_.getLiteral).getOrElse("NO_NAME")
  val operatorTypeName =  o.getObjectProperties(ind)(o.hasOperatorTypeProp.get).headOption.map(getName).getOrElse("NO_OPERATOR_TYPE_NAME")

  val parameter1Type = getClassesFor(o.hasParameter1Prop.get).headOption
  val returnType = getClassesFor(o.hasReturnValueProp.get).headOption


  def getClassesFor(objProp : OWLObjectProperty) =
    o.getTypes(ind).filter(_.getObjectPropertiesInSignature.contains(objProp)).flatMap(_.getClassesInSignature.map(getName(_)))

  protected def getName( i : OWLIndividual ) : String =
    i.toStringID.replaceAll(".*#", "")

  protected def getName( entity : OWLEntity ) : String =
    entity.toStringID.replaceAll(".*#", "")

  override def toString: String = {
    "FunctionDescription " + name + " with opType " + operatorTypeName + " param1 " + parameter1Type + " => " + returnType
  }
}

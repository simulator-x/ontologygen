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

/**
 * Created by dwiebusch on 01.09.14
 */
trait Writable {
   def getName : String
   def getEntityString : Option[String]
   def getEntityDescription : Option[String]
   def getSVarDescriptions : Map[String, String]

   def getFullName : String =
     "simx.core.ontology.types." + getName

   def getSymbolString : String =
     "val " + deCap(getName) + " = OntologySymbol(Symbol(\"" + getName.capitalize + "\"))"

   protected def deCap( s : String ) : String =
     if (s.length() < 2) s.toLowerCase else s.charAt(0).toLower + s.substring(1)
 }

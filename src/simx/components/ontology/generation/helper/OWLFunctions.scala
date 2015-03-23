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

package simx.components.ontology.generation.helper

import org.semanticweb.owlapi.model._

/**
 * Created by dwiebusch on 01.09.14
 */
object OWLFunctions{
  def cap(s: String): String =
    s.charAt(0).toUpper + s.substring(1)

  def deCap( s : String ) : String =
    if (s.length() < 2) s.toLowerCase else s.charAt(0).toLower + s.substring(1)

  def getName( iri : IRI ) : String =
    iri.toString.replaceAll(".*#", "")

  def getName( entity : HasIRI ) : String =
    getName(entity.getIRI)

  def getName( individual : OWLIndividual ) : String =
    getName(individual.asOWLNamedIndividual : OWLNamedObject)

  def indentAllLines(level: Int)(multiLineString: String) = {
    val res = new StringBuilder
    if(multiLineString.nonEmpty)
      (0 until level).foreach(i => res.append('\t'))
    multiLineString.foreach(c => {
      res.append(c)
      if (c == '\n') (0 until level).foreach(i => res.append('\t'))
    })
    res
  }
}

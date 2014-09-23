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

import java.io.File

import org.semanticweb.owlapi.model.IRI

/**
 * Created by IntelliJ IDEA.
 * User: martin
 * Date: 27/02/14
 * Time: 12:19
 */
trait OntoDefinitions {

  protected def corePath: String

  //OWLPropertyNames
  protected val hasConstructor = "hasConstructor"
  protected val forComponent = "forComponent"
  protected val hasDataType = "hasDataType"

  protected val hasPrecondition = "hasPrecondition"
  protected val hasParameter = "hasParameter"
  protected val describedBy = "describedBy"
  protected val isSubjectOf = "isSubjectOf"
  protected val isObjectOf = "isObjectOf"
  protected val hasEffect = "hasEffect"
  protected val hasRole = "hasRole"

  protected val inPackage = "inPackage"
  protected val hasAspect = "hasAspect"
  protected val basedOn = "basedOn"
  protected val has_a = "has"

  //shortcuts
  val symbolsBase = "Concept"
  val baseName    = "SVarDescription"
  val oSymbol     = "OntologySymbol"
  val oMember     = "SVarDescription"
  val nullName    = "nullType"
  val actionIRI   = IRI.create("http://www.hci.uni-wuerzburg.de/ontologies/simx/SimxCoreOntology.owl#Action")

  protected val outPkg        = ".ontology.types"
  protected val outFileNames  = "package.scala"

  //Files
  protected val symbolsFile   =
    corePath + File.separator + "core/src/simx/core/ontology/Symbols.scala"
  protected val entitiesFile  =
    corePath + File.separator + "core/src/simx/core/ontology/entities/Entities.scala"
  protected val entityDescriptionsFile =
    corePath + File.separator + "core/src/simx/core/ontology/entities/EntityDescriptions.scala"
}

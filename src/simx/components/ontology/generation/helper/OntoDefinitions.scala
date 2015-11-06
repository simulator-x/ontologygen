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

  protected def coreDir: File

  //OWLPropertyNames
  protected val hasName = "hasName"
  protected val implementedBy = "implementedBy"
  protected val hasConstructor = "hasConstructor"
  protected val forComponent = "forComponent"
  protected val hasDataType = "hasDataType"

  protected val describesProperty = "describesProperty"
  protected val hasPrecondition = "hasPrecondition"
  protected val hasParameter = "hasParameter"
  protected val providesProperty = "providesProperty"
  protected val requiresProperty = "requiresProperty"
  protected val describedBy = "describedBy"
  protected val hasSubject = "hasSubject"
  protected val hasPredicate = "hasPredicate"
  protected val hasProperty = "hasProperty"
  protected val hasObject = "hasObject"
  protected val hasEffect = "hasEffect"
  protected val hasRole = "hasRole"

  protected val inPackage = "inPackage"
  protected val hasAspect = "hasAspect"
  protected val hasValue = "hasValue"
  protected val basedOn = "basedOn"
  protected val has_a = "has"

  protected val prefixOperatorType = "PrefixOperatorType"
  protected val infixOperatorType = "InfixOperatorType"

  //shortcuts
  val symbolsBase   = "Concept"
  val descBbaseName = "SVarDescription"
  val oSymbol       = "OntologySymbol"
  val oMember       = "SVarDescription"
  val nullName      = "NullType"
  val actionIRI     = IRI.create("http://www.hci.uni-wuerzburg.de/ontologies/simx/SimxCoreOntology.owl#Action")
  val functionIRI   = IRI.create("http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/Actions.owl#Function")

  protected val outPkg        = ".ontology.types"
  protected val outFileNames  = "package.scala"

  //Files
  protected val symbolsFile   =
    coreDir + File.separator + "core/src/simx/core/ontology/Symbols.scala"
  protected val aspectsFile   =
    coreDir + File.separator + "core/src/simx/core/ontology/aspects/Aspects.scala"
  protected val semanticTraitsFile   =
    coreDir + File.separator + "core/src/simx/core/ontology/semtraits/package.scala"
  protected val actionsFile   =
    coreDir + File.separator + "core/src/simx/core/ontology/actions/Actions.scala"
  protected val entitiesFile  =
    coreDir + File.separator + "core/src/simx/core/ontology/entities/Entities.scala"
  protected val entityDescriptionsFile =
    coreDir + File.separator + "core/src/simx/core/ontology/entities/EntityDescriptions.scala"
  protected val eventDescriptionsFile =
    coreDir + File.separator + "core/src/simx/core/ontology/entities/EventDescriptions.scala"
  protected def functionFile(pkgName: String)  =
    moduleFile(pkgName, "ontology/functions/Functions.scala")
  protected def interpolatorFile(pkgName: String)  =
    moduleFile(pkgName, "ontology/functions/Interpolators.scala")
  protected def sVarDescFile(pkgName: String) =
    moduleFile(pkgName, "ontology/types/package.scala")
  protected def moduleFile(pkgName: String, pathInModule: String) = {
    val moduleDir = new File(coreDir, pkgName.replaceFirst("simx.", "").replace(".", "/"))
    val srcDir = new File(moduleDir, "src")
    val modulePackageDir = new File(srcDir, pkgName.replace(".", "/"))
    new File(modulePackageDir, pathInModule)
  }
}

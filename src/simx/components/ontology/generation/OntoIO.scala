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

package simx.components.ontology.generation

import scala.io.Source
import java.io.File

/**
 * Created by IntelliJ IDEA.
 * User: martin
 * Date: 27/02/14
 * Time: 11:55
 */
trait OntoIO extends OntoDefinitions {

  private val templatesPath = "/simx/components/ontology/generation/templates/"
  private val templatesExtension = ".scala.tpl"

  private def headerFrom(template: String, packageName: String, suffix: String = "\n\n") =
    packageName + "\n\n" + loadTemplate("HeaderComment") + "\n\n" + loadTemplate(template) + suffix

  private def loadTemplate(name: String) =
    Source.fromInputStream(getClass.getResourceAsStream(templatesPath + name + templatesExtension)).
      getLines().mkString("\n")

  private def filenameFromPackage( pkgName : String ) = {
    val dir = "." + File.separator + pkgName + File.separator + "src" + File.separator +
      (pkgName + outPkg).replaceAll("\\.", File.separator) + File.separator
    val tmp = new File(dir)
    if (!tmp.exists)
      tmp.mkdir
    dir + outFileNames
  }

  protected val symbolsHeader = headerFrom("SymbolsHeader", "package simx.core.ontology", "\n\n\t")
  protected val entitiesHeader = headerFrom("EntitiesHeader", "package simx.core.ontology.entities")
  protected val descriptionHeader = headerFrom("EntityDescriptionsHeader", "package simx.core.ontology.entities")
  protected def typesHeader(packageName: String) = headerFrom("TypesHeader", packageName, "\n\t")

}

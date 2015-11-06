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

import java.io.File
import java.net.URL

import scala.io.Source

/**
 * Created by dwiebusch on 01.09.14
 */
object OntoGenSimxOntology {
  /**
   * Generates Symbols, SVarDescriptions, and EntityDescriptions
   * from an ontology for a SimXApplication.
   * @param args Requires two space-separated strings: < SimX base directory > < SimXApplication working directory >
   */
  def main(args: Array[String]) {
    val base = args.toList.headOption.
      getOrElse(throw new Exception(
      "[error][Ontology Generation] No simx base directory in program arguments."))
    val wd = args.toList.tail.headOption.
      getOrElse(throw new Exception(
      "[error][Ontology Generation] No working directory in program arguments."))
    val p = new OntoGenTwo(new File(base))
    val simxOntologyFile = new File(wd + "/simxOntology.owl").getAbsoluteFile
    if(!simxOntologyFile.exists())
      throw new Exception("[error][Ontology Generation] No 'simxOntology.owl' found in " + wd)
        p.myLoad(simxOntologyFile)

//    val url = "localhost:8080/" + wd.replace(new File(base).getCanonicalPath, "") + "/simxOntology.owl"
//    println(Source.fromURL("http://localhost:8080" +  wd.replace(new File(base).getCanonicalPath, "") + "/simxOntology.owl").mkString)

    p.parse()
    p.shutdown()
  }
}

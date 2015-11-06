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

package simx.components.ontology.generation.server

import org.eclipse.jetty.server.{Handler, Server}
import org.eclipse.jetty.server.handler.{DefaultHandler, HandlerList, ResourceHandler}

/**
 *
 * Created by dennis on 01.06.15.
 */

object OntoFileServer{
  def main(args: Array[String]) {
    val s = new OntoFileServer(".")
  }
}

class OntoFileServer(directory : String, port : Int = 8080) {
  def stop() =
    server.stop()

  private val server =
    new Server(port)

  private val serverThread =
    new Thread() {
      override def run(): Unit = {
        val resource_handler = new ResourceHandler()
        resource_handler.setResourceBase(directory)

        val handlers = new HandlerList()
        handlers.setHandlers(Array[Handler](resource_handler, new DefaultHandler()))
        server.setHandler(handlers)

        server.start()
        server.join()
      }
    }

  serverThread.start()
}


scalaSource in Compile <<= baseDirectory(_ / "src")

unmanagedJars in Compile <<= baseDirectory map { base => ((base ** "lib") ** "*.jar").classpath }

unmanagedClasspath in Compile += baseDirectory.value / "config" 

autoCompilerPlugins := true

libraryDependencies <<= (scalaVersion, libraryDependencies) { (ver, deps) =>
    deps :+ compilerPlugin("org.scala-lang.plugins" % "scala-continuations-plugin_2.11.2" % "1.0.2")
}

ivyXML := scala.xml.XML.load( ontologyGen.base + "/ivy.xml" ) \ "dependencies"

classDirectory in Compile <<= target(_ / "scala/classes")

classDirectory in Test <<= target(_ / "scala/test-classes")

lazy val copyTemplates = taskKey[Set[java.io.File]]("Genera")

copyTemplates := IO.copy( ((baseDirectory.value / "src/simx/components/ontology/generation/templates") ** "*.tpl").get map {f => (f, baseDirectory.value / "target/scala/classes/simx/components/ontology/generation/templates" / f.getName)}  )

generateOntology <<= generateOntology dependsOn copyTemplates

mainClass := Some("simx.components.ontology.generation.OntoGenSimxOntology")
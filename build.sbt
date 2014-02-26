scalaSource in Compile <<= baseDirectory(_ / "src")

unmanagedJars in Compile <<= baseDirectory map { base => ((base ** "lib") ** "*.jar").classpath }

autoCompilerPlugins := true

libraryDependencies <<= (scalaVersion, libraryDependencies) { (ver, deps) =>
    deps :+ compilerPlugin("org.scala-lang.plugins" % "continuations" % ver)
}

scalacOptions += "-P:continuations:enable"

ivyXML := scala.xml.XML.load( ontologygen.base + "/ivy.xml" ) \ "dependencies"

classDirectory in Compile <<= target(_ / "scala/classes")

classDirectory in Test <<= target(_ / "scala/test-classes")

TaskKey[Unit]("copy-templates") := IO.copy( ((baseDirectory.value / "src/simx/components/ontology/generation/templates") ** "*.tpl").get map {f => (f, baseDirectory.value / "target/scala/classes/simx/components/ontology/generation/templates" / f.getName)}  )
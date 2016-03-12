libraryDependencies ++= Seq(
	compilerPlugin("org.scala-lang.plugins" % ("scala-continuations-plugin_" + scalaVersion.value) % "1.0.2"),
	"org.eclipse.jetty" % "jetty-server" % "9.2.15.v20160210",
	"org.scala-lang" % "scala-library-all" % scalaVersion.value,
    "com.hermit-reasoner" % "org.semanticweb.hermit" % "1.3.8.4",
    "net.sourceforge.owlapi" % "owlapi-distribution" % "3.5.0",
	"org.apache.logging.log4j" % "log4j-slf4j-impl" % "2.5",
	"org.apache.logging.log4j" % "log4j-api" % "2.5",
	"org.apache.logging.log4j" % "log4j-core" % "2.5"
)

scalaSource in Compile <<= baseDirectory(_ / "src")

unmanagedJars in Compile <<= baseDirectory map { base => ((base ** "lib") ** "*.jar").classpath }

unmanagedClasspath in Compile += baseDirectory.value / "config" 

autoCompilerPlugins := true

classDirectory in Compile <<= target(_ / "scala/classes")

classDirectory in Test <<= target(_ / "scala/test-classes")

lazy val copyTemplates = taskKey[Set[java.io.File]]("Genera")

copyTemplates := IO.copy( ((baseDirectory.value / "src/simx/components/ontology/generation/templates") ** "*.tpl").get map {f => (f, baseDirectory.value / "target/scala/classes/simx/components/ontology/generation/templates" / f.getName)}  )

generateOntology <<= generateOntology dependsOn copyTemplates

mainClass := Some("simx.components.ontology.generation.OntoGenSimxOntology")
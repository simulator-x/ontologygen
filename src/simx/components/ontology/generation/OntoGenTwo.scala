/*
 * Copyright 2012 The SIRIS Project
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

import java.lang.Object
import java.net._
import java.nio.file.{Files, Paths}

//import com.clarkparsia.pellet.owlapiv3.{PelletReasoner, PelletReasonerFactory}
import java.io._

import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.io.StringDocumentSource
import org.semanticweb.owlapi.model._
import org.semanticweb.owlapi.reasoner.{OWLReasoner, OWLReasonerFactory}
import org.semanticweb.owlapi.util.{DefaultPrefixManager, InferredOntologyGenerator}
import org.slf4j.LoggerFactory
import simx.components.ontology.generation.helper.{WritableForPackage, OWLFunctions, OntoIO, OntologyException}
import simx.components.ontology.generation.member._

import scala.collection.JavaConversions.asScalaSet
import scala.xml.XML


object OntoGenTwo{
  def main( args : Array[String]){
    val p = new OntoGenTwo(new File("."))
    val url = args.headOption.getOrElse(throw new Exception("You must provide a URL when running OntoGenTwo"))
    p.load(url)
    p.parse()
  }

  def apply[T](self : T, onlyForComponent : Option[String] = None) : OntoGenTwo = self match {
    case c : Class[_] => getInstance(c,  onlyForComponent)
    case _ => getInstance(self.getClass, onlyForComponent)
  }


  private def getInstance[T](c : Class[T], onlyForComponent : Option[String] = None) : OntoGenTwo = {
    val pkgParts = c.getPackage.getName.split("\\.").reverse
    var base = new File("").getAbsolutePath.replaceAll(pkgParts.head + ".*", pkgParts.head)
    pkgParts.dropWhile( !base.endsWith(_) ).dropWhile{ first =>
      if (base.endsWith(first)){
        base = base.substring(0, base.length - first.length -1 )
        true
      }  else false
    }
    new OntoGenTwo(new File(base), onlyForComponent)
  }
}

class OntoGenTwo(val coreDir : File, onlyForComponent : Option[String] = None) extends OntoIO {

  private implicit val parser = this
  private val log = LoggerFactory.getLogger(this.asInstanceOf[Object].getClass)

  //internal variables
  private   val cacheDir                                      = "./.ontoCache"
  var manager                                       = OWLManager.createOWLOntologyManager
  protected var prefixManager : Option[DefaultPrefixManager]  = None
  protected var ontology      : Option[OWLOntology]           = None
  protected var ontologyIRI   : Option[IRI]                   = None

  protected def setOntology( onto : OWLOntology ) {
    ontology = Some(onto)
    setOntologyIRI(onto.getOntologyID.getOntologyIRI)
    prefixManager = Some( new DefaultPrefixManager(ontologyIRI.getOrElse(IRI.create("")).toString + "#") )
  }

  def setOntologyIRI( iri : IRI ) {
    ontologyIRI = Some(iri)
  }

  def load( file : File ) : this.type =
    load(Some(IRI.create(file)))

  def load( url : String ) : this.type =
    load(Some(IRI.create(url)))

  def myLoad(f : File): Unit ={
    val online = try {
      val sock = new Socket("www.hci.uni-wuerzburg.de", 80)
      if (sock != null){
        sock.close()
        true
      } else
        false
    } catch {
      case e : UnknownHostException => false
      case e : SocketException => false
      case e : Throwable => throw e
    }

    if (online) {
      load(f)
      cache()
    } else {
      println("No internet connection available, working on cached files")
      getImports(f) map { new URL(_) } foreach _myLoad
      setOntology(manager.loadOntologyFromOntologyDocument(f))
    }
  }

  private def getImports(file : File): Seq[String] = {
    val ontologyNode = XML.loadFile(file) \\ "Ontology"
    (ontologyNode \\ "Import" map { _.text }) ++ (ontologyNode \\ "imports" map { _.attributes.asAttrMap("rdf:resource") })
  }

  private def _myLoad(url : URL): Unit = {
    val file = new File(urlToCacheFile(url.toString))
    getImports(file) map { new URL(_) } foreach _myLoad

    if (!loadedFiles.contains(url))
      loadedFiles += url -> manager.loadOntologyFromOntologyDocument(removeImports(file))
  }

  private var loadedFiles = Map[URL, OWLOntology]()

  private def removeImports(file : File) : StringDocumentSource = {
    def removeNode(toClean : String, node : String, pos : Int = -1, inside : Int = 0) : String = {
      if (inside == 0) {
        val snip1 = toClean.indexOf("<" + node + ">", pos+1)
        val snip2 = toClean.indexOf("<" + node + " ", pos+1)
        if (snip1 > 0 || snip2 > 0) {
          if (snip2 < 0 || (snip1 > 0 && snip1 < snip2))
            return removeNode(toClean, node, snip1, inside = 1)
          else
            return removeNode(toClean, node, snip2, inside = 2)
        }
      } else if (inside == 1){
        val end = toClean.indexOf("</" + node + ">", pos)
        val cutResult = toClean.substring(0, pos) + toClean.substring(end + ("</" + node + ">").length)
        return removeNode(cutResult, node, pos, 0)
      } else if (inside == 2) {
        val end = toClean.indexOf("/>", pos)
        val cutResult = toClean.substring(0, pos) + toClean.substring(end+2)
        return removeNode(cutResult, node, pos, 0)
      }
      toClean
    }

    val b = new Array[Byte](file.length().toInt)
    new FileInputStream(file).read(b)
    val p = StringBuilder.newBuilder.appendAll(b.map(_.toChar)).toString().
      replaceAll("\\<Import\\>.*\\</Import\\>", "")

    new StringDocumentSource(removeNode(p, "owl:imports"))
  }

  private def urlToCacheFile(in : String) =
    cacheDir + File.separator + in.replaceAll("[:/\\.]+", "_") + ".owl_cache"

  def cache(): Unit ={
    val p = Paths.get(cacheDir)
    if (!p.toFile.exists())
      Files.createDirectory(p)

    manager.getOntologies.foreach{
      ontology =>
        val fName = urlToCacheFile(ontology.getOntologyID.getOntologyIRI.toString)
        val sink = new File(fName)
        sink.createNewFile()
        val stream = new FileOutputStream(sink)
        manager.saveOntology(ontology, stream)
        stream.close()
    }
  }

  def load( iri : Option[IRI] = None ) : this.type = {
    if (iri.isDefined)
      ontologyIRI = iri
    if (ontologyIRI.isEmpty)
      throw OntologyException("tried to load ontology although iri was not set")
    //actually load the ontology
    if (! ontologyIRI.get.isAbsolute ) {
      println("IRI was not absoulte, trying to prepend 'file://' -> " + IRI.create("file://" + iri.get.toString))
      ontologyIRI = Some(IRI.create("file://" + iri.get.toString))
    }
    setOntology(manager.loadOntologyFromOntologyDocument(ontologyIRI.get))
    this
  }

  def parse(){
    if (ontology.isEmpty)
      throw OntologyException("No ontology was loaded, so nothing could be parsed")
    else
      init()

    members               = collectMembers(entryPoint.get) ++ SimXRelation.parse(ontology.get)
    val actionList        = collectActions(actionClass.get).flatMap(getIndividuals)
    var symbolsList       = collectMembers(getOWLClass(symbolsBase).get).map(m => m.getSymbolString)
    var svarDescList      = Set[WritableForPackage]()
    var entityStringList  = Set[String]()
    var entityDescList    = Set[String]()
    var aspectDescList    = Set[String]()
    var actionDescList    = Set[String]()
    var functionList      = Set[WritableForPackage]()
    var semtraitDescList  = Set[String]()

    //println(actionList.map(new OntologyAction(_)))

    //    ontology.foreach(o => {
    //      members.map(_.owlClass).foreach( m1 => {
    //        val olo = manager.getOWLDataFactory.
    //          getOWLObjectSomeValuesFrom(
    //            manager.getOWLDataFactory.getOWLObjectProperty("supports_calculation_of", prefixManager.get),
    //            m1
    //          )
    //        members.map(_.owlClass).foreach( m2 => {
    //          if(m2.getSuperClasses(ontology.get).toList.contains(olo))
    //            println(m2, olo)
    //        })
    //      })
    //    })


    //getIndividuals(functionClass.get).foreach(x => println(new FunctionDescription(x)))


    members.foreach{ m =>
      log.info( m + "\n" )

      symbolsList = symbolsList + m.getSymbolString
      svarDescList ++= m.getSVarDescriptions
      entityStringList ++= m.getEntityStrings
      entityDescList ++= m.getEntityDescriptions
      aspectDescList ++= m.getAspectDescriptions
      actionDescList ++= m.getActionDescriptions
      functionList ++= m.getFunctions
      semtraitDescList ++= m.getSemtraitDescriptions
    }

    if (onlyForComponent.isEmpty){
      write(symbolsFile,  symbolsHeader + interleave(4)(symbolsList.toList.sorted).mkString("\n\t") + "\n}")
      write(entitiesFile, entitiesHeader + interleave(6)(entityStringList.toList.sorted).mkString("\n"))
      write(entityDescriptionsFile,   descriptionHeader + interleave(11)(entityDescList.toList.sorted).mkString("\n"))
      write(aspectsFile, aspectsHeader + aspectDescList.mkString("\n\n"))
//      write(actionsFile, actionsHeader + "object Actions{\n\t" + actionDescList.mkString("\n\t") + "\n}" )
      write(semanticTraitsFile, semtraitHeader + "package object semtraits{\n\t" + semtraitDescList.mkString("\n\t") + "\n}" )
    }

    onlyForComponent.foreach(comp => svarDescList = svarDescList.filter(_.packageName == comp))
    onlyForComponent.foreach(comp => functionList = functionList.filter(_.packageName == comp))

    //Ensure that a function file is written for every module, that has a types file
    functionList ++= svarDescList.map(_.packageName).map(createDummyFunction)

    writeForPackage(functionList, functionsHeader, functionFile)
    writeForPackage(svarDescList, typesHeader, sVarDescFile, 1, Some((7, 1)))
  }

  def createDummyFunction(pkgName: String) = new WritableForPackage {
    def packageName: String = pkgName
    def toScalaCode: String = ""
  }

  def toAnnotationLiteral(a: OWLAnnotation): Option[String] = a.getValue match {
    case l: OWLLiteral => Some(l.getLiteral)
    case _ => None
  }

  def getAnnotationFor(i : OWLIndividual)(annoProp : OWLAnnotationProperty): List[String] = i match {
    case e: OWLEntity =>
      manager.getOntologies.map(o => {
        e.getAnnotations(o, annoProp).map(toAnnotationLiteral).flatten
      }).flatten.toList
    case _ => Nil
  }

  def getObjectPropertyAnnotationsFor(i : OWLIndividual)(aProp : OWLAnnotationProperty) : Map[OWLIndividual, Set[OWLAnnotation]] = {
    manager.getOntologies.foldLeft(Map[OWLIndividual, Set[OWLAnnotation]]()){
      (map, o) => o.getObjectPropertyAssertionAxioms(i).foldLeft(map){
        (m, ax) => m.updated(ax.getObject, m.getOrElse(ax.getObject, Set()) ++ ax.getAnnotations.filter(_.getProperty equals aProp))
      }.filterNot(_._2.isEmpty)
    }
  }

  protected def interleave(p: Int, amount: Int)(in : List[String]) : List[String] =
    interleave(p: Int, (0 until amount).map(i => "").toList)(in : List[String])

  protected def interleave(p : Int, toInsert: List[String] = List(""))(in : List[String]) : List[String] = in match {
    case head :: neck :: tail =>
      head :: (if (head.charAt(p) == neck.charAt(p)) interleave(p, toInsert)(neck :: tail) else toInsert ::: interleave(p, toInsert)(neck :: tail))
    case list => list
  }

  private def createParentDirs(f : File, isParent : Boolean = false){
    if (!f.exists()){
      createParentDirs(f.getParentFile, isParent = true)
      if (isParent) f.mkdir() else f.createNewFile()
    }
  }

  protected def write(filename : String, toWrite : String){
    val outFile = new File(filename)
    createParentDirs(outFile)
    val writer = new BufferedWriter(new FileWriter(outFile))
    writer.write(toWrite)
    writer.close()
  }

  protected def writeForPackage(
    toWrite: Iterable[WritableForPackage],
    pkgToHeader: (String) => String,
    pkgToFile: (String) => File,
    indent: Int = 1,
    spacing: Option[(Int,Int)] = None
  ) {
    toWrite.groupBy(_.packageName).foreach{ group => {
      val pkgName = group._1
      var singleItemCode = group._2.toList.map(_.toScalaCode).sorted
      //TODO 'User' and 'Material' are generated twice. Is this a false specification in the ontology?
      singleItemCode = singleItemCode.distinct
      spacing.foreach{s => singleItemCode = interleave(s._1, s._2)(singleItemCode)}
      val itemCode = OWLFunctions.indentAllLines(indent)(singleItemCode.mkString("\n"))
      val code = pkgToHeader(pkgName) + itemCode + "\n}"
      write(pkgToFile(pkgName), code)
    }}
  }






  protected def write(filename : File, toWrite : String) {
    write(filename.getAbsolutePath, toWrite)
  }


  def collectMembers( c : OWLClass) : Set[OntologyMember] =
    collectMembers(c, Set())

  protected def collectMembers( c : OWLClass, prev : Set[OntologyMember] ) : Set[OntologyMember] =
    prev ++ asScalaSet(c.getSubClasses(manager.getOntologies)).flatMap{
      c => if (c.isAnonymous) prev.empty else collectMembers(c.asOWLClass, Set(OntologyMember(c.asOWLClass(), this)))
    }

  def getSVarDescriptionIndividuals: Set[OWLIndividual] =
    members.map{case desc : OntologySVarDescription => Some(desc); case _ => None}.flatten.map(_.getIndividuals).flatten

  protected def collectActions( c : OWLClass) : Set[OWLClass] =
    asScalaSet(c.getSubClasses(manager.getOntologies)).flatMap(x => collectActions(x.asOWLClass())).toSet + c

  private var effectClass : Option[OWLClass] = None
  private var aspectClass : Option[OWLClass] = None
  private var entityClass : Option[OWLClass] = None
  private var entityDescClass : Option[OWLClass] = None
  private var sVarDescClass : Option[OWLClass] = None
  private var entryPoint : Option[OWLClass] = None
  private var actionClass : Option[OWLClass] = None
  private var functionClass : Option[OWLClass] = None
  private var relationClass : Option[OWLClass] = None
  private var propertyClass : Option[OWLClass] = None
  private var componentClass : Option[OWLClass] = None
  private var preconditionClass : Option[OWLClass] = None
  private var nullType : Option[OWLIndividual] = None
  private var ctorProp : Option[OWLDataProperty] = None
  private var inPackageProp : Option[OWLDataProperty] = None
  private var hasValueProp : Option[OWLDataProperty] = None
  private var hasTypeStringProp : Option[OWLDataProperty] = None
  private var hasRoleProp   : Option[OWLObjectProperty] = None
  private var hasEffectProp : Option[OWLObjectProperty] = None
  private var hasAObjectProp : Option[OWLObjectProperty] = None
  private var baseObjectProp : Option[OWLObjectProperty] = None
  private var hasSubjectProp : Option[OWLObjectProperty] = None
  private var hasPredicateProp : Option[OWLObjectProperty] = None
  private var hasPropertyProp : Option[OWLObjectProperty] = None
  private var describedByProp : Option[OWLObjectProperty] = None
  private var describesProp : Option[OWLObjectProperty] = None
  private var hasObjectProp : Option[OWLObjectProperty] = None
  private var hasParameterProp : Option[OWLObjectProperty] = None
  private var requiresParameterProp : Option[OWLObjectProperty] = None
  private var providesParameterProp : Option[OWLObjectProperty] = None
  private var forCompObjectProp : Option[OWLObjectProperty] = None
  private var dataTypeObjectProp : Option[OWLObjectProperty] = None
  private var hasAspectObjectProp : Option[OWLObjectProperty] = None
  private var hasPreconditionProp : Option[OWLObjectProperty] = None
  private var hasDefaultValueAnnotation : Option[OWLAnnotationProperty] = None
  private var commentAnnotation : Option[OWLAnnotationProperty] = None
  private var overridesProvideAnnotation : Option[OWLAnnotationProperty] = None
  var hasParameter1Prop : Option[OWLObjectProperty] = None
  var hasReturnValueProp : Option[OWLObjectProperty] = None
  var hasOperatorTypeProp : Option[OWLObjectProperty] = None
  var hasNameProp : Option[OWLDataProperty] = None
  var implementedByProp : Option[OWLDataProperty] = None
  var members: Set[Writable] = Set()
  var reasoner: Option[OWLReasoner] = None

  private def init() {

    ontology.foreach { o =>
      println("Inferring axioms")

      val factory: OWLReasonerFactory =
        new org.semanticweb.HermiT.Reasoner.ReasonerFactory()
      //        //PelletReasonerFactory.getInstance()

      reasoner =
        Some(factory.createReasoner(o))//, new SimpleConfiguration(new ConsoleProgressMonitor()))

      val inferredOntologyGenerator = new InferredOntologyGenerator(reasoner.get)
      inferredOntologyGenerator.fillOntology(manager, o)
      println("Axioms inferred")
    }

    val dataProps = asScalaSet(manager.getOntologies).foldLeft(Set[OWLDataProperty]()){
      (set, onto) => set ++ onto.getDataPropertiesInSignature.toSet
    }
    val objectProps = asScalaSet(manager.getOntologies).foldLeft(Set[OWLObjectProperty]()){
      (set, onto) => set ++ onto.getObjectPropertiesInSignature.toSet
    }
    nullType            = getOWLClass(nullName).collect{
      case c => c.getIndividuals(manager.getOntologies).iterator().next()
    }

    entityClass         = getOWLClass("Entity")
    entityDescClass     = getOWLClass("EntityDescription")
    effectClass         = getOWLClass("Effect")
    aspectClass         = getOWLClass("Aspect")
    relationClass       = getOWLClass("Relation")
    preconditionClass   = getOWLClass("Precondition")
    propertyClass       = getOWLClass("Property")
    componentClass      = getOWLClass("Component")
    entryPoint          = getOWLClass("Siris_Concept")
    sVarDescClass       = getOWLClass(descBbaseName)
    actionClass         = getOWLClass(actionIRI)
    functionClass       = getOWLClass(functionIRI)
    hasAObjectProp      = objectProps.find( OWLFunctions.getName(_) equals has_a )
    baseObjectProp      = objectProps.find( OWLFunctions.getName(_) equals basedOn )
    hasAspectObjectProp = objectProps.find( OWLFunctions.getName(_) equals hasAspect )
    hasPreconditionProp = objectProps.find( OWLFunctions.getName(_) equals hasPrecondition )
    dataTypeObjectProp  = objectProps.find( OWLFunctions.getName(_) equals hasDataType )
    forCompObjectProp   = objectProps.find( OWLFunctions.getName(_) equals forComponent )
    hasParameterProp    = objectProps.find( OWLFunctions.getName(_) equals hasParameter )
    providesParameterProp = objectProps.find( OWLFunctions.getName(_) equals providesParameter )
    requiresParameterProp = objectProps.find( OWLFunctions.getName(_) equals requiresParameter )
    describedByProp     = objectProps.find( OWLFunctions.getName(_) equals describedBy )
    describesProp       = objectProps.find( OWLFunctions.getName(_) equals describesProperty )
    hasSubjectProp      = objectProps.find( OWLFunctions.getName(_) equals hasSubject )
    hasObjectProp       = objectProps.find( OWLFunctions.getName(_) equals hasObject )
    hasPredicateProp    = objectProps.find( OWLFunctions.getName(_) equals hasPredicate )
    hasPropertyProp     = objectProps.find( OWLFunctions.getName(_) equals hasProperty )
    hasEffectProp       = objectProps.find( OWLFunctions.getName(_) equals hasEffect )
    hasRoleProp         = objectProps.find( OWLFunctions.getName(_) equals hasRole )
    hasOperatorTypeProp = objectProps.find( OWLFunctions.getName(_) equals "hasOperatorType" )
    hasReturnValueProp  = objectProps.find( OWLFunctions.getName(_) equals "hasReturnValue" )
    hasParameter1Prop   = objectProps.find( OWLFunctions.getName(_) equals "hasParameter1" )
    hasDefaultValueAnnotation = getAnnotationProperty("hasDefaultValue")
    commentAnnotation   = getAnnotationProperty("comment")
    overridesProvideAnnotation = getAnnotationProperty("overridesProvide")

    inPackageProp       = dataProps.find( OWLFunctions.getName(_) equals inPackage )
    hasValueProp        = dataProps.find( OWLFunctions.getName(_) equals hasValue )
    hasTypeStringProp   = dataProps.find( OWLFunctions.getName(_) equals "hasTypeString" )
    ctorProp            = dataProps.find( OWLFunctions.getName(_) equals hasConstructor )
    hasNameProp         = dataProps.find( OWLFunctions.getName(_) equals hasName )
    implementedByProp   = dataProps.find( OWLFunctions.getName(_) equals implementedBy )
  }

  protected def getAnnotationProperty( name : String ) : Option[OWLAnnotationProperty] =
    manager.getOntologies.flatMap(_.getAnnotationPropertiesInSignature).find(_.toStringID.endsWith("#" + name ))

  protected def getOWLClass( name : String ) : Option[OWLClass] = ontology match {
    case Some(o) => o.getClassesInSignature(true).toArray(Array[OWLClass]()).find( _.toStringID.endsWith("#"+name))
    case None => None
  }

  protected def getOWLClass( iri : IRI ) : Option[OWLClass] = ontology match {
    case Some(o) => o.getClassesInSignature(true).toArray(Array[OWLClass]()).find( _.getIRI.equals(iri) )
    case None => None
  }

  def getNullTypeClass : OWLIndividual =
    nullType.get

  def getEntityDescClass : OWLClass =
    entityDescClass.get

  def getEntityClass : OWLClass =
    entityClass.get

  def getAspectClass : OWLClass =
    aspectClass.get

  def getPreconditionClass : OWLClass =
    preconditionClass.get

  def getEffectClass : OWLClass =
    effectClass.get

  def getActionClass : OWLClass =
    actionClass.get

  def getRelationClass : OWLClass =
    relationClass.get

  def getSVarDescriptionClass : OWLClass =
    sVarDescClass.get

  def getpropertyClass : OWLClass =
    propertyClass.get

  def getComponentClass : OWLClass =
    componentClass.get

  def getBasedOnProp : OWLObjectPropertyExpression =
    baseObjectProp.get

  def getDataTypeProp : OWLObjectPropertyExpression =
    dataTypeObjectProp.get

  def getInPackageProp : OWLDataPropertyExpression =
    inPackageProp.get

  def getHasValueProp : OWLDataPropertyExpression =
    hasValueProp.get

  def getForComponentProp : OWLObjectPropertyExpression =
    forCompObjectProp.get

  def getHasAProp : OWLObjectPropertyExpression =
    hasAObjectProp.get

  def getHasPropertyProp : OWLObjectPropertyExpression =
    hasPropertyProp.get

  def getCtorProp : OWLDataPropertyExpression =
    ctorProp.get

  def getHasAspectObjectProp : OWLObjectProperty =
    hasAspectObjectProp.get

  def getHasPreconditionProp : OWLObjectProperty =
    hasPreconditionProp.get

  def getHasParameterProp : OWLObjectProperty =
    hasParameterProp.get

  def getProvidesParameterProp : OWLObjectProperty =
    providesParameterProp.get

  def getRequiresParameterProp : OWLObjectProperty =
    requiresParameterProp.get

  def getDescribedByProp : OWLObjectProperty =
    describedByProp.get

  def getDescribesProp : OWLObjectProperty =
    describesProp.get

  def getHasSubjectProp : OWLObjectProperty =
    hasSubjectProp.get

  def getHasPredicateProp : OWLObjectProperty =
    hasPredicateProp.get

  def getHasObjectProp : OWLObjectProperty =
    hasObjectProp.get

  def getHasEffectProp : OWLObjectProperty =
    hasEffectProp.get

  def getHasRoleProp : OWLObjectProperty =
    hasRoleProp.get

  def getHasDefaultValue : OWLAnnotationProperty =
    hasDefaultValueAnnotation.get

  def getComment : OWLAnnotationProperty =
    commentAnnotation.get

  def getHasTypeStringProp : OWLDataProperty =
    hasTypeStringProp.get

  def getOverridesProvide : OWLAnnotationProperty =
    overridesProvideAnnotation.get

  def getSuperClasses( of : OWLClass, recurse : Boolean = true ) : Set[OWLClassExpression] = {
    val direct = asScalaSet(of.getSuperClasses(manager.getOntologies)).toSet
    if (recurse)
      direct ++ direct.flatMap( x => if (x.isAnonymous) Set(x) else getSuperClasses(x.asOWLClass(), recurse = true) )
    else
      direct
  }

  def getSubClasses(of : OWLClass, recurse : Boolean = false): Set[OWLClassExpression] ={
    val direct = of.getSubClasses(manager.getOntologies).toSet
    if (recurse)
      direct ++ direct.flatMap(x => if (x.isAnonymous) Set(x) else getSubClasses(x.asOWLClass(), recurse))
    else
      direct
  }

  def getAnonymousSuperClasses(of : OWLClass, recurse : Boolean = true) : Set[OWLClassExpression] =
    getSuperClasses(of, recurse).filter(_.isAnonymous)

  def getNamedSuperClasses(of : OWLClass, recurse : Boolean = true) : Set[OWLClassExpression] =
    getSuperClasses(of, recurse).filterNot(_.isAnonymous)

  def getTypes( i : OWLIndividual ) : Set[OWLClassExpression] =
    asScalaSet(i.getTypes(manager.getOntologies)).toSet

  def getIndividuals( c : OWLClass ) : Set[OWLIndividual] =
    asScalaSet(c.getIndividuals(manager.getOntologies)).toSet

  def getObjectProperties( individual : OWLIndividual )( prop : OWLObjectPropertyExpression ) =
    asScalaSet(manager.getOntologies).foldLeft(Set[OWLIndividual]()){
      (set, onto) =>
        val subSet = individual.getObjectPropertyValues(onto).get(prop)
        if (subSet != null) set ++ asScalaSet(subSet).toSet else set
    }

  def getDataProperties( individual : OWLIndividual )( prop : OWLDataPropertyExpression ) =
    asScalaSet(manager.getOntologies).foldLeft(Set[OWLLiteral]()){
      (set, onto) =>
        val subset = individual.getDataPropertyValues(onto).get(prop)
        if (subset != null) set ++ asScalaSet(subset).toSet else set
    }

  //Alternative using the reasoner
  //o.reasoner.get.getInstances(classExpression, true).getFlattened.toSet
  def getIndividuals(classExpression: OWLClassExpression): Set[OWLNamedIndividual] =
    manager.getOntologies.flatMap(_.getIndividualsInSignature).
      filter(_.getTypes(manager.getOntologies).contains(classExpression)).toSet
}


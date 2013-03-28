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

import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.util.DefaultPrefixManager
import org.semanticweb.owlapi.model._
import collection.JavaConversions.asScalaSet
import org.slf4j.LoggerFactory
import java.io.{FileWriter, BufferedWriter, File}

/**
 * @author dwiebusch
 * Date: 20.09.11
 * Time: 18:11
 */

case class OntologyException(reason : String) extends Exception(reason)

object OntoGenTwo{
  def main( args : Array[String]){
    val p = new OntoGenTwo(".")
    val url = args.headOption.getOrElse(throw new Exception("You must provide a URL when running OntoGenTwo"))
    p.load(url)
    p.parse()
  }

  def getName( iri : IRI ) : String =
    iri.toString.replaceAll(".*#", "")

  def getName( entity : OWLEntity ) : String =
    entity.toStringID.replaceAll(".*#", "")

  def getName( individual : OWLIndividual ) : String =
    getName(individual.asOWLNamedIndividual : OWLEntity)

  def apply[T](self : T, onlyForComponent : Option[String] = None) : OntoGenTwo = self match {
    case c : Class[T] => getInstance(c,  onlyForComponent)
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
    new OntoGenTwo(base, onlyForComponent)
  }
}

class OntoGenTwo(corePath : String, onlyForComponent : Option[String] = None){
  private val log = LoggerFactory.getLogger(this.asInstanceOf[Object].getClass)
  //OWLPropertyNames
  private val hasConstructor = "hasConstructor"
  private val forComponent = "forComponent"
  private val hasDataType = "hasDataType"
  private val inPackage = "inPackage"
  private val hasAspect = "hasAspect"
  private val basedOn = "basedOn"
  private val has_a = "has"

  //shortcuts
  val symbolsBase = "Concept"
  val baseName = "SVarDescription"
  val oSymbol  = "OntologySymbol"
  val oMember  = "SVarDescription"
  val nullName = "nullType"

  private val outPkg        = ".ontology.types"
  private val outFileNames  = "Types.scala"
  private val symbolsObject = "Symbols"

  //Files
  private val symbolsFile   = corePath + File.separator + "core/src/simx/core/ontology/" + symbolsObject + ".scala"
  private val entitiesFile  = corePath + File.separator + "core/src/simx/core/ontology/entities/Entities.scala"
  private val eDescsFile    = corePath + File.separator + "core/src/simx/core/ontology/entities/EntityDescriptions.scala"


  private val symbolsHeader = "package simx.core.ontology\n\n" +
    "import simx.core.entity.description.Semantics\n" +
    "import simx.core.ontology.types.OntologySymbol\n\n" +
    "object "+symbolsObject+"{\n" +
    "  private implicit def symToSem( s : Symbol ) =  new Semantics {\n" +
    "    def toSymbol = s\n" +
    "  }\n\n\t"

  private val entitiesHeader = "package simx.core.ontology.entities\n\n" +
    "import simx.core.entity.Entity\n\n"

  private val descriptionHeader = "package simx.core.ontology.entities\n\n" +
    "import simx.core.ontology\n" +
    "import simx.core.entity.description.EntityAspect\n" +
    "import simx.core.ontology.SpecificDescription\n\n"

  private val typesHeader = "import simx.core.ontology.SVarDescription\n" +
    "import simx.core.ontology.EntitySVarDescription\n" +
    "import simx.core.ontology.entities._\n" +
    "import simx.core.ontology.Symbols\n\n"

  private def filenameFromPackage( pkgName : String) = {
    val dir = "." + File.separator + pkgName + File.separator + "src" + File.separator +
      (pkgName + outPkg).replaceAll("\\.", File.separator) + File.separator
    val tmp = new File(dir)
    if (!tmp.exists)
      tmp.mkdir
    dir + outFileNames
  }


  //internal variables
  protected var manager                                       = OWLManager.createOWLOntologyManager
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

  def load( iri : Option[IRI] = None ) : this.type = {
    if (iri.isDefined)
      ontologyIRI = iri
    if (ontologyIRI.isEmpty)
      throw OntologyException("tried to load ontology although iri was not set")
    //actually load the ontology
    log.info("loading " + ontologyIRI.get)
    setOntology(manager.loadOntologyFromOntologyDocument(ontologyIRI.get))
    this
  }

  def parse(){
    if (ontology.isEmpty)
      throw OntologyException("No ontology was loaded, so nothing could be parsed")
    else
      init()

    val members           = collectMembers(baseClass.get)
    var symbolsList       = collectMembers(getClass(symbolsBase).get).map(m => m.getSymbolString)
    var svarDescLists     = Map[String, List[String]]()
    var entityStringList  = List[String]()
    var entityDescList    = List[String]()

    members.foreach{ m =>
      log.info( m + "\n" )

      symbolsList = symbolsList + m.getSymbolString
      m.getSVarDescriptions.foreach{ desc =>
        svarDescLists = svarDescLists.updated(desc._1, desc._2 :: svarDescLists.getOrElse(desc._1, Nil))
      }
      if (m.isEntity){
        entityStringList = m.getEntityString :: entityStringList
        entityDescList = m.getEntityDescription :: entityDescList
      }
    }

    if (onlyForComponent.isEmpty){
      write(symbolsFile,  symbolsHeader + interleave(symbolsList.toList.sorted, 4 ).mkString("\n\t") + "\n}")
      write(entitiesFile, entitiesHeader + interleave(entityStringList.sorted,  6 ).mkString("\n"))
      write(eDescsFile,   descriptionHeader + interleave(entityDescList.sorted, 11).mkString("\n"))
    }
    svarDescLists.foreach{ t =>
      if (onlyForComponent.collect{ case comp => comp equals t._1}.getOrElse(true)){
        val out = corePath + File.separator +
            t._1.replaceFirst("simx.", "").replaceAll("\\.ontology", "" ).replaceAll("\\.", File.separator) +
            File.separator + "src" + File.separator + t._1.replaceAll("\\.", File.separator) + "/types/Types.scala"
        write(out, "package " + t._1 + ".types\n\n" + typesHeader + interleave(t._2.sorted, 7).mkString("\n") )
      }
    }
  }

  protected def interleave(in : List[String], p : Int) : List[String] = in match {
    case head :: neck :: tail =>
      head :: (if (head.charAt(p) == neck.charAt(p)) interleave(neck :: tail, p) else "" :: interleave(neck :: tail, p))
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

  protected def collectMembers( c : OWLClass, prev : Set[OntologyMember] = Set() ) : Set[OntologyMember] =
    prev ++ asScalaSet(c.getSubClasses(manager.getOntologies)).flatMap{
      c => collectMembers(c.asOWLClass, Set(new OntologyMember(c.asOWLClass(), this)))
    }

  private var baseClass : Option[OWLClass] = None
  private var entityClass : Option[OWLClass] = None
  private var nullType : Option[OWLIndividual] = None
  private var ctorProp : Option[OWLDataProperty] = None
  private var inPackageProp : Option[OWLDataProperty] = None
  private var hasAObjectProp : Option[OWLObjectProperty] = None
  private var baseObjectProp : Option[OWLObjectProperty] = None
  private var forCompObjectProp : Option[OWLObjectProperty] = None
  private var dataTypeObjectProp : Option[OWLObjectProperty] = None
  private var hasAspectObjectProp : Option[OWLObjectProperty] = None


  private def init() {
    val dataProps = asScalaSet(manager.getOntologies).foldLeft(Set[OWLDataProperty]()){
      (set, onto) => set ++ asScalaSet(onto.getDataPropertiesInSignature).toSet
    }
    val objectProps = asScalaSet(manager.getOntologies).foldLeft(Set[OWLObjectProperty]()){
      (set, onto) => set ++ asScalaSet(onto.getObjectPropertiesInSignature).toSet
    }
    nullType            = getClass(nullName).collect{
      case c => c.getIndividuals(manager.getOntologies).iterator().next()
    }
    entityClass         = getClass("Entity")
    baseClass           = getClass(baseName)
    hasAObjectProp      = objectProps.find( OntoGenTwo.getName(_) equals has_a )
    baseObjectProp      = objectProps.find( OntoGenTwo.getName(_) equals basedOn )
    hasAspectObjectProp = objectProps.find( OntoGenTwo.getName(_) equals hasAspect )
    dataTypeObjectProp  = objectProps.find( OntoGenTwo.getName(_) equals hasDataType )
    forCompObjectProp   = objectProps.find( OntoGenTwo.getName(_) equals forComponent )
    inPackageProp       = dataProps.find( OntoGenTwo.getName(_) equals inPackage )
    ctorProp            = dataProps.find( OntoGenTwo.getName(_) equals hasConstructor )
  }

  protected def getClass( name : String ) : Option[OWLClass] = ontology match {
    case Some(o) => o.getClassesInSignature(true).toArray(Array[OWLClass]()).find( _.toStringID.endsWith("#"+name))
    case None => None
  }

  def getNullTypeClass : OWLIndividual =
    nullType.get

  def getEntityClass : OWLClass =
    entityClass.get

  def getBasedOnProp : OWLObjectPropertyExpression =
    baseObjectProp.get

  def getDataTypeProp : OWLObjectPropertyExpression =
    dataTypeObjectProp.get

  def getInPackageProp : OWLDataPropertyExpression =
    inPackageProp.get

  def getForComponentProp : OWLObjectPropertyExpression =
    forCompObjectProp.get

  def getHasAProp : OWLObjectPropertyExpression =
    hasAObjectProp.get

  def getCtorProp : OWLDataPropertyExpression =
    ctorProp.get

  def getHasAspectObjectProp : OWLObjectProperty =
    hasAspectObjectProp.get

  def getSuperClasses( of : OWLClass, recurse : Boolean = true ) : Set[OWLClassExpression] = {
    val direct = asScalaSet(of.getSuperClasses(manager.getOntologies)).toSet
    if (recurse)
      direct ++ direct.flatMap( x => if (x.isAnonymous) Set(x) else getSuperClasses(x.asOWLClass(), recurse = true) )
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


}

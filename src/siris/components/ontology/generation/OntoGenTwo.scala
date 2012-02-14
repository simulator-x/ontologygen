package siris.components.ontology.generation

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

object OntoGenTwo{
  def main( args : Array[String]){
    val p = new OntoGenTwo
    p.load(new File("/Users/dwiebusch/ontologies/NewCoreOntology/NewCoreOntology.owl"))
    p.parse()
  }

  def getName( iri : IRI ) : String =
    iri.toString.replaceAll(".*#", "")

  def getName( entity : OWLEntity ) : String =
    entity.toStringID.replaceAll(".*#", "")

  def getName( individual : OWLIndividual ) : String =
    getName(individual.asOWLNamedIndividual : OWLEntity)
}

class OntoGenTwo{
  private val log = LoggerFactory.getLogger(getClass())
  //OWLPropertyNames
  private val hasConstructor = "hasConstructor"
  private val forComponent = "forComponent"
  private val hasDataType = "hasDataType"
  private val inPackage = "inPackage"
  private val hasAspect = "hasAspect"
  private val basedOn = "basedOn"
  private val has_a = "has"

  //shortcuts
  val nullName = "nullType"
  val baseName = "SVarDescription"
  val oSymbol = "OntologySymbol"
  val oMember = "SVarDescription"

  private val outPkg        = ".types"
  private val outFileNames  = "Types.scala"
  private val symbolsObject = "Symbols2"
  //Files
  private val corePath      = "../../../core/src/"
  private val symbolsFile   = corePath + "siris/core/ontology/"+symbolsObject+".scala"
  private val entitiesFile  = corePath + "siris/core/ontology/entities/Entities.scala"
  private val eDescsFile    = corePath + "siris/core/ontology/entities/EntityDescriptions.scala"

  private val symbolsHeader = "package siris.core.ontology\n\n" +
    "import siris.core.entity.description.Semantics\n" +
    "import siris.core.ontology.types.OntologySymbol\n\n" +
    "object "+symbolsObject+"{\n" +
    "  private implicit def symToSem( s : Symbol ) =  new Semantics {\n" +
    "    def toSymbol = s\n" +
    "  }\n\n\t"

  private val entitiesHeader = "package siris.core.ontology.entities\n\n" +
    "import siris.core.entity.component.Removability\n" +
    "import siris.core.entity.Entity\n\n"

  private val descriptionHeader = "package siris.core.ontology.entities\n\n" +
    "import siris.core.entity.description.AspectBase\n" +
    "import siris.core.ontology.SpecificDescription\n\n"

  private val typesHeader = "import siris.core.ontology.{"+symbolsObject+" => Symbols, SVarDescription}\n" +
    "import siris.core.ontology.EntitySVarDescription\n" +
    "import siris.core.ontology.entities._\n\n"

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

  def load( file : File ) {
    load(Some(IRI.create(file)))
  }

  def load( iri : Option[IRI] = None ) {
    if (iri.isDefined)
      ontologyIRI = iri
    if (ontologyIRI.isEmpty)
      throw OntologyException("tried to load ontology although iri was not set")
    //actually load the ontology
    log.info("loading " + ontologyIRI.get)
    setOntology(manager.loadOntologyFromOntologyDocument(ontologyIRI.get))
  }

  def parse(){
    if (ontology.isEmpty)
      throw OntologyException("No ontology was loaded, so nothing could be parsed")
    else
      init()

    val members           = collectMembers(baseClass.get)
    var symbolsList       = List[String]()
    var svarDescLists     = Map[String, List[String]]()
    var entityStringList  = List[String]()
    var entityDescList    = List[String]()

    members.foreach{ m =>
      log.info( m + "\n" )

      symbolsList = m.getSymbolString :: symbolsList
      m.getSVarDescriptions.foreach{ desc =>
        svarDescLists = svarDescLists.updated(desc._1, desc._2 :: svarDescLists.getOrElse(desc._1, Nil))
      }
      if (m.isEntity){
        entityStringList = m.getEntityString :: entityStringList
        entityDescList = m.getEntityDescription :: entityDescList
      }
    }

    write(symbolsFile,  symbolsHeader + interleave(symbolsList.sorted,        4 ).mkString("\n\t") + "\n}")
    write(entitiesFile, entitiesHeader + interleave(entityStringList.sorted,  6 ).mkString("\n"))
    write(eDescsFile,   descriptionHeader + interleave(entityDescList.sorted, 11).mkString("\n"))
    svarDescLists.foreach{ t => write(
        corePath + t._1.replaceAll("\\.", "/")+"/types/Types.scala",
        "package " + t._1 + ".types\n\n" + typesHeader + interleave(t._2.sorted, 7).mkString("\n")
    ) }
  }

  protected def interleave(in : List[String], p : Int) : List[String] = in match {
    case head :: neck :: tail =>
      head :: (if (head.charAt(p) == neck.charAt(p)) interleave(neck :: tail, p) else "" :: interleave(neck :: tail, p))
    case list => list
  }

  protected def write(filename : String, toWrite : String){
    val writer = new BufferedWriter(new FileWriter(new File(filename)))
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
      direct ++ direct.flatMap( x => if (x.isAnonymous) Set(x) else getSuperClasses(x.asOWLClass(), true) )
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


package siris.components.ontology.generation

import java.io.File
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.util.DefaultPrefixManager
import org.semanticweb.owlapi.model._
import collection.JavaConversions
import org.slf4j.LoggerFactory

/**
 * @author dwiebusch
 * Date: 20.09.11
 * Time: 18:11
 */

object OntoGenTwo{
  def main( args : Array[String]){
    val p = new OntoGenTwo
    p.load(new File("/opt/local/etc/sirisOntology/application/SiXtonsCurse/config/Config.owl"))
    p.parse()
  }
}

class OntoGenTwo{
  private val log = LoggerFactory.getLogger(getClass())
  //OWLPropertyNames
  private val hasConstructor = "hasConstructor"
  private val forComponent = "forComponent"
  private val hasDataType = "hasDataType"
  private val inPackage = "inPackage"
  private val basedOn = "basedOn"
  private val has_a = "has_a"

  //shortcuts
  val nullName = "nullType"
  val baseName = "Siris_Concept"
  val oSymbol = "OntologySymbol"
  val oMember = "SVarDescription"

  //Files
  private val ontPkg = "siris.ontology"
  private val symbolsObject = "Symbols"
  private val outFileNames = "Types_tmp.scala"
  private val outPkg = ".types"
  private val symbolsFile = "./"+ontPkg+"/src/siris/ontology/Symbols.scala"

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
    val members = collectMembers(baseClass.get)
    members.foreach( m => log.info( m + "\n") )
    println(members.mkString("\n"))
  }

  def collectMembers( c : OWLClass ) : Set[OntologyMember] =
    JavaConversions.asScalaSet(c.getSubClasses(manager.getOntologies)).flatMap{
      c => collectMembers(c.asOWLClass) }.toSet + new OntologyMember(c, this)

  private var baseClass : Option[OWLClass] = None
  private var entityClass : Option[OWLClass] = None
  private var nullType : Option[OWLIndividual] = None
  private var ctorProp : Option[OWLDataProperty] = None
  private var inPackageProp : Option[OWLDataProperty] = None
  private var hasAObjectProp : Option[OWLObjectProperty] = None
  private var baseObjectProp : Option[OWLObjectProperty] = None
  private var forCompDataProp : Option[OWLObjectProperty] = None
  private var dataTypeObjectProp : Option[OWLObjectProperty] = None


  private def init() {
    val dataProps = JavaConversions.asScalaSet(manager.getOntologies).foldLeft(Set[OWLDataProperty]()){
      (set, onto) => set ++ JavaConversions.asScalaSet(onto.getDataPropertiesInSignature).toSet
    }
    val objectProps = JavaConversions.asScalaSet(manager.getOntologies).foldLeft(Set[OWLObjectProperty]()){
      (set, onto) => set ++ JavaConversions.asScalaSet(onto.getObjectPropertiesInSignature).toSet
    }
    nullType            = getClass(nullName).collect{
      case c => c.getIndividuals(manager.getOntologies).iterator().next()
    }
    entityClass         = getClass("Entity")
    baseClass           = getClass(baseName)
    hasAObjectProp      = objectProps.find( getName(_) equals has_a )
    baseObjectProp      = objectProps.find( getName(_) equals basedOn )
    dataTypeObjectProp  = objectProps.find( getName(_) equals hasDataType )
    forCompDataProp     = objectProps.find( getName(_) equals forComponent )
    inPackageProp       = dataProps.find( getName(_) equals inPackage )
    ctorProp            = dataProps.find( getName(_) equals hasConstructor )
  }

  protected def getClass( name : String ) : Option[OWLClass] = ontology match {
    case Some(o) => o.getClassesInSignature(true).toArray(Array[OWLClass]()).find( _.toStringID.endsWith("#"+name))
    case None => None
  }

  def getNullTypeClass : OWLIndividual =
    nullType.get

  def getEntityClass : OWLClassExpression =
    entityClass.get

  def getBasedOnProp : OWLObjectPropertyExpression =
    baseObjectProp.get

  def getDataTypeProp : OWLObjectPropertyExpression =
    dataTypeObjectProp.get

  def getInPackageProp : OWLDataPropertyExpression =
    inPackageProp.get

  def getForComponentProp : OWLObjectPropertyExpression =
    forCompDataProp.get

  def getHasAProp : OWLObjectPropertyExpression =
    hasAObjectProp.get

  def getCtorProp : OWLDataPropertyExpression =
    ctorProp.get

  def getSuperClasses( of : OWLClassExpression, recurse : Boolean = true ) : Set[OWLClassExpression] = {
    val direct = JavaConversions.asScalaSet(of.asOWLClass().getSuperClasses(manager.getOntologies)).toSet
    if (recurse) direct ++ direct.flatMap( getSuperClasses(_, true) ) else direct
  }

  def getTypes( i : OWLIndividual ) : Set[OWLClassExpression] =
    JavaConversions.asScalaSet(i.getTypes(manager.getOntologies)).toSet

  def getIndividuals( c : OWLClass ) : Set[OWLIndividual] =
    JavaConversions.asScalaSet(c.getIndividuals(manager.getOntologies)).toSet

  def getObjectProperties( individual : OWLIndividual )( prop : OWLObjectPropertyExpression ) =
    JavaConversions.asScalaSet(manager.getOntologies).foldLeft(Set[OWLIndividual]()){
      (set, onto) =>
        val subSet = individual.getObjectPropertyValues(onto).get(prop)
        if (subSet != null) set ++ JavaConversions.asScalaSet(subSet).toSet else set
    }

  def getDataProperties( individual : OWLIndividual )( prop : OWLDataPropertyExpression ) =
    JavaConversions.asScalaSet(manager.getOntologies).foldLeft(Set[OWLLiteral]()){
      (set, onto) =>
        val subset = individual.getDataPropertyValues(onto).get(prop)
        if (subset != null) set ++ JavaConversions.asScalaSet(subset).toSet else set
    }

  protected def getName( iri : IRI ) : String =
    iri.toString.replaceAll(".*#", "")

  protected def getName( entity : OWLEntity ) : String =
    entity.toStringID.replaceAll(".*#", "")

  protected def getName( individual : OWLIndividual ) : String =
    getName(individual.asOWLNamedIndividual : OWLEntity)
}


package siris.core.ontology.generation

import siris.core.ontology.Symbols
import java.io.File
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.util.DefaultPrefixManager
import org.semanticweb.owlapi.model._
import collection.JavaConversions

/**
 * @author dwiebusch
 * Date: 20.09.11
 * Time: 18:11
 */

object OntoGenTwo{
  def main( args : Array[String]){
    val p = new OntoGenTwo
    p.load(new File("ontology/ontologyFiles/current/application/SiXtonsCurse/config/Config.owl"))
    p.parse()
  }
}

class OntoGenTwo {
  //OWLPropertyNames
  private val hasConstructor = "hasConstructor"
  private val forComponent = "forComponent"
  private val hasDataType = "hasDataType"
  private val inPackage = "inPackage"
  private val basedOn = "basedOn"
  private val has_a = "has_a"

  //shortcuts
  val nullName = Symbols.nullType.value.toString
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
    println("loading " + ontologyIRI.get)
    setOntology(manager.loadOntologyFromOntologyDocument(ontologyIRI.get))
  }

  def parse(){
    if (ontology.isEmpty)
      throw OntologyException("No ontology was loaded, so nothing could be parsed")
    else
      init()
    val members = collectMembers(baseClass.get)
    members.foreach( m => println( m + "\n") )
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
    if (!recurse)
      return direct
    direct ++ direct.flatMap( getSuperClasses(_, true) )
  }

  def getTypes( i : OWLIndividual ) : Set[OWLClassExpression] =
    JavaConversions.asScalaSet(i.getTypes(manager.getOntologies)).toSet

  def getIndividuals( c : OWLClass ) : Set[OWLIndividual] =
    JavaConversions.asScalaSet(c.getIndividuals(manager.getOntologies)).toSet

  def getObjectProperties( individual : OWLIndividual )( prop : OWLObjectPropertyExpression ) : Set[OWLIndividual] = {
    JavaConversions.asScalaSet(manager.getOntologies).foldLeft(Set[OWLIndividual]()){
      (set, onto) =>
        val subSet = individual.getObjectPropertyValues(onto).get(prop)
        if (subSet != null) set ++ JavaConversions.asScalaSet(subSet).toSet else set
    }
  }

  def getDataProperties( individual : OWLIndividual )( prop : OWLDataPropertyExpression ) : Set[OWLLiteral] =
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


object OntologyMember{
  private var registry = Map[OWLIndividual, OntologyMember]()
  def register( key : OWLIndividual, value : OntologyMember ) {
    registry = registry.updated(key, value)
  }

  def apply( key : OWLIndividual) : Option[OntologyMember] =
    registry.get(key)
}

class OntologyMember ( c : OWLClass, o : OntoGenTwo ){
  getIndividuals.foreach( OntologyMember.register(_, this) )

  def getName : String =
    c.toStringID.replaceAll(".*#", "")

  def getSymbolString : String =
    "val " + deCap(getName) + " = OntologySymbol(Symbol(\"" + getName + "\"))"

  def getEntityString : String =
    if (isEntity) "class " + getName + "( e : Entity = new Entity() ) extends Entity(e)" else ""

  def getSVarDescriptions : Map[String, String] =
    getIndividuals.foldLeft(Map[String, String]()){
      (m, i) => m.updated(getTargetComponent(i), if (isEntity) getEntitySVarDescription else getSVarDescription(i))
    }

  def getEntityDescription : String =
    if (isEntity)
      "case class " + getName + "EntityDescription( aspects : AspectBase* ) " +
        "extends SpecificDescription(" + getName + "Description, aspects.toList" +
        (if (getAnnotations(getIndividuals.head).nonEmpty) ", " + getAnnotations(getIndividuals.head).mkString(", ") else "" ) + ")"
    else ""

  protected def getIndividuals : Set[OWLIndividual] =
    o.getIndividuals(c)

  protected def getName( i : OWLIndividual ) : String =
    i.toStringID.replaceAll(".*#", "")

  protected def getName( entity : OWLEntity ) : String =
    entity.toStringID.replaceAll(".*#", "")

  protected def getEntitySVarDescription : String =
    "object "+ getName +" extends EntitySVarDescription(Symbols."+deCap(getName)+
      ", new " + getName + "(_) with Removability)"

  protected def getSVarDescription( i : OWLIndividual ) : String = {
    val base =  OntologyMember(getBase(i).get).collect{
      case x => getTargetComponent(getBase(i).get) + ".types."+ x.getName
    }.getOrElse("siris.ontology.types.NullType")
    "object " + getName + " extends SVarDescription" + typeString(i) + "("+ base + " as Symbols." + getName +
      getConstructor(i) + ")"
  }

  protected def typeString( i : OWLIndividual ) : String =
    getDataType(i).collect{
      case dt => "[" + getName(dt) + ", " + getName(getBaseDataType(i).getOrElse(dt)) + "]"
    }.getOrElse("")

  protected def getConstructor(i : OWLIndividual) : String =
    o.getDataProperties(i)(o.getCtorProp).headOption.collect{
      case literal => " createdBy " + literal.getLiteral
    }.getOrElse("")

  protected def getBaseDataType( i : OWLIndividual ) : Option[OWLIndividual] = {
    getBase(i).collect{
      case base if (base != i) => getDataType(base) match {
        case None => getBaseDataType(base)
        case data => data
      }
    }.getOrElse(None)
  }

  protected def getTargetComponent( i : OWLIndividual ) : String =
    o.getObjectProperties(i)(o.getForComponentProp).headOption.collect{
      case x => o.getDataProperties(x)(o.getInPackageProp).headOption.collect{
        case literal => literal.getLiteral
      }.getOrElse("")
    }.getOrElse("")

  protected def getAnnotations( i : OWLIndividual ) : Set[String] =
    o.getObjectProperties(i)(o.getHasAProp).map{
      x => OntologyMember(x).collect{ case something => something.getName }.getOrElse("")
    }

  protected def getBase( i : OWLIndividual ) : Option[OWLIndividual] =
    o.getObjectProperties(i)(o.getBasedOnProp).headOption

  protected def getDataType( i : OWLIndividual ) : Option[OWLIndividual] =
    o.getObjectProperties(i)(o.getDataTypeProp).headOption

  protected def isEntity : Boolean =
    o.getSuperClasses(c).contains(o.getEntityClass)

  protected def generatesSVar( i : OWLIndividual ) : Boolean =
    getIndividuals.nonEmpty

  protected def deCap( s : String ) : String =
    if (s.length() < 2) s.toLowerCase else s.charAt(0).toLower + s.substring(1)

  override def toString =
    "OntologyMember " + getName + ":\n--------------------------------------------\n" +
      "Symbol:\n\t" + getSymbolString + "\n" + "Descriptions:\n\t" + getSVarDescriptions.mkString("\n\t") +
      (if (isEntity) "\nAssocEntity:\n\t" + getEntityString + "\nEntityDescription:\n\t" + getEntityDescription else "")
}
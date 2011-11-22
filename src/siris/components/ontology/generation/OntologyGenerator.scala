package siris.components.ontology.generation

import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.util.DefaultPrefixManager
import org.semanticweb.owlapi.model._
import java.io.{BufferedWriter, FileWriter, File}
import java.lang.Exception

/* author: dwiebusch
 * date: 13.09.2010
 */

object OntologyGenerator{
  def main(args : Array[String]) {
    val p = new OntologyGenerator
    p.load(new File("siris.core.ontology/ontologyFiles/current/application/SiXtonsCurse/config/Config.owl"))
    p.parse()
  }
}

case class OntologyException(val reason : String) extends Exception(reason)

class OntologyGenerator{
  //OWLPropertyNames
  private val hasConstructor = "hasConstructor"
  private val forComponent = "forComponent"
  private val hasDataType = "hasDataType"
  private val inPackage = "inPackage"
  private val basedOn = "basedOn"
  private val has_a = "has_a"

  //shortcuts
  private val nullName = "nullType"
  private val baseName = "Siris_Concept"
  private val oSymbol = "OntologySymbol"
  private val oMember = "SVarDescription"

  //Files
  private val ontPkg = "siris.ontology"
  private val symbolsObject = "Symbols"
  private val outFileNames = "Types.scala"
  private val outPkg = ".types"
  private val symbolsFile = "./"+ontPkg+"/src/siris/core/ontology/Symbols.scala"

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

  case class Description(val name : String = "", val ctor : String = "", val dataType : String = "",
                         val base : String = "", val component : String = "")

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
      throw new OntologyException("tried to load ontology although iri was not set")
    //actually load the ontology
    println("loading " + ontologyIRI.get)
    setOntology(manager.loadOntologyFromOntologyDocument(ontologyIRI.get))
  }

  def parse( ontology : Option[OWLOntology] = None ) {
    if ( ontology.isDefined )
      setOntology(ontology.get)
    if (this.ontology.isEmpty)
      throw new OntologyException("tried to parse ontology although no ontology was specified")
    //actual parsing
    println("parsing ...")
    val parsedTree = getClass(baseName) match {
      case Some(owlClass) => getIndividuals(owlClass)
      case None => throw new OntologyException("Could not find \""+ baseName + "\" class, cannot create Semantic Symbols")
    }
//    val parsedTree2 = getClass("Entity") match {
//      case Some(owlClass) => getIndividuals(owlClass)
//      case None => throw new OntologyException("Could not find \""+ baseName + "\" class, cannot create Semantic Symbols")
//    }
//    parsedTree2.children.foreach(generateEntities)
    //writing
    println("writing ...")
    writeToFile(symbolsFile, generateSemanticSymbolsFile(parsedTree))
    for ( (pkg, classes) <- generateClassesFiles(parsedTree))
      writeToFile(filenameFromPackage(pkg), generateClassFile(pkg, classes))
    println("finished generating.")

  }

  private def generateEntities( p : ParseTree ) {
    p.children.foreach(generateEntities)

    val str1 = "case class " + p.name + "EntityDescription( aspects : AspectBase* ) extends SpecificDescription" +
      "(types." + p.name + ", aspects.toList"
    val str2 = "object " + p.name + " extends EntitySVarDescription(Symbols." + deCap(p.name) +
      ", new " + p.name + "(_) with Removability)"
    val str3 = "class " + p.name + "( e : Entity = new Entity() ) extends Entity(e)"

    var str4 = p.entries.map( i => getObjectProperty(i)(has_a)).flatMap( s => s.map(getName(_)) ).mkString(", ")
    if (str4.nonEmpty)
      str4 = ", " + str4
    println(str1 + str4 + ")" + "\n" + str2 + "\n" + str3 )
  }

  protected def writeToFile(fname : String, text : String) {
    println("\t" + fname)
    val writer = new BufferedWriter(new FileWriter(new File(fname)))
    writer.write(text)
    writer.close()
  }

  private def toSet[T]( set : java.util.Set[T] ) =
    set.toArray.asInstanceOf[Array[T]].toSet

  private def collectClassNames(t : ParseTree) : List[String] =
    if (t.children.isEmpty) List(t.name) else t.children.foldLeft(List[String]()){ _ ::: collectClassNames(_) }

  private def collectClasses(tree : ParseTree) : List[ParseTree] = tree.children.foldLeft(List[ParseTree]()){
    (list, elem) => if (elem.entries.nonEmpty) elem :: collectClasses(elem) ::: list else collectClasses(elem) ::: list
  }

  private def generateClassFile( pkg : String, classes : List[String] ) : String = {
    "package " + pkg + outPkg + "\n\n" + "import "+ ontPkg + ".{" + oMember + ", " + symbolsObject + "}\n" +
      "\n" + interleave(classes, s => s, 7)
  }

  private def isNullType( d : Description ) =
    deCap(d.name).equals(nullName)

  private def generateClassesFiles( tree : ParseTree ) : Map[String, List[String]] = {
    val classes  = collectClasses(tree).flatMap(generateClasses)
    val members = addMissingBaseMembers {
      classes.foldLeft(Map[String, Map[String, Description]]()){
        (map, desc) => map.updated(desc.component, map.getOrElse(desc.component, Map()) + (desc.name -> desc))
      }
    }
    members.map{ kv => kv._1 -> kv._2.values.filterNot( isNullType ).map{ completeDef( _, members ) }.toList }
  }

  private def addMissingBaseMembers(in : Map[String, Map[String, Description]]) = in.foldLeft(in.empty){ (map, elem) =>
    if ( elem._1 equals ontPkg )
      map.updated(ontPkg, map.getOrElse(ontPkg, Map()) ++ elem._2)
    else
      map.updated(ontPkg, elem._2.foldLeft(map.getOrElse(ontPkg, in.getOrElse(ontPkg, Map()))){
        (m, e) => if (m.contains(e._1)) m else m.updated(e._1, toBaseDesc(e._2, in))
      }).updated(elem._1, elem._2)
  }

  private def toBaseDesc( d : Description, mems : Map[String, Map[String, Description]] ) = {
    val b = getTopmostBase(d, mems).getOrElse(throw new Exception(""))
    Description(d.name, "", "", b.component + outPkg + "." + b.name, ontPkg)
  }

  private def generateClasses( info : ParseTree ) : List[Description] = info.entries.filter(isSVarDescription).map {
    individual =>
      val objPropGetter  = getObjectProperty(individual) _
      val dataPropGetter = getDataProperty(individual) _
      val base      = getBase(objPropGetter)
      val component = collect(forComponent, objPropGetter, getComponentPackage _)
      val dataType  = collect(hasDataType, objPropGetter,  getName(_ : OWLIndividual) )
      val ctor      = collect(hasConstructor, dataPropGetter, " createdBy(" + (_ : OWLLiteral).getLiteral + ")")
      Description(info.name, ctor, dataType, base, component)
  }.toList

  private def completeDef( d : Description, mems : Map[String, Map[String, Description]] ) : String = {
    "object " + d.name + " extends " + oMember + getTopmostBase(d, mems).collect{
      case sth if (d.dataType.nonEmpty) =>
        "[" + d.dataType + ", " + (if (sth.dataType.nonEmpty) sth.dataType else d.dataType) + "]"
    }.getOrElse("") + "( " +  d.base + " as "+symbolsObject+"." + deCap(d.name) + d.ctor + " )"
  }

  private def getTopmostBase( d : Description, mems : Map[String, Map[String, Description]] ) : Option[Description] = {
    val split = splitBase(d)
    mems.getOrElse(split._1, Map()).get(split._2) match {
      case Some(bm) if (!deCap(splitBase(bm)._2).equals( nullName )) => getTopmostBase(bm, mems)
      case bm => bm
    }
  }

  private def splitBase(d : Description) = {
    val split = d.base.split(outPkg + ".")
    if (split.size != 2)
      throw new Exception("could not split base: " + d.base + " ( size = " + split.size + " )")
    split(0) -> split(1)
  }

  private def isSVarDescription( individual : OWLIndividual ) : Boolean =
    getObjectProperty(individual)(basedOn).nonEmpty && getObjectProperty(individual)(forComponent).nonEmpty

  private def collect[A, T, Z]( key : A, getter : A => Set[T], fun : T => String, default : String = "") : String =
    getter(key).headOption.collect{ case something => fun(something) }.getOrElse(default)

  private def getBase( g : String => Set[OWLIndividual] ) =
    getDataProperty(getObjectProperty(g(basedOn).head)(forComponent).head)(inPackage).head.getLiteral + outPkg + "." +
      collect(basedOn, g, collect(_ : OWLIndividual, getTypes, toName, nullName), nullName)

  private def toName( ex : OWLClassExpression ) =
    getName(ex.asOWLClass)

  private def getTypes( i : OWLIndividual) : Set[OWLClassExpression] =
    toSet(i.getTypes(manager.getOntologies))

  private def getComponentPackage( comp : OWLIndividual ) : String =
    getDataProperty(comp)(inPackage).headOption.collect{ case x => x.getLiteral }.getOrElse("")

  private def getObjectProperties =
    toSet(ontology.get.getObjectPropertiesInSignature(true))

  private def getDataProperties =
    toSet(ontology.get.getDataPropertiesInSignature(true))

  private def getObjectProperty( individual : OWLIndividual )( name : String ) : Set[OWLIndividual] =
    getObjectProperties.find( _.toString.endsWith(name+">")).collect{
      case prop => toSet(manager.getOntologies).foldLeft(Set[OWLIndividual]()){
        (set, ont) => set ++ toSet(individual.getObjectPropertyValues(prop, ont))
      }
    }.getOrElse(Set())

  private def getDataProperty( individual : OWLIndividual )( name : String ) : Set[OWLLiteral] =
    getDataProperties.find( _.toString.endsWith(name+">") ).collect {
      case prop => toSet(manager.getOntologies).foldLeft(Set[OWLLiteral]()){
        (set, ont) => set ++ toSet(individual.getDataPropertyValues(prop, ont))
      }
    }.getOrElse(Set())

  private def collectSymbolNames : ParseTree => Set[String] =
    x => x.children.foldLeft(Set(x.name)){ _ ++ collectSymbolNames(_) }

  private def generateSemanticSymbolsFile( semanticSymbols : ParseTree ) : String =
    "package "+ ontPkg +"\n\nimport siris.core.entity.description.Semantics\n" +
      "import siris.ontology.types.OntologySymbol\n\nobject "+ symbolsObject + "{\n  " +
      "private implicit def symToSem( s : Symbol ) =  new Semantics {\n    def toSymbol = s\n  }" +
      interleave(collectSymbolNames(semanticSymbols).toList,
        sym => "  val " + deCap(sym) + " = " + oSymbol + "(Symbol(\"" + deCap(sym) + "\"))") +
      "\n}\n"

  private def interleave( in : List[String], map : String => String, skip : Int = 0) : String =
    in.sorted.foldLeft((' ', "")){ (pre, s) => s.charAt(skip) -> (pre._2 + newLine(pre._1, s, skip) + map(s)) }._2

  private def newLine(last : Char, next : String, ignore : Int) : String =
    if (last != next.charAt(ignore)) "\n\n" else "\n"

  private def deCap(s : String) : String =
    if (s.length != 0) s.charAt(0).toLower + s.substring(1) else ""

  protected def getName( iri : IRI ) : String =
    iri.toString.replaceAll(".*#", "")

  protected def getName( entity : OWLEntity ) : String =
    entity.toStringID.replaceAll(".*#", "")

  protected def getName( individual : OWLIndividual ) : String =
    getName(individual.asOWLNamedIndividual : OWLEntity)

  protected def getIndividuals( c : OWLClass, parent : Option[ParseTree] = None) : ParseTree = {
    val retVal = new ParseTree
    retVal.name = getName(c)
    retVal.parent = parent
    retVal.entries = c.getIndividuals(manager.getOntologies).toArray(Array[OWLIndividual]()).toSet
    retVal.children = c.getSubClasses(manager.getOntologies).toArray(Array[OWLClass]()).foldLeft(Set[ParseTree]()){
      (set, owlClass) => set + getIndividuals(owlClass, Some(retVal))
    }
    retVal
  }

  protected def getClass( name : String ) : Option[OWLClass] = ontology match {
    case Some(o) => o.getClassesInSignature(true).toArray(Array[OWLClass]()).find( _.toStringID.endsWith("#"+name))
    case None => None
  }
}

protected class ParseTree{
  var name  = "undefined"
  var children : Set[ParseTree]     = Set()
  var entries  : Set[OWLIndividual] = Set()
  var parent   : Option[ParseTree]  = None

  def collectEntries : Set[OWLIndividual] =
    children.foldLeft(entries)( _ ++ _.collectEntries )

  override def toString : String =
    toString(0)

  def toString(depth : Int = 0) : String =
    mkDepth(depth) + "Name: "+ name +
      mkDepth(depth) + "Entries: " +  (if (entries.nonEmpty)  entries.mkString(mkDepth(depth+1)) else "None") +
      mkDepth(depth) + "Children: " +
      (if (children.nonEmpty)
        children.foldLeft(""){(oldVal, elem) => oldVal + elem.toString(depth+1)}
      else
        "None")

  private def mkDepth(depth : Int) : String =
    "\n" + (for (i <- 0 until depth) yield ("  ")).mkString
}

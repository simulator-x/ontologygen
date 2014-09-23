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
import org.slf4j.LoggerFactory
import simx.components.ontology.generation.helper.{OntologyException, OWLFunctions, OntoIO}
import simx.components.ontology.generation.member.{SimXRelation, OntologyMember}
import collection.JavaConversions.asScalaSet
import java.io.{FileWriter, BufferedWriter, File}


object OntoGenTwo{
  def main( args : Array[String]){
    val p = new OntoGenTwo(".")
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
    new OntoGenTwo(base, onlyForComponent)
  }
}

class OntoGenTwo(val corePath : String, onlyForComponent : Option[String] = None) extends OntoIO {
  private implicit val parser = this
  private val log = LoggerFactory.getLogger(this.asInstanceOf[Object].getClass)

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

    val members           = collectMembers(baseClass.get) ++ SimXRelation.parse(ontology.get)
    val actionList        = collectActions(actionClass.get).flatMap(getIndividuals)
    var symbolsList       = collectMembers(getOWLClass(symbolsBase).get).map(m => m.getSymbolString)
    var svarDescLists     = Map[String, List[String]]()
    var entityStringList  = List[String]()
    var entityDescList    = List[String]()

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

    members.foreach{ m =>
      log.info( m + "\n" )

      symbolsList = symbolsList + m.getSymbolString
      m.getSVarDescriptions.foreach{ desc =>
        svarDescLists = svarDescLists.updated(desc._1, desc._2 :: svarDescLists.getOrElse(desc._1, Nil))
      }
      m.getEntityString.collect { case str => entityStringList = str :: entityStringList }
      m.getEntityDescription.collect{ case str => entityDescList = str :: entityDescList }
    }

    if (onlyForComponent.isEmpty){
      write(symbolsFile,  symbolsHeader + interleave(symbolsList.toList.sorted, 4 ).mkString("\n\t") + "\n}")
      write(entitiesFile, entitiesHeader + interleave(entityStringList.sorted,  6 ).mkString("\n"))
      write(entityDescriptionsFile,   descriptionHeader + interleave(entityDescList.sorted, 11).mkString("\n"))
    }
    svarDescLists.foreach{ t =>
      if (onlyForComponent.collect{ case comp => comp equals t._1}.getOrElse(true)){
        val out = corePath + File.separator +
          t._1.replaceFirst("simx.", "").replace(".ontology", "" ).replace(".", File.separator) +
          File.separator + "src" + File.separator + t._1.replace(".", File.separator) + "/types/package.scala"
        write(out, typesHeader("package " + t._1) + interleave(t._2.sorted, 7).mkString("\n\t") + "\n}" )
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
      c => collectMembers(c.asOWLClass, Set(OntologyMember(c.asOWLClass(), this)))
    }

  protected def collectActions( c : OWLClass) : Set[OWLClass] =
    asScalaSet(c.getSubClasses(manager.getOntologies)).flatMap(x => collectActions(x.asOWLClass())).toSet + c

  private var baseClass : Option[OWLClass] = None
  private var effectClass : Option[OWLClass] = None
  private var entityClass : Option[OWLClass] = None
  private var actionClass : Option[OWLClass] = None
  private var relationClass : Option[OWLClass] = None
  private var preconditionClass : Option[OWLClass] = None
  private var nullType : Option[OWLIndividual] = None
  private var ctorProp : Option[OWLDataProperty] = None
  private var inPackageProp : Option[OWLDataProperty] = None
  private var hasRoleProp   : Option[OWLObjectProperty] = None
  private var hasEffectProp : Option[OWLObjectProperty] = None
  private var hasAObjectProp : Option[OWLObjectProperty] = None
  private var baseObjectProp : Option[OWLObjectProperty] = None
  private var isObjectOfProp : Option[OWLObjectProperty] = None
  private var describedByProp : Option[OWLObjectProperty] = None
  private var isSubjectOfProp : Option[OWLObjectProperty] = None
  private var hasParameterProp : Option[OWLObjectProperty] = None
  private var forCompObjectProp : Option[OWLObjectProperty] = None
  private var dataTypeObjectProp : Option[OWLObjectProperty] = None
  private var hasAspectObjectProp : Option[OWLObjectProperty] = None
  private var hasPreconditionProp : Option[OWLObjectProperty] = None


  private def init() {
    val dataProps = asScalaSet(manager.getOntologies).foldLeft(Set[OWLDataProperty]()){
      (set, onto) => set ++ asScalaSet(onto.getDataPropertiesInSignature).toSet
    }
    val objectProps = asScalaSet(manager.getOntologies).foldLeft(Set[OWLObjectProperty]()){
      (set, onto) => set ++ asScalaSet(onto.getObjectPropertiesInSignature).toSet
    }
    nullType            = getOWLClass(nullName).collect{
      case c => c.getIndividuals(manager.getOntologies).iterator().next()
    }
    entityClass         = getOWLClass("Entity")
    effectClass         = getOWLClass("Effect")
    relationClass       = getOWLClass("Relation")
    preconditionClass   = getOWLClass("Precondition")
    baseClass           = getOWLClass(baseName)
    actionClass         = getOWLClass(actionIRI)
    hasAObjectProp      = objectProps.find( OWLFunctions.getName(_) equals has_a )
    baseObjectProp      = objectProps.find( OWLFunctions.getName(_) equals basedOn )
    hasAspectObjectProp = objectProps.find( OWLFunctions.getName(_) equals hasAspect )
    hasPreconditionProp = objectProps.find( OWLFunctions.getName(_) equals hasPrecondition )
    dataTypeObjectProp  = objectProps.find( OWLFunctions.getName(_) equals hasDataType )
    forCompObjectProp   = objectProps.find( OWLFunctions.getName(_) equals forComponent )
    hasParameterProp    = objectProps.find( OWLFunctions.getName(_) equals hasParameter )
    describedByProp     = objectProps.find( OWLFunctions.getName(_) equals describedBy )
    isSubjectOfProp     = objectProps.find( OWLFunctions.getName(_) equals isSubjectOf )
    isObjectOfProp      = objectProps.find( OWLFunctions.getName(_) equals isObjectOf )
    hasEffectProp       = objectProps.find( OWLFunctions.getName(_) equals hasEffect )
    hasRoleProp         = objectProps.find( OWLFunctions.getName(_) equals hasRole )
    inPackageProp       = dataProps.find( OWLFunctions.getName(_) equals inPackage )
    ctorProp            = dataProps.find( OWLFunctions.getName(_) equals hasConstructor )
  }

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

  def getEntityClass : OWLClass =
    entityClass.get

  def getPreconditionClass : OWLClass =
    preconditionClass.get

  def getEffectClass : OWLClass =
    effectClass.get

  def getActionClass : OWLClass =
    actionClass.get

  def getRelationClass : OWLClass =
    relationClass.get

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

  def getHasPreconditionProp : OWLObjectProperty =
    hasPreconditionProp.get

  def getHasParameterProp : OWLObjectProperty =
    hasParameterProp.get

  def getDescribedByProp : OWLObjectProperty =
    describedByProp.get

  def getIsSubjectOfProp : OWLObjectProperty =
    isSubjectOfProp.get

  def getIsObjectOfProp : OWLObjectProperty =
    isObjectOfProp.get

  def getHasEffectProp : OWLObjectProperty =
    hasEffectProp.get

  def getHasRoleProp : OWLObjectProperty =
    hasRoleProp.get

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


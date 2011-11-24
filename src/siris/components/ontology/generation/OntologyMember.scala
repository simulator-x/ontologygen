package siris.components.ontology.generation

import org.semanticweb.owlapi.model.{OWLEntity, OWLClass, OWLIndividual}

/**
 * User: dwiebusch
 * Date: 22.11.11
 * Time: 16:14
 */


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
    if (isEntity) "class " + getName + "( e : Entity = new Entity ) extends Entity(e) with Removability" else ""

  def getSVarDescriptions : Map[String, String] =
    getIndividuals.foldLeft(Map[String, String]()){
      (m, i) => m.updated(getTargetComponent(i), if (isEntity) getEntitySVarDescription else getSVarDescription(i))
    }

  def getEntityDescription : String =
    if (isEntity)
      "case class " + getName + "EntityDescription( aspects : AspectBase* ) " +
        "extends SpecificDescription(" + getName + "Description, aspects.toList" + (
        if (getAnnotations(getIndividuals.head).nonEmpty)
          ", " + getAnnotations(getIndividuals.head).mkString(", ")
        else
          ""
        ) + ")"
    else ""

  protected def getIndividuals : Set[OWLIndividual] =
    o.getIndividuals(c)

  protected def getName( i : OWLIndividual ) : String =
    i.toStringID.replaceAll(".*#", "")

  protected def getName( entity : OWLEntity ) : String =
    entity.toStringID.replaceAll(".*#", "")

  protected def getEntitySVarDescription : String =
    "object "+ getName +" extends EntitySVarDescription(Symbols."+deCap(getName)+
      ", new " + getName + "(_) )"

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
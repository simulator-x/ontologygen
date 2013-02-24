package simx.components.ontology.generation

import org.semanticweb.owlapi.model.OWLClass


/**
 * User: dwiebusch
 * Date: 25.11.11
 * Time: 16:35
 */

case class DescriptionStub(has          : List[OWLClass]          = Nil,
                           hasAspect    : List[OWLClass]          = Nil,
                           oneOf        : List[DescriptionStub]   = Nil){
  def merge(that : DescriptionStub) =
    if (isEmpty && that.isEmpty)
      None
    else
      Some(DescriptionStub(that.has ::: has, that.hasAspect ::: hasAspect, that.oneOf ::: oneOf))


  def isEmpty =
    has.isEmpty && hasAspect.isEmpty && oneOf.isEmpty

  def getFeatureString : String = has.foldLeft(Set[String]()){
    _ ++ OntologyMember(_).collect{ case x => Set(x.getFullName) }.getOrElse(Set())
  }.mkString(", ")

  override def toString =
    "DescriptionStub" +
      (if (has.nonEmpty) " providing " + has.map(OntoGenTwo.getName).mkString(", ")  else "") +
      (if (hasAspect.nonEmpty) " with aspects " + hasAspect.map(OntoGenTwo.getName).mkString(", ") else "") +
      (if (oneOf.nonEmpty) " with one of (" + oneOf.mkString(", ") + ")" else "")
}

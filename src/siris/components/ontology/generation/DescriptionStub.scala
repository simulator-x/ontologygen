package siris.components.ontology.generation

import org.semanticweb.owlapi.model.OWLClass
import collection.mutable.HashMap
import java.util.UUID
import actors.{Actor, OutputChannel, AbstractActor}


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

  override def toString() =
    "DescriptionStub" +
      (if (has.nonEmpty) " providing " + has.map(OntoGenTwo.getName).mkString(", ")  else "") +
      (if (hasAspect.nonEmpty) " with aspects " + hasAspect.map(OntoGenTwo.getName).mkString(", ") else "") +
      (if (oneOf.nonEmpty) " with one of (" + oneOf.mkString(", ") + ")" else "")
}

class SVarActor2 extends Actor{
  val id = UUID.randomUUID()
  def act() {}
  
  def toMyActor : MyActor = 
    MyActor(id)
}

abstract class MyActor(id : UUID) extends AbstractActor{
  def unpack : AbstractActor =
    MyActor.get(id)
}

object MyActor{
  val map = HashMap[UUID, AbstractActor]()

  def get(id : UUID) =
    map(id)

  def main(args : Array[String]){
    new MyActor(UUID.randomUUID())
  }
}
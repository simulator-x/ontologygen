/*
 * Copyright 2015 The SIRIS Project
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

package simx.components.ontology.generation.member

import org.semanticweb.owlapi.vocab.OWLFacet
import simx.components.ontology.generation.helper.OWLFunctions

import collection.JavaConversions._
import org.semanticweb.owlapi.model._
import simx.components.ontology.generation.OntoGenTwo

/**
 *
 * Created by dennis on 20.02.15.
 */
class OntologySemTrait(owlClass : OWLClass)(implicit o : OntoGenTwo) extends OntologyMember(owlClass){

  val properties = o.getSubClasses(owlClass, recurse = true).foldLeft(Set[PropertyStub]()) {
    (set, subCls) => if (subCls.isAnonymous) set ++ PropertyCollector(recurse = true).parse(subCls)._1 else set
  }

  val relations = o.getSubClasses(owlClass, recurse = true).foldLeft(Set[SemtraitProperty]()) {
    (set, subCls) => if (subCls.isAnonymous) set ++ PropertyCollector(recurse = true).parse(subCls)._2 else set
  }

  override def toString: String =
    "object " + getName +" extends SemanticTrait(" + properties.mkString(" :: ") + relations.mkString(", ") + ")"

  override def getSemtraitDescriptions =
    if (properties.nonEmpty) List(toString) else Nil
}

private case class PropertyCollector(recurse : Boolean = true)(implicit o : OntoGenTwo)
  extends VisitorAdapter((Set[PropertyStub](), Set[SemtraitProperty]()))
{
  private val undesiredProperties = Set(o.getForComponentProp)

  private def addToResult(p : PropertyStub): Unit ={
    result = result._1 + p -> result._2
  }

  private def addToResult(p : SemtraitProperty): Unit ={
    result = result._1 -> (result._2 + p)
  }

  override def visit(someValues: OWLObjectSomeValuesFrom) {
    if (someValues.getProperty == o.getHasPropertyProp){
      if (someValues.getFiller.isAnonymous)
        parse(someValues.getFiller) // this should never happen
      else {
        val cls = someValues.getFiller.asOWLClass()
        addToResult(PropertyStub(cls))
        if (recurse)
          visit(cls)
      }
    }
    else if ( !undesiredProperties.contains(someValues.getProperty) && !someValues.getProperty.isAnonymous) {
      if (someValues.getFiller.isAnonymous){
        val (cls, value) = ValueParser().parse(someValues.getFiller)
        if (cls.isDefined && value.isDefined)
          addToResult(ComplexRelationStub(someValues.getProperty.asOWLObjectProperty(), cls.get, value.get ))
      } else
        addToResult(RelationStub(someValues.getProperty.asOWLObjectProperty(), someValues.getFiller.asOWLClass()))
    }
  }

  override def visit(owlObjectIntersectionOf: OWLObjectIntersectionOf){
    owlObjectIntersectionOf.getOperands.foreach(_ accept this)
  }

  override def visit(owlClass: OWLClass){
    o.getSubClasses(owlClass, recurse = false).filter(_.isAnonymous).foreach(parse)
  }

  override def visit(minCard: OWLObjectMinCardinality){
    if (!minCard.getFiller.isAnonymous && !minCard.getProperty.isAnonymous)
      addToResult(RelationStub(minCard.getProperty.asOWLObjectProperty(), minCard.getFiller.asOWLClass(), ">=", minCard.getCardinality))
  }

  override def visit(maxCard: OWLObjectMaxCardinality){
    if (!maxCard.getFiller.isAnonymous && !maxCard.getProperty.isAnonymous)
      addToResult(RelationStub(maxCard.getProperty.asOWLObjectProperty(), maxCard.getFiller.asOWLClass(), "<=", maxCard.getCardinality))
  }

  override def visit(exactCard: OWLObjectExactCardinality){
    if (!exactCard.getFiller.isAnonymous && !exactCard.getProperty.isAnonymous)
      addToResult(RelationStub(exactCard.getProperty.asOWLObjectProperty(), exactCard.getFiller.asOWLClass(), "==", exactCard.getCardinality))
  }

  override def visit(hasValue: OWLObjectHasValue): Unit ={
    if (!hasValue.getFiller.isAnonymous && !hasValue.getProperty.isAnonymous)
      addToResult(FullRelationStub(hasValue.getProperty.asOWLObjectProperty(), hasValue.getFiller.asOWLNamedIndividual()))
  }

  override def visit(dataValue: OWLDataHasValue): Unit ={
    if (!dataValue.getProperty.isAnonymous)
      addToResult(DataRelationStub(dataValue.getProperty.asOWLDataProperty(), Set("==" -> dataValue.getFiller)))
  }

  override def visit(dataRange: OWLDataSomeValuesFrom): Unit ={
    if (!dataRange.getFiller.isDatatype && !dataRange.getProperty.isAnonymous)
      addToResult(DataRangeVisitor(dataRange.getProperty.asOWLDataProperty()).parse(dataRange.getFiller))
  }
}


protected case class PropertyStub(propClass : OWLClass){
  override def toString: String =
    OWLFunctions.getName(propClass)
}


protected abstract class SemtraitProperty{
  override def toString: String =
    prettyPrint

  def prettyPrint : String
}

protected case class RelationStub(relation : OWLObjectProperty, filler : OWLClass, comparator : String = "==", cardinality: Int = 1) extends SemtraitProperty{
  override def prettyPrint: String =
    if (comparator == "==" && cardinality == 1 )
      OWLFunctions.getName(relation) + "(" + OWLFunctions.getName(filler) + ")"
    else
      OWLFunctions.getName(relation) + "( _ " +  comparator + " " + cardinality  + ", " + OWLFunctions.getName(filler) + ")"
}

protected case class ComplexRelationStub(relation : OWLObjectProperty, cls : OWLClass, value : OWLLiteral) extends SemtraitProperty{
  override def prettyPrint: String = OWLFunctions.getName(relation) + "("  + OWLFunctions.getName(cls) + "(" + value.getLiteral  + "))"
}

protected case class FullRelationStub(relation : OWLObjectProperty, filler : OWLNamedIndividual) extends SemtraitProperty{
  override def prettyPrint: String =
    OWLFunctions.getName(relation) + "(" + OWLFunctions.getName(filler.getIRI) + ")"
}

protected case class DataRelationStub(relation : OWLDataProperty, filler : Set[(String, OWLLiteral)]) extends SemtraitProperty{
  override def prettyPrint: String =
    OWLFunctions.getName(relation) + "( x => " + filler.map(t => " x " + t._1 + " " + t._2.getLiteral ).mkString(" && ")  + ")"
}

protected case class FullDataRelationStub(relation : OWLDataProperty, filler : OWLLiteral) extends SemtraitProperty{
  override def prettyPrint: String =
    OWLFunctions.getName(relation) + "(" + filler.getLiteral + ")"
}




protected case class DataRangeVisitor(prop: OWLDataProperty) extends OWLDataRangeVisitor{
  private var result : DataRelationStub = null

  def parse(dataRange : OWLDataRange): DataRelationStub = {
    dataRange.accept(this)
    result
  }

  override def visit(restriction: OWLDatatypeRestriction): Unit = {
    val dataType = restriction.getDatatype
    val restrictions = restriction.getFacetRestrictions.map{ fRestr =>
      (fRestr.getFacet match {
        case OWLFacet.MIN_EXCLUSIVE => Some("> ")
        case OWLFacet.MIN_INCLUSIVE => Some(">= ")
        case OWLFacet.MAX_INCLUSIVE => Some("<= ")
        case OWLFacet.MAX_EXCLUSIVE => Some("< ")
        case _ =>
          println("error in data value restriction, will be ignored")
          None
      }) -> fRestr.getFacetValue
    }
    result = DataRelationStub(prop, restrictions.filter(_._1.isDefined).map(x => x._1.get -> x._2).toSet)
  }

  override def visit(owlDatatype: OWLDatatype){}
  override def visit(owlDataOneOf: OWLDataOneOf){}
  override def visit(owlDataComplementOf: OWLDataComplementOf){}
  override def visit(owlDataIntersectionOf: OWLDataIntersectionOf){}
  override def visit(owlDataUnionOf: OWLDataUnionOf){}
}

protected case class ValueParser(s : (Option[OWLClass], Option[OWLLiteral]) = None -> None)(implicit o : OntoGenTwo)
  extends VisitorAdapter(s)
{
  override def visit(owlDataHasValue: OWLDataHasValue){
    if (owlDataHasValue.getProperty == o.getHasValueProp)
      result = result._1 -> Some(owlDataHasValue.getFiller)
  }

  override def visit(owlClass: OWLClass) {
    result = Some(owlClass) -> result._2
  }

  override def visit(owlObjectIntersectionOf: OWLObjectIntersectionOf): Unit ={
    owlObjectIntersectionOf.getOperands.foreach(_ accept this)
  }
}
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

import org.semanticweb.owlapi.model._

abstract class VisitorAdapter[T](var result : T) extends OWLClassExpressionVisitor{
  def handleUnknown(x : Any): Unit = {}

  def parse(that : OWLClassExpression) : T = {
    that.accept(this)
    result
  }

  override def visit(owlClass: OWLClass): Unit = handleUnknown(owlClass)
  override def visit(owlObjectUnionOf: OWLObjectUnionOf): Unit = handleUnknown(owlObjectUnionOf)
  override def visit(owlObjectIntersectionOf: OWLObjectIntersectionOf): Unit = handleUnknown(owlObjectIntersectionOf)
  override def visit(owlDataAllValuesFrom: OWLDataAllValuesFrom): Unit = handleUnknown(owlDataAllValuesFrom)
  override def visit(owlDataHasValue: OWLDataHasValue): Unit = handleUnknown(owlDataHasValue)
  override def visit(owlObjectMinCardinality: OWLObjectMinCardinality): Unit = handleUnknown(owlObjectMinCardinality)
  override def visit(owlObjectHasValue: OWLObjectHasValue): Unit = handleUnknown(owlObjectHasValue)
  override def visit(owlObjectAllValuesFrom: OWLObjectAllValuesFrom): Unit = handleUnknown(owlObjectAllValuesFrom)
  override def visit(owlObjectSomeValuesFrom: OWLObjectSomeValuesFrom): Unit = handleUnknown(owlObjectSomeValuesFrom)
  override def visit(owlObjectComplementOf: OWLObjectComplementOf): Unit = handleUnknown(owlObjectComplementOf)
  override def visit(owlDataSomeValuesFrom: OWLDataSomeValuesFrom): Unit = handleUnknown(owlDataSomeValuesFrom)
  override def visit(owlObjectOneOf: OWLObjectOneOf): Unit = handleUnknown(owlObjectOneOf)
  override def visit(owlObjectHasSelf: OWLObjectHasSelf): Unit = handleUnknown(owlObjectHasSelf)
  override def visit(owlObjectMaxCardinality: OWLObjectMaxCardinality): Unit = handleUnknown(owlObjectMaxCardinality)
  override def visit(owlObjectExactCardinality: OWLObjectExactCardinality): Unit = handleUnknown(owlObjectExactCardinality)
  override def visit(owlDataMaxCardinality: OWLDataMaxCardinality): Unit = handleUnknown(owlDataMaxCardinality)
  override def visit(owlDataExactCardinality: OWLDataExactCardinality): Unit = handleUnknown(owlDataExactCardinality)
  override def visit(owlDataMinCardinality: OWLDataMinCardinality): Unit = handleUnknown(owlDataMinCardinality)
}

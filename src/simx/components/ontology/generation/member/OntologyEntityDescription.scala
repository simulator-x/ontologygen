/*
 * Copyright 2014 The SIRIS Project
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

import org.semanticweb.owlapi.model.{OWLObjectIntersectionOf, OWLIndividual, OWLClass}
import simx.components.ontology.generation.OntoGenTwo
import simx.components.ontology.generation.helper.{WritableForPackage, OWLFunctions}
import collection.JavaConversions._

/**
 * Created by dwiebusch on 01.09.14
 */
class OntologyEntityDescription(override val owlClass : OWLClass)(implicit o : OntoGenTwo )
  extends OntologyMember(owlClass)(o){
  override def getEntityStrings : List[String] =
    o.getIndividuals(owlClass).flatMap{
      getDescribedClasses(_).
        filterNot(_ equals o.getEntityClass).
        map( i => "class " + OWLFunctions.getName(i) + "( e : Entity, a : SVarActor ) extends Entity(e)(a) ")
    }.toList

  override def isEntity: Boolean =
    true

  override def getSVarDescriptions = {
    o.getIndividuals(owlClass).
      flatMap(getDescribedClasses(_).filterNot(_ equals o.getEntityClass)).map(cls => {
        new WritableForPackage {
          def packageName: String = "simx.core"
          def toScalaCode: String =
            "object " + OWLFunctions.getName(cls) + " extends EntitySValDescription(Entity.valueDescription as Symbols." + deCap(OWLFunctions.getName(cls)) + ", " +
            "new simx.core.ontology.entities." + OWLFunctions.getName(cls) + "(_, _), \"" + cls.toStringID + "\")"
        }
      }).toList
  }

  protected def tab(preChar : String = "", i : Int = 1) =
    preChar + "\n" + ("\t" * i)

  protected case class AspectFinder() extends VisitorAdapter(Set[OWLClass]()){
    override def visit(intersection: OWLObjectIntersectionOf) =
      intersection.getOperands.foreach(_ accept this)

    override def visit(owlClass: OWLClass) =
      if (o.getSuperClasses(owlClass).contains(o.getAspectClass))
        result += owlClass
  }

  protected def getEntitySVarDescription( i : OWLIndividual ) =
    getDescribedClasses(i).filterNot(_ equals o.getEntityClass).map{ cls =>
      import o._
      var aspects = getObjectProperties(i)(getHasAspectObjectProp).map(OntologyAspectIndividual(_)(o)).toList
      val generalAspects = getSubClasses(cls, recurse = false).flatMap(AspectFinder().parse)

      // generic version
      if (generalAspects.nonEmpty){
        val possibleAspects = generalAspects.map{ aspType => aspType -> o.getIndividuals(aspType) }.filterNot{
          generalAsp => aspects.exists( moreSpecificAsp => getTypes(moreSpecificAsp.i).contains(generalAsp._1) )
        }
        if (possibleAspects.forall(_._2.size == 1))
          aspects ++= possibleAspects.map(pa => OntologyAspectIndividual(pa._2.head)).toList
      }

      if (aspects.nonEmpty) {
        val overrides = getObjectPropertyAnnotationsFor(i)(getOverridesProvide).map{
          ovr => OntologyAspectIndividual(ovr._1) -> ovr._2.map(x => deCap(x.getValue.toString.replaceAll(".*#", "")))
        }

        val filteredParameters = aspects.foldLeft(Map[OntologyAspectIndividual, List[Parameter]]()){
          (map, aspect) => map.updated(aspect, aspect.parameters.filterNot{
            parameter => overrides.exists(ovr => ovr._2.contains(parameter.name) && ovr._1 != aspect)
          })
        }

        "case class " + OWLFunctions.getName(i) + tab("(") +
          filteredParameters.values.flatten.toList.
            sortWith((a, b) => (a.defaultValue.isEmpty && b.defaultValue.nonEmpty) || a.name < b.name ).
            map(p => p.optionString + (if (p.defaultValue.isEmpty) " = Right(null)" else "" )).
            mkString(tab(",")) + "\n) " +
          "extends EntityDescription " + tab("(", 2) +
          aspects.map{
            aspect =>
              OWLFunctions.getName(aspect.name.get.asOWLClass()) + tab("(", 3) +
                filteredParameters(aspect).map(p => p.name + " = " + p.name ).mkString(tab(",", 3)) + tab() + ")"
          }.mkString(tab(",")) +
          "\n)"
      }
      else {
        "class " + OWLFunctions.getName(cls) + "EntityDescription( val name: Symbol, aspects : List[EntityAspect] ) " +
          "extends SpecificEntityDescription(ontology.types." + OWLFunctions.getName(cls) + ", aspects, name, Nil" +
          getDescriptionStub.collect { case stub => ", Seq(" + stub.getFeatureString + ")"}.getOrElse("") +
          ")"
      }
    }.toList

  override def getEntityDescriptions : List[String] =
    o.getIndividuals(owlClass).foldLeft(List[String]()){
      (list, elem) => getEntitySVarDescription(elem) ::: list
    }


  override def toString =
    "OntologyMember " + getName + ":\n--------------------------------------------\n" +
      "Symbol:\n\t" + getSymbolString + "\n" + "Descriptions:\n\t" + getSVarDescriptions.mkString("\n\t") +
      "\nAssocEntity:\n\t" + getEntityStrings + "\nEntityDescription:\n\t" + getEntityDescriptions

}

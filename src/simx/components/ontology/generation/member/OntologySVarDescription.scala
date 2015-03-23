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

import org.semanticweb.owlapi.model.{OWLIndividual, OWLClass}
import simx.components.ontology.generation.OntoGenTwo
import simx.components.ontology.generation.helper.{Infix, Prefix, OWLFunctions, WritableForPackage}

/**
 * Created by dwiebusch on 01.09.14
 */
class OntologySVarDescription(override val owlClass : OWLClass)(implicit o : OntoGenTwo )
  extends OntologyMember(owlClass)(o){


  def toSVarDesc(pkg: String)(scalaCode: String) = new WritableForPackage {
    def packageName: String = pkg
    def toScalaCode: String = scalaCode
  }

  override def getSVarDescriptions = getIndividuals.map(getSVarDescriptionsFor).flatten.toList

  override def toString =
    "OntologyMember " + getName + ":\n--------------------------------------------\n" +
      "Symbol:\n\t" + getSymbolString + "\n" + "Descriptions:\n\t" + getSVarDescriptions.mkString("\n\t")

  //TODO: Why is this called twice?
  private def getSVarDescriptionsFor( i : OWLIndividual ) : List[WritableForPackage] = {
    import OntologyMember._

    val describedClasses = getDescribedClasses(i).filterNot{ cls => o.getSuperClasses(cls).contains(o.getEntityClass) }
    if (describedClasses.isEmpty)
      return Nil


    val baseDesc = getBase(i) match {
      case Some(b) if OntologyMember(b).isDefined =>
        getTargetComponent(getBase(i).get) + ".ontology.types."+ OWLFunctions.getName(getDescribedClasses(b).head)  //OntologyMember(b).get.getName
      case _ =>
        OntologyMember.nullTypeClassName
    }

    describedClasses.map{cls =>
      val semantic = "simx.core.ontology.Symbols." + deCap(OWLFunctions.getName(cls))

      val iriString = "\"" + cls.toStringID + "\""

      val superDesc = typeString(i) match {
        case Some(typ) =>
          sVarDescClassName + "(" + baseDesc + " as " + semantic + " withType " + typ + " definedAt " + iriString + ")"
        case None =>
          sVarDescClassName + "(" + baseDesc + " as " + semantic + " definedAt " + iriString + ")"
      }

      val clsName = OWLFunctions.getName(cls)

      val prefixFunctions: Set[PlainFunction] = o.members.map{ m =>
        m.getFunctions.filter{ f =>
          (f.getReturnValue.get == cls) && f.getOperatorType.isInstanceOf[Prefix]
        }
      }.flatten

      val infixFunctions: Set[PlainFunction] = o.members.map{ m =>
        m.getFunctions.filter{ f =>
          (f.getParameters.head._2 == cls) && f.getOperatorType.isInstanceOf[Infix]
        }
      }.flatten

      val sVarConstructorCode =
        OWLFunctions.indentAllLines(1)("override def apply(value: dataType) = new " + clsName + "(value)") + "\n"

      def functionCode(functions :Set[PlainFunction]) =
        if(functions.isEmpty) ""
        else OWLFunctions.indentAllLines(1)(functions.map(_.getSVarDescriptionCode).mkString("\n")) + "\n"

      val objectCode =
        "object " + clsName + " extends " + superDesc +
        " {\n" + sVarConstructorCode + functionCode(prefixFunctions) + "}"

      val classCode =
        "class " + clsName + "(private val _value : " + clsName + ".dataType) extends SVal(_value, " + clsName + ".valueDescription, " + clsName + ")" +
        (if(infixFunctions.nonEmpty) " {\n" + functionCode(infixFunctions) + "}" else "")

      val code = objectCode + "\n" + classCode

      new WritableForPackage {
        def packageName: String = getTargetComponent(i)
        def toScalaCode: String = code
      }
    }.toList
  }
}

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

trait SVarDescriptionForPackage extends WritableForPackage {
  val interpolatorCode: Option[String]
}

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
  private def getSVarDescriptionsFor( i : OWLIndividual ) : List[SVarDescriptionForPackage] = {
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

      def getBaseTrace(i: OWLIndividual): List[OWLClass] = {
        getBase(i) match {
          case Some(bInd) =>
            val bClass = getDescribedClasses(bInd).filterNot{clazz => o.getSuperClasses(clazz).contains(o.getEntityClass)}.head
            bClass :: getBaseTrace(bInd)
          case None =>
            Nil
        }
      }

      val baseTrace = getBaseTrace(i)

      //simx.core.svaractor.semantictrait.base.ValueDescription[simx.core.svaractor.semantictrait.base.Base,simx.core.ontology.Symbols.boolean.SymbolType]
      def toValueDescription(bTrace: List[OWLClass]): String = {
        bTrace match {
          case head :: tail =>
            "simx.core.svaractor.semantictrait.base.ValueDescription[" + toValueDescription(tail) + ",simx.core.ontology.Symbols." + deCap(OWLFunctions.getName(head)) + ".SymbolType]"
          case Nil =>
            "simx.core.svaractor.semantictrait.base.Base"
        }
      }

      val baseSemantics = toValueDescription(baseTrace)

      val iriString = "\"" + cls.toStringID + "\""

      val dt = getDataType(i).map{t => getTypeString(t).getOrElse(OWLFunctions.getName(t))}
      val bt: Option[String] = getBaseDataType(i).map{t => getTypeString(t).getOrElse(OWLFunctions.getName(t))}
      val mostGeneralBase: String = getDataType(getMostGeneralBase(i)).map{t => getTypeString(t).getOrElse(OWLFunctions.getName(t))}.get
      val dataType = dt.getOrElse(bt.get)

      //simx.core.entity.description.SVal[Boolean,simx.core.entity.typeconversion.TypeInfo[Boolean,Boolean],simx.core.svaractor.semantictrait.base.ValueDescription[simx.core.svaractor.semantictrait.base.Base,simx.core.ontology.Symbols.boolean.SymbolType],simx.core.ontology.Symbols.key_m.SymbolType]
      val sValTypeString =
        "[" + dataType + "," +
        "simx.core.entity.typeconversion.TypeInfo[" + dataType + "," + dataType + "]," +
        baseSemantics + "," + //"simx.core.svaractor.semantictrait.base.Base," + //baseSemantic + ".SymbolType," +
        semantic + ".SymbolType]"

      val sValDescriptionTypeString =
        "[" + dataType + "," +
        mostGeneralBase + "," +
        baseSemantics + "," + //"simx.core.svaractor.semantictrait.base.Base," + //baseSemantic + ".SymbolType," +
        semantic + ".SymbolType]"

      val superDesc = typeString(i) match {
        case Some(typ) =>
          sVarDescClassName + sValDescriptionTypeString + "(" + baseDesc + " as " + semantic + " withType " + typ + " definedAt " + iriString + ")"
        case None =>
          sVarDescClassName + sValDescriptionTypeString + "(" + baseDesc + " as " + semantic + " definedAt " + iriString + ")"
      }

      val clsName = OWLFunctions.getName(cls)

      val prefixFunctions: Set[OntologyFunction] = o.members.map{ m =>
        m.getFunctions.filter{ f =>
          (f.getReturnType == i) && f.getOperatorType.isInstanceOf[Prefix]
        }
      }.flatten

      val infixFunctions: Set[OntologyFunction] = o.members.map{ m =>
        m.getFunctions.filter{ f =>
          (f.getParameterTypes.head == i) && f.getOperatorType.isInstanceOf[Infix]
        }
      }.flatten

      val historyType = "simx.core.entity.description.HistoryStorage.HistoryType[" + dataType + "]"

      val sVarConstructorCode =
        OWLFunctions.indentAllLines(1)("override def apply(value: dataType): SemanticSValType = new " + clsName + "(value, -1L, Nil)") + "\n" +
        OWLFunctions.indentAllLines(1)("override def apply(value: dataType, timestamp : scala.Long): SemanticSValType = new " + clsName + "(value, timestamp, Nil)") + "\n" +
        OWLFunctions.indentAllLines(1)("override def apply(value: dataType, timestamp : scala.Long, history: " + historyType + ") = new " + clsName + "WithHistory(value, timestamp, history)") + "\n"

      //implicit val posInterpol: Interpolator[ConstVec3f, simx.core.ontology.Symbols.position.SymbolType]
      def _interpolatorCode = "implicit val " + clsName.toLowerCase + "Interpolator: Interpolator[" + dataType + "," +  semantic + ".SymbolType] = new simx.core.ontology.functions.DataTypeBasedLinearInterpolator"

      def functionCode(functions :Set[OntologyFunction]) =
        if(functions.isEmpty) ""
        else OWLFunctions.indentAllLines(1)(functions.map(_.getSVarDescriptionCode).mkString("\n")) + "\n"

      //object Position extends simx.core.ontology.SValDescription[...](...) with Test[Position] {
      val objectCode =
        "object " + clsName + " extends " + superDesc + " with simx.core.ontology.types.SemanticSValType[" + clsName + "]"+
          " {" /*"\n  type SemanticSValType = " + clsName*/ + "\n" + sVarConstructorCode + functionCode(prefixFunctions) + "}"

      val classCode =
        "class " + clsName + "(private val _value : " + clsName + ".dataType, private val _timestamp: scala.Long, private val _history: " + historyType + ") extends simx.core.entity.description.SVal" + sValTypeString +  "(_value, " + clsName + ".valueDescription, " + clsName + ", _timestamp, _history)" + " {\n" +
        OWLFunctions.indentAllLines(1)("def withHistory = new " + clsName + "WithHistory(value, timestamp, history)") + "\n" +
        functionCode(infixFunctions) + "}" + "\n" +
        "class " + clsName + "WithHistory(_v: " + dataType + ", _t: scala.Long, _h: " + historyType + ") extends " + clsName + "(_v,_t,_h) with SValHistory[" + dataType + "," + semantic + ".SymbolType," + clsName +"] {" + "\n" +
        OWLFunctions.indentAllLines(1)("def newNonHistoryInstance(value: " + dataType + ") = " + clsName + "(value)") + "\n" +
        "}"

      val code = objectCode + "\n" + classCode

      new SVarDescriptionForPackage {
        def packageName: String = getTargetComponent(i)
        def toScalaCode: String = code
        val interpolatorCode: Option[String] = Some(_interpolatorCode)
      }
    }.toList
  }
}

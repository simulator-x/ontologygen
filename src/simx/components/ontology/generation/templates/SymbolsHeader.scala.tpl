import simx.core.entity.description.Semantics
import simx.core.ontology.types.OntologySymbol

object Symbols {
  private implicit def symToSem( s : Symbol ) =  Semantics(s)

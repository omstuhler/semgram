#' @importFrom rsyntax annotate_tqueries cast_text children NOT OR not_children parents tquery
NULL

###############################################################################################
##### Rule: Actor of nsubj act with object and a noun conjunct (entity)
##### Example: "Joseph, Sarah, and Steve call Michael and ENTITY." (Joseph Sarah Steve, call)
##### Example: "Joseph, Sarah, and Steve give Michael an apple and an ENTITY." (Joseph Sarah Steve, give)
##### Note: We can't collect more verbs because based on the grammar, it will be
##### unclear whether these will be transitive.

At_3 = function(tokens, entities, verb_pos, agent_patient_pos, extract){
  rule = tquery(OR(token = entities, appos_child = "appos_child"), relation = "conj",
                label = "Entity", fill = F,
                parents(relation = c("dobj", "dative"), pos = c("NOUN", "PROPN", "PRON"),
                        parents(pos = "VERB",
                                label = "treatment", fill = F,
                                children(pos = agent_patient_pos, relation = "nsubj",
                                         label = "Agent", fill = F,
                                         children(pos = agent_patient_pos, relation = c("conj", "appos"), req = F,
                                                  label = "Agent", fill = F, depth = 3
                                         )
                                )
                        )
                )
  )

  tokens = annotate_tqueries(tokens, "query", rule, overwrite = T, copy = F)

  if(all(is.na(tokens$query))){
    casted = data.table(doc_id = character(), ann_id = factor(), treatment = character(), Entity = character(), Agent = character())
  } else {
    casted = cast_text(tokens, 'query', text_col = extract)
  }
  return(casted)
}

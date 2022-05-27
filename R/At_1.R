#' @importFrom rsyntax annotate_tqueries cast_text children NOT OR not_children parents tquery
NULL

###############################################################################################
##### Rule: Actor of nsubj act with object
##### Example: "Joseph, Sarah, and Steve call ENTITY." (Joe Sarah Steve, call)
##### Example: "Joseph, Sarah, and Steve give Michael a ENTITY." (Joe Sarah Steve, give)

At_1 = function(tokens, entities, verb_pos, agent_patient_pos, extract){
  rule = tquery(OR(token = entities, appos_child = "appos_child"), relation = c("dobj", "dative"),
                label = "Entity", fill = F,
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

  tokens = annotate_tqueries(tokens, "query", rule, overwrite = T, copy = F)

  if(all(is.na(tokens$query))){
    casted = data.table(doc_id = character(), ann_id = factor(), treatment = character(), Entity = character(), Agent = character())
  } else {
    casted = cast_text(tokens, 'query', text_col = extract)
  }
  return(casted)
}

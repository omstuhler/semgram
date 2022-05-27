#' @importFrom rsyntax annotate_tqueries cast_text children NOT OR not_children parents tquery
NULL

###############################################################################################
##### Rule: Actor of conjuncted nsubj act with object
##### Example: "Joseph, Sarah, and Steve came and asked ENTITY." (Joe Sarah Steve, ask)

At_2 = function(tokens, entities, verb_pos, agent_patient_pos, extract){
  rule = tquery(OR(token = entities, appos_child = "appos_child"), relation = c("dobj", "dative"),
                label = "Entity", fill = F,
                parents(pos = "VERB", relation = c("conj","xcomp"),
                        label = "treatment", fill = F,
                        not_children(pos = agent_patient_pos, relation = "nsubj"),
                        parents(pos = c("VERB", "AUX"),
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

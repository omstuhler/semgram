#' @importFrom rsyntax annotate_tqueries cast_text children NOT OR not_children parents tquery
NULL

###############################################################################################
##### Rule: Object of verb with xcomp clause and have-verb conjunct
##### Example: "ENTITY wants to eat and have a conversation." (rice, grapes, steak)

H_8 = function(tokens, entities, verb_pos, agent_patient_pos, extract){
  rule = tquery(OR(token = entities, appos_child = "appos_child"), relation = "nsubj",
                label = "Entity", fill = F,
                parents(pos = c("VERB", "AUX"),
                        children(pos = "VERB", relation = "xcomp",
                                 not_children(pos = "AUX", lemma = "be", relation = "auxpass"),
                                 not_children(relation = "nsubj"),
                                 children(pos = c("VERB", "AUX"), relation = "conj",
                                          lemma = "have",
                                          children(pos = agent_patient_pos, relation = c("dobj", "dative"),
                                                   label = "Possession", fill = F,
                                                   children(pos = agent_patient_pos, relation = c("conj", "appos"), req = F,
                                                            label = "Possession", fill = F, depth = 3
                                                   )
                                          )
                                 )
                        )
                )
  )

  tokens = annotate_tqueries(tokens, "query", rule, overwrite = T, copy = F)
  if(all(is.na(tokens$query))){
    casted = data.table(doc_id = character(), ann_id = factor(), Entity = character(), Possession = character())
  } else {
    casted = cast_text(tokens, 'query', text_col = extract)
  }
  return(casted)
}

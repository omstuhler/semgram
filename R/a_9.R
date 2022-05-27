#' @importFrom rsyntax annotate_tqueries cast_text children NOT OR not_children parents tquery
NULL

###############################################################################################
##### Rule: Verb with xcomp clause and its conjuncts
##### Example: "ENTITY wants to eat and drink." (eat, drink)
##### Note: not_children inserted is in order to avoid passiveness.

a_9 = function(tokens, entities, verb_pos, agent_patient_pos, extract){
  rule = tquery(OR(token = entities, appos_child = "appos_child"), relation = "nsubj",
                label = "Entity", fill = F,
                parents(pos = c("VERB", "AUX"),
                        children(pos = "VERB", relation = "xcomp",
                                 not_children(pos = "AUX", lemma = "be", relation = "auxpass"),
                                 not_children(relation = "nsubj"),
                                 label = "action", fill = F,
                                 children(pos = "VERB", relation = "conj", req = F,
                                          not_children(pos = "AUX", lemma = "be", relation = "auxpass"),
                                          not_children(relation = "nsubj"),
                                          label = "action", fill = F,
                                          children(pos = "VERB", relation = "conj", req = F,
                                                   not_children(pos = "AUX", lemma = "be", relation = "auxpass"),
                                                   not_children(relation = "nsubj"),
                                                   label = "action", fill = F
                                          )
                                 )
                        )

                )
  )

  tokens = annotate_tqueries(tokens, "query", rule, overwrite = T, copy = F)
  if(all(is.na(tokens$query))){
    casted = data.table(doc_id = character(), ann_id = factor(), Entity = character(), action = character())
  } else {
    casted = cast_text(tokens, 'query', text_col = extract)
  }
  return(casted)
}

#' @importFrom rsyntax annotate_tqueries cast_text children NOT OR not_children parents tquery
NULL

###############################################################################################
##### Rule: Objects of verb with xcomp clause and ENTITY as subject conjunct
##### Example: "Jonathan and ENTITY want to eat rice, grapes, and steak." (eat, rice grapes steak)
##### Example: "Jonathan and ENTITY want to give Steven a present." (give, Steven present)
##### Note: not_children inserted is to avoid passiveness.

aP_13 = function(tokens, entities, verb_pos, agent_patient_pos, extract){
  rule = tquery(OR(token = entities, appos_child = "appos_child"), relation = "conj",
                label = "Entity", fill = F,
                parents(pos = c("NOUN", "PROPN", "PRON"), relation = "nsubj",
                        parents(pos = c("VERB", "AUX"),
                                children(pos = "VERB", relation = "xcomp",
                                         label = "action", fill = F,
                                         NOT(lemma = "have"),
                                         not_children(pos = "AUX", lemma = "be", relation = "auxpass"),
                                         not_children(relation = "nsubj"),
                                         children(pos = agent_patient_pos, relation = c("dobj", "dative"),
                                                  label = "Patient", fill = F,
                                                  children(pos = agent_patient_pos, relation = c("conj", "appos"), req = F,
                                                           label = "Patient", fill = F, depth = 3
                                                  )
                                         )
                                )
                        )
                )
  )

  tokens = annotate_tqueries(tokens, "query", rule, overwrite = T, copy = F)

  if(all(is.na(tokens$query))){
    casted = data.table(doc_id = character(), ann_id = factor(), action = character(), Entity = character(), Patient = character())
  } else {
    casted = cast_text(tokens, 'query', text_col = extract)
  }
  return(casted)
}

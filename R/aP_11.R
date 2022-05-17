###############################################################################################
##### Rule: Object of verb with xcomp clause
##### Example: "ENTITY wants to eat rice, grapes, and, steak." (eat, rice grapes steak)
##### Example: "ENTITY wants to give Steve a present." (give, Steve present)
##### Note: not_children inserted is to avoid passiveness.

aP_11 = function(tokens, entities, verb_pos, agent_patient_pos, extract){
  rule = tquery(OR(token = entities, appos_child = "appos_child"), relation = "nsubj",
                parents(pos = c("VERB", "AUX"),
                        children(pos = "VERB", relation = "xcomp",
                                 label = "act", fill = F,
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
  
  tokens = tokens %>% annotate_tqueries("query_aP", rule, overwrite = T, copy = F)
  
  if(all(is.na(tokens$query_aP))){
    aP_casted = data.table(doc_id = character(), ann_id = factor(), act = character(), Patient = character())
  } else {
    aP_casted = cast_text(tokens, 'query_aP', text_col = extract)
  }
  return(aP_casted)
}
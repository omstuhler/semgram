###############################################################################################
##### Rule: Object of nsubj act
##### Example: "ENTITY asked Joe, Sue, and Michael." (asked, Joe Sue Michael)
##### Example: "ENTITY give Joseph a present." (give, Joseph present)

aP_1 = function(tokens, entities, verb_pos, agent_patient_pos, extract){
  rule = tquery(OR(token = entities, appos_child = "appos_child"), relation = "nsubj",
                parents(pos = c("VERB", "AUX"),
                        fill = F, label = "act",
                        NOT(lemma = "have"),
                        children(pos = agent_patient_pos, relation = c("dobj", "dative"), label = "Patient",
                                 fill = F,
                                 children(pos = agent_patient_pos, relation = c("conj", "appos"), depth = 3, req = F,
                                          label = "Patient",
                                          fill = F
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
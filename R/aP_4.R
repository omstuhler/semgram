###############################################################################################
##### Rule: Object of conjuncted verb but ENTITY is conjunct of nominal subject
##### Example: "Joe and ENTITY came and kissed Joe, Sue, and Michael." (kissed, Joe Sue Michael)
##### Example: "Joe and ENTITY came and gave Steve a present." (gave, Steve present)

aP_4 = function(tokens, entities, verb_pos, agent_patient_pos, extract){
  rule = tquery(OR(token = entities, appos_child = "appos_child"), relation = "conj",
                parents(pos = c("NOUN", "PROPN", "PRON"), relation = "nsubj",
                        parents(pos = c("VERB", "AUX"),
                                children(pos = c("VERB", "AUX"), relation = "conj",
                                         fill = F, label = "act",
                                         NOT(lemma = "have"),
                                         not_children(relation = "nsubj", depth = 1),
                                         children(pos = agent_patient_pos, relation = c("dobj", "dative"),
                                                  label = "Patient",
                                                  fill = F,
                                                  children(pos = agent_patient_pos, relation = c("conj", "appos"),
                                                           label = "Patient", req = F, depth = 3,
                                                           fill = F
                                                  )
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
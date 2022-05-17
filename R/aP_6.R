###############################################################################################
##### Rule: Objects of passive subject with by and noun conjunct
##### Example: "Joseph, Sue and Michael are asked by Jack and ENTITY." (asked, Joe Sue Michael)
##### Example: "Mike and Steve were given a present by Jack and ENTITY." (given, Steve present)

aP_6 = function(tokens, entities, verb_pos, agent_patient_pos, extract){
  rule = tquery(OR(token = entities, appos_child = "appos_child"), relation = "conj",
                parents(pos = c("NOUN", "PROPN", "PRON"), relation = "pobj",
                        parents(pos = "ADP", lemma = "by", relation = "agent",
                                parents(pos = c("VERB", "AUX"),
                                        label = "act", fill = F,
                                        NOT(lemma = "have"),
                                        children(relation = c("nsubjpass", "dobj"), pos = agent_patient_pos,
                                                 label = "Patient", fill = F,
                                                 children(relation = c("conj", "appos"), pos = agent_patient_pos, req = F,
                                                          label = "Patient", fill = F, depth = 3
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
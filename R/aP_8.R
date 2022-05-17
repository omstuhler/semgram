###############################################################################################
##### Rule: Objects of passive subject with by and conjuncted verb (second verb)
##### Example: "Joseph, Sue and Michael were called and asked by ENTITY." (called, Joseph Sue Michael)

aP_8 = function(tokens, entities, verb_pos, agent_patient_pos, extract){
  rule = tquery(OR(token = entities, appos_child = "appos_child"), relation = "pobj",
                parents(pos = "ADP", lemma = "by", relation = "agent",
                        parents(pos = "VERB", relation = "conj",
                                parents(pos = "VERB",
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
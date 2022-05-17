###############################################################################################
##### Rule: Actor of nsubj act with object
##### Example: "Joseph, Sarah, and Steve call ENTITY." (Joe Sarah Steve, call)
##### Example: "Joseph, Sarah, and Steve give Michael a ENTITY." (Joe Sarah Steve, give)

At_1 = function(tokens, entities, verb_pos, agent_patient_pos, extract){
  rule = tquery(OR(token = entities, appos_child = "appos_child"), relation = c("dobj", "dative"),
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
  
  tokens = tokens %>% annotate_tqueries("query_At", rule, overwrite = T, copy = F)
  
  if(all(is.na(tokens$query_At))){
    At_casted = data.table(doc_id = character(), ann_id = factor(), treatment = character(), Agent = character())
  } else {
    At_casted = cast_text(tokens, 'query_At', text_col = extract)
  }
  return(At_casted)
}
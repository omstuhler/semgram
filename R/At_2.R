###############################################################################################
##### Rule: Actor of conjuncted nsubj act with object
##### Example: "Joseph, Sarah, and Steve came and asked ENTITY." (Joe Sarah Steve, ask)

At_2 = function(tokens, entities, verb_pos, agent_patient_pos, extract){
  rule = tquery(OR(token = entities, appos_child = "appos_child"), relation = c("dobj", "dative"),
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
  
  tokens = tokens %>% annotate_tqueries("query_At", rule, overwrite = T, copy = F)
  
  if(all(is.na(tokens$query_At))){
    At_casted = data.table(doc_id = character(), ann_id = factor(), treatment = character(), Agent = character())
  } else {
    At_casted = cast_text(tokens, 'query_At', text_col = extract)
  }
  return(At_casted)
}
###############################################################################################
##### Rule: Actor of by act with object and noun conjunct (entity)
##### Example: "Sue and ENTITY are asked by Peter, Joseph, and Sue." (Peter, ask)
##### Example: "Steve were given apples and ENTITY by Peter, Joseph, and Sue." (Peter, give)

At_5 = function(tokens, entities, verb_pos, agent_patient_pos, extract){
  rule = tquery(OR(token = entities, appos_child = "appos_child"), relation = "conj",
                parents(pos = c("NOUN", "PROPN", "PRON"), relation = c("nsubjpass", "dobj"),
                        parents(pos = "VERB",
                                label = "treatment", fill = F,
                                children(pos = "ADP", lemma = "by", relation = "agent",
                                         children(pos = agent_patient_pos, relation = "pobj",
                                                  label = "Agent", fill = F,
                                                  children(pos = agent_patient_pos, relation = c("conj", "appos"), req = F,
                                                           label = "Agent", fill = F, depth = 3
                                                  )
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
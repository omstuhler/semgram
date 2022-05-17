###############################################################################################
##### Rule: Actor of by act with object and noun conjunct (entity)
##### Example: "Sue and ENTITY are asked by Peter, Joseph, and Sue." (Peter, Joseph, Sue)
##### Example: "Steve were given apples and ENTITY by Peter, Joseph, and Sue." (Peter, Joseph, Sue)

agent_5 = function(tokens, entities, verb_pos, agent_patient_pos){
  rule = tquery(OR(token = entities, appos_child = "appos_child"), relation = "conj",
                parents(pos = c("NOUN", "PROPN", "PRON"), relation = c("nsubjpass"),
                        parents(pos = "VERB",
                                children(pos = "ADP", lemma = "by", relation = "agent",
                                         children(pos = agent_patient_pos, relation = "pobj",
                                                  label = "Motif", fill = F,
                                                  children(pos = agent_patient_pos, relation = c("conj", "appos"), req = F,
                                                           label = "Motif", fill = F, depth = 3
                                                  )
                                         )
                                )
                        )
                )
  )
  
  tokens = tokens %>%
    annotate_tqueries("obj_of_by_act_nconj_ac", rule, overwrite = T, copy = F)
  tokens[,c(ncol(tokens)-1,ncol(tokens))] = NULL
  return(tokens)
}
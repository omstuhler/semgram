###############################################################################################
##### Rule: Actor of by act with object
##### Example: "ENTITY is asked by Peter, Joseph, and Sue." (Peter Joseph Sue)
##### Example: "Steven was given ENTITY by Peter, Joseph, and Sue" (Peter Joseph Sue)

agent_4 = function(tokens, entities, verb_pos, agent_patient_pos){
  rule = tquery(OR(token = entities, appos_child = "appos_child"), relation = c("nsubjpass", "dobj"),
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
  
  tokens = tokens %>%
    annotate_tqueries("by_act_agent", rule, overwrite = T, copy = F)
  tokens[,c(ncol(tokens)-1,ncol(tokens))] = NULL
  return(tokens)
}
###############################################################################################
##### Rule: Objects of passive subject with by and conjuncted verb
##### Example: "Joseph, Sue and Michael were called and asked by ENTITY." (Joseph, Sue, Michael)

P_7 = function(tokens, entities, verb_pos, agent_patient_pos){
  rule = tquery(OR(token = entities, appos_child = "appos_child"), relation = "pobj",
                parents(pos = "ADP", lemma = "by", relation = "agent",
                        parents(pos = "VERB", relation = "conj",
                                parents(pos = "VERB",
                                        children(relation = c("nsubjpass", "dobj"), pos = agent_patient_pos,
                                                 label = "Motif", fill = F,
                                                 children(relation = c("conj", "appos"), pos = agent_patient_pos, req = F,
                                                          label = "Motif", fill = F, depth = 3
                                                 )
                                        )
                                )
                        )
                )
  )
  
  tokens = tokens %>%
    annotate_tqueries("by_act_obj_cverb", rule, overwrite = T, copy = F)
  tokens[,c(ncol(tokens)-1,ncol(tokens))] = NULL
  return(tokens)
}
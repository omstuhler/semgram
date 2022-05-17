###############################################################################################
##### Rule: Object of verb with xcomp clause and verb conjunct
##### Example: "ENTITY wants to chat and eat rice, grapes, and steak." (rice, grapes, steak)
##### Example: "ENTITY wants to chat and give Steven a present." (Steven, present)
##### Note: not_children inserted in order to avoid passiveness.

P_10 = function(tokens, entities, verb_pos, agent_patient_pos){
  rule = tquery(OR(token = entities, appos_child = "appos_child"), relation = "nsubj",
                parents(pos = c("VERB", "AUX"),
                        children(pos = "VERB", relation = "xcomp",
                                 not_children(pos = "AUX", lemma = "be", relation = "auxpass"),
                                 not_children(relation = "nsubj"),
                                 children(pos = "VERB", relation = "conj",
                                          NOT(lemma = "have"),
                                          children(pos = agent_patient_pos, relation = c("dobj", "dative"),
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
    annotate_tqueries("xcomp_act_obj_vconj", rule, overwrite = T, copy = F)
  tokens[,c(ncol(tokens)-1,ncol(tokens))] = NULL
  return(tokens)
}
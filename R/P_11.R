###############################################################################################
##### Rule: Objects of verb with xcomp clause and ENTITY as subject conjunct
##### Example: "Jonathan and ENTITY want to eat rice, grapes, and steak." (rice, grapes, steak)
##### Example: "Jonathan and ENTITY want to give Steven a present." (Steven, present)
##### Note: not_children inserted is to avoid passiveness.

P_11 = function(tokens, entities, verb_pos, agent_patient_pos){
  rule = tquery(OR(token = entities, appos_child = "appos_child"), relation = "conj",
                parents(pos = c("NOUN", "PROPN", "PRON"), relation = "nsubj",
                        parents(pos = c("VERB", "AUX"),
                                children(pos = "VERB", relation = "xcomp",
                                         not_children(pos = "AUX", lemma = "be", relation = "auxpass"),
                                         not_children(relation = "nsubj"),
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
    annotate_tqueries("xcomp_act_obj_nconj", rule, overwrite = T, copy = F)
  tokens[,c(ncol(tokens)-1,ncol(tokens))] = NULL
  return(tokens)
}
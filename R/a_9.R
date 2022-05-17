###############################################################################################
##### Rule: Verb with xcomp clause and its conjuncts
##### Example: "ENTITY wants to eat and drink." (eat, drink)
##### Note: not_children inserted is in order to avoid passiveness.

a_9 = function(tokens, entities, verb_pos, agent_patient_pos){
  rule = tquery(OR(token = entities, appos_child = "appos_child"), relation = "nsubj",
                parents(pos = c("VERB", "AUX"),
                        children(pos = "VERB", relation = "xcomp",
                                 not_children(pos = "AUX", lemma = "be", relation = "auxpass"),
                                 not_children(relation = "nsubj"),
                                 label = "Motif", fill = F,
                                 children(pos = "VERB", relation = "conj", req = F,
                                          not_children(pos = "AUX", lemma = "be", relation = "auxpass"),
                                          not_children(relation = "nsubj"),
                                          label = "Motif", fill = F,
                                          children(pos = "VERB", relation = "conj", req = F,
                                                   not_children(pos = "AUX", lemma = "be", relation = "auxpass"),
                                                   not_children(relation = "nsubj"),
                                                   label = "Motif", fill = F
                                          )
                                 )
                        )
                        
                )
  )
  
  tokens = tokens %>%
    annotate_tqueries("xcomp_act_conj_verb", rule, overwrite = T, copy = F)
  tokens[,c(ncol(tokens)-1,ncol(tokens))] = NULL
  
  return(tokens)
}
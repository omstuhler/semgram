###############################################################################################
##### Rule: Verb with xcomp clause and noun conjunct
##### Example: "Jack and ENTITY want to eat." (eat)
##### Note: not_children inserted in order to avoid passiveness.

a_10 = function(tokens, entities, verb_pos, agent_patient_pos){
  rule = tquery(OR(token = entities, appos_child = "appos_child"), relation = "conj",
                parents(pos = c("PROPN", "NOUN", "PRON"), relation = "nsubj",
                        parents(pos = c("VERB", "AUX"),
                                children(pos = "VERB", relation = "xcomp",
                                         not_children(pos = "AUX", lemma = "be", relation = "auxpass"),
                                         not_children(relation = "nsubj"),
                                         label = "Motif", fill = F,
                                         children(pos = "VERB", relation = "conj", req = F,
                                                  not_children(pos = "AUX", lemma = "be", relation = "auxpass"),
                                                  not_children(relation = "nsubj"),
                                                  label = "Motif", fill = F)
                                )
                        )
                )
  )
  
  tokens = tokens %>%
    annotate_tqueries("xcomp_act_conj_noun", rule, overwrite = T, copy = F)
  tokens[,c(ncol(tokens)-1,ncol(tokens))] = NULL
  
  return(tokens)
}
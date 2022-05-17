###############################################################################################
##### Rule: Objects of have-verb with xcomp clause and noun conjunct and subject
##### Example: "Jonathan and ENTITY want to have rice." (rice)

H_9 = function(tokens, entities, verb_pos, agent_patient_pos){
  rule = tquery(OR(token = entities, appos_child = "appos_child"), relation = "conj",
                parents(pos = c("NOUN", "PROPN", "PRON"), relation = "nsubj",
                        parents(pos = c("VERB", "AUX"),
                                children(pos = c("VERB", "AUX"), relation = "xcomp",
                                         not_children(pos = "AUX", lemma = "be", relation = "auxpass"),
                                         not_children(relation = "nsubj"),
                                         lemma = "have",
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
    annotate_tqueries("have_xcomp_act_obj_nconj", rule, overwrite = T, copy = F)
  tokens[,c(ncol(tokens)-1,ncol(tokens))] = NULL
  return(tokens)
}
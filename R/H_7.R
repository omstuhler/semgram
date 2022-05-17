###############################################################################################
##### Rule: Object of verb with xcomp clause have verb
##### Example: "ENTITY wants to have rice." (rice)

H_7 = function(tokens, entities, verb_pos, agent_patient_pos){
  rule = tquery(OR(token = entities, appos_child = "appos_child"), relation = "nsubj",
                parents(pos = c("VERB", "AUX"),
                        children(pos = c("VERB", "AUX"), relation = "xcomp",
                                 lemma = "have",
                                 not_children(pos = "AUX", lemma = "be", relation = "auxpass"),
                                 not_children(relation = "nsubj"),
                                 children(pos = agent_patient_pos, relation = c("dobj", "dative"),
                                          label = "Motif", fill = F,
                                          children(pos = agent_patient_pos, relation = c("conj", "appos"), req = F,
                                                   label = "Motif", fill = F, depth = 3
                                          )
                                 )
                        )
                )
  )
  
  tokens = tokens %>%
    annotate_tqueries("have_xcomp_act_obj", rule, overwrite = T, copy = F)
  tokens[,c(ncol(tokens)-1,ncol(tokens))] = NULL
  return(tokens)
}
###############################################################################################
##### Rule: Object of nsubj act
##### Example: "ENTITY calls Joe, Sue, and Michael." (Joe, Sue, Michael)
##### Example: "ENTITY give Joseph a present." (Joseph, present)
##### Example: "ENTITY called my friend Peter." (friend, Peter)
##### Note: The last sentence exemplifies how appositions are considered. This presumes that use_appos is set to TRUE.

P_1 = function(tokens, entities, verb_pos, agent_patient_pos){
  rule = tquery(OR(token = entities, appos_child = "appos_child"), relation = "nsubj",
                                     parents(pos = c("VERB", "AUX"), NOT(lemma = "have"),
                                             children(pos = agent_patient_pos, relation = c("dobj", "dative"), label = "Motif",
                                                      fill = F,
                                                      children(pos = agent_patient_pos, relation = c("conj", "appos"), depth = 3, req = F,
                                                               label = "Motif",
                                                               fill = F
                                                      )
                                             )
                                     )
  )
  
  tokens = tokens %>%
    annotate_tqueries("nsubj_obj_conj_act", rule, overwrite = T, copy = F)
  tokens[,c(ncol(tokens)-1,ncol(tokens))] = NULL
  return(tokens)
}
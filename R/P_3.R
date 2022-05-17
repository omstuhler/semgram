###############################################################################################
##### Rule: Object of conjuncted verb
##### Example: "ENTITY came and kissed Joe, Sue, and Michael." (Joe, Sue, Michael)
##### Example: "ENTITY came and gave Steve a present." (Steve, present)

P_3 = function(tokens, entities, verb_pos, agent_patient_pos){
  rule = tquery(OR(token = entities, appos_child = "appos_child"), relation = "nsubj",
                parents(pos = c("VERB", "AUX"),
                        children(pos = c("VERB", "AUX"), relation = "conj",
                                 NOT(lemma = "have"),
                                 not_children(relation = "nsubj", depth = 1),
                                 children(pos = agent_patient_pos, relation = c("dobj", "dative"),
                                          label = "Motif",
                                          fill = F,
                                          children(pos = agent_patient_pos, relation = c("conj", "appos"),
                                                   label = "Motif", req = F, depth = 3,
                                                   fill = F
                                          )
                                 )
                        )
                )
  )
  
  tokens = tokens %>%
    annotate_tqueries("nsubj_conj_obj_act", rule, overwrite = T, copy = F)
  tokens[,c(ncol(tokens)-1,ncol(tokens))] = NULL
  return(tokens)
}
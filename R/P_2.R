###############################################################################################
##### Rule: Object of nsubj act but ENTITY is conjunct of nominal subject
##### Example: "Joe and ENTITY asked Joe, Sue, and Michael." (Joe, Sue, Michael)
##### Example: "Steven and ENTITY give Joseph a present." (Joe, Sue, Michael)
##### Example: "Steven and ENTITY had lunch." (Joe, Sue, Michael)

P_2 = function(tokens, entities, verb_pos, agent_patient_pos){
  rule = tquery(OR(token = entities, appos_child = "appos_child"), relation = "conj",
                parents(pos = agent_patient_pos, relation = "nsubj",
                        parents(pos = c("VERB", "AUX"), NOT(lemma = "have"),
                                children(pos = agent_patient_pos, relation = c("dobj", "dative"), label = "Motif",
                                         fill = F,
                                         children(pos = agent_patient_pos, relation = c("conj", "appos"), req = F,
                                                  label = "Motif", fill = F, depth = 3
                                         )
                                )
                        )
                )
  )
  
  tokens = tokens %>%
    annotate_tqueries("nsubj_obj_conj", rule, overwrite = T, copy = F)
  tokens[,c(ncol(tokens)-1,ncol(tokens))] = NULL
  return(tokens)
}
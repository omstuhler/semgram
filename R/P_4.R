###############################################################################################
##### Rule: Object of conjuncted verb but ENTITY is conjunct of nominal subject
##### Example: "Joe and ENTITY came and kissed Joe, Sue, and Michael." (Joe, Sue, Michael)
##### Example: "Joe and ENTITY came and gave Steve a present." (Steve, present)

P_4 = function(tokens, entities, verb_pos, agent_patient_pos){
  rule = tquery(OR(token = entities, appos_child = "appos_child"), relation = "conj",
                parents(pos = c("NOUN", "PROPN", "PRON"), relation = "nsubj",
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
  )
  
  tokens = tokens %>%
    annotate_tqueries("nsubj_conj_subj_cons_obj", rule, overwrite = T, copy = F)
  tokens[,c(ncol(tokens)-1,ncol(tokens))] = NULL
  return(tokens)
}
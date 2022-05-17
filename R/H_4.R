###############################################################################################
##### Rule: Object of nsubj have act with conjunct noun
##### Example: "Joe and ENTITY have apples." (apples)

H_4 = function(tokens, entities, verb_pos, agent_patient_pos){
  rule = tquery(OR(token = entities, appos_child = "appos_child"), relation = "conj",
                parents(pos = agent_patient_pos, relation = "nsubj",
                        parents(pos = c("VERB", "AUX"), lemma = "have",
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
    annotate_tqueries("have_nsubj_obj_conj", rule, overwrite = T, copy = F)
  tokens[,c(ncol(tokens)-1,ncol(tokens))] = NULL
  return(tokens)
}
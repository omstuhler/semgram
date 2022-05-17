###############################################################################################
##### Rule: Object of nsubj have act
##### Example: "ENTITY has apples, grapes, and bananas. (apples, grapes, bananas)

H_3 = function(tokens, entities, verb_pos, agent_patient_pos){
  rule = tquery(OR(token = entities, appos_child = "appos_child"), relation = "nsubj",
                parents(pos = c("VERB", "AUX"), lemma = "have",
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
    annotate_tqueries("have_nsubj_obj_conj_act", rule, overwrite = T, copy = F)
  tokens[,c(ncol(tokens)-1,ncol(tokens))] = NULL
  return(tokens)
}
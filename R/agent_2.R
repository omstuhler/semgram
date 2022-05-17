###############################################################################################
##### Rule: Actor of conjuncted nsubj act with object
##### Example: "Joseph, Sarah, and Steve came and asked ENTITY." (Joe, Sarah, Steve)
##### Example: "They ran and attacked ENTITY" (They)

agent_2 = function(tokens, entities, verb_pos, agent_patient_pos){
  rule = tquery(OR(token = entities, appos_child = "appos_child"), relation = c("dobj", "dative"),
                parents(pos = "VERB", relation = c("conj","xcomp"),
                        not_children(pos = agent_patient_pos, relation = "nsubj"),
                        parents(pos = c("VERB", "AUX"),
                                children(pos = agent_patient_pos, relation = "nsubj",
                                         label = "Motif", fill = F,
                                         children(pos = agent_patient_pos, relation = c("conj", "appos"), req = F,
                                                  label = "Motif", fill = F, depth = 3
                                         )
                                )
                        )
                )
  )
  
  tokens = tokens %>%
    annotate_tqueries("dobj_treat_conj_actor", rule, overwrite = T, copy = F)
  tokens[,c(ncol(tokens)-1,ncol(tokens))] = NULL
  return(tokens)
}
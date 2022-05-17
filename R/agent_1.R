###############################################################################################
##### Rule: Actor of nsubj act with object
##### Example: "Joseph, Sarah, and Steve call ENTITY." (Joe, Sarah, Steve)
##### Example: "Joseph, Sarah, and Steve give Michael a ENTITY." (Joe, Sarah, Steve)

agent_1 = function(tokens, entities, verb_pos, agent_patient_pos){
  rule = tquery(OR(token = entities, appos_child = "appos_child"), relation = c("dobj", "dative"),
                parents(pos = "VERB",
                        children(pos = agent_patient_pos, relation = "nsubj",
                                 label = "Motif", fill = F,
                                 children(pos = agent_patient_pos, relation = c("conj", "appos"), req = F,
                                          label = "Motif", fill = F, depth = 3
                                 )
                        )
                )
  )
  
  tokens = tokens %>%
    annotate_tqueries("dobj_treat_actor", rule, overwrite = T, copy = F)
  tokens[,c(ncol(tokens)-1,ncol(tokens))] = NULL
  return(tokens)
}
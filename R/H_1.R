###############################################################################################
##### Rule: A possesive of entity
##### Example: "He liked ENTITY's friends, spouse, and family." (friends, spouse, family)

H_1 = function(tokens, entities, verb_pos, agent_patient_pos){
  rule = tquery(OR(token = entities, appos_child = "appos_child"), relation = "poss",
                parents(pos = c("NOUN", "PROPN"),
                        label = "Motif",
                        fill = F,
                        children(pos = c("NOUN", "PROPN"), relation = "conj", req = F,
                                 label = "Motif",
                                 fill = F,
                                 children(pos = c("NOUN", "PROPN"), relation = "conj", req = F,
                                          label = "Motif",
                                          fill = F
                                 )
                        )
                )
  )
  
  tokens = tokens %>%
    annotate_tqueries("posessive_o", rule, overwrite = T, copy = F)
  tokens[,c(ncol(tokens)-1,ncol(tokens))] = NULL
  return(tokens)
}

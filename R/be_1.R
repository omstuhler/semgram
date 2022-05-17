###############################################################################################
##### Rule: Being adjective
##### Example: "ENTITY is nice but dumb." (nice, dumb)
##### Example: "ENTITY is a winner and nice." (winner, nice)
##### Example: "ENTITY looks nice." (nice)
##### Example: "ENTITY is nice and a cool person." (nice, humble, cool, person)


be_1 = function(tokens, entities, verb_pos, agent_patient_pos){
  rule = tquery(OR(token = entities, appos_child = "appos_child"), relation = "nsubj",
                parents(pos = c("AUX", "VERB"),
                        children(pos = c("ADJ", "NOUN", "PROPN"), relation = c("acomp", "attr"), phrase_replacement = NA,
                                 label = "Motif",
                                 fill = F,
                                 children(pos = c("ADJ", "NOUN", "PROPN"), relation = c("conj", "appos", "amod"), req = F, phrase_replacement = NA,
                                          label = "Motif",
                                          fill = F, depth = 3
                                 )
                        )
                )
  )
  
  tokens = tokens %>%
    annotate_tqueries("being_adj", rule, overwrite = T, copy = F)
  tokens[,c(ncol(tokens)-1,ncol(tokens))] = NULL
  return(tokens)
}
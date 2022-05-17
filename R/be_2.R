###############################################################################################
##### Rule: Being adjective noun conjunct
##### Example: "Steve and ENTITY is nice but dumb." (nice, dumb)
##### Example: "Steve and ENTITY is a winner and nice." (winner, nice)
##### Example: "Steve and ENTITY looks nice." (nice)
##### Example: "Steve and ENTITY are nice, humble, and cool persons." (nice, humble, cool, person)

be_2 = function(tokens, entities, verb_pos, agent_patient_pos){
  rule = tquery(OR(token = entities, appos_child = "appos_child"), relation = "conj",
                parents(pos = c("NOUN", "PROPN", "PRON"), relation = "nsubj",
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
  )
  
  tokens = tokens %>%
    annotate_tqueries("being_adj_nconj", rule, overwrite = T, copy = F)
  tokens[,c(ncol(tokens)-1,ncol(tokens))] = NULL
  return(tokens)
}
###############################################################################################
##### Rule: Being an entity with conjunct
##### Example: "His favorite cousins were Steve and ENTITY." (cousin)
be_8 = function(tokens, entities, verb_pos, agent_patient_pos){
  rule = tquery(OR(token = entities, appos_child = "appos_child"), relation = "conj",
                parents(pos = c("NOUN", "PROPN", "PRON"), relation = "attr",
                        parents(pos = c("VERB", "AUX"), lemma = "be",
                                children(pos = c("NOUN", "PROPN"), relation = "nsubj",
                                         phrase_replacement = NA,
                                         label = "Motif",
                                         fill = F)
                        )
                )
  )
  
  tokens = tokens %>%
    annotate_tqueries("being_entity_c", rule, overwrite = T, copy = F)
  tokens[,c(ncol(tokens)-1,ncol(tokens))] = NULL
  return(tokens)
}
###############################################################################################
##### Rule: Being an entity
##### Example: "His favorite cousin was entity." (cousin)

be_7 = function(tokens, entities, verb_pos, agent_patient_pos){
  rule = tquery(OR(token = entities, appos_child = "appos_child"), relation = "attr",
                parents(pos = c("VERB", "AUX"), lemma = "be",
                        children(pos = c("NOUN", "PROPN"), relation = "nsubj",
                                 phrase_replacement = NA,
                                 label = "Motif",
                                 fill = F)
                )
  )
  
  tokens = tokens %>%
    annotate_tqueries("being_entity", rule, overwrite = T, copy = F)
  tokens[,c(ncol(tokens)-1,ncol(tokens))] = NULL
  return(tokens)
}
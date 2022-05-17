###############################################################################################
##### Rule: Being adjective as conjuncted verb
##### Example: "ENTITY won but remained sad and unmoved." (sad, unmoved)

##### Development note: be_4 and be_5 could easily be merged.

be_4 = function(tokens, entities, verb_pos, agent_patient_pos){
  rule = tquery(OR(token = entities, appos_child = "appos_child"), relation = "nsubj",
                parents(pos = c("VERB", "AUX"),
                        children(pos = c("AUX", "VERB"), relation = "conj",
                                 not_children(relation = "nsubj", depth = 1),
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
    annotate_tqueries("being_adj_vconj", rule, overwrite = T, copy = F)
  tokens[,c(ncol(tokens)-1,ncol(tokens))] = NULL
  return(tokens)
}
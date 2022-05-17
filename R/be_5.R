###############################################################################################
##### Rule: Being something as xcomp clause verb
##### Example: "ENTITY wants to be president." (president)
##### Example: "ENTITY tries to be a good president." (good, president)

##### Development note: be_4 (being_adj_vconj) and be_5 (being_adj_xcomp) could easily be merged.

be_5 = function(tokens, entities, verb_pos, agent_patient_pos){
  rule = tquery(OR(token = entities, appos_child = "appos_child"), relation = "nsubj",
                parents(pos = c("VERB", "AUX"),
                        children(pos = c("AUX", "VERB"), relation = "xcomp",
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
    annotate_tqueries("being_adj_xcomp", rule, overwrite = T, copy = F)
  tokens[,c(ncol(tokens)-1,ncol(tokens))] = NULL
  return(tokens)
}
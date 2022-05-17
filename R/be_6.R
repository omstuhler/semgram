###############################################################################################
##### Rule: Amod adjective
##### Example: "Steven got a nice, fresh ENTITY." (nice, fresh)

be_6 = function(tokens, entities, verb_pos, agent_patient_pos){
  rule = tquery(OR(token = entities, appos_child = "appos_child"),
                children(pos = "ADJ", relation = "amod", phrase_replacement = NA,
                         label = "Motif",
                         fill = F)
                )
  
  tokens = tokens %>%
    annotate_tqueries("amod_adj", rule, overwrite = T, copy = F)
  tokens[,c(ncol(tokens)-1,ncol(tokens))] = NULL
  return(tokens)
}
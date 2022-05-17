###############################################################################################
##### Rule: Appos parents
##### Example: "My friend ENTITY came." (friend)

be_3 = function(tokens, entities, verb_pos, agent_patient_pos){
  rule = tquery(appos_child = "appos_child",
                phrase_replacement = NA,
                label = "Motif",
                fill = F
  )
  
  tokens = tokens %>%
    annotate_tqueries("appos_char", rule, overwrite = T, copy = F)
  tokens[,c(ncol(tokens)-1,ncol(tokens))] = NULL
  return(tokens)
}
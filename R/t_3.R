###############################################################################################
##### Rule: Passive construction treatment
##### Example: "ENTITY is asked." (asked)

t_3 = function(tokens, entities, verb_pos, agent_patient_pos){
  rule = tquery(OR(token = entities, appos_child = "appos_child"), relation = c("nsubjpass"),
                parents(pos = "VERB",
                        label = "Motif", fill = F
                )
  )
  
  tokens = tokens %>%
    annotate_tqueries("obj_of_by_act", rule, overwrite = T, copy = F)
  tokens[,c(ncol(tokens)-1,ncol(tokens))] = NULL
  return(tokens)
}
###############################################################################################
##### Rule: Passive construction treatment and noun conjunct (entity)
##### Example: "Sue and ENTITY are asked." (asked)
##### Example: "Steve were given apples and ENTITY." (given)

t_4 = function(tokens, entities, verb_pos, agent_patient_pos){
  rule = tquery(OR(token = entities, appos_child = "appos_child"), relation = c("nsubjpass"),
                parents(pos = "VERB",
                        label = "Motif", fill = F
                )
  )
  
  tokens = tokens %>%
    annotate_tqueries("obj_of_by_act_nconj", rule, overwrite = T, copy = F)
  tokens[,c(ncol(tokens)-1,ncol(tokens))] = NULL
  return(tokens)
}
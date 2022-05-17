###############################################################################################
##### Rule: Object of nsubj act
##### Example: "Joe calls ENTITY." (calls)
##### Example: "Joe gives Michael a ENTITY." (gives)

t_1 = function(tokens, entities, verb_pos, agent_patient_pos){
  rule = tquery(OR(token = entities, appos_child = "appos_child"), relation = c("dobj", "dative"),
                parents(pos = "VERB",
                        label = "Motif",
                        fill = F)
  )
  
  tokens = tokens %>%
    annotate_tqueries("dobj_treat", rule, overwrite = T, copy = F)
  tokens[,c(ncol(tokens)-1,ncol(tokens))] = NULL
  return(tokens)
}
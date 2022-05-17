###############################################################################################
##### Rule: Passive subject with by
##### Example: "Sue is asked by Entity." (asked)

a_3 = function(tokens, entities, verb_pos, agent_patient_pos){
  rule = tquery(OR(token = entities, appos_child = "appos_child"), relation = "pobj",
                  parents(pos = "ADP", lemma = "by", relation = "agent",
                          parents(pos = "VERB",
                                  label = "Motif", fill = F)))
  
  tokens = tokens %>%
    annotate_tqueries("by_act", rule, overwrite = T, copy = F)
  tokens[,c(ncol(tokens)-1,ncol(tokens))] = NULL
  return(tokens)
}
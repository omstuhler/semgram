###############################################################################################
##### Rule: Passive subject with by conjunction
##### Example: "Sue is called and asked by ENTITY." (called)

a_5 = function(tokens, entities, verb_pos, agent_patient_pos){
  rule = tquery(OR(token = entities, appos_child = "appos_child"), relation = "pobj",
                parents(pos = "ADP", lemma = "by", relation = "agent",
                        parents(pos = c("VERB", "AUX"), relation = "conj",
                                parents(pos = "VERB",
                                        label = "Motif", fill = F)
                        )
                )
  )
  
  tokens = tokens %>%
    annotate_tqueries("by_act_2", rule, overwrite = T, copy = F)
  tokens[,c(ncol(tokens)-1,ncol(tokens))] = NULL
  return(tokens)
}
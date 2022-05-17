###############################################################################################
##### Rule: Passive subject with by conjunction second verb
##### Example: "Sue is called and asked by ENTITY." (asked)

a_6 = function(tokens, entities, verb_pos, agent_patient_pos){
  rule = tquery(OR(token = entities, appos_child = "appos_child"), relation = "pobj",
                parents(pos = "ADP", lemma = "by", relation = "agent",
                        parents(pos = "VERB", relation = "conj",
                                label = "Motif", fill = F
                        )
                )
  )
  
  tokens = tokens %>%
    annotate_tqueries("by_act_2_1", rule, overwrite = T, copy = F)
  tokens[,c(ncol(tokens)-1,ncol(tokens))] = NULL
  
  return(tokens)
}
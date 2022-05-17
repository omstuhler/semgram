###############################################################################################
##### Rule: Passive subject with by and noun conjunct
##### Example: "Sue is asked by Steve and ENTITY." (asked)

a_4 = function(tokens, entities, verb_pos, agent_patient_pos){
  rule = tquery(OR(token = entities, appos_child = "appos_child"), relation = "conj",
                parents(pos = c("NOUN", "PROPN", "PRON"), relation = "pobj",
                        parents(pos = "ADP", lemma = "by", relation = "agent",
                                parents(pos = "VERB",
                                        label = "Motif", fill = F
                                )
                        )
                )
  )
  
  tokens = tokens %>%
    annotate_tqueries("by_act_noun_conjunct", rule, overwrite = T, copy = F)
  tokens[,c(ncol(tokens)-1,ncol(tokens))] = NULL
  return(tokens)
}
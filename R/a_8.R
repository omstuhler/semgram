###############################################################################################
##### Rule: Passive subject with by verb conjunction and noun conjunction (second verb)
##### Example: "Sue is called and asked by Greg and ENTITY." (asked)

a_8 = function(tokens, entities, verb_pos, agent_patient_pos){
  rule = tquery(OR(token = entities, appos_child = "appos_child"), relation = "conj",
                parents(pos = c("NOUN", "PROPN", "PRON"), relation = "pobj",
                        parents(pos = "ADP", lemma = "by", relation = "agent",
                                parents(pos = c("VERB"), relation = "conj",
                                        label = "Motif", fill = F
                                )
                        )
                )
  )
  
  tokens = tokens %>%
    annotate_tqueries("by_act_2_noun_conj_1", rule, overwrite = T, copy = F)
  tokens[,c(ncol(tokens)-1,ncol(tokens))] = NULL
  
  return(tokens)
}
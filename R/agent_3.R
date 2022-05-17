###############################################################################################
##### Rule: Actor of nsubj act with object and a noun conjunct (entity)
##### Example: "Joseph, Sarah, and Steve call Michael and ENTITY." (Joseph, Sarah, Steve)
##### Example: "Joseph, Sarah, and Steve give Michael an apple and an ENTITY." (Joseph, Sarah, and Steve)
##### Note: We can't collect more verbs because based on the grammar, it will be
##### unclear whether these will be transitive.

agent_3 = function(tokens, entities, verb_pos, agent_patient_pos){
  rule = tquery(OR(token = entities, appos_child = "appos_child"), relation = "conj",
                parents(relation = c("dobj", "dative"), pos = c("NOUN", "PROPN", "PRON"),
                        parents(pos = "VERB",
                                children(pos = agent_patient_pos, relation = "nsubj",
                                         label = "Motif", fill = F,
                                         children(pos = agent_patient_pos, relation = c("conj", "appos"), req = F,
                                                  label = "Motif", fill = F, depth = 3
                                         )
                                )
                        )
                )
  )
  
  tokens = tokens %>%
    annotate_tqueries("dobj_nconj_treat", rule, overwrite = T, copy = F)
  tokens[,c(ncol(tokens)-1,ncol(tokens))] = NULL
  return(tokens)
}
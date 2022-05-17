###############################################################################################
##### Rule: noun conjunct of object of nsubj act
##### Example: "Joe calls Steve and ENTITY." (calls)
##### Example: "Joe gives Steve an apple and an ENTITY." (gives)

##### Note: We can't collect more verbs because based on the grammar, it will be
##### unclear whether these will be transitive.

t_2 = function(tokens, entities, verb_pos, agent_patient_pos){
  rule = tquery(OR(token = entities, appos_child = "appos_child"), relation = "conj",
                parents(relation = c("dobj", "dative"), pos = c("NOUN", "PROPN", "PRON"),
                        parents(pos = "VERB",
                                label = "Motif",
                                fill = F
                        )
                )
  )
  
  tokens = tokens %>%
    annotate_tqueries("dobj_conj_treat", rule, overwrite = T, copy = F)
  tokens[,c(ncol(tokens)-1,ncol(tokens))] = NULL
  return(tokens)
}
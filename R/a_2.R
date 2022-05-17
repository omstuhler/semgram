###############################################################################################
##### Rule: nsubj with conjuncted second actor and possibly second verb
##### Example: "Joe and ENTITY called Steve" (called)
##### Example: "Joe and ENTITY called and asked Steve." (called, asked)

a_2 = function(tokens, entities, verb_pos, agent_patient_pos){
  rule = tquery(OR(token = entities, appos_child = "appos_child"),
                                         relation = "conj",
                                         parents(pos = c("NOUN", "PROPN", "PRON"),  relation = "nsubj",
                                                 parents(pos = verb_pos,
                                                         label = "Motif", fill = F,
                                                         children(pos = verb_pos, relation = "conj", req = F,
                                                                  not_children(relation = "nsubj", depth = 1),
                                                                  label = "Motif", fill = F,
                                                                  children(get_aux_verbs_par = "YES",
                                                                           pos = verb_pos, relation = "aux", req = F,
                                                                           label = "Motif", fill = F
                                                                  ),
                                                                  children(get_aux_verbs_par = "YES",
                                                                           pos = verb_pos, relation = "aux", req = F,
                                                                           label = "Motif", fill = F
                                                                  )
                                                         )
                                                 )
                                         )
  )
  
  tokens = tokens %>%
    annotate_tqueries("nsubj_act_noun_conj_verb_conj", rule, overwrite = T, copy = F)
  tokens[,c(ncol(tokens)-1,ncol(tokens))] = NULL
  return(tokens)
}
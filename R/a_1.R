###############################################################################################
##### Rule: nsubj act or conjuncted second verb
##### Example: "ENTITY asked Joe." (asked)
##### Example: "ENTITY called and asked Joe." (called, asked)
##### Note: Note that the current basic annotation scheme cannot distinguish between a dependent of the first
##### conjunct and a shared dependent of the whole coordination (see https://universaldependencies.org/u/dep/conj.html).
##### E.g. in "ENTITY called and Joe answered." "answered" would be picked up as action of ENTITY if we just took conj-dependents.
##### To prevent this, we add a not_children condition to avoid cases where an independent subject is named.

a_1 = function(tokens, entities, verb_pos, agent_patient_pos){
  rule = tquery(OR(token = entities, appos_child = "appos_child"), relation = "nsubj",
         parents(pos = verb_pos,
                 label = "Motif", fill = F,
                 children(pos = verb_pos, relation = "conj", req = F,
                          not_children(relation = "nsubj", depth = 1),
                          label = "Motif", fill = F,
                          children(get_aux_verbs_par = "YES",
                                   pos = verb_pos, relation = "aux", req = F,
                                   label = "Motif", fill = F
                          )
                 ),
                 children(get_aux_verbs_par = "YES",
                          pos = verb_pos, relation = "aux", req = F,
                          label = "Motif", fill = F
                 )
         )
  )
  
  tokens = tokens %>%
    annotate_tqueries("nsubj_act_conj", rule, overwrite = T, copy = F)
  tokens[,c(ncol(tokens)-1,ncol(tokens))] = NULL
  return(tokens)
}
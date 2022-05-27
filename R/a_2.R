#' @importFrom rsyntax annotate_tqueries cast_text children NOT OR not_children parents tquery
NULL

###############################################################################################
##### Rule: nsubj with conjuncted second actor and possibly second verb
##### Example: "Joe and ENTITY called Steve" (called)
##### Example: "Joe and ENTITY called and asked Steve." (called, asked)

a_2 = function(tokens, entities, verb_pos, agent_patient_pos, extract){
  rule = tquery(OR(token = entities, appos_child = "appos_child"),
                                         relation = "conj", label = "Entity", fill = F,
                                         parents(pos = c("NOUN", "PROPN", "PRON"),  relation = "nsubj",
                                                 parents(pos = verb_pos,
                                                         label = "action", fill = F,
                                                         children(pos = verb_pos, relation = "conj", req = F,
                                                                  not_children(relation = "nsubj", depth = 1),
                                                                  label = "action", fill = F,
                                                                  children(get_aux_verbs_par = "YES",
                                                                           pos = verb_pos, relation = "aux", req = F,
                                                                           label = "action", fill = F
                                                                  ),
                                                                  children(get_aux_verbs_par = "YES",
                                                                           pos = verb_pos, relation = "aux", req = F,
                                                                           label = "action", fill = F
                                                                  )
                                                         )
                                                 )
                                         )
  )
  tokens = annotate_tqueries(tokens, "query", rule, overwrite = T, copy = F)
  if(all(is.na(tokens$query))){
    casted = data.table(doc_id = character(), ann_id = factor(), Entity = character(), action = character())
  } else {
    casted = cast_text(tokens, 'query', text_col = extract)
  }
  return(casted)
}

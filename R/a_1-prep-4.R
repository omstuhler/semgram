#' @importFrom rsyntax annotate_tqueries cast_text children NOT OR not_children parents tquery

NULL

###############################################################################################
##### Rule: nsubj act's conj verb without prep and its aux dependent
##### Example: "ENTITY could come and might play." (play)

a_1_prep_4 = function(tokens, entities, verb_pos, agent_patient_pos, extract, verb_prep_dist){
  rule = tquery(OR(token = entities, appos_child = "appos_child"), relation = "nsubj",
                label = "Entity", fill = F,
                parents(pos = verb_pos,
                        children(pos = verb_pos, relation = "conj",
                                 not_children(relation = "nsubj"),
                                 not_children(pos = "ADP", relation = c("prep","prt"),
                                              max_window = c(0,verb_prep_dist)),
                                 label = "action", fill = F,
                                 children(get_aux_verbs_par = "YES",
                                          pos = verb_pos, relation = "aux", req = F,
                                          label = "action", fill = F
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

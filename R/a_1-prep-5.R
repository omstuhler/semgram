#' @importFrom rsyntax annotate_tqueries cast_text children NOT OR not_children parents tquery

NULL

###############################################################################################
##### Rule: aux dependent of nsubj act with prep
##### Example: "ENTITY could play against Joe." (could)

a_1_prep_5 = function(tokens, entities, verb_pos, agent_patient_pos, extract, verb_prep_dist){
  rule = tquery(OR(token = entities, appos_child = "appos_child"), relation = "nsubj",
                label = "Entity", fill = F,
                parents(pos = verb_pos,
                        children(pos = "ADP", relation = c("prep","prt"),
                                 max_window = c(0,verb_prep_dist)),
                        children(get_aux_verbs_par = "YES",
                                 pos = verb_pos, relation = "aux",
                                 label = "action", fill = F
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

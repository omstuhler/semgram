#' @importFrom rsyntax annotate_tqueries cast_text children NOT OR not_children parents tquery
#' @importFrom stringr str_c
NULL

###############################################################################################
##### Rule: Pobject nsubj act
##### Example: "Joe plays with ENTITY." (play-with)

t_1_prep_1 = function(tokens, entities, verb_pos, agent_patient_pos, extract){
  rule = tquery(OR(token = entities, appos_child = "appos_child"), relation = c("pobj"),
                label = "Entity", fill = F,
                parents(pos = "ADP", relation = c("prep","prt"),
                        label = "preposition", fill = F,
                        parents(pos = "VERB",
                                label = "treatment",
                                fill = F,
                                max_window = c(Inf, 0))
                        )
                )

  tokens = annotate_tqueries(tokens, "query", rule, overwrite = T, copy = F)
  if(all(is.na(tokens$query))){
    casted = data.table(doc_id = character(), ann_id = factor(), Entity = character(), treatment = character())
  } else {
    casted = cast_text(tokens, 'query', text_col = extract)
    casted$treatment = str_c(casted$treatment, "-",casted$preposition)
    casted$preposition = NULL  
  }
  return(casted)
}

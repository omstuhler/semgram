#' @importFrom rsyntax annotate_tqueries cast_text children NOT OR not_children parents tquery
#' @importFrom stringr str_c
NULL

###############################################################################################
##### Rule: Actor of nsubj act with pobject
##### Example: "Joe plays with ENTITY." (Joe, play-with)
At_1_prep_1 = function(tokens, entities, verb_pos, agent_patient_pos, extract){
  rule = tquery(OR(token = entities, appos_child = "appos_child"), relation = "pobj",
                label = "Entity", fill = F,
                parents(pos = "ADP", relation = c("prep","prt"),
                        label = "preposition", fill = F,
                        parents(pos = "VERB",
                                label = "treatment",
                                fill = F,
                                max_window = c(Inf, 0),
                                children(pos = agent_patient_pos, relation = "nsubj",
                                         label = "Agent", fill = F,
                                         children(pos = agent_patient_pos, relation = c("conj", "appos"), req = F,
                                                  label = "Agent", fill = F, depth = 3
                                                  )
                                         )
                                )
                        )
                )
  
  tokens = annotate_tqueries(tokens, "query", rule, overwrite = T, copy = F)
  if(all(is.na(tokens$query))){
    casted = data.table(doc_id = character(), ann_id = factor(), treatment = character(), Entity = character(), Agent = character())
  } else {
    casted = cast_text(tokens, 'query', text_col = extract)
    casted$treatment = str_c(casted$treatment, "-",casted$preposition)
    casted$preposition = NULL
  }
  return(casted)
}

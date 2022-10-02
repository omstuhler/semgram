#' @importFrom rsyntax annotate_tqueries cast_text children NOT OR not_children parents tquery
#' @importFrom stringr str_c str_remove

NULL

###############################################################################################
##### Rule: nsubj act's conj verb with prep
##### Example: "ENTITY came and played against Joe." (play_against)

##### Note: Note that the current basic annotation scheme cannot distinguish between a dependent of the first
##### conjunct and a shared dependent of the whole coordination (see https://universaldependencies.org/u/dep/conj.html).
##### E.g. in "ENTITY called and Joe answered." "answered" would be picked up as action of ENTITY if we just took conj-dependents.
##### To prevent this, we add a not_children condition to avoid cases where an independent subject is named.

a_1_prep_2 = function(tokens, entities, verb_pos, agent_patient_pos, extract, verb_prep_dist){
  rule = tquery(OR(token = entities, appos_child = "appos_child"), relation = "nsubj",
                label = "Entity", fill = F,
                parents(pos = verb_pos,
                        children(pos = verb_pos, relation = "conj",
                                 not_children(relation = "nsubj"),
                                 label = "action", fill = F,
                                 children(pos = "ADP", relation = c("prep","prt"),
                                          max_window = c(0,verb_prep_dist),
                                          label = "preposition", fill = F)
                        )
                )
  )
  
  tokens = annotate_tqueries(tokens, "query", rule, overwrite = T, copy = F)
  
  if(all(is.na(tokens$query))){
    casted = data.table(doc_id = character(), ann_id = factor(), Entity = character(), action = character())
  } else {
    casted = cast_text(tokens, 'query', text_col = extract)
    casted$preposition = str_remove(casted$preposition, " .+") # disregard all but first prep
    casted$action = str_c(casted$action, "-",casted$preposition)
    casted$preposition = NULL
  }
  return(casted)
}

#' @importFrom rsyntax annotate_tqueries cast_text children NOT OR not_children parents tquery
#' @importFrom stringr str_c str_remove str_detect
NULL

###############################################################################################
##### Rule: Pobject of verb with xcomp clause
##### Example: "ENTITY wants to play with Sue and Steve." (play-with, Sue Steve)
##### Note: not_children inserted is to avoid passiveness.

aP_11_prep_1 = function(tokens, entities, verb_pos, agent_patient_pos, extract, verb_prep_dist){
  rule = tquery(OR(token = entities, appos_child = "appos_child"), relation = "nsubj",
                label = "Entity", fill = F,
                parents(pos = c("VERB", "AUX"),
                        children(pos = "VERB", relation = "xcomp",
                                 label = "action", fill = F,
                                 NOT(lemma = "have"),
                                 not_children(pos = "AUX", lemma = "be", relation = "auxpass"),
                                 not_children(relation = "nsubj"),
                                 children(pos = "ADP", relation = c("prep","prt"),
                                          max_window = c(0,verb_prep_dist),
                                          label = "preposition", fill = F,
                                          children(pos = agent_patient_pos, relation = c("pobj"), label = "Patient",
                                                   fill = F,
                                                   children(pos = agent_patient_pos, relation = c("conj", "appos"), depth = 3, req = F,
                                                            label = "Patient",
                                                            fill = F
                                                   )
                                          )
                                 )
                        )
                )
  )

  tokens = annotate_tqueries(tokens, "query", rule, overwrite = T, copy = F)

  if(all(is.na(tokens$query))){
    casted = data.table(doc_id = character(), ann_id = factor(), action = character(), Entity = character(), Patient = character())
  } else {
    casted = cast_text(tokens, 'query', text_col = extract)
    multi_preps = str_detect(casted$preposition, " ")
    casted$preposition = ifelse(multi_preps, str_remove(casted$preposition, " .+"), casted$preposition)
    casted$Patient = ifelse(multi_preps, str_remove(casted$Patient, " .+"), casted$Patient)
    casted$action = str_c(casted$action, "-",casted$preposition)
    casted$preposition = NULL
  }
  return(casted)
}

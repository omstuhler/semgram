#' @importFrom rsyntax annotate_tqueries cast_text children NOT OR not_children parents tquery
#' @importFrom stringr str_c str_remove str_detect
NULL

###############################################################################################
##### Rule: Object of preposition of nsubj act
##### Example: "ENTITY slammed it on the table." (slam-on, table)
##### Example: "He played with fork and knife." (play, fork knife)

aP_1_prep_1 = function(tokens, entities, verb_pos, agent_patient_pos, extract, verb_prep_dist){
  rule = tquery(OR(token = entities, appos_child = "appos_child"), relation = "nsubj",
                label = "Entity", fill = F,
                parents(pos = c("VERB", "AUX"),
                        fill = F, label = "action",
                        NOT(lemma = "have"),
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

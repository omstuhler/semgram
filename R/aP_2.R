#' @importFrom rsyntax annotate_tqueries cast_text children NOT OR not_children parents tquery
NULL

###############################################################################################
##### Rule: Object of nsubj act but ENTITY is conjunct of nominal subject
##### Example: "Steven and ENTITY asked Joe, Sue, and Michael." (ask, Joe Sue Michael)
##### Example: "Steven and ENTITY give Joseph a present." (give, Joseph present)
##### Example: "Steven and ENTITY had lunch." (had, lunch)

aP_2 = function(tokens, entities, verb_pos, agent_patient_pos, extract){
  rule = tquery(OR(token = entities, appos_child = "appos_child"), relation = "conj",
                label = "Entity", fill = F,
                parents(pos = agent_patient_pos, relation = "nsubj",
                        parents(pos = c("VERB", "AUX"), label = "action", fill = F,
                                NOT(lemma = "have"),
                                children(pos = agent_patient_pos, relation = c("dobj", "dative"), label = "Patient",
                                         fill = F,
                                         children(pos = agent_patient_pos, relation = c("conj", "appos"), req = F,
                                                  depth = 3, label = "Patient",
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
  }
  return(casted)
}

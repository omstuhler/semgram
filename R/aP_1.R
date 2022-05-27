#' @importFrom rsyntax annotate_tqueries cast_text children NOT OR not_children parents tquery
NULL

###############################################################################################
##### Rule: Object of nsubj act
##### Example: "ENTITY asked Joe, Sue, and Michael." (asked, Joe Sue Michael)
##### Example: "ENTITY give Joseph a present." (give, Joseph present)

aP_1 = function(tokens, entities, verb_pos, agent_patient_pos, extract){
  rule = tquery(OR(token = entities, appos_child = "appos_child"), relation = "nsubj",
                label = "Entity", fill = F,
                parents(pos = c("VERB", "AUX"),
                        fill = F, label = "action",
                        NOT(lemma = "have"),
                        children(pos = agent_patient_pos, relation = c("dobj", "dative"), label = "Patient",
                                 fill = F,
                                 children(pos = agent_patient_pos, relation = c("conj", "appos"), depth = 3, req = F,
                                          label = "Patient",
                                          fill = F
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

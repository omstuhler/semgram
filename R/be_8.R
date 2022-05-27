#' @importFrom rsyntax annotate_tqueries cast_text children NOT OR not_children parents tquery
NULL

###############################################################################################
##### Rule: Being an entity with conjunct
##### Example: "His favorite cousins were Steve and ENTITY." (cousin)
be_8 = function(tokens, entities, verb_pos, agent_patient_pos, extract){
  rule = tquery(OR(token = entities, appos_child = "appos_child"), relation = "conj",
                label = "Entity", fill = F,
                parents(pos = c("NOUN", "PROPN", "PRON"), relation = "attr",
                        parents(pos = c("VERB", "AUX"), lemma = "be",
                                children(pos = c("NOUN", "PROPN"), relation = "nsubj",
                                         phrase_replacement = NA,
                                         label = "characterization",
                                         fill = F)
                        )
                )
  )

  tokens = annotate_tqueries(tokens, "query", rule, overwrite = T, copy = F)
  if(all(is.na(tokens$query))){
    casted = data.table(doc_id = character(), ann_id = factor(), Entity = character(), characterization = character())
  } else {
    casted = cast_text(tokens, 'query', text_col = extract)
  }
  return(casted)
}

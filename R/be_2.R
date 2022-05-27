#' @importFrom rsyntax annotate_tqueries cast_text children NOT OR not_children parents tquery
NULL

###############################################################################################
##### Rule: Being adjective noun conjunct
##### Example: "Steve and ENTITY is nice but dumb." (nice, dumb)
##### Example: "Steve and ENTITY is a winner and nice." (winner, nice)
##### Example: "Steve and ENTITY looks nice." (nice)
##### Example: "Steve and ENTITY are nice, humble, and cool persons." (nice, humble, cool, person)

be_2 = function(tokens, entities, verb_pos, agent_patient_pos, extract){
  rule = tquery(OR(token = entities, appos_child = "appos_child"), relation = "conj",
                label = "Entity", fill = F,
                parents(pos = c("NOUN", "PROPN", "PRON"), relation = "nsubj",
                        parents(pos = c("AUX", "VERB"),
                                children(pos = c("ADJ", "NOUN", "PROPN"), relation = c("acomp", "attr"), phrase_replacement = NA,
                                         label = "characterization",
                                         fill = F,
                                         children(pos = c("ADJ", "NOUN", "PROPN"), relation = c("conj", "appos", "amod"), req = F, phrase_replacement = NA,
                                                  label = "characterization",
                                                  fill = F, depth = 3
                                         )
                                )
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

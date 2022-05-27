#' @importFrom rsyntax annotate_tqueries cast_text children NOT OR not_children parents tquery
NULL

###############################################################################################
##### Rule: Being something as xcomp clause verb
##### Example: "ENTITY wants to be president." (president)
##### Example: "ENTITY tries to be a good president." (good, president)

##### Development note: be_4 (being_adj_vconj) and be_5 (being_adj_xcomp) could easily be merged.

be_5 = function(tokens, entities, verb_pos, agent_patient_pos, extract){
  rule = tquery(OR(token = entities, appos_child = "appos_child"), relation = "nsubj",
                label = "Entity", fill = F,
                parents(pos = c("VERB", "AUX"),
                        children(pos = c("AUX", "VERB"), relation = "xcomp",
                                 not_children(relation = "nsubj", depth = 1),
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

#' @importFrom rsyntax annotate_tqueries cast_text children NOT OR not_children parents tquery
NULL

###############################################################################################
##### Rule: Object of second conjuncted have verb with actor in conjunct position.
##### Example: "Joe and ENTITY came and had a cake." (Joe, Sue, Michael)

H_6 = function(tokens, entities, verb_pos, agent_patient_pos, extract){
  rule = tquery(OR(token = entities, appos_child = "appos_child"), relation = "conj",
                label = "Entity", fill = F,
                parents(pos = c("NOUN", "PROPN", "PRON"), relation = "nsubj",
                        parents(pos = c("VERB", "AUX"),
                                children(pos = c("VERB", "AUX"), relation = "conj",
                                         lemma = "have",
                                         not_children(relation = "nsubj", depth = 1),
                                         children(pos = agent_patient_pos, relation = c("dobj", "dative"),
                                                  label = "Possession",
                                                  fill = F,
                                                  children(pos = agent_patient_pos, relation = c("conj", "appos"),
                                                           label = "Possession", req = F, depth = 3,
                                                           fill = F
                                                  )
                                         )
                                )
                        )
                )
  )

  tokens = annotate_tqueries(tokens, "query", rule, overwrite = T, copy = F)
  if(all(is.na(tokens$query))){
    casted = data.table(doc_id = character(), ann_id = factor(), Entity = character(), Possession = character())
  } else {
    casted = cast_text(tokens, 'query', text_col = extract)
  }
  return(casted)
}

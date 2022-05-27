#' @importFrom rsyntax annotate_tqueries cast_text children NOT OR not_children parents tquery
NULL

###############################################################################################
##### Rule: Passive subject with by verb conjunction and noun conjunction (second verb)
##### Example: "Sue is called and asked by Greg and ENTITY." (asked)

a_8 = function(tokens, entities, verb_pos, agent_patient_pos, extract){
  rule = tquery(OR(token = entities, appos_child = "appos_child"), relation = "conj",
                label = "Entity", fill = F,
                parents(pos = c("NOUN", "PROPN", "PRON"), relation = "pobj",
                        parents(pos = "ADP", lemma = "by", relation = "agent",
                                parents(pos = c("VERB"), relation = "conj",
                                        label = "action", fill = F
                                )
                        )
                )
  )

  tokens = annotate_tqueries(tokens, "query", rule, overwrite = T, copy = F)
  if(all(is.na(tokens$query))){
    casted = data.table(doc_id = character(), ann_id = factor(), Entity = character(), action = character())
  } else {
    casted = cast_text(tokens, 'query', text_col = extract)
  }
  return(casted)
}

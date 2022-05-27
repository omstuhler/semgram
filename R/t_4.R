#' @importFrom rsyntax annotate_tqueries cast_text children NOT OR not_children parents tquery
NULL

###############################################################################################
##### Rule: Passive construction treatment and noun conjunct (entity)
##### Example: "Sue and ENTITY are asked." (asked)
##### Example: "Steve were given apples and ENTITY." (given)

t_4 = function(tokens, entities, verb_pos, agent_patient_pos, extract){
  rule = tquery(OR(token = entities, appos_child = "appos_child"), relation = c("nsubjpass"),
                label = "Entity", fill = F,
                parents(pos = c("NOUN", "PROPN", "PRON"), relation = c("nsubjpass"),
                        parents(pos = "VERB",
                                label = "treatment", fill = F
                        )
                )
  )

  tokens = annotate_tqueries(tokens, "query", rule, overwrite = T, copy = F)
  if(all(is.na(tokens$query))){
    casted = data.table(doc_id = character(), ann_id = factor(), Entity = character(), treatment = character())
  } else {
    casted = cast_text(tokens, 'query', text_col = extract)
  }
  return(casted)
}

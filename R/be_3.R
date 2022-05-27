#' @importFrom rsyntax annotate_tqueries cast_text children NOT OR not_children parents tquery
NULL

###############################################################################################
##### Rule: Appos parents
##### Example: "My friend ENTITY came." (friend)

be_3 = function(tokens, entities, verb_pos, agent_patient_pos, extract){

  rule = tquery(token = entities, relation = "appos",
                label = "Entity", fill = F,
                       parents(pos = c("NOUN", "PROPN", "PRON"), NOT(token = entities),
                               label = "characterization", fill = F
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

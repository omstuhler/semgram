###############################################################################################
##### Rule: Amod adjective
##### Example: "Steven got a nice, fresh ENTITY." (nice, fresh)

be_6 = function(tokens, entities, verb_pos, agent_patient_pos, extract){
  rule = tquery(OR(token = entities, appos_child = "appos_child"),
                label = "Entity", fill = F,
                children(pos = "ADJ", relation = "amod", phrase_replacement = NA,
                         label = "characterization",
                         fill = F)
                )
  
  tokens = tokens %>% annotate_tqueries("query", rule, overwrite = T, copy = F)
  if(all(is.na(tokens$query))){
    casted = data.table(doc_id = character(), ann_id = factor(), Entity = character(), characterization = character())
  } else {
    casted = cast_text(tokens, 'query', text_col = extract)
  }
  return(casted)
}
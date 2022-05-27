#' @importFrom rsyntax annotate_tqueries cast_text children NOT OR not_children parents tquery
NULL

###############################################################################################
##### Rule: A possesive of entity
##### Example: "The breaks of the ENTITIY were broken." (breaks)
##### Note: we look for both parent and children conjunctions because the dependency trees predicted acan be highly
#####       irregular on this regard.

H_2 = function(tokens, entities, verb_pos, agent_patient_pos, extract){
  rule = tquery(OR(token = entities, appos_child = "appos_child"), relation = "pobj",
                label = "Entity", fill = F,
                parents(token =  "of", relation = "prep",
                        parents(pos = c("NOUN", "PROPN"),
                                label = "Possession",
                                fill = F,
                                parents(pos = c("NOUN", "PROPN"), relation = "conj", req = F,
                                        label = "Possession",
                                        fill = F
                                ),
                                children(pos = c("NOUN", "PROPN"), relation = "conj", req = F,
                                         label = "Possession",
                                         fill = F,
                                         children(pos = c("NOUN", "PROPN"), relation = "conj", req = F,
                                                  label = "Possession",
                                                  fill = F
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

#' @importFrom rsyntax annotate_tqueries cast_text children NOT OR not_children parents tquery
NULL

###############################################################################################
##### Rule: noun conjunct of object of nsubj act
##### Example: "Joe calls Steve and ENTITY." (calls)
##### Example: "Joe gives Steve an apple and an ENTITY." (gives)

##### Note: We can't collect more verbs because based on the grammar, it will be
##### unclear whether these will be transitive.

t_2 = function(tokens, entities, verb_pos, agent_patient_pos, extract){
  rule = tquery(OR(token = entities, appos_child = "appos_child"), relation = "conj",
                label = "Entity", fill = F,
                parents(relation = c("dobj", "dative"), pos = c("NOUN", "PROPN", "PRON"),
                        parents(pos = "VERB",
                                label = "treatment",
                                fill = F
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

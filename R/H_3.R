###############################################################################################
##### Rule: Object of nsubj have act
##### Example: "ENTITY has apples, grapes, and bananas. (apples, grapes, bananas)

H_3 = function(tokens, entities, verb_pos, agent_patient_pos, extract){
  rule = tquery(OR(token = entities, appos_child = "appos_child"), relation = "nsubj",
                label = "Entity", fill = F,
                parents(pos = c("VERB", "AUX"), lemma = "have",
                        children(pos = agent_patient_pos, relation = c("dobj", "dative"), label = "Possession",
                                 fill = F,
                                 children(pos = agent_patient_pos, relation = c("conj", "appos"), depth = 3, req = F,
                                          label = "Possession",
                                          fill = F
                                 )
                        )
                )
  )
  
  tokens = tokens %>% annotate_tqueries("query", rule, overwrite = T, copy = F)
  if(all(is.na(tokens$query))){
    casted = data.table(doc_id = character(), ann_id = factor(), Entity = character(), Possession = character())
  } else {
    casted = cast_text(tokens, 'query', text_col = extract)
  }
  return(casted)
}
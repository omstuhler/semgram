###############################################################################################
##### Rule: Object of nsubj have act with conjunct noun
##### Example: "Joe and ENTITY have apples." (apples)

H_4 = function(tokens, entities, verb_pos, agent_patient_pos, extract){
  rule = tquery(OR(token = entities, appos_child = "appos_child"), relation = "conj",
                label = "Entity", fill = F,
                parents(pos = agent_patient_pos, relation = "nsubj",
                        parents(pos = c("VERB", "AUX"), lemma = "have",
                                children(pos = agent_patient_pos, relation = c("dobj", "dative"), label = "Possession",
                                         fill = F,
                                         children(pos = agent_patient_pos, relation = c("conj", "appos"), req = F,
                                                  label = "Possession", fill = F, depth = 3
                                         )
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
###############################################################################################
##### Rule: Actor of nsubj act with object and a noun conjunct (entity)
##### Example: "Joseph, Sarah, and Steve call Michael and ENTITY." (Joseph Sarah Steve, call)
##### Example: "Joseph, Sarah, and Steve give Michael an apple and an ENTITY." (Joseph Sarah Steve, give)
##### Note: We can't collect more verbs because based on the grammar, it will be
##### unclear whether these will be transitive.

At_3 = function(tokens, entities, verb_pos, agent_patient_pos, extract){
  rule = tquery(OR(token = entities, appos_child = "appos_child"), relation = "conj",
                parents(relation = c("dobj", "dative"), pos = c("NOUN", "PROPN", "PRON"),
                        parents(pos = "VERB",
                                label = "treatment", fill = F,
                                children(pos = agent_patient_pos, relation = "nsubj",
                                         label = "Agent", fill = F,
                                         children(pos = agent_patient_pos, relation = c("conj", "appos"), req = F,
                                                  label = "Agent", fill = F, depth = 3
                                         )
                                )
                        )
                )
  )
  
  tokens = tokens %>% annotate_tqueries("query_At", rule, overwrite = T, copy = F)
  
  if(all(is.na(tokens$query_At))){
    At_casted = data.table(doc_id = character(), ann_id = factor(), treatment = character(), Agent = character())
  } else {
    At_casted = cast_text(tokens, 'query_At', text_col = extract)
  }
  return(At_casted)
}
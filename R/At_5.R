#' @importFrom rsyntax annotate_tqueries cast_text children NOT OR not_children parents tquery
NULL

###############################################################################################
##### Rule: Actor of by act with object and noun conjunct (entity)
##### Example: "Sue and ENTITY are asked by Peter, Joseph, and Sue." (Peter, ask)
##### Example: "Steve were given apples and ENTITY by Peter, Joseph, and Sue." (Peter, give)

At_5 = function(tokens, entities, verb_pos, agent_patient_pos, extract){
  rule = tquery(OR(token = entities, appos_child = "appos_child"), relation = "conj",
                label = "Entity", fill = F,
                parents(pos = c("NOUN", "PROPN", "PRON"), relation = c("nsubjpass", "dobj"),
                        parents(pos = "VERB",
                                label = "treatment", fill = F,
                                children(pos = "ADP", lemma = "by", relation = "agent",
                                         children(pos = agent_patient_pos, relation = "pobj",
                                                  label = "Agent", fill = F,
                                                  children(pos = agent_patient_pos, relation = c("conj", "appos"), req = F,
                                                           label = "Agent", fill = F, depth = 3
                                                  )
                                         )
                                )
                        )
                )
  )

  tokens = annotate_tqueries(tokens, "query", rule, overwrite = T, copy = F)

  if(all(is.na(tokens$query))){
    casted = data.table(doc_id = character(), ann_id = factor(), treatment = character(), Entity = character(), Agent = character())
  } else {
    casted = cast_text(tokens, 'query', text_col = extract)
  }
  return(casted)
}

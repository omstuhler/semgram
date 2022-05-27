#' @importFrom rsyntax annotate_tqueries cast_text children NOT OR not_children parents tquery
NULL

###############################################################################################
##### Rule: Actor of by act with object
##### Example: "ENTITY is asked by Peter, Joseph, and Sue." (Peter Joseph Sue, ask)
##### Example: "Steven was given ENTITY by Peter, Joseph, and Sue." (Peter Joseph Sue, give)

At_4 = function(tokens, entities, verb_pos, agent_patient_pos, extract){
  rule = tquery(OR(token = entities, appos_child = "appos_child"), relation = c("nsubjpass", "dobj"),
                label = "Entity", fill = F,
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

  tokens = annotate_tqueries(tokens, "query", rule, overwrite = T, copy = F)

  if(all(is.na(tokens$query))){
    casted = data.table(doc_id = character(), ann_id = factor(), treatment = character(), Entity = character(), Agent = character())
  } else {
    casted = cast_text(tokens, 'query', text_col = extract)
  }
  return(casted)
}

#' @importFrom rsyntax annotate_tqueries cast_text children NOT OR not_children parents tquery
NULL

###############################################################################################
##### Rule: Objects of passive subject with by
##### Example: "Joe, Susan, and Michael are asked by ENTITY." (asked, Joe Sue Michael)
##### Example: "Steve is given a present by ENTITY." (given, Steve present)

aP_5 = function(tokens, entities, verb_pos, agent_patient_pos, extract){
  rule = tquery(OR(token = entities, appos_child = "appos_child"), relation = "pobj",
                label = "Entity", fill = F,
                parents(pos = "ADP", lemma = "by", relation = "agent",
                        parents(pos = c("VERB", "AUX"),
                                label = "action", fill = F,
                                NOT(lemma = "have"),
                                children(relation = c("nsubjpass", "dobj"), pos = agent_patient_pos,
                                         label = "Patient", fill = F,
                                         children(relation = c("conj", "appos"), pos = agent_patient_pos, req = F,
                                                  label = "Patient", fill = F, depth = 3
                                         )
                                )
                        )
                )
  )

  tokens = annotate_tqueries(tokens, "query", rule, overwrite = T, copy = F)

  if(all(is.na(tokens$query))){
    casted = data.table(doc_id = character(), ann_id = factor(), action = character(), Entity = character(), Patient = character())
  } else {
    casted = cast_text(tokens, 'query', text_col = extract)
  }
  return(casted)
}

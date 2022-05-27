#' @importFrom rsyntax annotate_tqueries cast_text children NOT OR not_children parents tquery
NULL

###############################################################################################
##### Rule: nsubj act or conjuncted second verb
##### Example: "ENTITY asked Joe." (asked)
##### Example: "ENTITY called and asked Joe." (called, asked)
##### Note: Note that the current basic annotation scheme cannot distinguish between a dependent of the first
##### conjunct and a shared dependent of the whole coordination (see https://universaldependencies.org/u/dep/conj.html).
##### E.g. in "ENTITY called and Joe answered." "answered" would be picked up as action of ENTITY if we just took conj-dependents.
##### To prevent this, we add a not_children condition to avoid cases where an independent subject is named.

a_1 = function(tokens, entities, verb_pos, agent_patient_pos, extract){
  rule = tquery(OR(token = entities, appos_child = "appos_child"), relation = "nsubj",
                label = "Entity", fill = F,
         parents(pos = verb_pos,
                 label = "action", fill = F,
                 children(pos = verb_pos, relation = "conj", req = F,
                          not_children(relation = "nsubj", depth = 1),
                          label = "action", fill = F,
                          children(get_aux_verbs_par = "YES",
                                   pos = verb_pos, relation = "aux", req = F,
                                   label = "action", fill = F
                          )
                 ),
                 children(get_aux_verbs_par = "YES",
                          pos = verb_pos, relation = "aux", req = F,
                          label = "action", fill = F
                 )
         )
  )

  tokens = annotate_tqueries(tokens, "query", rule, overwrite = T, copy = F)
  if(all(is.na(tokens$query))){
    casted = data.table(doc_id = character(), ann_id = factor(), Entity = character(), action = character())
  } else {
    casted = cast_text(tokens, 'query', text_col = extract)
  }
  return(casted)
}

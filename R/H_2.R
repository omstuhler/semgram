###############################################################################################
##### Rule: A possesive of entity
##### Example: "The breaks of the ENTITIY were broken." (breaks)
##### Note: we look for both parent and children conjunctions because the dependency trees predicted acan be highly
#####       irregular on this regard.

H_2 = function(tokens, entities, verb_pos, agent_patient_pos){
  rule = tquery(OR(token = entities, appos_child = "appos_child"), relation = "pobj",
                parents(token =  "of", relation = "prep",
                        parents(pos = c("NOUN", "PROPN"),
                                label = "Motif",
                                fill = F,
                                parents(pos = c("NOUN", "PROPN"), relation = "conj", req = F,
                                        label = "Motif",
                                        fill = F
                                ),
                                children(pos = c("NOUN", "PROPN"), relation = "conj", req = F,
                                         label = "Motif",
                                         fill = F,
                                         children(pos = c("NOUN", "PROPN"), relation = "conj", req = F,
                                                  label = "Motif",
                                                  fill = F
                                         )
                                )
                        )
                )
  )
  
  tokens = tokens %>%
    annotate_tqueries("posessive_of", rule, overwrite = T, copy = F)
  tokens[,c(ncol(tokens)-1,ncol(tokens))] = NULL
  return(tokens)
}
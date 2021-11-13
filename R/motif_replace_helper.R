motif_replace_helper = function(annotated_tokens, motif_names, extract_p = "lemma"){
  for(motif_name in motif_names){
    annotated_tokens[[motif_name]] = as.character(ifelse(annotated_tokens[[motif_name]] %in% c("Motif", "Motif.x", "Motif.y"), annotated_tokens[[extract_p]], NA))
  }
  return(annotated_tokens)
}

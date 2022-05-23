#' @importFrom stringr str_replace_all
NULL

retrieve_sentence = function(doc_i, sentence_i, tok_obj){
  sent = paste0(subset(tok_obj, tok_obj$doc_id == doc_i & tok_obj$sentence == sentence_i)$token, collapse = " ")
  sent = str_replace_all(sent, " (?=[,.;'])", "")
  str_replace_all(sent, " {2,}", " ")
}
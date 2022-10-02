#' @importFrom stringr str_replace_all
NULL

retrieve_paragraph = function(doc_i, sentence_i, tok_obj){
  sent = paste0(subset(tok_obj, tok_obj$doc_id == doc_i & as.numeric(tok_obj$sentence) == as.numeric(sentence_i))$token, collapse = " ")
  sent = str_replace_all(sent, " (?=[,.;'])", "")
  sent = str_replace_all(sent, " {2,}", " ")
  
  sent_pre = paste0(subset(tok_obj, tok_obj$doc_id == doc_i & as.numeric(tok_obj$sentence) == (as.numeric(sentence_i)-1))$token, collapse = " ")
  sent_pre = str_replace_all(sent_pre, " (?=[,.;'])", "")
  sent_pre = str_replace_all(sent_pre, " {2,}", " ")
  
  sent_post = paste0(subset(tok_obj, tok_obj$doc_id == doc_i & as.numeric(tok_obj$sentence) == (as.numeric(sentence_i)+1))$token, collapse = " ")
  sent_post = str_replace_all(sent_post, " (?=[,.;'])", "")
  sent_post = str_replace_all(sent_post, " {2,}", " ")
  
  paste0(sent_pre, " ", sent, " ", sent_post)
}
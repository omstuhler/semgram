retrieve_sentence = function(doc_i, sentence_i){
  sent = paste0(subset(tokens, tokens$doc_id == doc_i & tokens$sentence_id == sentence_i)$token, collapse = " ")
  sent = str_replace_all(sent, " (?=[,.;'])", "")
  str_replace_all(sent, " {2,}", " ")
}

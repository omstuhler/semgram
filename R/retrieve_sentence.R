retrieve_sentence = function(doc_i, sentence_i){
  paste0(subset(tokens, tokens$doc_id == doc_i & tokens$sentence_id == sentence_i)$token, collapse = " ")
}
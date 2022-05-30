#' @importFrom stringr str_c str_detect str_split str_extract
#' @importFrom data.table data.table
#' @importFrom rsyntax annotate_tqueries NOT parents tquery
NULL

#' Extract semantic motifs from parsed text object
#'
#'
#' This function extracts semantic motifs from text. The input is a data.frame representing a parsed text such as those returned by spacyr::spacy_parse().
#' The output is a list of data.frames containing semantic motifs such as actions or characterizations of textual entities. For a detailed explanation, see Stuhler (2022).
#' 
#' This is the main function for extracting semantic motifs around entities. Extraction is done by applying a set of extraction rules to the parsed text object
#' that includes part-of-speech tags and dependency relations. Details on the scope of these rules, the theoretical reasoning behind them, and the markup used for motifs can be found in Stuhler (2022).
#' For a recent application, see Stuhler (2021). The following is an abbreviated explanation of the motif classes from Stuhler (2022: 22-23).
#' 
#' Action motifs imply that an entity is doing something. The most straightforward example of this is when the entity serves as a nominal subject
#' of a verb ("ENTITY calls." - a_call). There are various syntactic constructions, however, in which a verb is considered an action despite the entity not being its nominal
#' subject. This includes instances in which the entity is the conjunct of a 
#' nominal subject ("John and ENTITY called." - a_call), there are multiple verbs ("ENTITY calls and asks." - a_call, a_ask), 
#' the entity  serves as an appositional modifier of a nominal subject (My friend ENTITY called. - a_call), 
#' and passive constructions ("John was called by ENTITY." - a_call). All actions are either lexical verbs or, if explicitly specified, auxiliary verbs.
#' 
#' Patient motifs are things that the entity of interest acts towards. They are usually objects of 
#' transitive verbs that were identified as an entity’s action. These objects can be in accusative case ("ENTITY asks John." - aP_ask_John) 
#' or in dative case if the verb is ditransitive ("ENTITY asks John a question." - aP_ask_John, aP_ask_question). 
#' Any action motif can lead to multiple Patient motifs – as any transitive verb can have multiple conjunct objects 
#' ("ENTITY calls John, Jane, and Steve." - aP_call_John, aP_call_Jane, aP_call_Steve).
#' Beyond objects, nominal passive subjects are also considered patients ("John is asked by ENTITY." - aP_ask_John).
#' 
#' Treatment motifs imply that something is done to an entity of interest. 
#' This is the case when the entity is the object of a transitive verb. The relationship 
#' between treatments and the entity is analogous to that of actions and patients.
#' The entity can function as accusative ("John calls ENTITY" - t_call) or dative 
#' ("John gives ENTITY an apple." - t_give) object, as
#' nominal passive subject ("ENTITY was called." - t_call), or as conjunct of any 
#' of these ("John calls Peter and ENTITY" - t_call).
#' 
#' Agent motifs are things that act towards the entity of interest via a treatment
#' motif. In most cases, agents are the nominal subject of a verb that has
#' been identified as a treatment motif ("John calls ENTITY." - t_call). However, 
#' agents need not
#' take that position and can be conjuncts ("Peter and John ask ENTITY." - At_Peter_ask, At_John_ask) 
#' or appositional modifiers ("My friend John
#' asked your brother ENTITY." - At_friend_ask, At_John_ask) of the nominal subject. Generally, 
#' the relationship between agents and treatments is analogous to that of the entity and actions, 
#' so that the transitive verb may take different positions ("John came and asked ENTITY." - 
#' At_John_ask; "John wants to ask ENTITY." At_John_ask), and passive constructions in which 
#' the entity serves as nominal passive subject ("ENTITY is asked by John." - At_John_ask) are considered.
#' 
#' Beyond these process motifs, there are two classes of stasis motifs.
#' Characterizations are characteristics ascribed to the entity of interest. There
#' are several ways in which this can happen. The most common one is via a
#' copular verb, that has either an adjectival ("ENTITY is kind." - be_kind; "ENTITY looks sad." - 
#' be_sad; "ENTITY is kind
#' and honest." - be_kind, be_honest) or nominal ("ENTITY is the winner." - be_winner; "ENTITY 
#' hopes to remain president." - be_president) attribute dependent. However, adjectives can also 
#' be direct dependents of the entity ("John bought a cheap, new ENTITY." - be_cheap, be_new) to 
#' be considered characterizations. Furthermore, nominal subjects of copular verbs with the 
#' entity as attribute dependent ("The winner was ENTITY." - be_winner) and heads with the 
#' entity as appositional modifier ("My brother ENTITY won." - be_brother) are considered characterizations.
#' 
#' Possessions are things that the entity of interest is said to possess. The rule
#' set accounts for three ways in which this can be expressed. First, when the
#' entity serves as a possession modifier to a noun, said noun and its conjunct
#' dependents are considered possessions ("ENTITY‘s spouse, friends, and parents were 
#' shocked." - H_spouse, H_friend, H_parent). Second, constructions where
#' the entity serves as object dependent of the preposition “of” can lead to possessions ("The 
#' breaks and wheels of the ENTITY were old." - H_breaks, H_wheels). Third, if the entity serves 
#' as nominal subject of “have” or one of its inflections, its direct object and nominal 
#' conjunctions thereof are considered possessions ("ENTITY has friends and enemies." - H_friend, H_enemy). 
#' Note that “have” is a transitive verb, but within the grammar, it is not considered an action, 
#' and consequently its objects aren’t considered patients.
#' 
#' @param tokens A tokens data.frame with predicted dependencies as generated, for instance, by spacyr::spacy_parse(). Dependencies need to be in ClearNLP style. This tag set is used by all English language models implemented in spaCy. Other languages or dependency grammars are currently not supported.
#' @param entities Specifies the core entities around which to extract motifs. This can be a single character string or a vector of character strings.
#' By default, multi-token strings such as "Harry Potter" will be parsed and considered. Note that this parameter is case-sensitive.
#' It defaults to "*" in which case any token is treated as a potential entity.
#' @param motif_classes A character vector specifying which motif classes should be considered in the extraction.
#' This can include "t" for treatments, "a" for actions, "be" for characterizations, "H" for possessions, as well as "At" and "aP" for agent-treatment and action-patient motifs respectively.
#' By default, all motif classes are considered. Note, however, that runtime increases with the number of motif classes considered.
#' @param custom_cols Generally, the columns in the tokens object should be labeled as follows: "doc_id", "sentence_id", "token_id", "token", "lemma", "pos", "head_token_id" , "dep_rel". If the columns in your tokens object are not labeled according to this scheme, provide the matching column names to custom_cols in the corresponding order.
#' @param fast If set to true, some of the more specific extraction rules are not applied. This results in fewer extractions but faster run time. Defaults to FALSE.
#' @param parse_multi_token_entities Should multi-token entities (e.g., "Harry Potter") be considered? Defaults to TRUE. When using multi-token entities, it is crucial that tokens are separated by a space character. Input should match the tokenized version in the tokens object.
#' For instance, hyphens are usually considered a token in tokenization, so that "Claude Levi-Strauss" should be passed to the function as "Claude Levi - Strauss".
#' @param extract Defines whether extracted motifs are represented in "lemma" or "token" form. Defaults to "lemma" which reduces sparsity and is preferable for most purposes.
#' @param markup If TRUE, motifs will also be provided as collapsed markup tokens (e.g., "aP_ask_Harry"). Defaults to FALSE.
#' @param add_sentence If TRUE, the sentence for each motif is added to the extracted motif. Note that this is done by pasting together the tokens of the sentence, so that the representation might differ minimally from the original text. Nonetheless, this can be helpful for validation and for a mode of analyses that switches between distant and close readings of the text. Defaults to FALSE. Note that setting this to TRUE will noticeably increase runtime.
#' @param be_entity Should things that are linked to an entity via "being" (or one of its lemmas) be considered as characterization motifs?
#' For example, if we are extracting characterizations around the "immigrants" in the sentence "my parents are immigrants", should we extract the characterization motif "be_parent"? Defaults to TRUE.
#' @param get_aux_verbs Should auxiliary verbs (e.g., can, could, may, must) be considered actions? Defaults to FALSE.
#' @param aux_verb_markup Should auxiliary verbs with "to" be marked up so that "going" in "going to eat" becomes "going-to".
#' Note that this will not affect cases of the sort "going to the bar." This can be useful for analyses concerning modality. Defaults to TRUE.
#' @param pron_as_ap Should pronouns be considered agents and patients? Defaults to FALSE.
#' @param use_appos Should things linked to an entity via an appositional modifier be considered as equivalent to the entity?
#' For example, if we specify our entity to be "Peter" in the sentence "My brother Peter left.", should "brother" be considered equivalent to "Peter"?
#' Only if use_appos = TRUE, we can extract "leaving" as action motif associated with Peter, as the subject associated with "leaving" is "brother". Defaults to TRUE.
#' @param lowercase Should all tokens and lemmas be lowercased? Defaults to FALSE.
#' @param verbose Should progress be reported during execution? Defaults to FALSE.
#' @return A list with six dataframes, one for each motif class. List elements of motif classes not specified in the motif_classes parameter will be empty.
#' @export
#' @references
#' Stuhler, O. (2022) "Who Does What To Whom? Making Text Parsers Work for Sociological Inquiry." Sociological Methods and Research. <doi: 10.1177/00491241221099551>.
#' 
#' Stuhler, O. (2021) "What's in a category? A new approach to Discourse Role Analysis." Poetics 88. <doi:10.1016/j.poetic.2021.101568>.
#' @examples
#' # Given data.frame with parsed sentence – as can be generated with spacyr::spacy_parse().
#' tokens_df = data.frame(doc_id = rep("text1", 4),
#'                        sentence_id = rep(1, 4),
#'                        token_id = 1:4,
#'                        token = c("Emil", "chased", "the", "thief"),
#'                        lemma = c("Emil", "chase", "the", "thief"),
#'                        pos = c("PROPN", "VERB", "DET", "NOUN"),
#'                        head_token_id = c(2,2,4,2),
#'                        dep_rel = c("nsubj", "ROOT", "det", "dobj")
#'                        )
#'
#' # Extract motifs around specific entities, here "Emil"
#' extract_motifs(tokens = tokens_df, entities = c("Emil"))
#'
#' # Extract all possible motifs
#' extract_motifs(tokens = tokens_df, entities = "*")

extract_motifs = function(tokens,
                          entities = "*",
                          motif_classes = c("t", "a", "be", "H", "At", "aP"),
                          custom_cols,
                          fast = F,
                          parse_multi_token_entities = T,
                          extract = "lemma",
                          markup = F,
                          add_sentence = F,
                          be_entity = T,
                          get_aux_verbs = F,
                          aux_verb_markup = T,
                          pron_as_ap = F,
                          use_appos = T,
                          lowercase = F,
                          verbose = F){

  ###############################################################################################
  #####################################Pre-processing############################################
  ###############################################################################################

  ###############################################################################################
  ##### Text input
  if(missing("tokens")){
    stop("It seems you didn't provide a tokens object.", call. = FALSE)
  }
  if(missing("entities")){
    message("It seems you didn't specify any core entities to extract motifs around. Defaulting to entities = *.")
  }

  ###############################################################################################
  ##### If custom_cols is provided, adjust the columns
  if(!missing("custom_cols")){
    if(length(custom_cols) != 8){
      stop("You provided a custom columns vector of length other than 8.", call. = FALSE)
    } else {
      names(tokens)[which(names(tokens) == custom_cols[1])] = "doc_id"
      names(tokens)[which(names(tokens) == custom_cols[2])] = "sentence_id"
      names(tokens)[which(names(tokens) == custom_cols[3])] = "token_id"
      names(tokens)[which(names(tokens) == custom_cols[4])] = "token"
      names(tokens)[which(names(tokens) == custom_cols[5])] = "lemma"
      names(tokens)[which(names(tokens) == custom_cols[6])] = "pos"
      names(tokens)[which(names(tokens) == custom_cols[7])] = "head_token_id"
      names(tokens)[which(names(tokens) == custom_cols[8])] = "dep_rel"
    }
  }

  ###############################################################################################
  ##### Unify column labels in tokens dataframe
  ##### Development Note: this should be taken out and subsequent code adjusted.
  if("sentence_id" %in% names(tokens)){
    names(tokens)[which(names(tokens) == "sentence_id")] = "sentence"
  }
  if("dep_rel" %in% names(tokens)){
    names(tokens)[which(names(tokens) == "dep_rel")] = "relation"
  }

  ###############################################################################################
  ##### Replace

  # Get all instances of entities that are multigrams
  if(T %in% str_detect(entities, " ") & parse_multi_token_entities == T){
    for(entity in entities){
      if(entity == entities[1]){
        phrase_df = data.frame()
        master_length = dim(tokens)[1]
      }

      entity_split = unlist(str_split(entity, pattern = " "))
      entity_length = length(entity_split)

      if(entity_length>1){
        if(verbose){cat("Replacing phrase: ", entity, "\n")}
        length_seq = 1:entity_length
        which_first_id = which(tokens[,"token"] == entity_split[1])

        for(rownumber in which_first_id-1){
          if(isTRUE(all.equal(unname(unlist(tokens[c(rownumber+length_seq), "lemma"])), entity_split))){
            for(token in length_seq){
              row_index = c(rownumber+token)
              if(nrow(phrase_df) == 0){
                phrase_df = rbind(phrase_df, c(tokens[row_index,],
                                               phrase_replacement = paste(entity_split, collapse = " ")),
                                  stringsAsFactors = F)
              } else {
                if(!paste(tokens[row_index,][,c("doc_id", "sentence","token_id")], collapse = "_") %in%
                   str_c(phrase_df$doc_id, phrase_df$sentence, phrase_df$token_id, sep = "_")){
                  phrase_df = rbind(phrase_df, c(tokens[row_index,],
                                                 phrase_replacement = paste(entity_split, collapse = " ")),
                                    stringsAsFactors = F)
                }
              }
            }
          }
        }
      }
    }

    # Merge in the phrase replacements
    if(exists("phrase_df") & nrow(phrase_df) > 0){
      tokens = merge(tokens, phrase_df[,c("doc_id", "sentence", "token_id", "phrase_replacement")],
                     by = c("doc_id", "sentence", "token_id"), all.x = T)
      tokens$token = ifelse(!is.na(tokens$phrase_replacement), tokens$phrase_replacement, tokens$token)
      tokens$lemma = ifelse(!is.na(tokens$phrase_replacement), tokens$phrase_replacement, tokens$lemma)
    } else {
      tokens$phrase_replacement = NA
    }
  } else {
    tokens$phrase_replacement = NA
  }
  if(T %in% str_detect(entities, " ") & parse_multi_token_entities == F){
    message("Warning: multi-token entities were detected but parsing them was set to FALSE.\n")
  }


  ###############################################################################################
  ##### Mark up tokens with eligible appos children
  if(use_appos){
    appos_child = tquery(token = entities, relation = "appos",
                         parents(pos = c("NOUN", "PROPN", "PRON"), NOT(token = entities),
                                 label = "appos_child", fill = F
                         )
    )

    tokens = annotate_tqueries(tokens, "appos_child", appos_child, overwrite = T, copy = F)
    tokens[,c(ncol(tokens)-1,ncol(tokens))] = NULL
    } else {
      tokens$appos_child = ""
  }


  ###############################################################################################
  ##### Set pronoun parameter
  if(pron_as_ap){
    agent_patient_pos = c("NOUN", "PROPN", "PRON")
    tokens$lemma = ifelse(tokens$lemma == "-PRON-", tokens$token, tokens$lemma)
    } else {
      agent_patient_pos = c("NOUN", "PROPN")
  }

  ###############################################################################################
  ##### Replace auxiliary phrases

  # Get going to, need to, etc. instances
  if(aux_verb_markup){
    if(verbose){cat("Replacing aux phrases.. ", "\n")}
    aux_phrases = list(c("going", "to"),
                       c("need", "to"), c("needed", "to"),
                       c("have", "to"), c("had", "to"),
                       c("ought", "to"))

    for(aux_phrase in 1:length(aux_phrases)){
      if(aux_phrase == 1){
        aux_phrase_df = data.frame()
        master_length = dim(tokens)[1]
      }

      aux_phrase_split = aux_phrases[[aux_phrase]]

      length_seq = 1:2
      which_first_id = which(tokens[,"token"] == aux_phrase_split[1])

      for(rownumber in which_first_id-1){
        if(isTRUE(all.equal(unname(unlist(tokens[c(rownumber+length_seq), "token"])), aux_phrase_split)) &
           tokens[rownumber+length_seq[2], "relation"] == "aux"){
          for(token in length_seq){
            row_index = c(rownumber+token)
            if(nrow(aux_phrase_df) == 0){
              aux_phrase_df = rbind(aux_phrase_df, c(tokens[row_index,],
                                                     aux_phrase_replacement = paste(aux_phrase_split, collapse = "-")),
                                stringsAsFactors = F)
            } else {
              if(!paste(tokens[row_index,][,c("doc_id", "sentence","token_id")], collapse = "_") %in%
                 str_c(aux_phrase_df$doc_id, aux_phrase_df$sentence, aux_phrase_df$token_id, sep = "_")){
                aux_phrase_df = rbind(aux_phrase_df, c(tokens[row_index,],
                                               aux_phrase_replacement = paste(aux_phrase_split, collapse = "-")),
                                  stringsAsFactors = F)
              }
            }
          }
        }
      }
    }

    # Merge in the phrase replacements
    if(exists("aux_phrase_df") & nrow(aux_phrase_df) > 0){
      tokens = merge(tokens, aux_phrase_df[,c("doc_id", "sentence", "token_id", "aux_phrase_replacement")],
                     by = c("doc_id", "sentence", "token_id"), all.x = T)


      tokens$token = ifelse(!is.na(tokens$aux_phrase_replacement), tokens$aux_phrase_replacement, tokens$token)
      tokens$lemma = ifelse(!is.na(tokens$aux_phrase_replacement), tokens$aux_phrase_replacement, tokens$lemma)
    } else {
      tokens$aux_phrase_replacement = ""
    }
  } else {
    tokens$aux_phrase_replacement = ""
  }

  ###############################################################################################
  ##### Set auxiliary markup parameter
  if(get_aux_verbs){
    if(!aux_verb_markup){
      message("You are extracting auxiliary verbs but aux_verb_markup is set to FALSE. This may not give ideal results.\n")
    }
    tokens$get_aux_verbs_par = "YES"
    verb_pos = c("VERB", "AUX")
  } else {
    tokens$get_aux_verbs_par = "NO"
    verb_pos = c("VERB")
  }

  ###############################################################################################
  ######################################Rule implementation######################################
  ###############################################################################################

  ###############################################################################################
  ############################################Action#############################################
  ###############################################################################################

  if("a" %in% motif_classes){
    if(verbose){cat("Extracting actions\n")}

    ###############################################################################################
    ##### Run fast rules
    tryCatch({
      nsubj_act_conj = a_1(tokens, entities, verb_pos, agent_patient_pos, extract)
    }, error = function(e){
      message("There was an error in extracting action motifs (a_1). Some action motifs might not have been extracted properly. This is an important rule and you probably shouldn't proceed.")
      nsubj_act_conj <<- data.table(doc_id = character(), ann_id = factor(), Entity = character(), action = character())
    })

    ###############################################################################################
    ##### Run slow rules
    if(fast){
      nsubj_act_noun_conj_verb_conj =
        by_act =
        by_act_noun_conjunct =
        by_act_2 =
        by_act_2_1 =
        by_act_2_noun_conj =
        by_act_2_noun_conj_1 =
        xcomp_act_conj_verb =
        xcomp_act_conj_noun = data.table(doc_id = character(), ann_id = factor(), Entity = character(), action = character())

    } else {
      tryCatch({
        nsubj_act_noun_conj_verb_conj = a_2(tokens, entities, verb_pos, agent_patient_pos, extract)
      }, error = function(e){
        message("There was an error in extracting action motifs (a_2). Some action motifs might not have been extracted properly. This is an important rule and you probably shouldn't proceed.")
        nsubj_act_noun_conj_verb_conj <<- data.table(doc_id = character(), ann_id = factor(), Entity = character(), action = character())
      })
      tryCatch({
        by_act = a_3(tokens, entities, verb_pos, agent_patient_pos, extract)
      }, error = function(e){
        message("There was an error in extracting action motifs (a_3). Some action motifs might not have been extracted properly. This is an important rule and you probably shouldn't proceed.")
        by_act <<- data.table(doc_id = character(), ann_id = factor(), Entity = character(), action = character())
      })
      tryCatch({
        by_act_noun_conjunct = a_4(tokens, entities, verb_pos, agent_patient_pos, extract)
      }, error = function(e){
        message("There was an error in extracting action motifs (a_4). Some action motifs might not have been extracted properly. This is an important rule and you probably shouldn't proceed.")
        by_act_noun_conjunct <<- data.table(doc_id = character(), ann_id = factor(), Entity = character(), action = character())
      })
      tryCatch({
        by_act_2 = a_5(tokens, entities, verb_pos, agent_patient_pos, extract)
      }, error = function(e){
        message("There was an error in extracting action motifs (a_5). Some action motifs might not have been extracted properly. This is an important rule and you probably shouldn't proceed.")
        by_act_2 <<- data.table(doc_id = character(), ann_id = factor(), Entity = character(), action = character())
      })
      tryCatch({
        by_act_2_1 = a_6(tokens, entities, verb_pos, agent_patient_pos, extract)
      }, error = function(e){
        message("There was an error in extracting action motifs (a_6). Some action motifs might not have been extracted properly. This is an important rule and you probably shouldn't proceed.")
        by_act_2_1 <<- data.table(doc_id = character(), ann_id = factor(), Entity = character(), action = character())
      })
      tryCatch({
        by_act_2_noun_conj = a_7(tokens, entities, verb_pos, agent_patient_pos, extract)
      }, error = function(e){
        message("There was an error in extracting action motifs (a_7). Some action motifs might not have been extracted properly. This is an important rule and you probably shouldn't proceed.")
        by_act_2_noun_conj <<- data.table(doc_id = character(), ann_id = factor(), Entity = character(), action = character())
      })
      tryCatch({
        by_act_2_noun_conj_1 = a_8(tokens, entities, verb_pos, agent_patient_pos, extract)
      }, error = function(e){
        message("There was an error in extracting action motifs (a_8). Some action motifs might not have been extracted properly. This is an important rule and you probably shouldn't proceed.")
        by_act_2_noun_conj_1 <<- data.table(doc_id = character(), ann_id = factor(), Entity = character(), action = character())
      })
      tryCatch({
        xcomp_act_conj_verb = a_9(tokens, entities, verb_pos, agent_patient_pos, extract)
      }, error = function(e){
        message("There was an error in extracting action motifs (a_9). Some action motifs might not have been extracted properly. This is an important rule and you probably shouldn't proceed.")
        xcomp_act_conj_verb <<- data.table(doc_id = character(), ann_id = factor(), Entity = character(), action = character())
      })
      tryCatch({
        xcomp_act_conj_noun = a_10(tokens, entities, verb_pos, agent_patient_pos, extract)
      }, error = function(e){
        message("There was an error in extracting action motifs (a_10). Some action motifs might not have been extracted properly. This is an important rule and you probably shouldn't proceed.")
        xcomp_act_conj_noun <<- data.table(doc_id = character(), ann_id = factor(), Entity = character(), action = character())
      })
    }
  }

  ###############################################################################################
  #####################################Action & Patients#########################################
  ###############################################################################################

  if("aP" %in% motif_classes){
    if(verbose){cat("Extracting action-patients\n")}
    ###############################################################################################
    tryCatch({
      nsubj_obj_conj_act_aP_casted = aP_1(tokens, entities, verb_pos, agent_patient_pos, extract)
    }, error = function(e){
      message("There was an error in extracting action-patient motifs (aP_1). Some action-patient motifs might not have been extracted properly. This is an important rule and you probably shouldn't proceed.")
      nsubj_obj_conj_act_aP_casted <<- data.table(doc_id = character(), ann_id = factor(), action = character(), Entity = character(), Patient = character())
    })

    ###############################################################################################
    if(fast){
      nsubj_obj_conj_aP_casted =
        nsubj_conj_obj_act_aP_casted =
        nsubj_conj_subj_cons_obj_aP_casted =
        by_act_obj_aP_casted =
        by_act_obj_nc_aP_casted =
        by_act_obj_cverb_1_aP_casted =
        by_act_obj_cverb_2_aP_casted =
        by_act_obj_cverb_cobj_1_aP_casted =
        by_act_obj_cverb_cobj_2_aP_casted =
        xcomp_act_obj_aP_casted =
        xcomp_act_obj_vconj_aP_casted =
        xcomp_act_obj_nconj_aP_casted =
        xcomp_act_obj_nconj_vconj_aP_casted = data.table(doc_id = character(), ann_id = factor(), action = character(), Entity = character(), Patient = character())

    } else {
      tryCatch({
        nsubj_obj_conj_aP_casted = aP_2(tokens, entities, verb_pos, agent_patient_pos, extract)
      }, error = function(e){
        message("There was an error in extracting action-patient motifs (aP_2). Some action-patient motifs might not have been extracted properly. This is a secondary rule and you can probably proceed.")
        nsubj_obj_conj_aP_casted <<- data.table(doc_id = character(), ann_id = factor(), action = character(), Entity = character(), Patient = character())
      })
      tryCatch({
        nsubj_conj_obj_act_aP_casted = aP_3(tokens, entities, verb_pos, agent_patient_pos, extract)
      }, error = function(e){
        message("There was an error in extracting action-patient motifs (aP_3). Some action-patient motifs might not have been extracted properly. This is a secondary rule and you can probably proceed.")
        nsubj_conj_obj_act_aP_casted <<- data.table(doc_id = character(), ann_id = factor(), action = character(), Entity = character(), Patient = character())
      })
      tryCatch({
        nsubj_conj_subj_cons_obj_aP_casted = aP_4(tokens, entities, verb_pos, agent_patient_pos, extract)
      }, error = function(e){
        message("There was an error in extracting action-patient motifs (aP_4). Some action-patient motifs might not have been extracted properly. This is a secondary rule and you can probably proceed.")
        nsubj_conj_subj_cons_obj_aP_casted <<- data.table(doc_id = character(), ann_id = factor(), action = character(), Entity = character(), Patient = character())
      })
      tryCatch({
        by_act_obj_aP_casted = aP_5(tokens, entities, verb_pos, agent_patient_pos, extract)
      }, error = function(e){
        message("There was an error in extracting action-patient motifs (aP_5). Some action-patient motifs might not have been extracted properly. This is a secondary rule and you can probably proceed.")
        by_act_obj_aP_casted <<- data.table(doc_id = character(), ann_id = factor(), action = character(), Entity = character(), Patient = character())
      })
      tryCatch({
        by_act_obj_nc_aP_casted = aP_6(tokens, entities, verb_pos, agent_patient_pos, extract)
      }, error = function(e){
        message("There was an error in extracting action-patient motifs (aP_6). Some action-patient motifs might not have been extracted properly. This is a secondary rule and you can probably proceed.")
        by_act_obj_nc_aP_casted <<- data.table(doc_id = character(), ann_id = factor(), action = character(), Entity = character(), Patient = character())
      })
      tryCatch({
        by_act_obj_cverb_1_aP_casted = aP_7(tokens, entities, verb_pos, agent_patient_pos, extract)
      }, error = function(e){
        message("There was an error in extracting action-patient motifs (aP_7). Some action-patient motifs might not have been extracted properly. This is a secondary rule and you can probably proceed.")
        by_act_obj_cverb_1_aP_casted <<- data.table(doc_id = character(), ann_id = factor(), action = character(), Entity = character(), Patient = character())
      })
      tryCatch({
        by_act_obj_cverb_2_aP_casted = aP_8(tokens, entities, verb_pos, agent_patient_pos, extract)
      }, error = function(e){
        message("There was an error in extracting action-patient motifs (aP_8). Some action-patient motifs might not have been extracted properly. This is a secondary rule and you can probably proceed.")
        by_act_obj_cverb_2_aP_casted <<- data.table(doc_id = character(), ann_id = factor(), action = character(), Entity = character(), Patient = character())
        tokens$by_act_obj_cverb_2_aP <<- NA
      })
      tryCatch({
        by_act_obj_cverb_cobj_1_aP_casted = aP_9(tokens, entities, verb_pos, agent_patient_pos, extract)
      }, error = function(e){
        message("There was an error in extracting action-patient motifs (aP_9). Some action-patient motifs might not have been extracted properly. This is a secondary rule and you can probably proceed.")
        by_act_obj_cverb_cobj_1_aP_casted <<- data.table(doc_id = character(), ann_id = factor(), action = character(), Entity = character(), Patient = character())
      })
      tryCatch({
        by_act_obj_cverb_cobj_2_aP_casted = aP_10(tokens, entities, verb_pos, agent_patient_pos, extract)
      }, error = function(e){
        message("There was an error in extracting action-patient motifs (aP_10). Some action-patient motifs might not have been extracted properly. This is a secondary rule and you can probably proceed.")
        by_act_obj_cverb_cobj_2_aP_casted <<- data.table(doc_id = character(), ann_id = factor(), action = character(), Entity = character(), Patient = character())
      })
      tryCatch({
        xcomp_act_obj_aP_casted = aP_11(tokens, entities, verb_pos, agent_patient_pos, extract)
      }, error = function(e){
        message("There was an error in extracting action-patient motifs (aP_11). Some action-patient motifs might not have been extracted properly. This is a secondary rule and you can probably proceed.")
        xcomp_act_obj_aP_casted <<- data.table(doc_id = character(), ann_id = factor(), action = character(), Entity = character(), Patient = character())
      })
      tryCatch({
        xcomp_act_obj_vconj_aP_casted = aP_12(tokens, entities, verb_pos, agent_patient_pos, extract)
      }, error = function(e){
        message("There was an error in extracting action-patient motifs (aP_12). Some action-patient motifs might not have been extracted properly. This is a secondary rule and you can probably proceed.")
        xcomp_act_obj_vconj_aP_casted <<- data.table(doc_id = character(), ann_id = factor(), action = character(), Entity = character(), Patient = character())
      })
      tryCatch({
        xcomp_act_obj_nconj_aP_casted = aP_13(tokens, entities, verb_pos, agent_patient_pos, extract)
      }, error = function(e){
        message("There was an error in extracting action-patient motifs (aP_13). Some action-patient motifs might not have been extracted properly. This is a secondary rule and you can probably proceed.")
        xcomp_act_obj_nconj_aP_casted <<- data.table(doc_id = character(), ann_id = factor(), action = character(), Entity = character(), Patient = character())
      })
      tryCatch({
        xcomp_act_obj_nconj_vconj_aP_casted = aP_14(tokens, entities, verb_pos, agent_patient_pos, extract)
      }, error = function(e){
        message("There was an error in extracting action-patient motifs (aP_14). Some action-patient motifs might not have been extracted properly. This is a secondary rule and you can probably proceed.")
        xcomp_act_obj_nconj_vconj_aP_casted <<- data.table(doc_id = character(), ann_id = factor(), action = character(), Entity = character(), Patient = character())
      })
    }
  }


  ###############################################################################################
  ########################################Treatments#############################################
  ###############################################################################################
  if("t" %in% motif_classes){
    if(verbose){cat("Extracting treatments\n")}
    ###############################################################################################
    tryCatch({
      dobj_treat = t_1(tokens, entities, verb_pos, agent_patient_pos, extract)
    }, error = function(e){
      message("There was an error in extracting treatment motifs (t_1). Some treatment motifs might not have been extracted properly. This is an important rule and you probably shouldn't proceed.")
      dobj_treat <<- data.table(doc_id = character(), ann_id = factor(), Entity = character(), treatment = character())
    })

    if(fast){
      ###############################################################################################
      dobj_conj_treat =
        obj_of_by_act =
        obj_of_by_act_nconj = data.table(doc_id = character(), ann_id = factor(), Entity = character(), treatment = character())

    } else {
      tryCatch({
        dobj_conj_treat = t_2(tokens, entities, verb_pos, agent_patient_pos, extract)
      }, error = function(e){
        message("There was an error in extracting treatment motifs (t_2). Some treatment motifs might not have been extracted properly. This is an important rule and you probably shouldn't proceed.")
        dobj_conj_treat <<- data.table(doc_id = character(), ann_id = factor(), Entity = character(), treatment = character())
      })
      tryCatch({
        obj_of_by_act = t_3(tokens, entities, verb_pos, agent_patient_pos, extract)
      }, error = function(e){
        message("There was an error in extracting treatment motifs (t_3). Some treatment motifs might not have been extracted properly. This is an important rule and you probably shouldn't proceed.")
        obj_of_by_act <<- data.table(doc_id = character(), ann_id = factor(), Entity = character(), treatment = character())
      })
      tryCatch({
        obj_of_by_act_nconj = t_4(tokens, entities, verb_pos, agent_patient_pos, extract)
      }, error = function(e){
        message("There was an error in extracting treatment motifs (t_4). Some treatment motifs might not have been extracted properly. This is an important rule and you probably shouldn't proceed.")
        obj_of_by_act_nconj <<- data.table(doc_id = character(), ann_id = factor(), Entity = character(), treatment = character())
      })
    }
  }

  ###############################################################################################
  ###################################Agents & Treatments#########################################
  ###############################################################################################
  if("At" %in% motif_classes){
    if(verbose){cat("Extracting agent-treatments\n")}
    ###############################################################################################
    tryCatch({
      dobj_treat_actor_At_casted = At_1(tokens, entities, verb_pos, agent_patient_pos, extract)
    }, error = function(e){
      message("There was an error in extracting agent-treatment motifs (At_1). Some agent-treatment motifs might not have been extracted properly. This is an important rule and you probably shouldn't proceed.")
      dobj_treat_actor_At_casted <<- data.table(doc_id = character(), ann_id = factor(), treatment = character(), Entity = character(), Agent = character())
    })

    ###############################################################################################
    if(fast){
      dobj_treat_conj_actor_At_casted =
        dobj_nconj_treat_At_casted =
        by_act_agent_At_casted =
        obj_of_by_act_nconj_ac_At_casted = data.table(doc_id = character(), ann_id = factor(), treatment = character(), Entity = character(), Agent = character())
    } else {
      tryCatch({
        dobj_treat_conj_actor_At_casted = At_2(tokens, entities, verb_pos, agent_patient_pos, extract)
      }, error = function(e){
        message("There was an error in extracting agent-treatment motifs (At_2). Some agent-treatment motifs might not have been extracted properly. This is a secondary rule and you can probably proceed.")
        dobj_treat_conj_actor_At_casted <<- data.table(doc_id = character(), ann_id = factor(), treatment = character(), Entity = character(), Agent = character())
      })
      tryCatch({
        dobj_nconj_treat_At_casted = At_3(tokens, entities, verb_pos, agent_patient_pos, extract)
      }, error = function(e){
        message("There was an error in extracting agent-treatment motifs (At_3). Some agent-treatment motifs might not have been extracted properly. This is a secondary rule and you can probably proceed.")
        dobj_nconj_treat_At_casted <<- data.table(doc_id = character(), ann_id = factor(), treatment = character(), Entity = character(), Agent = character())
      })
      tryCatch({
        by_act_agent_At_casted = At_4(tokens, entities, verb_pos, agent_patient_pos, extract)
      }, error = function(e){
        message("There was an error in extracting agent-treatment motifs (At_4). Some agent-treatment motifs might not have been extracted properly. This is a secondary rule and you can probably proceed.")
        by_act_agent_At_casted <<- data.table(doc_id = character(), ann_id = factor(), treatment = character(), Entity = character(), Agent = character())
        tokens$by_act_agent_At <<- NA
      })
      tryCatch({
        obj_of_by_act_nconj_ac_At_casted = At_5(tokens, entities, verb_pos, agent_patient_pos, extract)
      }, error = function(e){
        message("There was an error in extracting agent-treatment motifs (At_5). Some agent-treatment motifs might not have been extracted properly. This is a secondary rule and you can probably proceed.")
        obj_of_by_act_nconj_ac_At_casted <<- data.table(doc_id = character(), ann_id = factor(), treatment = character(), Entity = character(), Agent = character())
        tokens$obj_of_by_act_nconj_ac_At <<- NA
      })
    }
  }

  ###############################################################################################
  #####################################Characterizations#########################################
  ###############################################################################################

  if("be" %in% motif_classes){
    if(verbose){cat("Extracting characterizations\n")}
    ###############################################################################################
    tryCatch({
      being_adj = be_1(tokens, entities, verb_pos, agent_patient_pos, extract)
    }, error = function(e){
      message("There was an error in extracting characterization motifs (be_1). Some characterization motifs might not have been extracted properly. This is an important rule and you probably shouldn't proceed.")
      being_adj <<- data.table(doc_id = character(), ann_id = factor(), Entity = character(), characterization = character())
    })
    tryCatch({
      amod_adj = be_6(tokens, entities, verb_pos, agent_patient_pos, extract)
    }, error = function(e){
      message("There was an error in extracting characterization motifs (be_6). Some characterization motifs might not have been extracted properly. This is an important rule and you probably shouldn't proceed.")
      amod_adj <<- data.table(doc_id = character(), ann_id = factor(), Entity = character(), characterization = character())

    })

    ###############################################################################################
    if(fast){
      being_adj_nconj =
        appos_char =
        being_adj_vconj =
        being_adj_xcomp = data.table(doc_id = character(), ann_id = factor(), Entity = character(), characterization = character())

    } else {
      tryCatch({
        being_adj_nconj = be_2(tokens, entities, verb_pos, agent_patient_pos, extract)
      }, error = function(e){
        message("There was an error in extracting characterization motifs (be_2). Some characterization motifs might not have been extracted properly. This is a secondary rule and you can probably proceed.")
        being_adj_nconj <<- data.table(doc_id = character(), ann_id = factor(), Entity = character(), characterization = character())
      })
      tryCatch({
        appos_char = be_3(tokens, entities, verb_pos, agent_patient_pos, extract)
      }, error = function(e){
        message("There was an error in extracting characterization motifs (be_3). Some characterization motifs might not have been extracted properly. This is a secondary rule and you can probably proceed.")
        appos_char <<- data.table(doc_id = character(), ann_id = factor(), Entity = character(), characterization = character())
      })
      tryCatch({
        being_adj_vconj = be_4(tokens, entities, verb_pos, agent_patient_pos, extract)
      }, error = function(e){
        message("There was an error in extracting characterization motifs (be_4). Some characterization motifs might not have been extracted properly. This is a secondary rule and you can probably proceed.")
        being_adj_vconj <<- data.table(doc_id = character(), ann_id = factor(), Entity = character(), characterization = character())
      })
      tryCatch({
        being_adj_xcomp = be_5(tokens, entities, verb_pos, agent_patient_pos, extract)
      }, error = function(e){
        message("There was an error in extracting characterization motifs (be_5). Some characterization motifs might not have been extracted properly. This is a secondary rule and you can probably proceed.")
        being_adj_xcomp <<- data.table(doc_id = character(), ann_id = factor(), Entity = character(), characterization = character())
      })
    }


    if(be_entity){
      ###############################################################################################
      tryCatch({
        being_entity = be_7(tokens, entities, verb_pos, agent_patient_pos, extract)
      }, error = function(e){
        message("There was an error in extracting characterization motifs (be_7). Some characterization motifs might not have been extracted properly. This is an important rule and you probably shouldn't proceed.")
        being_entity <<- data.table(doc_id = character(), ann_id = factor(), Entity = character(), characterization = character())
      })

      ###############################################################################################
      if(fast){
        being_entity_c = data.table(doc_id = character(), ann_id = factor(), Entity = character(), characterization = character())
      } else {
        tryCatch({
          being_entity_c = be_8(tokens, entities, verb_pos, agent_patient_pos, extract)
        }, error = function(e){
          message("There was an error in extracting characterization motifs (be_8). Some characterization motifs might not have been extracted properly. This is a secondary rule and you can probably proceed.")
          being_entity_c <<- data.table(doc_id = character(), ann_id = factor(), Entity = character(), characterization = character())
        })
      }
    } else {
      being_entity =
        being_entity_c = data.table(doc_id = character(), ann_id = factor(), Entity = character(), characterization = character())
    }
  }


  ###############################################################################################
  ########################################Posessions#############################################
  ###############################################################################################
  if("H" %in% motif_classes){
    if(verbose){cat("Extracting possessions\n")}
    ###############################################################################################
    tryCatch({
      posessive_o = H_1(tokens, entities, verb_pos, agent_patient_pos, extract)
    }, error = function(e){
      message("There was an error in extracting posession motifs (H_1). Some posession motifs might not have been extracted properly. This is an important rule and you probably shouldn't proceed.")
      posessive_o <<- data.table(doc_id = character(), ann_id = factor(), Entity = character(), Possession = character())
    })
    tryCatch({
      have_nsubj_obj_conj_act = H_3(tokens, entities, verb_pos, agent_patient_pos, extract)
    }, error = function(e){
      message("There was an error in extracting possession motifs (H_3). Some possession motifs might not have been extracted properly. This is an important rule and you probably shouldn't proceed.")
      have_nsubj_obj_conj_act <<- data.table(doc_id = character(), ann_id = factor(), Entity = character(), Possession = character())
    })

    ###############################################################################################
    if(fast){
      posessive_of =
        have_nsubj_obj_conj =
        have_nsubj_conj_obj_act =
        have_nsubj_conj_subj_cons_obj =
        have_xcomp_act_obj =
        have_xcomp_act_obj_vconj =
        have_xcomp_act_obj_nconj =
        have_xcomp_act_obj_nconj_vconj = data.table(doc_id = character(), ann_id = factor(), Entity = character(), Possession = character())

    } else {
      tryCatch({
        posessive_of = H_2(tokens, entities, verb_pos, agent_patient_pos, extract)
      }, error = function(e){
        message("There was an error in extracting posession motifs (H_2). Some posession motifs might not have been extracted properly. This is a secondary rule and you can probably proceed.")
        posessive_of <<- data.table(doc_id = character(), ann_id = factor(), Entity = character(), Possession = character())
      })
      tryCatch({
        have_nsubj_obj_conj = H_4(tokens, entities, verb_pos, agent_patient_pos, extract)
      }, error = function(e){
        message("There was an error in extracting possession motifs (H_4). Some possession motifs might not have been extracted properly. This is a secondary rule and you can probably proceed.")
        have_nsubj_obj_conj <<- data.table(doc_id = character(), ann_id = factor(), Entity = character(), Possession = character())
      })
      tryCatch({
        have_nsubj_conj_obj_act = H_5(tokens, entities, verb_pos, agent_patient_pos, extract)
      }, error = function(e){
        message("There was an error in extracting possession motifs (H_5). Some possession motifs might not have been extracted properly. This is a secondary rule and you can probably proceed.")
        have_nsubj_conj_obj_act <<- data.table(doc_id = character(), ann_id = factor(), Entity = character(), Possession = character())
      })
      tryCatch({
        have_nsubj_conj_subj_cons_obj = H_6(tokens, entities, verb_pos, agent_patient_pos, extract)
      }, error = function(e){
        message("There was an error in extracting possession motifs (H_6). Some possession motifs might not have been extracted properly. This is a secondary rule and you can probably proceed.")
        have_nsubj_conj_subj_cons_obj <<- data.table(doc_id = character(), ann_id = factor(), Entity = character(), Possession = character())
      })
      tryCatch({
        have_xcomp_act_obj = H_7(tokens, entities, verb_pos, agent_patient_pos, extract)
      }, error = function(e){
        message("There was an error in extracting possession motifs (H_7). Some possession motifs might not have been extracted properly. This is a secondary rule and you can probably proceed.")
        have_xcomp_act_obj <<- data.table(doc_id = character(), ann_id = factor(), Entity = character(), Possession = character())
      })
      tryCatch({
        have_xcomp_act_obj_vconj = H_8(tokens, entities, verb_pos, agent_patient_pos, extract)
      }, error = function(e){
        message("There was an error in extracting possession motifs (H_8). Some possession motifs might not have been extracted properly. This is a secondary rule and you can probably proceed.")
        have_xcomp_act_obj_vconj <<- data.table(doc_id = character(), ann_id = factor(), Entity = character(), Possession = character())
      })
      tryCatch({
        have_xcomp_act_obj_nconj = H_9(tokens, entities, verb_pos, agent_patient_pos, extract)
      }, error = function(e){
        message("There was an error in extracting possession motifs (H_9). Some possession motifs might not have been extracted properly. This is a secondary rule and you can probably proceed.")
        have_xcomp_act_obj_nconj <<- data.table(doc_id = character(), ann_id = factor(), Entity = character(), Possession = character())
      })
      tryCatch({
        have_xcomp_act_obj_nconj_vconj = H_10(tokens, entities, verb_pos, agent_patient_pos, extract)
      }, error = function(e){
        message("There was an error in extracting possession motifs (H_10). Some possession motifs might not have been extracted properly. This is a secondary rule and you can probably proceed.")
        have_xcomp_act_obj_nconj_vconj <<- data.table(doc_id = character(), ann_id = factor(), Entity = character(), Possession = character())
      })
    }
  }


  ###############################################################################################
  ######################################Lowercase extractions####################################
  ###############################################################################################

  ###############################################################################################
  ##### Lowercase
  if(lowercase){
    tokens$token = tolower(tokens$token)
    tokens$lemma = tolower(tokens$lemma)
  }


  ###############################################################################################
  ###################################Combine the motifs to a list################################
  ###############################################################################################

  ###############################################################################################
  actions = if("a" %in% motif_classes){
    actions = rbind(nsubj_act_conj,
                             nsubj_act_noun_conj_verb_conj,
                             by_act,
                             by_act_noun_conjunct,
                             by_act_2,
                             by_act_2_1,
                             by_act_2_noun_conj,
                             by_act_2_noun_conj_1,
                             xcomp_act_conj_verb,
                             xcomp_act_conj_noun)
    if(lowercase){
      actions$Entity = tolower(actions$Entity)
      actions$action = tolower(actions$action)
    }

    if(nrow(actions)>0){
      actions$action = str_split(actions$action, " ")
      actions = with(actions,
                     {data.frame(lapply(actions[,c("doc_id", "ann_id", "Entity")], rep, times=lengths(action)),action=unlist(action))}
                     )
      } else {character(0)}
  } else {character(0)}
  if(markup & length(actions) > 0){actions$markup = paste0("a_", actions$action)}
  if(add_sentence & length(actions) > 0){
    actions$sentence = mapply(retrieve_sentence, actions$doc_id, str_extract(actions$ann_id, "(?<=[.])[0-9]+(?=.)"), MoreArgs = list(tok_obj = tokens))
  }


  ###############################################################################################
  treatments = if("t" %in% motif_classes){
    treatments = rbind(dobj_treat,
                       dobj_conj_treat,
                       obj_of_by_act,
                       obj_of_by_act_nconj
                       )
    if(lowercase){
      treatments$Entity = tolower(treatments$Entity)
      treatments$treatment = tolower(treatments$treatment)
    }

    if(nrow(treatments)>0){
      treatments$treatment = str_split(treatments$treatment, " ")
      treatments = with(treatments,
                     {data.frame(lapply(treatments[,c("doc_id", "ann_id", "Entity")], rep, times=lengths(treatment)),treatment=unlist(treatment))}
      )
    } else {character(0)}
  } else {character(0)}
  if(markup & length(treatments) > 0){treatments$markup = paste0("t_", treatments$treatment)}
  if(add_sentence & length(treatments) > 0){
    treatments$sentence = mapply(retrieve_sentence, treatments$doc_id, str_extract(treatments$ann_id, "(?<=[.])[0-9]+(?=.)"), MoreArgs = list(tok_obj = tokens))
  }

  ###############################################################################################
  characterizations = if("be" %in% motif_classes){
    characterizations = rbind(being_adj,
                              being_adj_nconj,
                              being_adj_vconj,
                              being_adj_xcomp,
                              appos_char,
                              amod_adj,
                              being_entity,
                              being_entity_c
                              )
    if(lowercase){
      characterizations$Entity = tolower(characterizations$Entity)
      characterizations$characterization = tolower(characterizations$characterization)
    }

    if(nrow(characterizations)>0){
      characterizations$characterization = str_split(characterizations$characterization, " ")
      characterizations = with(characterizations,
                        {data.frame(lapply(characterizations[,c("doc_id", "ann_id", "Entity")], rep, times=lengths(characterization)),characterization=unlist(characterization))}
      )
    } else {character(0)}
  } else {character(0)}
  if(markup & length(characterizations) > 0){characterizations$markup = paste0("be_", characterizations$characterization)}
  if(add_sentence & length(characterizations) > 0){
    characterizations$sentence = mapply(retrieve_sentence, characterizations$doc_id, str_extract(characterizations$ann_id, "(?<=[.])[0-9]+(?=.)"), MoreArgs = list(tok_obj = tokens))
  }

  ###############################################################################################
  possessions = if("H" %in% motif_classes){
    possessions = rbind(posessive_o,
                        posessive_of,
                        have_nsubj_obj_conj_act,
                        have_nsubj_obj_conj,
                        have_nsubj_conj_obj_act,
                        have_nsubj_conj_subj_cons_obj,
                        have_xcomp_act_obj,
                        have_xcomp_act_obj_vconj,
                        have_xcomp_act_obj_nconj,
                        have_xcomp_act_obj_nconj_vconj
    )
    if(lowercase){
      possessions$Entity = tolower(possessions$Entity)
      possessions$characterization = tolower(possessions$characterization)
    }

    if(nrow(possessions)>0){
      possessions$Possession = str_split(possessions$Possession, " ")
      possessions = with(possessions,
                               {data.frame(lapply(possessions[,c("doc_id", "ann_id", "Entity")], rep, times=lengths(Possession)),Possession=unlist(Possession))}
      )
    } else {character(0)}
  } else {character(0)}
  if(markup & length(possessions) > 0){possessions$markup = paste0("H_", possessions$Possession)}
  if(add_sentence & length(possessions) > 0){
    possessions$sentence = mapply(retrieve_sentence, possessions$doc_id, str_extract(possessions$ann_id, "(?<=[.])[0-9]+(?=.)"), MoreArgs = list(tok_obj = tokens))
  }

  ################################################################################################
  agent_treatments = if("At" %in% motif_classes){
    agent_treatments = rbind(dobj_treat_actor_At_casted,
                             dobj_treat_conj_actor_At_casted,
                             dobj_nconj_treat_At_casted,
                             by_act_agent_At_casted,
                             obj_of_by_act_nconj_ac_At_casted
    )
    if(lowercase){
      agent_treatments$Entity = tolower(agent_treatments$Entity)
      agent_treatments$treatment = tolower(agent_treatments$treatment)
      agent_treatments$Agent = tolower(agent_treatments$Agent)
    }

    if(nrow(agent_treatments)>0){
      agent_treatments$Agent = str_split(agent_treatments$Agent, " ")
      agent_treatments = with(agent_treatments,
                         {data.frame(lapply(agent_treatments[,c("doc_id", "ann_id", "treatment", "Entity")], rep, times=lengths(Agent)),Agent=unlist(Agent))}
      )
      agent_treatments$treatment = str_split(agent_treatments$treatment, " ")
      agent_treatments = with(agent_treatments,
                              {data.frame(lapply(agent_treatments[,c("doc_id", "ann_id", "Agent", "Entity")], rep, times=lengths(treatment)),treatment=unlist(treatment))}
      )
    } else {character(0)}
  } else {character(0)}
  if(length(agent_treatments)>0){
    agent_treatments = agent_treatments[,c("doc_id", "ann_id", "Entity", "Agent", "treatment")]
    if(markup){agent_treatments$markup = paste0("At_", agent_treatments$Agent, "_", agent_treatments$treatment)}
  }
  if(add_sentence & length(agent_treatments) > 0){
    agent_treatments$sentence = mapply(retrieve_sentence, agent_treatments$doc_id, str_extract(agent_treatments$ann_id, "(?<=[.])[0-9]+(?=.)"), MoreArgs = list(tok_obj = tokens))
  }

  ################################################################################################
  action_Patients = if("aP" %in% motif_classes){
    action_Patients = rbind(nsubj_obj_conj_act_aP_casted,
                            nsubj_obj_conj_aP_casted,
                            nsubj_conj_obj_act_aP_casted,
                            nsubj_conj_subj_cons_obj_aP_casted,
                            by_act_obj_aP_casted,
                            by_act_obj_nc_aP_casted,
                            by_act_obj_cverb_1_aP_casted,
                            by_act_obj_cverb_2_aP_casted,
                            by_act_obj_cverb_cobj_1_aP_casted,
                            by_act_obj_cverb_cobj_2_aP_casted,
                            xcomp_act_obj_aP_casted,
                            xcomp_act_obj_vconj_aP_casted,
                            xcomp_act_obj_nconj_aP_casted,
                            xcomp_act_obj_nconj_vconj_aP_casted)

    if(lowercase){
      action_Patients$Entity = tolower(action_Patients$Entity)
      action_Patients$action = tolower(action_Patients$action)
      action_Patients$Patient = tolower(action_Patients$Patient)
    }

    if(nrow(action_Patients)>0){
      action_Patients$Patient = str_split(action_Patients$Patient, " ")
      action_Patients = with(action_Patients,
                              {data.frame(lapply(action_Patients[,c("doc_id", "ann_id", "action", "Entity")], rep, times=lengths(Patient)), Patient = unlist(Patient))}
      )
      action_Patients$action = str_split(action_Patients$action, " ")
      action_Patients = with(action_Patients,
                              {data.frame(lapply(action_Patients[,c("doc_id", "ann_id", "Patient", "Entity")], rep, times=lengths(action)), action = unlist(action))}
      )
    } else {character(0)}
  } else {character(0)}
  if(length(action_Patients) >0){
    action_Patients = action_Patients[,c("doc_id", "ann_id", "Entity",  "action", "Patient")]
    if(markup){action_Patients$markup = paste0("aP_", action_Patients$action, "_", action_Patients$Patient)}
  }
  if(add_sentence & length(action_Patients) > 0){
    action_Patients$sentence = mapply(retrieve_sentence, action_Patients$doc_id, str_extract(action_Patients$ann_id, "(?<=[.])[0-9]+(?=.)"), MoreArgs = list(tok_obj = tokens))
  }

  ##### Combine to list object
  motif_list = list("actions" = actions,
                    "treatments" = treatments,
                    "characterizations" = characterizations,
                    "possessions" = possessions,
                    "agent_treatments" = agent_treatments,
                    "action_patients" = action_Patients)


  ###############################################################################################
  ###########################################Return##############################################
  ###############################################################################################
  return(motif_list)
}

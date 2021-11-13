# semgram: package for extracting semantically rich relations from text

This repository hosts the ```semgram``` R package.

`semgram` extracts semantic motifs from textual data. It uses an entity-centered semantic grammar that distinguishes six classes of motifs: actions of an entity, treatments of an entity, agents acting upon an entity, patients acted upon by an entity, characterizations of an entity, and possessions of an entity. `semgram` uses a comprehensive set of extraction rules to recover semantic motifs from dependency trees (the output of dependency parsers). In doing this, it builds on functionalities of [`spacyr`](https://cran.r-project.org/web/packages/spacyr/index.html) for dependency parsing and [`rsyntax`](https://github.com/vanatteveldt/rsyntax) for implementing rules querying dependency trees.

A short demo can be found [here](https://htmlpreview.github.io/?https://github.com/review-account/semgram/blob/master/semgram_demo.html).

## Installation

Assuming you have installed `devtools`, you can install the package by running the following code.

```R
devtools::install_github("review-account/semgram")
```

## Example

The first step in extracting semantinc motifs from text is to pass it through an annotation pipeline. You can do this by running `spacy_parse`.

```R
text = "Emil chased the thief."
tokens_df = spacy_parse(text, dependency=T, entity = F)
tokens_df

#>   doc_id sentence_id token_id  token lemma   pos head_token_id dep_rel
#> 1  text1           1        1   Emil  Emil PROPN             2   nsubj
#> 2  text1           1        2 chased chase  VERB             2    ROOT
#> 3  text1           1        3    the   the   DET             4     det
#> 4  text1           1        4  thief thief  NOUN             2    dobj
#> 5  text1           1        5      .     . PUNCT             2   punct

```

The working horse of `semgram` is the `extract_motifs` function to which we pass an annotated tokens object. We also have to specify in which entity we are interested (here "Emil"). By default, `extract_motifs` extracts motifs for all motif classes. 

In the example sentence, we find an action motif (a_chase), a patient motif (P_thief), as well as a composite action-Patient motif (aP_chase_thief). For some more functionalities, check out the [demo](https://htmlpreview.github.io/?https://github.com/review-account/semgram/blob/master/semgram_demo.html).

```R
extract_motifs(tokens = tokens_df, entities = c("Emil"))

#> List of 8
#>  $ acts             : chr "a_chase"
#>  $ patients         : chr "P_thief"
#>  $ treatments       : chr(0) 
#>  $ agents           : chr(0) 
#>  $ characterizations: chr(0) 
#>  $ possessions      : chr(0) 
#>  $ agent_treatments : chr(0) 
#>  $ action_Patients  : chr "aP_chase_thief"

```

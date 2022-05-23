# semgram: R package for extracting semantic motifs from text

This repository hosts the ```semgram``` R package.

`semgram` extracts semantic motifs from textual data. For details, please refer to this recent [paper](https://journals.sagepub.com/doi/full/10.1177/00491241221099551). `semgram` uses an entity-centered semantic grammar that distinguishes six classes of motifs: actions of an entity, treatments of an entity, agents acting upon an entity, patients acted upon by an entity, characterizations of an entity, and possessions of an entity. `semgram` uses a comprehensive set of extraction rules to recover semantic motifs from dependency trees (the output of dependency parsers). A short demo can be found [here](https://htmlpreview.github.io/?https://github.com/omstuhler/semgram/blob/master/semgram_demo.html).

`semgram` builds on functionalities of [`spacyr`](https://cran.r-project.org/web/packages/spacyr/index.html) for dependency parsing and [`rsyntax`](https://github.com/vanatteveldt/rsyntax) for implementing rules querying dependency trees. If you find yourself wanting to extract relations other than those incorporated in the `semgram` grammar and don't mind implementing the formal rules to do this from scratch, [`rsyntax`](https://github.com/vanatteveldt/rsyntax) is the way to go. You might also find their [`rsyntaxRecipes`](https://github.com/kasperwelbers/rsyntaxRecipes) useful.

If you use `semgram` in your research, please cite as follows:

> Stuhler, Oscar (2022). "Who does What to Whom? Making Text Parsers Work for Sociological Inquiry." *Sociological Methods & Research*. doi: 10.1177/00491241221099551

## Installation

Assuming you have installed `devtools`, you can install the package by running the following code.

```R
devtools::install_github("omstuhler/semgram")
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

In the example sentence, we find an action motif (a_chase) and well as a composite action-Patient motif (aP_chase_thief). For some more functionalities, check out the [demo](https://htmlpreview.github.io/?https://github.com/omstuhler/semgram/blob/master/semgram_demo.html).

```R
extract_motifs(tokens = tokens_df, entities = c("Emil"))

#> List of 8
#>  $actions   			
#>	doc_id	ann_id		Entity	action
#>	text1	text1.1.1  	Emil  	chase
#>  $treatments
#>	character(0)
#>  $characterizations
#>	character(0)
#>  $possessions
#>	character(0)
#>  $agent_treatments
#>	character(0)
#>  $action_Patients	
#>	doc_id	ann_id		Entity	action 	Patient
#>	text1 	text1.1.2	Emil  	chase   thief
```

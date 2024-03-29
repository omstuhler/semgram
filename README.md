# semgram: Extracting Semantic Motifs from Textual Data

`semgram` is an R package for extracting semantic motifs around entities in textual data. `semgram` uses an entity-centered semantic grammar that distinguishes six classes of motifs: actions of an entity, treatments of an entity, agents acting upon an entity, patients acted upon by an entity, characterizations of an entity, and possessions of an entity. `semgram` uses a comprehensive set of extraction rules to recover semantic motifs from dependency trees (the output of dependency parsers). For details, please refer to [this recent paper](https://journals.sagepub.com/doi/full/10.1177/00491241221099551). A demo can be found [here](https://htmlpreview.github.io/?https://github.com/omstuhler/semgram/blob/master/vignettes/demo.html).

`semgram` uses text objects with part-of-speech and dependency annotations. You can generate these in R by using the [`spacyr`](https://CRAN.R-project.org/package=spacyr) package. Furthermore, `semgram` builds on [`rsyntax`](https://CRAN.R-project.org/package=rsyntax) for implementing rules querying dependency trees. If you find yourself wanting to extract relations other than those incorporated in the `semgram` grammar and don't mind implementing the formal rules to do this from scratch, [`rsyntax`](https://github.com/vanatteveldt/rsyntax) is the way to go.

If you use `semgram` in your research, please cite it as follows:

> Stuhler, Oscar (2022). "Who does What to Whom? Making Text Parsers Work for Sociological Inquiry." *Sociological Methods & Research*. [doi: 10.1177/00491241221099551.](https://journals.sagepub.com/doi/full/10.1177/00491241221099551)

## Installation

If you want to install the development version, you will need `devtools`.

```R
# Install from CRAN (currently 0.1.0)
install.packages("semgram")

# Or, to install the development version (currently 0.1.1, for differences, see NEWS.md)
devtools::install_github("omstuhler/semgram")
```

## Example

The first step in extracting semantic motifs from text is to pass it through an annotation pipeline. You can do this by running `spacyr::spacy_parse()`. Make sure you annotate dependencies.

```R
text = "Emil chased the thief."
tokens_df = spacyr::spacy_parse(text, dependency = T)
tokens_df

#>   doc_id sentence_id token_id  token lemma   pos head_token_id dep_rel
#> 1  text1           1        1   Emil  Emil PROPN             2   nsubj
#> 2  text1           1        2 chased chase  VERB             2    ROOT
#> 3  text1           1        3    the   the   DET             4     det
#> 4  text1           1        4  thief thief  NOUN             2    dobj
#> 5  text1           1        5      .     . PUNCT             2   punct
```

The working horse of `semgram` is the `extract_motifs` function to which we pass an annotated tokens object. We can also specify in which entity we are interested (here "Emil"). By default, `extract_motifs` extracts motifs for all motif classes (actions, patients, treatments, etc.).

In the example sentence, we find an action motif (a_chase) as well as a composite action-Patient motif (aP_chase_thief). For some more functionalities, [check out the demo](https://htmlpreview.github.io/?https://github.com/omstuhler/semgram/blob/master/vignettes/demo.html).

```R
extract_motifs(tokens = tokens_df, entities = c("Emil"), markup = T)

#> List of 8
#>  $actions   			
#>	doc_id	ann_id		Entity	action  markup
#>	text1	text1.1.1  	Emil  	chase   a_chase
#>  $treatments
#>	character(0)
#>  $characterizations
#>	character(0)
#>  $possessions
#>	character(0)
#>  $agent_treatments
#>	character(0)
#>  $action_patients	
#>	doc_id	ann_id		Entity	action 	Patient markup
#>	text1 	text1.1.2	Emil  	chase   thief   aP_chase_thief
```

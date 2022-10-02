# semgram 0.1.1

## Improvements
* Verbs followed by a preposition can now be be considered as compositional actions. Objects linked to verbs via a preposition can be considered as actions. This considerably increases the number of extracted patients. For details, consider the documentation for the `verb_prep` and `verb_prep_greedy` parameters in the `extract_motifs()` function.
* When using `extract_motifs()` the `ann_id` in the returned data.frames should now realiably link to the _entity_ associated with the respective motif.
* Fixed some typos in documentation, shortened README.md.
* When using `extract_motifs()`, you can now add a pseudo paragraph to each motif extraction. This makes it possible to better examine the context of a motif and may be especially useful for research integrating formal and qualitative modes of reading.

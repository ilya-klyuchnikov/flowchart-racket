(Original: http://people.cis.ksu.edu/~hatcliff/FPEPS/)

An implementation in Racket of a simple evaluator, online partial evaluator, and offline partial evaluator for the language FCL (flowchart language).

This implementation is based on the notes "An introduction to partial evaluation using a simple flowchart language" from the 1998 DIKU Summer School on Partial Evaluation.

Files:

* `parse.rkt` – implements parsing (from s-expression), unparsing (to s-expression), and a procedure to collect all variables appearing in an FCL program.
* `util.rkt` – evaluation of quoted expressions, operations, utilities for hashes, lists as sets, pending lists, 
* `eval.rkt`  – the main loop of interpreter interpreter (looping over blocks till return)
* `online.rkt` – the online PE (refactored into racket)
* `offline.rkt` – the offline PE
* `mix.rkt` - I do not remember what it does :)


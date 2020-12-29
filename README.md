## Flowchart in Racket [![Build Status](https://travis-ci.org/ilya-klyuchnikov/flowchart-racket.png)](https://travis-ci.org/ilya-klyuchnikov/flowchart-racket)

(Original: http://people.cis.ksu.edu/~hatcliff/FPEPS/)

An implementation in Racket of a simple evaluator, online partial evaluator, and offline partial evaluator for the language FCL (flowchart language).

This implementation is based on the notes "An introduction to partial evaluation using a simple flowchart language" from the 1998 DIKU Summer School on Partial Evaluation.

Below are some outdated notes.

## Files

* `parse.rkt` – implements parsing (from s-expression), unparsing (to s-expression), and a procedure to collect all variables appearing in an FCL program.
* `util.rkt` – evaluation of quoted expressions, operations, utilities for hashes, lists as sets, pending lists,
* `eval.rkt`  – the main loop of interpreter interpreter (looping over blocks till return)
* `online.rkt` – the online PE (refactored into racket)
* `offline.rkt` – the offline PE.
* `mix.rkt` -

## Notes

There are several differences between partial evaluation and supercompilation:

1. Partial evaluation doesn't perform full case analysis.
2. Partial evaluation performs very limited case analysis - unfolding of `if` construct. But it doesn't try to limit d-vars based on this. It will proceed both branches with the same store! (So, it lacks positive information propagation)

## Partial evaluation for flowchart language in a nutshell

Each block in residual program correspond to a pair

`(label, sd-state)`

where sd-state is a map of var to values. value is either (S const) or D.

Why is the difference in online/mix-jump with labels??

### Termination

In general case both `online` and `mix` do not terminate.

When do they terminate? They terminate if input program (on supplied data) terminate!

BTW, this is an illustration of difference between PE and SC. - If input program terminate, then PE in a degenerative form has no problem, but SC even in this case needs a whistle.

# TODO

1. Examples for non-termination of `mix` and `online`.
2. Examples for termination of PE in degenerative form and non-termination of SC without a whistle. Hint: in the program it should be a decomposition of a dynamic data.

# Self-interpreter for Flowchart.

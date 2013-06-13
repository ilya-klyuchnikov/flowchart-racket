Original: http://people.cis.ksu.edu/~hatcliff/FPEPS/


This directory contains files that implement in SCM Scheme, a simple
evaluator, online partial evaluator, and offline partial evaluator for
the language FCL.

The implementations are based on the notes "An introduction to
partial evaluation using a simple flowchart language" from
the 1998 DIKU Summer School on Partial Evaluation.

Files:

  parse.scm -- implements parsing, unparsing, and a procedure to
               collect all variables appearing in an FCL program.
  eval.scm  -- the interpreter
  online.scm -- the online PE
  offline.scm -- the offline PE
  lib-fcl-shared.scm -- help procedures used by evaluator and PE's such as
           system constants (initial store value, etc.),
           definitions of state data structures
           procedures for evaluation of primitives
           procedures that simply getting and putting objects to a file
           common list functions (e.g., foldr and friends)
           static/dynamic tags for PE's
           label management for residual programs
  lib-table.scm -- implements a table ADT used to represent stores,
           divisions, blockmaps, etc.
  lib-set.scm   -- implements a set ADT used for the seen set.
  lib-pending.scm -- implements the pending list

Subdirectories
  examples -- contains examples from the text and others used
           for debugging.
  outputs  -- use this for the results of specialization


Here are some examples for running the systems.  The transcript
below starts from the UNIX prompt.  However, I recommend that
you use EMACS and run SCM inside a shell in EMACS.  See the
web page for how to set that up.


Evaluation:
-------------

-----

% scm
SCM version 4e6 (speedy), Copyright (C) 1990, 1991, 1992, 1993, 1994, 1995, 1996 Free Software Foundation.
SCM comes with ABSOLUTELY NO WARRANTY; for details type `(terms)'.
This is free software, and you are welcome to redistribute it
under certain conditions; type `(terms)' for details.
;loading /usr/lib/slib/require
;done loading /usr/lib/slib/require.scm
;loading /usr/lib/slib/Link
;done loading /usr/lib/slib/Link.scm
;loading /usr/lib/slib/Transcen.scm
;done loading /usr/lib/slib/Transcen.scm
;Evaluation took 60 mSec (0 in gc) 13225 cells work, 16816 bytes other

> (load "eval.scm")
;loading eval.scm
   
   ...[snip, snip]...

;Evaluation took 70 mSec (10 in gc) 13846 cells work, 12762 bytes other
#<unspecified>

> (eval-fcl-file "examples/power.fcl" '(5 2))
;Evaluation took 10 mSec (0 in gc) 5920 cells work, 1142 bytes other
25
-----

Online PE:
-------------

-----
> (load "online.scm")
> (online-file "examples/power.fcl" '(n) '(2) "outputs/power-2.fcl")w
((m)
 (init-0)
 ((init-0 () (goto test-1))
  (test-1 () (goto loop-2))
  (loop-2 ((result := (* 1 m))) (goto test-3))
  (test-3 () (goto loop-4))
  (loop-4 ((result := (* result m))) (goto test-5))
  (test-5 () (goto end-6))
  (end-6 () (return result))))
;Evaluation took 40 mSec (20 in gc) 20787 cells work, 2195 bytes other
-----

The user supplies a list of the static parameter names (e.g., '(n)) as
well as the static values (e.g., '(2)).  This will specialize the
power program to the exponent value 2.  Here the residual program is
displayed, plus it is written to the file "outputs/power-2.fcl".

Now we can run the residual program.

-----
> (load "eval.scm")
> (eval-fcl-file "outputs/power-2.fcl" '(5))
;Evaluation took 20 mSec (10 in gc) 5354 cells work, 1153 bytes other
25
-----

Offline PE
-------------

This works the same as online.

-----
> (load "offline.scm")
> (offline-file "examples/power.fcl" '(n) '(2) "outputs/power-2.fcl")

  ....
-----

etc.

There are a couple of extra procedures in the offline case that
allow you to see the intermediate results of division and
annotated program.

The procedures take a parsed program as input, so we have to
fetch the program and parse it manually.

-------
> (define power (get-file-object "examples/power.fcl"))
;Evaluation took 0 mSec (0 in gc) 78 cells work, 87 bytes other
#<unspecified>

> (offline-debug-div (parse power) '(n))
((m . #(dynamic ()))
 (n . #(static ()))
 (result . #(dynamic ())))
;Evaluation took 0 mSec (0 in gc) 5071 cells work, 613 bytes other
27

>  (offline-debug-ann (parse power) '(n))
#(program
  (m n)
  init
  (#(block
     init
     (#(d-assign result #(d-const 1)))
     #(d-goto test))
   #(block
     test
     ()
     #(if #(app < (#(varref n) #(const 1))) end loop))
   #(block
     loop
     (#(d-assign
        result
        #(d-app * (#(varref result) #(varref m))))
      #(assign n #(app - (#(varref n) #(const 1)))))
     #(d-goto test))
   #(block end () #(d-return #(varref result)))))
;Evaluation took 10 mSec (0 in gc) 15085 cells work, 1751 bytes other
50
------

get-file-object is defined in lib-fcl-shared.scm.  It just
reads a single object from a file.


For each of the top-level calls above, there are versions that
do not read from files (e.g., eval-fcl, online, offline).

**** Note: **** Finally, when switching between eval, online, and
offline, you should always reload the files from disk to be safe ---
some procedure names are not unique (I haven't been able to track
these down yet).

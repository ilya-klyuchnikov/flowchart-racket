#lang racket/base

(require racket/file)
(require rackunit)

(require "eval.rkt")

(check-equal? (eval-fcl-file "examples/power.fcl" '(5 2)) 25)

(check-equal?
 (eval-fcl-file "examples/int.fcl" (list (file->value "examples/power.fcl") '(5 2)))
 25)

(define Q
  '((0 if  0 goto 4)
    (1 if -1 goto 4)
    (2 right)
    (3 goto 0)
    (4 write 1)))

(define Right '(0 0))

(check-equal?
 (eval-fcl-file "examples/turing.rkt" (list Q Right))
 '(1 0))

; this doesn't work yet since turing has non-flat expressions
#;(check-equal?
 (eval-fcl-file "examples/int.rkt" (list (file->value "examples/turing.rkt") (list Q Right)))
 25)

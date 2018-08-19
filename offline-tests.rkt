#lang racket/base

(require racket/file)
(require racket/pretty)
(require rackunit)

(require "parse.rkt")
(require "eval.rkt")
(require "offline.rkt")

(define power-s0 (file->value "examples/power.fcl"))
(define power (parse-program power-s0))

(define power-s1 (unparse-program (offline-prog power '(n) '(1))))
(define power-s2 (unparse-program (offline-prog power '(n) '(2))))

(check-equal? (eval-fcl power-s1 '(5)) 5)
(check-equal? (eval-fcl (unparse-program (offline-prog power '(n) '(2))) '(5)) 25)

(define turing-s0 (file->value "examples/turing.rkt"))
(define turing (parse-program turing-s0))

(define Q1
  '((0 if  0 goto 4)
    (1 if -1 goto 4)
    (2 right)
    (3 goto 0)
    (4 write 1)))

(define Q2
  '((0 write 1)))

(define turing-s1 (unparse-program (offline-prog turing '(Q) (list Q1))))
(check-equal? (eval-fcl turing-s1 (list '(0 0))) '(1 0))

(display "\n===OFFLINE===\n")
(display "\noriginal power of 2 arguments:\n")
(pretty-display power-s0)
(display "\nspecialized power for n=1:\n")
(pretty-display power-s1)
(display "\nspecialized power for n=2:\n")
(pretty-display power-s2)

(display "\noriginal turing of 2 arguments:\n")
(pretty-display turing-s0)
(display "\nspecialized turing for program Q=Q2:\n")
(pretty-display turing-s1)

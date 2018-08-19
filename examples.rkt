#lang racket/base

;; I do not remember how this examples appeared

(require racket/file)
(require "parse.rkt" "eval.rkt" "online.rkt" "mix.rkt")

(define t-prog (file->value "examples/turing.rkt"))

(define Q1
  '((0 if  0 goto 4)
    (1 if -1 goto 4)
    (2 right)
    (3 goto 0)
    (4 write 1)))

(define right1
  '(1 1 1 1 1))

(define right2
  '(0 0 0 0 0))

(eval-fcl t-prog (list Q1 right1))
(eval-fcl t-prog (list Q1 right2))

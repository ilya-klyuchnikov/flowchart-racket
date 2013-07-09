#lang racket

(require "parse.rkt" "eval.rkt" "online.rkt")

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

(online-file "examples/power.fcl"
                 '(n)
                 '(2)
                 "outputs/power-2.fcl")
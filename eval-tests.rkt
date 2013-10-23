#lang racket

(require "eval.rkt")
(require rackunit)

(check-equal? (eval-fcl-file "examples/power.fcl" '(5 2)) 25)
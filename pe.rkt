#lang racket

(provide (struct-out static) 
         (struct-out dynamic) 
         state->label state->label-reset)

(require "util.rkt")
(require "eval.rkt")

(struct static  (obj) #:transparent)
(struct dynamic (obj) #:transparent)

(define-values (s->l i) (values (make-hash) 0))
(define (gen base) 
  (set! i (add1 i)) (string->symbol (string-append (~a base) "-" (~a i))))
(define (state->label-reset) (set!-values (s->l i) (values (make-hash) 0)))
(define (state->label s)
  (unless (hash-has-key? s->l s) (hash-set! s->l s (gen (state-label s))))
  (hash-ref s->l s))

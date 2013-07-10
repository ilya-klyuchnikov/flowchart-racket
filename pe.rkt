#lang racket

(provide (struct-out static) 
         (struct-out dynamic) 
         pe-val->object state->label state->label-reset)

(require "eval.rkt")

(struct static  (obj) #:transparent)
(struct dynamic (obj) #:transparent)

(define (pe-val->object pe-val)
  (match pe-val
    [(static obj) obj]
    [(dynamic obj) obj]))

(define-values (s->l i) (values (make-hash) 0))
(define (gen base) 
  (set! i (add1 i)) (string->symbol (string-append (~a base) "-" (~a i))))
(define (state->label-reset) (set! s->l (make-hash)))
(define (state->label s)
  (unless (hash-has-key? s->l s) (hash-set! s->l s (gen (state-label s))))
  (hash-ref s->l s))

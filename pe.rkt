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

;; utilities for managing state->label map
(define (make-label state i)
  (string->symbol (string-append (~a (state-label state)) "-" (~a i))))
(define s->l (make-hash)) ;mapping
(define (state->label-reset) (set! s->l (make-hash)))
(define (state->label s)
  (unless (hash-has-key? s->l s) 
    (hash-set! s->l s (make-label s (hash-count s->l))))
  (hash-ref s->l s))
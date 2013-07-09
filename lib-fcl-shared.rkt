#lang racket

(provide (all-defined-out))
(define init-store-val 0)                 

;;
;; Block name for extra block added in offline PE when some of the
;; static parameters become dynamic when computing division.
;;
(define sys-lift-block-label 'SYS-lift-block)

;;-----------------------------
;;
;; State structure
;;
;;-----------------------------

(struct state (label store) #:transparent)
(struct halt (value) #:transparent)

(define (halt-state? state)
  (halt? (state-label state)))


;;-----------------------------
;;
;; Evaluation of primitives
;;
;;-----------------------------

;;(op args) -> value 
(define eval-op
  (lambda (op vals)
    (case op
      ((+) (+ (car vals)
              (cadr vals)))
      ((-) (- (car vals)
              (cadr vals)))
      ((*) (* (car vals)
              (cadr vals)))
      ((/) (quotient (car vals)
                     (cadr vals)))
      ((%) (modulo    (car vals)
                      (cadr vals)))
      ((=) (equal? (car vals) (cadr vals)))
      ((<) (< (car vals) (cadr vals)))
      ((>) (> (car vals) (cadr vals)))
      ;;
      ;; list stuff
      ;;
      ((car) (car (car vals)))
      ((cdr) (cdr (car vals)))
      ((cons) (cons (car vals)
                    (cadr vals)))
      ((null) '())
      ((list) vals)
      ((null?) (null? (car vals)))
      ((pair?) (pair? (car vals)))
      ((list?) (pair? (car vals)))
      ((hd) (match (car vals) [(cons x _) x] [_ -1]) )
      ((tl) (match (car vals) [(cons _ x) x] [_ '()]) )
      ;;
      ;; misc
      ;;
      ((test) (if (car vals)
                  (cadr vals)
                  (caddr vals)))
      (else (error "Undefined operation:" op)))))

;;-----------------------------
;;
;; PE tags
;;
;;   Note: we will use these tags in several different ways.
;;   So the field name for the tagged object is simply 'obj'
;;-----------------------------
(struct static (obj) #:transparent)
(struct dynamic (obj) #:transparent)

(define pe-val->object
  (lambda (pe-val)
    (match pe-val
      [(static obj) obj]
      [(dynamic obj) obj])))

(define (pretty-print->file path obj)
  (call-with-output-file path 
    (λ (out) (pretty-print obj out)) 
    #:exists 'replace))

;; utilities for managing state->label map
(define (make-label state i)
  (string->symbol (string-append (~a (state-label state)) "-" (~a i))))
(define s->l (make-hash)) ;mapping
(define (state->label-reset) (set! s->l (make-hash)))
(define (state->label s)
  (unless (hash-has-key? s->l s) 
    (hash-set! s->l s (make-label s (hash-count s->l))))
  (hash-ref s->l s))

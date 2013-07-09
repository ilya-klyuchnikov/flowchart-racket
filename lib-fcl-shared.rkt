#lang racket
;;====================================================================
;;
;; File: lib-fcl-shared.scm
;; Author: John Hatcliff and Shawn Laubach
;; Last modified: June 28, 1998
;;
;; Contains procedures that are shared between evaluator and
;; partial evaluators, e.g.,
;;
;;   - system constants (initial store value, etc.)
;;   - definitions of state data structures
;;   - procedures for evaluation of primitives
;;   - procedures that simply getting and putting objects to a file
;;   - common list functions (e.g., foldr and friends)
;;   - static/dynamic tags for PE's
;;   - label management for residual programs
;;
;;====================================================================

(provide (all-defined-out))
;;-----------------------------
;;
;; System constants
;;
;;-----------------------------

;;
;; Initial store value
;;
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

(define halt-state?
  (lambda (state)
    (halt? (state-label state))))


;;-----------------------------
;;
;; Evaluation of primitives
;;
;;-----------------------------

;;(op x args) -> value 
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
      ((=) (make-FCL-boolean (equal? (car vals)
                                     (cadr vals))))
      ((<) (make-FCL-boolean (< (car vals) (cadr vals))))
      ((>) (make-FCL-boolean (> (car vals) (cadr vals))))
      ;;
      ;; list stuff
      ;;
      ((car) (car (car vals)))
      ((cdr) (cdr (car vals)))
      ((cons) (cons (car vals)
                    (cadr vals)))
      ((null) '())
      ((list) vals)
      ((null?) (make-FCL-boolean (null? (car vals))))
      ((pair?) (make-FCL-boolean (pair? (car vals))))
      ((list?) (make-FCL-boolean (pair? (car vals))))
      ((hd) (match (car vals) [(cons x _) x] [_ -1]) )
      ((tl) (match (car vals) [(cons _ x) x] [_ '()]) )
      ;;
      ;; misc
      ;;
      ((test) (if (is-true? (car vals))
                  (cadr vals)
                  (caddr vals)))
      (else (error "Undefined operation:" op)))))

;;
;; FCL booleans:
;;
;;   true  = non-zero
;;   false = 0
;;

;; converts Scheme boolean to FCL boolean
(define make-FCL-boolean
  (lambda (boolean)
    (if boolean
        1
        0)))

;; value -> #t | #f
(define is-true?
  (lambda (value)
    (not (equal? value 0))))

;; value -> #t | #f
(define is-false?
  (lambda (value)
    (equal? value 0)))

;;-----------------------------
;;
;; List functions
;;
;;-----------------------------

(define foldr
  (lambda (b f)
    (letrec ((loop (lambda (l)
                     (if (null? l)
                         b
                         (f (car l)
                            (loop (cdr l)))))))
      loop)))

(define foldl
  (lambda (b f)
    (letrec ((loop (lambda (l acc)
                     (if (null? l)
                         acc
                         (loop (cdr l)
                               (f (car l) acc))))))
      (lambda (l) (loop l b)))))




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
      [(dynamic obj) obj]
      [else (error "Unrecognized pe-val in pe-val->object: " pe-val)])))


;;-----------------------------
;;
;; File I/O
;;
;;   Simply procedures to get and put a single object to a file
;;
;;-----------------------------

(define put-file-object
  (lambda (out-file-name obj)
    (let ((out    (open-output-file out-file-name)))
      (begin
        (pretty-print obj out)
        (close-output-port out)))))


;; utilities for managing state->label map
(define (make-label state i)
  (string->symbol (string-append (~a (state-label state)) "-" (~a i))))
(define s->l (make-hash)) ;mapping
(define (state->label-reset) (set! s->l (make-hash)))
(define (state->label s)
  (unless (hash-has-key? s->l s) 
    (hash-set! s->l s (make-label s (hash-count s->l))))
  (hash-ref s->l s))

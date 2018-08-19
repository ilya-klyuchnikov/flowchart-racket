#lang racket/base

(require racket/list)
(require racket/match)
(require racket/pretty)

(provide (all-defined-out))

;------------------------------------------------
; State
;------------------------------------------------
(struct state (label store) #:transparent)
(struct halt (value) #:transparent)
(define (halt-state? state) (halt? (state-label state)))
(define init-val null)

;------------------------------------------------
; Hash utils
;------------------------------------------------

;construct hash from keys and vals
(define (hash-kv keys vals)
  (hash-set-kv* (hash) keys vals))

;extend hash with keys and vals
(define (hash-set-kv* h keys vals)
  (define args (append* (for/list ([k keys] [v vals]) (list k v))))
  (apply hash-set* (cons h args)))

;init hash with keys bound to the same val
(define (hash-init keys val)
  (for/hash ([k keys]) (values k val)))

;filter hash by values
(define (hash-filter-by-val h pred?)
  (for/hash ([(k v) h] #:when (pred? v)) (values k v)))

;------------------------------------------------
;lists as with sets
;------------------------------------------------

(define (add-set element set)
  (union (list element) set))

(define (union set1 set2)
  (remove-duplicates (append set1 set2)))

(define (intersect set1 set2)
  (for/list ([i set1] #:unless (member i set2)) i))

;------------------------------------------------
;pending list
;------------------------------------------------

(define (add-pending-depth-first state pending)
  (append (list state) pending))

(define (add-pending-depth-first* states pending)
  (append states pending))

(define (add-pending-breath-first state pending)
  (append pending (list state)))

(define (add-pending-breath-first* state pending)
  (append pending state))

; redefine these to switch between depth/breadth first
(define add-pending add-pending-depth-first)
(define add-pending* add-pending-depth-first*)


;------------------------------------------------
; Evaluation of static expressions
; functions above become available in FCL
;------------------------------------------------
(define (hd l) (if (empty? l) -1 (first l)))
(define (tl l) (if (empty? l) '() (rest l)))

;---------------------------------------
; utility functions for self-interpreter
;----------------------------------------
(define (mk-blockmap blocks)
  (hash-kv (map first blocks) (map rest blocks)))

(define (prg-init-label program)
  (first (second program)))

(define (prg-blocks prg)
  (first (rest (rest prg))))

(define (prg-blockmap prg)
  (mk-blockmap (prg-blocks prg)))

(define-namespace-anchor a1)
(define ns1 (namespace-anchor->namespace a1))

(define (eval-flat-arg arg store)
  (match arg
    [(list 'quote e) e]
    [v (if (number? v) v (hash-ref store v))]))

(define (f1 x y) 1)

(define (eval-flat-exp exp store)
  (match exp
    [(list 'quote e) e]
    [(cons op args) (apply (eval op ns1) (map (λ (v) (eval-flat-arg v store)) args))]
    [(and v (? symbol? v)) (hash-ref store v)]
    [(and n (? number? n)) n]))

(define-namespace-anchor a)
(define ns (namespace-anchor->namespace a))

(define (eval-op op args)
  (apply (eval op ns) args))

;------------------------------------------------
;pretty-printing to file
;------------------------------------------------

(define (pretty-print->file path obj)
  (call-with-output-file path
    (λ (out) (pretty-print obj out))
    #:exists 'replace))

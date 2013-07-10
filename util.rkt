#lang racket

(provide (all-defined-out))

;------------------------------------------------
;hash utils
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
;pretty-printing to file
;------------------------------------------------

(define (pretty-print->file path obj)
  (call-with-output-file path 
    (λ (out) (pretty-print obj out)) 
    #:exists 'replace))

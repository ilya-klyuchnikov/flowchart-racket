#lang racket

;; different utils
(provide
  hash-set-kv*
  hash-init
  hash-filter-by-val
  hash-kv
  pretty-print->file)

;internal utilities
(define (interleave xs ys)
  (append* (for/list ([x xs] [y ys]) (list x y))))

(define (pair->list p) (list (car p) (cdr p)))
(define (flat-pairs ls) (apply append (map pair->list ls)))

;hash utilities
;construct hash from keys and vals
(define (hash-kv keys vals)
  (apply hash (interleave keys vals)))

;extend hash with keys and vals
(define (hash-set-kv* h keys vals)
  (apply hash-set* (cons h (interleave keys vals))))

;init hash with keys bound to the same val
(define (hash-init keys val)
  (apply hash (append-map (λ (x) (list x val)) keys)))

(define (hash-filter-by-val h pred?)
  (apply hash (flat-pairs (filter (λ (p) (pred? (cdr p))) (hash->list h)))))

(define (pretty-print->file path obj)
  (call-with-output-file path 
    (λ (out) (pretty-print obj out)) 
    #:exists 'replace))

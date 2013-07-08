#lang racket

(provide hash-set-kv* hash-init restrict-table hash-kv)

; utils
(define (interleave xs ys) 
  (append* (for/list ([x xs] [y ys]) (list x y))))

(define (pair->list p) (list (car p) (cdr p)))
(define (flat-pairs ls) (apply append (map pair->list ls)))

(define (hash-kv keys vals) 
  (apply hash (interleave keys vals)))

(define (hash-set-kv* table keys vals)
  (apply hash-set* (cons table (interleave keys vals))))

(define (hash-init keys value)
  (apply hash (append-map (λ (x) (list x value)) keys)))

(define (restrict-table pred? table)
  (apply hash (flat-pairs (filter (λ (p) (pred? (cdr p))) (hash->list table)))))

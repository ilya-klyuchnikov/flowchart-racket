#lang racket

(provide update-table* initialize-table restrict-table)

; utils
(define (interleave xs ys) 
  (append* (for/list ([x xs] [y ys]) (list x y))))

(define (pair->list p) (list (car p) (cdr p)))
(define (flat-pairs ls) (apply append (map pair->list ls)))

(define (update-table* keys vals table)
  (apply hash-set* (cons table (interleave keys vals))))

(define (initialize-table keys value)
  (apply hash (append-map (λ (x) (list x value)) keys)))

(define (restrict-table pred? table)
  (apply hash (flat-pairs (filter (λ (p) (pred? (cdr p))) (hash->list table)))))

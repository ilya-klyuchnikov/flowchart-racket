#lang racket

(provide eval-debug online-debug)
(require "parse.rkt")

(define eval-debug-level 0)

(define (eval-debug state)
  (when (> eval-debug-level 0)
    (begin
      (display "-------------------------------\n")
      (newline)
      (display "LABEL: ")
      (pretty-print (state-label state))
      (newline)
      (display "STORE: ")
      (newline)
      (pretty-print (state-store state))
      (newline))))

(define online-debug-level 0)
;; 0 = no debug info
;; > 0 = print out contents of state on each transition
;; > 1 = print out above plus pending and seen
;; > 2 = print out above plus residual blocks
;; > 3 = print out above plus label management table
(define online-debug
  (lambda (state pending seen res-blocks)
    (if (> online-debug-level 0)
        (begin
          (display "-------------------------------")
          (newline)
          (display "LABEL: ")
          (pretty-print (state-label state))
          (newline)
          (display "STORE: ")
          (pretty-print (state-store state))
          (newline)
          (if (> online-debug-level 1)
              (begin
                (display "...............")
                (newline)
                (display "Pending: ")
                (newline)
                (pretty-print pending)
                (newline)
                (newline)
                (display "...............")
                (newline)
                (display "Seen: ")
                (newline)
                (pretty-print seen)
                (newline)
                (if (> online-debug-level 2)
                    (begin
                      (display "...............")
                      (newline)
                      (display "Residual blocks: ")
                      (newline)
                      (pretty-print (reverse res-blocks))
                      (newline)
                      (newline))
                    '()))
              '()))
        '())))


#lang racket
;;====================================================================
;;
;; File: lib-pending.scm
;; Author: John Hatcliff and Shawn Laubach
;; Last modified: June 28, 1998
;;
;;
;;  Implements the pending set used by both the online and offline
;;  partial evaluators.  We use an ADT for the pending so that
;;  we can switch between depth-first and bread-first specialization
;;  without modifying the specializer.
;;
;;  The pending set is implemented using a list.  For depth-first
;;  specialization, items are retrieved in a LIFO (stack-like)
;;  manner.  For breadth-first, it is FIFO (queue-like).  Modify
;;  the add-pending procedures below to switch between LIFO and FIFO.
;;
;;====================================================================

(provide (all-defined-out))

;; pending
(define empty-pending '())

;; pending -> #t | #f
(define empty-pending? null?)

;; pending -> state x pending
(define remove-pending
  (lambda (pending)
    (if (null? pending)
        (error "Empty pending list!")
        pending)))
;; 'car' will give next state,
;; 'cdr' will give the rest of pending

;; state x pending -> pending
(define add-pending-depth-first
  (lambda (state pending)
    (cons state pending)))

;; states x pending -> pending
(define add-pending-depth-first*
  (lambda (states pending)
    (append states pending)))

;; state x pending -> pending
(define add-pending-breath-first
  (lambda (state pending)
    (append pending (list state))))

;; states x pending -> pending
(define add-pending-breath-first*
  (lambda (state pending)
    (append pending state)))

;;
;; redefine these to switch between depth/breadth first
;;

;; state x pending -> pending
(define add-pending add-pending-depth-first)

;; states x pending -> pending
(define add-pending* add-pending-depth-first*)


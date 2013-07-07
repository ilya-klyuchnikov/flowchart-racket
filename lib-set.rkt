#lang racket
;;====================================================================
;;
;; File:   lib-set.scm
;; Author: John Hatcliff and Shawn Laubach
;; Last modified: June 28, 1998
;;
;;
;;  Defines some set functions that are used to implement the seen
;;  set.  Sets are implemented as lists.  Other operations outside
;;  this file make use of the fact that lists are implemented as
;;  sets.... so this implementation is not completely hidden.
;;
;;====================================================================
(provide (all-defined-out))

(define empty-set '
  ())

(define (empty-set? set)  
  (equal? set empty-set))

(define singleton-set 
  list)

(define (in-set? element set) 
  (member element set))

(define (add-set element set)
  (union-set (singleton-set element) set))

(define (union-set set1 set2)
    (remove-duplicates (append set1 set2)))

(define union*-set
  (lambda (set-list)
    (if (null? set-list)
        '()
        (union-set (car set-list) (union*-set (cdr set-list))))))

(define (diff-set s1 s2) (remove* s2 s1))

(define inter-set
  (lambda (set1 set2)
    (diff-set set2 (diff-set set2 set1))))

(define inter*-set
  (lambda (set-list)
    (if (null? set-list)
        '()
        (if (equal? (length set-list) 1)
            (car set-list)
            (inter-set (car set-list) (inter*-set (cdr set-list)))))))

(define map-set map)

(define set->list
  (lambda (set)
    set))

(define list->set
  (lambda (list)
    (union*-set (map singleton-set list))))

(define subset?
  (lambda (set1 set2)
    (if (memq '#f (map (lambda (element)
                         (in-set? element set2))
                       set1))
        #f
        #t)))

(define equal-sets?
  (lambda (set1 set2)
    (and (subset? set1 set2)
         (subset? set2 set1))))


(define size-set
  (lambda (set)
    (length set)))
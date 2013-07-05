#lang racket
;;====================================================================
;;
;; File:   lib-table.scm
;; Author: John Hatcliff and Shawn Laubach
;; Last modified: June 28, 1998
;;
;;
;;  Defines an assoc list reprentation for lookup tables.
;;
;;    ((key1 . val1) ... (keyn . valn))
;;
;;  *** IMPORTANT ***
;;  The table structure always maintains the order of keys.  We
;;  rely on this when testing for equality of tables (e.g.,
;;  used when comparing states in checking seen-before list)
;;  --- table equality is implemented by equal?
;;
;;====================================================================

(define empty-table '())

;; (key x val x table) -> table
(define (update-table key val table)
  (if (null? table)
      (list (cons key val))
      (if (equal? (caar table) key)
          (cons (cons key val) (cdr table))
          (cons (car table) (update-table key val (cdr table))))))

;; (key x table) -> value
(define (lookup-table key table)
  (if (null? table)
      'unknown-table-key
      (if (equal? (caar table) key)
          (cdar table)
          (lookup-table key (cdr table)))))

;; (keys x vals x table) -> table
;;   - builds a table from a list of keys and a list of values
(define (update-table* keys vals table)
  (if (null? keys)
      table
      (let ((new-table (update-table (car keys) (car vals) table)))
        (update-table* (cdr keys) (cdr vals) new-table))))

;; vars x value x table -> table
;;   - initializes each key to value in table
(define (initialize-table keys value table)
  (if (null? keys)
      table
      (initialize-table (cdr keys)
                        value
                        (update-table (car keys) value table))))

;; table -> keys
;;   - retreives all the keys in a table
(define (domain-table table)
  (map car table))

;; table -> values
;;   - retreives all the values in a table
(define (image-table table)
  (map cdr table))

;; key x table -> #t | #f
(define (key-defined-table? key table)
  (assoc key table))

;; table x table -> #t | #f
;;   - must have equal domains (same order, too)
;;     and equal images (same order, too)
(define (equal-tables? table1 table2)
  (equal? table1 table2))

;; table x table -> table
(define (merge-table str1 str2)
  (if (null? str1)
      str2
      (merge-table (cdr str1) (update-table (caar str1) (cdar str1) str2))))

;; predicate x table -> table
;;   - returns a new table where the image is all the values from the
;;     argument table that satisfy the predicate pred?
;;
(define (restrict-table pred? table)
  (if (null? table)
      empty-table
      (let ((first-entry (car table))
            (rest        (cdr table)))
        (if (pred? (cdr first-entry))
            (cons first-entry (restrict-table pred? rest))
            (restrict-table pred? rest)))))

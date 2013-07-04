#lang racket
;;====================================================================
;;
;; File: parse.scm
;; Author: John Hatcliff and Shawn Laubach
;; Last modified: June 28, 1998
;;
;;
;;  Implements parse and unparse for a list-based concrete syntax for
;;  FCL. Also implements the procedure collect-vars that collects
;;  all the variables occuring in the program.
;;
;;   *** WARNING ***: The list based syntax is different than the
;;  concrete syntax presented in the lecture notes.  The list-based
;;  syntax greatly simplifies the parsing for Schmeme.
;;
;;  Notes:
;;   - new primitives added by the user must be added to the list
;;   'primitives' defined below.
;;
;;====================================================================

;;====================
;;  Concrete Syntax
;;====================

;;<Program>    ::= (<var>* (<label>) <basicblock>+)
;;<block>      ::= (<label> <assignment>* <jump>)
;;<Assignment> ::= (<var> := <exp>)
;;<exp>        ::= <const>
;;               | (<op> <exp-1> ... <exp-n>)
;;               | <var>
;;<jump>       ::= (goto <label>)
;;               | (return <exp>)
;;               | (if <exp> <label> <label>)
;;<const>      ::= <val> | (quote <datum>)
;;<label>      ::= any symbol
;;<op>         ::= + | - | * | < | > | = | ...
;;<var>        ::= any alphanumeric symbol not used as an operator
;;<val>        ::= any number
;;<datum>      ::= any scheme S-expression
;;

;;====================
;;  Abstract Syntax
;;====================

(require rackunit)

(struct program    (params init-label blocks)   #:transparent)
(struct block      (label assigns jump)         #:transparent)
(struct assign     (var exp)                    #:transparent)
(struct const      (datum)                      #:transparent)
(struct varref     (var)                        #:transparent)
(struct app        (op exps)                    #:transparent) 
(struct goto       (label)                      #:transparent) 
(struct if-jump    (exp then-label else-label)  #:transparent)
(struct return     (exp)                        #:transparent)

;;
;; New primitives added by user must be added to this list.
;;
(define primitives
  '(+ - * / % = < > car cdr cons null list null? pair? list? test))

(define (primitive? prim)
  (member prim primitives))

(define (parse-program prog)
  (program (car prog)
           (caadr prog)
           (map parse-block (caddr prog))))


(define (parse-block s)
  (block (car s)
         (map parse-assign (cadr s))
         (parse-jump (caddr s))))

(define (parse-assign s)
  (assign (car s)
          (parse-exp (caddr s))))

(define (parse-exp exp)
  (if (pair? exp)
      (if (equal? 'quote (car exp))
          (const (cadr exp))
          (if (primitive? (car exp))
              (app (car exp) (map parse-exp (cdr exp)))
              (error "Parse: invalid expression:" exp)))
      (if (number? exp)
          (const exp)
          (if (primitive? exp)
              (app exp '())
              (varref exp)))))

(define (parse-jump jump)
  (case (car jump)
    ((goto) (goto (cadr jump)))
    ((return) (return (parse-exp (cadr jump))))
    ((if) (if-jump (parse-exp (cadr jump))
                   (caddr jump)
                   (cadddr jump)))
    (else (error "Parse: invalid jump: " jump))))

;; testing
;; testing
;; testing
(check-not-false 
 (primitive? '*))

(check-false 
 (primitive? 'print))

(check-equal? 
 (parse-exp '1) 
 (const 1))

(check-equal? 
 (parse-exp 'a) 
 (varref 'a))

(check-equal? 
 (parse-exp '(* 1 2)) 
 (app '* (list (const 1) (const 2))))

(check-equal? 
 (parse-exp '*)
 (app '* '()))

(check-exn
 exn:fail?
 (lambda () (parse-exp '(1))))

(check-equal?
 (parse-assign '(result := (* result m)))
 (assign 'result (app '* (list (varref 'result) (varref 'm)))))

(check-equal? 
 (parse-jump '(goto test))
 (goto 'test))

(check-equal?
 (parse-jump '(return 1))
 (return (const 1)))

(check-equal?
 (parse-jump '(if n end loop))
 (if-jump (varref 'n) 'end 'loop))

(check-equal?
 (parse-block '(init ((a := 1) (b := 2)) (goto test)))
 (block 'init (list (assign 'a (const 1)) (assign 'b (const 2))) (goto 'test)))

(define prog1
  '((m n)
    (init)
    {
     (init ((result := 1))
           (goto test))     
     (end ()
          (return result))
     }))

(check-equal?
 (parse-program prog1)
 (program '(m n) 'init (list (block 'init (list (assign 'result (const 1))) (goto 'test)) (block 'end '() (return (varref 'result))))))


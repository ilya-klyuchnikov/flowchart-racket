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
(provide (all-defined-out))

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


;;==========================================================================
;;  Parse
;;
;;   Converts list-based FCL concrete syntax to abstract syntax
;;
;;==========================================================================

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

;;==========================================================================
;;  Unparse
;;
;;   Converts abstract syntax to list-based FCL concrete syntax 
;;
;;==========================================================================

(define (unparse-exp exp)
  (match exp
    [(app op exps) (cons op (map unparse-exp exps))]
    [(varref var) var]
    [(const datum)
     (if (number? datum) datum (list 'quote datum))]
    [else (error "Unparse: invalid exp AST: " exp)]))

(define (unparse-jump exp)
  (match exp
    [(return exp) (list 'return (unparse-exp exp))]
    [(goto label) (list 'goto label)]
    [(if-jump exp then-label else-label)
     (list 'if (unparse-exp exp) then-label else-label)]
    [else (error "Unparse: invalid jump AST:" exp)]))

(define (unparse-assign asg)
  (match asg
    [(assign var exp)
     (list var ':= (unparse-exp exp))]
    [else (error "Unparse: invalid assign AST:" assign)]))

(define (unparse-block b)
  (match b
    [(block label assigns jump)
     (list label
           (map unparse-assign assigns)
           (unparse-jump jump))]
    [else (error "Unparse: invalid basic block AST:" block)]))

(define unparse-program
  (lambda (prog)
    (match prog
      [(program params init blocks)
       (list params (list init) (map unparse-block blocks))]
      [else
       (error "Unparse: invalid program AST:" prog)])))

;;=======================
;;  Collect Variables
;;
;;   Collects all variables occuring in a program
;;
;;=======================

(define collect-vars-exp
  (lambda (exp)
    (match exp
      [(app op exps)
       (set-union*
        (map collect-vars-exp exps))]
      [(varref var) (set var)]
      [(const datum) (set)]
      [else (error "Collect-vars: invalid exp AST: " exp)])))

(define collect-vars-jump
  (lambda (exp)
    (match exp
      [(return exp) (collect-vars-exp exp)]
      [(goto label) (set)]
      [(if-jump exp then-label else-label) (collect-vars-exp exp)]
      [else (error "Collect-vars: invalid jump AST:" exp)])))

(define collect-vars-assign
  (lambda (s)
    (match s
      [(assign var exp)
       (set-union (set var) (collect-vars-exp exp))]
      [else (error "Collect-vars: invalid assign AST:" assign)])))

(define collect-vars-block
  (lambda (b)
    (match b
      [(block label assigns jump)
       (set-union (set-union*
                   (map collect-vars-assign assigns))
                  (collect-vars-jump jump))]
      [else (error "Collect-vars: invalid basic block AST:" block)])))

(define collect-vars-prog
  (lambda (prog)
    (if (program? prog)
        (set-union (list->set (program-params prog))
                   (set-union*
                    (map collect-vars-block (program-blocks prog))))
        (error "Collect-vars: invalid program AST:" prog))))

(define (set-union* sets)
  (if (empty? sets) 
      (set) 
      (set-union (car sets) (set-union* (cdr sets)))))

(define (collect-vars prog)
  (set->list (collect-vars-prog prog)))


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

(define s-prog
  '((m n)
    (init)
    {
     (init ((result := 1))
           (goto test))     
     (end ()
          (return result))
     }))

(define ast-prog
  (program 
   '(m n) 
   'init 
   (list 
    (block 'init (list (assign 'result (const 1))) (goto 'test)) 
    (block 'end '() (return (varref 'result))))))

(check-equal? (parse-program s-prog) ast-prog)
(check-equal? (unparse-program ast-prog) s-prog)

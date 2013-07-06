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


#|
============================================================================
  Parse
  Converts list-based FCL concrete syntax to abstract syntax
============================================================================
|#

(define (parse-program s)
  (match s 
    [(list args (list label) blocks) (program args label (map parse-block blocks))]))

(define (parse-block s)
  (match s 
    [(list lbl assigns jump) (block lbl (map parse-assign assigns) (parse-jump jump))]))

(define (parse-assign s)
  (match s 
    [(list id ':= exp) (assign id (parse-exp exp))]))

(define (parse-exp exp)
  (match exp
    [(cons 'quote e) e]
    [(cons (and op (? primitive?)) args) (app op (map parse-exp args))]
    [(? number?) (const exp)]
    [(? primitive?) (app exp '())]
    [(? symbol?) (varref exp)]
    [_ (error "Parse: invalid exp: " exp)]))

(define (parse-jump jump)
  (match jump
    [(list 'goto label) (goto label)]
    [(list 'return exp) (return (parse-exp exp))]
    [(list 'if exp l1 l2) (if-jump (parse-exp exp) l1 l2)]
    [_ (error "Parse: invalid jump: " jump)]))

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
    [(const datum) (quasiquote (unquote datum))]
    [_ (error "Unparse: invalid exp AST: " exp)]))

(define (unparse-jump exp)
  (match exp
    [(return e) (list 'return (unparse-exp e))]
    [(goto label) (list 'goto label)]
    [(if-jump e l1 l2) (list 'if (unparse-exp e) l1 l2)]
    [_ (error "Unparse: invalid jump AST: " exp)]))

(define (unparse-assign asg)
  (match asg
    [(assign v e) (list v ':= (unparse-exp e))]
    [_ (error "Unparse: invalid assign AST:" assign)]))

(define (unparse-block b)
  (match b
    [(block lbl as j) (list lbl (map unparse-assign as) (unparse-jump j))]
    [_ (error "Unparse: invalid basic block AST:" block)]))

(define (unparse-program prog)
  (match prog
    [(program vs i bs) (list vs (list i) (map unparse-block bs))]
    [_ (error "Unparse: invalid program AST:" prog)]))

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

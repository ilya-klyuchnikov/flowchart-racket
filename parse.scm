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

(define-record program    (params init-label blocks))
(define-record block      (label assigns jump))
(define-record assign     (var exp))
(define-record const      (datum))
(define-record varref     (var))
(define-record app        (op exps))
(define-record goto       (label))
(define-record if         (exp then-label else-label))
(define-record return     (exp))

;;==========================================================================
;;  Parse
;;
;;   Converts list-based FCL concrete syntax to abstract syntax
;;
;;==========================================================================

(define parse-program
  (lambda (prog)
    (make-program (car prog)
		  (caadr prog)
		  (map parse-block (caddr prog)))))

(define parse-block
  (lambda (block)
    (make-block (car block)
		     (map parse-assign (cadr block))
		     (parse-jump (caddr block)))))

(define parse-assign
  (lambda (assign)
    (make-assign (car assign)
		     (parse-exp (caddr assign)))))

(define parse-exp
  (lambda (exp)
    (if (pair? exp)
	(if (equal? 'quote (car exp))
	    (make-const (cadr exp))
	    (if (primitive? (car exp))
		(make-app (car exp) (map parse-exp (cdr exp)))
		(error "Parse: invalid expression:" exp)))
	(if (number? exp)
	    (make-const exp)
	    (if (primitive? exp)
		(make-app exp '())
		(make-varref exp))))))

(define parse-jump
  (lambda (jump)
    (case (car jump)
      ((goto) (make-goto (cadr jump)))
      ((return) (make-return (parse-exp (cadr jump))))
      ((if) (make-if (parse-exp (cadr jump))
		     (caddr jump)
		     (cadddr jump)))
      (else (error "Parse: invalid jump: " jump)))))

;;
;; New primitives added by user must be added to this list.
;;
(define primitives
   '(+ - * / % = < > car cdr cons null list null? pair? list? test))

(define primitive?
  (lambda (prim)
    (memq prim primitives)))

(define parse parse-program)


;;==========================================================================
;;  Unparse
;;
;;   Converts abstract syntax to list-based FCL concrete syntax 
;;
;;==========================================================================

(define unparse-exp
  (lambda (exp)
    (variant-case exp
      (app (op exps)
	   (cons op (map unparse-exp exps)))
      (varref (var) var)
      (const (datum)
	     (if (number? datum)
		 datum
		 (list 'quote datum)))
      (else error "Unparse: invalid exp AST: " exp))))

(define unparse-jump
  (lambda (exp)
    (variant-case exp
      (return (exp) (list 'return (unparse-exp exp)))
      (goto (label) (list 'goto label))
      (if (exp then-label else-label)
	  (list 'if
		(unparse-exp exp)
		then-label
		else-label))
      (else (error "Unparse: invalid jump AST:" exp)))))

(define unparse-assign
  (lambda (assign)
    (variant-case assign
      (assign (var exp)
		  (list var
			':=
			(unparse-exp exp)))
      (else (error "Unparse: invalid assign AST:" assign)))))

(define unparse-block
  (lambda (block)
    (variant-case block
      (block (label assigns jump)
		  (list label
			(map unparse-assign assigns)
			(unparse-jump jump)))
      (else (error "Unparse: invalid basic block AST:" block)))))

(define unparse-prog
  (lambda (prog)
    (if (program? prog)
	(list (program->params prog)
	      (list (program->init-label prog))
	      (map unparse-block (program->blocks prog)))
	(error "Unparse: invalid program AST:" prog))))

(define unparse unparse-prog)

;;=======================
;;  Collect Variables
;;
;;   Collects all variables occuring in a program
;;
;;=======================

(define collect-vars-exp
  (lambda (exp)
    (variant-case exp
      (app (op exps)
	   (union*-set (map collect-vars-exp exps)))
      (varref (var) (singleton-set var))
      (const (datum)
	     empty-set)
      (else error "Collect-vars: invalid exp AST: " exp))))

(define collect-vars-jump
  (lambda (exp)
    (variant-case exp
      (return (exp) (collect-vars-exp exp))
      (goto (label) empty-set)
      (if (exp then-label else-label)
	  (collect-vars-exp exp))
      (else (error "Collect-vars: invalid jump AST:" exp)))))

(define collect-vars-assign
  (lambda (assign)
    (variant-case assign
      (assign (var exp)
		  (add-set var (collect-vars-exp exp)))
      (else (error "Collect-vars: invalid assign AST:" assign)))))

(define collect-vars-block
  (lambda (block)
    (variant-case block
      (block (label assigns jump)
		  (union-set (union*-set
			       (map collect-vars-assign assigns))
			     (collect-vars-jump jump)))
      (else (error "Collect-vars: invalid basic block AST:" block)))))

(define collect-vars-prog
  (lambda (prog)
    (if (program? prog)
	(union-set (program->params prog)
		   (union*-set 
		     (map collect-vars-block (program->blocks prog))))
	(error "Collect-vars: invalid program AST:" prog))))

(define collect-vars collect-vars-prog)


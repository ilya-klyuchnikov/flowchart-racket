#lang racket
;;====================================================================
;;
;; File: eval.scm
;; Author: John Hatcliff and Shawn Laubach
;; Last modified: June 28, 1998
;;
;;====================================================================

;;====================================================================
;;  FCL Evaluator 
;;
;;  Interpreter for FCL programs.  The structure of the interpreter
;;  is meant to match the FCL operational semantics given in the
;;  lecture notes.
;;
;;  The evaluator uses a parser (defined in parse.scm).  parse.scm
;;  also defines the abstract syntax and the procedure 'collect-vars'
;;  that collects all the variables used in an FCL program.
;;
;;  The main data structures used in the interpreter are a
;;  store and a blockmap.  Both of these are implemented as tables.
;;  The table ADT is defined in table.scm.
;;
;;  lib-fcl-shared.scm contains some helper functions and definitions
;;  of FCL state (label/store pair) and boolean value.
;;
;;====================================================================

;; get auxiliary files
(require "parse.rkt")
(require "lib-fcl-shared.rkt")
(require "lib-table.rkt")


;;--------------------------------------------------------------
;;  Top-level calls
;;
;; - eval-fcl takes a list giving the concrete syntax of an FCL
;;     program and a list of argument values.
;;
;; - eval-fcl-file takes the program argument from a file.
;;
;;
;;  Example call: these call the power program to compute 5^2.
;;
;;  > (define power (get-file-object "examples/power.fcl"))
;;  > (eval-fcl power '(5 2))
;;
;;  > (eval-fcl-file "examples/power.fcl" '(5 2))
;;
;;  Note: get-file-object is in lib-fcl-shared.scm
;;
;;--------------------------------------------------------------

(define eval-fcl
  (lambda (prog args)
    (eval-prog (parse-program prog) args)))

(define eval-fcl-file 
  (lambda (file-name args)
    (let* ((prog   (file->value file-name))
	   (result (eval-fcl prog args)))
      result)))


;;--------------------------------------------------------------
;;  Evaluator Procedures
;;
;;  There is one procedure for each syntactic category in FCL.
;;  The structure closely follows the operational semantics
;;  definition.
;;--------------------------------------------------------------

;;(prog x args) -> value
(define eval-prog
  (lambda (prog args)
    (let* (;;
	   ;; get program pieces
	   ;;
	   (params     (program-params prog))
	   (blocks     (program-blocks prog))
	   (init-label (program-init-label prog))
	   ;;
	   ;; make initial store
	   ;;
	   (vars           (collect-vars prog))    ;; get all prog vars
	   (pre-init-store (initialize-table       ;; initialize all vars to
			     vars                  ;;   default value
			     init-store-val        
			     empty-table))
	   (init-store     (update-table*          ;; initialize parameter
			     params                ;;   values
			     args
			     pre-init-store))
	   ;;
	   ;; make initial state and blockmap
	   ;;
	   (init-state (make-state init-label init-store))
	   (blockmap (update-table* (map block->label blocks)
				    blocks
				    empty-table)))
	   ;;
	   ;; do transitions until halt<v> is reached
	   ;;
      (compute-transitions init-state
			   blockmap))))


;;---------------------------------------------------------------
;; compute-transitions: (state x blockmap) -> value
;;
;;   Does state transitions until (halt<v>, store) is reached
;;
;;---------------------------------------------------------------

(define compute-transitions
  (lambda (init-state blockmap)
    (letrec ((transition (lambda (state)
			   (let* ((result-state (eval-block
						  (lookup-table
						    (state->label state)
						    blockmap)
						  (state->store state)))
				  ;;
				  ;; debugging procedure
				  ;;   (see bottom of file)
				  ;;
				  (temp (eval-debug state)))
			     (if (halt? (state->label result-state))
				 (halt->value (state->label result-state))
				 (transition result-state))))))
      (transition init-state))))


;; (block x store) -> state
(define eval-block
  (lambda (block store)
    (let* ((new-store (eval-assigns (block->assigns block) store))
           (new-label (eval-jump (block->jump block) new-store)))
      (make-state new-label new-store))))

;;(assigns x store) -> store
(define eval-assigns
  (lambda (assigns store)
    (if (null? assigns)
	store
	(let ((new-store (eval-assign (car assigns) store)))
	  (eval-assigns (cdr assigns) new-store)))))

;;(assign x store) -> store
(define eval-assign
  (lambda (assign store)
    (let ((val (eval-exp (assign->exp assign)
			 store))
	  (var (assign->var assign)))
      (update-table var val store))))

;;(jump x store) -> label
(define eval-jump
  (lambda (jump store)
    (variant-case jump
      (goto (label) label)
      (return (exp) (make-halt (eval-exp exp store)))
      (if (exp then-label else-label)
	  (if (is-true? (eval-exp exp store))
	      then-label
	      else-label))
      (else (error "Unrecognized jump in eval-jump: " jump)))))

;;(exp x store) -> value
(define eval-exp
  (lambda (exp store)
    (variant-case exp
      (const (datum) datum)
      (varref (var) (lookup-table var store))
      (app (op exps) (eval-op
		       op
		       (map (lambda (exp) (eval-exp exp store))
			    exps)))
                     ;; note: eval-op is in lib-fcl-shared.scm
      (else (error "Unrecognized exp in eval-exp: " exp)))))


;;------------------------------------------------------------
;;  Debugging and Tracing
;;
;;   - state values can be printed/suppressed by changing value
;;  of debug-level variable below.
;;
;;
;; debug-level = 0 : no debug info
;; debug-level > 0 : print out contents of state on each transition
;;
;;------------------------------------------------------------

(define debug-level 0)

(define eval-debug
  (lambda (state)
    (if (> debug-level 0)
	(begin
	  (display "-------------------------------")
	  (newline)
	  (display "LABEL: ")
	  (pretty-print (state->label state))
	  (newline)
	  (display "STORE: ")
	  (newline)
	  (pretty-print (state->store state))
	  (newline))
	())))







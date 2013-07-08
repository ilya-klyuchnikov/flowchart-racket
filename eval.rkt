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
(provide eval-fcl eval-fcl-file)

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

(define (eval-fcl prog args)
  (eval-prog (parse-program prog) args))

(define (eval-fcl-file file-name args)
  (eval-fcl (file->value file-name) args))


;;--------------------------------------------------------------
;;  Evaluator Procedures
;;
;;  There is one procedure for each syntactic category in FCL.
;;  The structure closely follows the operational semantics
;;  definition.
;;--------------------------------------------------------------

;;(prog x args) -> value
(define (eval-prog prog args)
  (let* (;; get program pieces
         (params     (program-params prog))
         (blocks     (program-blocks prog))
         (init-label (program-init-label prog))
         ;; make initial store
         (vars           (collect-vars prog))    ;; get all prog vars           
         (pre-init-store (initialize-table       ;; initialize all vars to
                          vars                  ;;   default value
                          init-store-val))
         (init-store     (update-table*          ;; initialize parameter
                          params                ;;   values
                          args
                          pre-init-store))
         ;; make initial state and blockmap
         (init-state (state init-label init-store))
         (blockmap (update-table* (map block-label blocks)
                                  blocks
                                  (hash))))
    ;; do transitions until halt<v> is reached
    (compute-transitions init-state blockmap)))


;;---------------------------------------------------------------
;; compute-transitions: (state blockmap) -> value
;; Does state transitions until (halt<v>, store) is reached
;;---------------------------------------------------------------
(define (compute-transitions st blockmap)
  (define (transition st)
    (match (eval-block (hash-ref blockmap (state-label st)) (state-store st))
      [(state (halt v) _) v]
      [s1 (transition s1)]))      
  (transition st))


;; (block store) -> state
(define (eval-block block store)
  (let* ([store1 (eval-assigns (block-assigns block) store)]
         [label1 (eval-jump (block-jump block) store1)])
    (state label1 store1)))

;;(assigns store) -> store
(define (eval-assigns assigns store)
  (match assigns
    ['() store]
    [(cons a as) (eval-assigns as (eval-assign a store))]))

;;(assign store) -> store
(define (eval-assign asgn store)
  (match asgn 
    [(assign v exp) (hash-set store v (eval-exp exp store))]))

;;(jump store) -> label
(define (eval-jump jump store)
  (match jump
    [(goto label) label]
    [(return exp) (halt (eval-exp exp store))]
    [(if-jump exp l1 l2)
     (if (is-true? (eval-exp exp store)) l1 l2)]
    [_ (error "Unrecognized jump in eval-jump: " jump)]))

;;(exp x store) -> value
(define (eval-exp exp store)
  (match exp
    [(const datum) datum]
    [(varref var) (hash-ref store var)]
    [(app op exps) (eval-op op (map (λ (exp) (eval-exp exp store)) exps))]
    [_ (error "Unrecognized exp in eval-exp: " exp)]))

#lang racket

(provide eval-fcl eval-fcl-file)

(require "parse.rkt")
(require "lib-fcl-shared.rkt")
(require "lib-table.rkt")

;;  Examples:
;;  > (define power (file->value "examples/power.fcl"))
;;  > (eval-fcl power '(5 2))
;;  > (eval-fcl-file "examples/power.fcl" '(5 2))

(define (eval-fcl prog args)
  (eval-prog (parse-program prog) args))

(define (eval-fcl-file file-name args)
  (eval-fcl (file->value file-name) args))

;;Evaluator Procedures

;;(prog args) -> value
(define (eval-prog prog args)
  (let* ([params (program-params prog)]
         [blocks (program-blocks prog)]
         [vars   (collect-vars prog)]
         [label0 (program-init-label prog)]
         [store0  (hash-set-kv* (hash-init vars init-store-val) params args)]
         [state0 (state label0 store0)]
         [blockmap (hash-kv (map block-label blocks) blocks)])
    (compute-transitions state0 blockmap)))

;;(state blockmap) -> value
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
    [(if-jump exp l1 l2) (if (is-true? (eval-exp exp store)) l1 l2)]))

;;(exp x store) -> value
(define (eval-exp exp store)
  (match exp
    [(const datum) datum]
    [(varref var) (hash-ref store var)]
    [(app op exps) (eval-op op (map (λ (exp) (eval-exp exp store)) exps))]))

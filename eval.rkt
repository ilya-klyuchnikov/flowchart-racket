#lang racket

(provide eval-fcl eval-fcl-file eval-op init-val (struct-out state) (struct-out halt) halt-state?)

(require "parse.rkt")
(require "debug.rkt")
(require "util.rkt")

;;  Examples:
;;  > (define power (file->value "examples/power.fcl"))
;;  > (eval-fcl power '(5 2))
;;  > (eval-fcl-file "examples/power.fcl" '(5 2))

(define init-val null)
(struct halt (value) #:transparent)
(define (halt-state? state) (halt? (state-label state)))

;; TODO: is it possible to make it more elegant??
(define (hd l) (if (empty? l) -1 (first l)))
(define (tl l) (if (empty? l) '() (rest l)))
(define-namespace-anchor a)
(define ns (namespace-anchor->namespace a))

(define (eval-op op args)  
  (apply (eval op ns) args))

(define (eval-fcl prog args)
  (eval-prog (parse-program prog) args))

(define (eval-fcl-file file-name args)
  (eval-fcl (file->value file-name) args))

;;Evaluator Procedures

;;(prog args) -> value
(define (eval-prog prog args)
  (let* ([params  (program-params prog)]
         [blocks  (program-blocks prog)]
         [vars    (collect-vars prog)]
         [store0  (hash-set-kv* (hash-init vars init-val) params args)]
         [state0  (state (program-init-label prog) store0)]
         [blockmap (hash-kv (map block-label blocks) blocks)])
    (compute-transitions state0 blockmap)))

;;(state blockmap) -> value
(define (compute-transitions st blockmap)
  (define (transition st)
    (define st1 
      (eval-block (hash-ref blockmap (state-label st)) (state-store st)))
    (eval-debug st1)
    (match st1 
      [(state (halt v) _) v]
      [s1 (transition s1)]))      
  (transition st))

;; (block store) -> state
(define (eval-block block store)
  (define store1 (eval-assigns (block-assigns block) store))
  (define label1 (eval-jump (block-jump block) store1))
  (state label1 store1))

;;(assigns store) -> store
(define (eval-assigns assigns store)
  (match assigns
    ['() store]
    [(cons a as) (eval-assigns as (eval-assign a store))]))

;;(assign store) -> store
(define (eval-assign asgn store)
  (match asgn 
    [(assign v exp) (hash-set store v (eval-exp exp store))]))

;;(jump store) -> label | (halt value)
(define (eval-jump jump store)
  (match jump
    [(goto label) label]
    [(return exp) (halt (eval-exp exp store))]
    [(if-jump exp l1 l2) (if (eval-exp exp store) l1 l2)]))

;;(exp x store) -> value
(define (eval-exp exp store)
  (match exp
    [(const datum) datum]
    [(varref var) (hash-ref store var)]
    [(app op exps) (eval-op op (map (λ (exp) (eval-exp exp store)) exps))]))

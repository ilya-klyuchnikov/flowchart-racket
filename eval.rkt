#lang racket

(provide eval-fcl eval-fcl-file eval-op eval-exp)

(require "parse.rkt")
(require "util.rkt")

;;  Examples:
;;  > (define power (file->value "examples/power.fcl"))
;;  > (eval-fcl power '(5 2))
;;  > (eval-fcl-file "examples/power.fcl" '(5 2))

;--------------------------------------------------------------------
; Evaluator Procedures
;--------------------------------------------------------------------

(define (eval-fcl-file file-name args)
  (eval-fcl (file->value file-name) args))

(define (eval-fcl prog args)
  (eval-prog (parse-program prog) args))

; (prog args) -> value
(define (eval-prog prog args)
  (let* ([params      (program-params prog)]
         [blocks      (program-blocks prog)]
         [vars        (collect-vars prog)]
         [init-store  (hash-set-kv* (hash-init vars init-val) params args)]
         [blockmap    (hash-kv (map block-label blocks) blocks)])
    (loop (program-init-label prog) init-store blockmap)))

; (label, store, blocks) -> value
; the main loop of interpretation
; makes transitions till return
(define (loop label store blockmap)
  (define (transition label store)
    (match (eval-block  (hash-ref blockmap label) store) 
      [(state (halt v) store) v]
      [(state label store) (transition label store)]))
  (transition label store))

; (block, store) -> state
(define (eval-block block store)
  (define store1 (eval-assigns (block-assigns block) store))
  (define label1 (eval-jump (block-jump block) store1))
  (state label1 store1))

; (assigns, store) -> store
(define (eval-assigns assigns store)
  (foldl eval-assign store assigns))

; (assign, store) -> store
(define (eval-assign asgn store)
  (match asgn 
    [(assign v exp) (hash-set store v (eval-exp exp store))]))

; (jump, store) -> label | (halt value)
(define (eval-jump jump store)
  (match jump
    [(goto label) label]
    [(return exp) (halt (eval-exp exp store))]
    [(if-jump exp l1 l2) (if (eval-exp exp store) l1 l2)]))

;; (exp x store) -> value
(define (eval-exp exp store)
  (match exp
    [(const datum) datum]
    [(varref var) (hash-ref store var)]
    [(app op exps) (eval-op op (map (λ (exp) (eval-exp exp store)) exps))]))

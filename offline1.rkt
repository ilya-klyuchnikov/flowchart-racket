#lang racket

(provide online-prog)

(require "parse.rkt" "eval.rkt" "pe.rkt" "util.rkt" "debug.rkt")
;;--------------------------------------------------------------
;;  Online PE Procedures

(define s-vs null)
;;(prog x (static)vars x (static)vals) -> (residual)prog
(define (online-prog prog s-vars s-vals)
  (let* ([params          (program-params prog)]
         [_               (set! s-vs s-vars)]
         [blocks          (program-blocks prog)]
         [vars            (collect-vars prog)]
         [d-params        (remove* s-vs params)]
         [init-store      (hash-set-kv* (hash) s-vs s-vals)]
         (init-state      (state (program-init-label prog) init-store))
         [_               (state->label-reset)]
         [res-init-label  (state->label init-state)]
         [blockmap        (hash-kv (map block-label blocks) blocks)]
         [pending         (list init-state)]
         [seen            (list)]
         [res-blocks      (transitions blockmap pending seen '())])
    (program d-params res-init-label (reverse res-blocks))))

;; the main loop
(define (transitions blockmap pending seen blocks)
  (define (loop pending seen blocks)
    (if (empty? pending) blocks
        (match-let* ([(cons st pending1) pending]
                     [(state lbl store)  st]
                     [_ (online-debug st pending1 seen blocks)])
          (if (member st seen)
              (loop pending1 seen blocks)
              (let*-values 
                  ([(states block) (online-block (hash-ref blockmap lbl) store)]
                   [(non-halts)    (filter-not halt-state? states)]
                   [(pending2)     (add-pending* non-halts pending1)]
                   [(seen2)        (add-set st seen)])
                (loop pending2 seen2 (cons block blocks)))))))
  (loop pending seen blocks))

;;(block x store) -> states, block
(define (online-block bl store)
  (let*-values 
      ([(new-store res-assigns) (online-assigns (block-assigns bl) store)]
       [(new-labels res-jump) (online-jump (block-jump bl) new-store)])
    (values
     (map (λ (l) (state l new-store)) new-labels)
     (block (state->label (state (block-label bl) store)) res-assigns res-jump))))

; (assigns x store) -> (store x assigns)
(define (online-assigns assigns store)
  (for/fold ([st store] [res-assigns '()]) 
    ([asg assigns])    
    (let-values ([(store1 delta) (online-assign asg st)])
      (values store1 (append res-assigns delta)))))

;;(assign store) -> (store x assigns)
(define (online-assign asg store)
  (match-let ([(assign v e) asg])
    (if (member v s-vs) ;static
       ; todo - should we lift here of not??
       (values (hash-set store v (eval-exp e store)) '())
       (values store (list (assign v (online-exp e store)))))))

;;(jump x store) -> (labels x jump)
(define (online-jump jump store)
  (match jump
    [(goto label)
     (values (list label) (goto (state->label (state label store))))]
    [(return exp)      
     (values (list (halt '())) (return (online-exp exp store)))]
    [(if-jump exp l1 l2)
     (define r-l1 (state->label (state l1 store)))
     (define r-l2 (state->label (state l2 store)))
     (match (online-exp exp store)
       [(const e) (if e (values (list l1) (goto r-l1)) (values (list l2) (goto r-l2)))]
       [e (values (list l1 l2) (if-jump e r-l1 r-l2))])]))

;;(exp store) -> exp
(define (online-exp exp store)
  (match exp
    [(const datum) (const datum)]
    [(varref var) (if (member var s-vs) (const (hash-ref store var)) (varref var))]
    [(app op es) 
      (let ([es (map (λ (e) (online-exp e store)) es)])
        (if (andmap const? es) (const (eval-exp (app op es) store)) (app op es)))]))

(define power (file->value "examples/power.fcl"))
(unparse-program (online-prog (parse-program power) '(n) '(5)))
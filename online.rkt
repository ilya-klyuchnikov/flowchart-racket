#lang racket

(provide online-prog)
(require "parse.rkt" "eval.rkt" "pe.rkt" "util.rkt")

(define (make-store vars s-params s-vals d-params)
  (define s 
    (hash-set-kv* (hash-init vars (static '())) s-params (map static s-vals)))
  (hash-set-kv* s d-params (map (λ (v) (dynamic (varref v))) d-params)))

;;--------------------------------------------------------------
;;  Online PE Procedures

;;(prog x (static)vars x (static)vals) -> (residual)prog
(define (online-prog prog s-params s-vals)
  (let* ([params          (program-params prog)]
         [blocks          (program-blocks prog)]
         [vars            (collect-vars prog)]
         [d-params        (remove* s-params params)]
         [init-store      (make-store vars s-params s-vals d-params)]
         (init-state      (state (program-init-label prog) init-store))
         [_               (state->label-reset)]
         [res-init-label  (state->label init-state)]
         [blockmap        (hash-kv (map block-label blocks) blocks)]
         [pending         (list init-state)]
         [seen            (list)]
         [res-blocks (transitions blockmap pending seen '())])
    (program d-params res-init-label (reverse res-blocks))))

;; the main loop
(define (transitions blockmap pending seen blocks)
  (define (loop pending seen blocks)
    (if (empty? pending) blocks
        (match-let* ([(cons st pending1) pending]
                     [(state lbl store)  st])
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
    (values (map (λ (label) (state label new-store)) new-labels)
            (block (state->label (state (block-label bl) store))
                   res-assigns
                   res-jump))))

; (assigns x store) -> (store x assigns)
(define (online-assigns assigns store)
  (for/fold ([st store] [res-assigns '()]) 
    ([asg assigns])    
    (let-values ([(store1 delta) (online-assign asg st)])
      (values store1 (append res-assigns delta)))))

;;(assign store) -> (store x assigns)
(define (online-assign asg store)
  (match-let ([(assign v e) asg])
    (match (online-exp e store)
      [(static obj)
       (values (hash-set store v (static obj)) '())]
      [(dynamic obj)
       (values (hash-set store v (dynamic (varref v))) (list (assign v obj)))])))

;;(jump x store) -> (labels x jump)
(define (online-jump jump store)
  (match jump
    [(goto label)
     (values (list label) (goto (state->label (state label store))))]
    [(return exp)      
     (values (list (halt '())) (return (lift (online-exp exp store))))]
    [(if-jump exp l1 l2)
     (define r-l1 (state->label (state l1 store)))
     (define r-l2 (state->label (state l2 store)))
     (match (online-exp exp store)
       [(static obj)
        (if obj (values (list l1) (goto r-l1)) (values (list l2) (goto r-l2)))]
       [(dynamic obj)        
        (values (list l1 l2) (if-jump obj r-l1 r-l2))])]))

;;(exp store) -> pe-val
(define (online-exp exp store)
  (match exp
    [(const datum) (static datum)]
    [(varref var)  (hash-ref store var)]
    [(app op es) (online-op op (map (λ (e) (online-exp e store)) es))]
    [_ (error "Unrecognized exp in online-exp: " exp)]))

;;(op pe-vals) -> pe-val
(define (online-op op pe-vals)
  (if (andmap static? pe-vals)
      (static (eval-op op (map pe-val->object pe-vals)))
      (dynamic (app op (map lift pe-vals)))))

;; pe-val -> exp     
(define (lift pe-val)
  (match pe-val
    [(static obj) (const obj)]
    [(dynamic obj) obj]))

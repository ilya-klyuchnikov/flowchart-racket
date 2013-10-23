#lang racket

(provide online-prog)
(require "parse.rkt" "eval.rkt" "pe.rkt" "util.rkt")

(define (make-store vars s-params s-vals d-params)
  (define s 
    (hash-set-kv* (hash-init vars (static '())) s-params (map static s-vals)))
  (hash-set-kv* s d-params (map (λ (v) (dynamic (varref v))) d-params)))

;; prog - a program for specialization
;; s-vars - a list of "static vars"
;; s-vals - a list of corresponding values of s-vars
;; returns a residual program
(define (online-prog prog s-vars s-vals)
  (let* ([params          (program-params prog)]
         [blocks          (program-blocks prog)]
         [vars            (collect-vars prog)]
         [d-params        (remove* s-vars params)]
         [init-store      (make-store vars s-vars s-vals d-params)]
         (init-state      (state (program-init-label prog) init-store))
         [_               (state->label-reset)]
         [blockmap        (hash-kv (map block-label blocks) blocks)]
         [res-blocks (transitions init-state blockmap)])
    (program d-params (block-label (first res-blocks)) res-blocks)))

;; the main point
;; takes an initial state and a program (= blockmap)
;; and produces a list of residual blocks
;; the first block will be an initial one
(define (transitions init-state blockmap)
  (define (loop pending seen res-blocks)
    (if (empty? pending) res-blocks
        (match-let* ([(cons st pending1) pending]
                     [(state lbl store)  st])          
          (let*-values 
              ([(states block) (online-block (hash-ref blockmap lbl) store)]
               [(non-halts)    (filter-not halt-state? states)]                   
               [(seen2)        (add-set st seen)]
               [(pending2)     (remove* seen2 (add-pending* non-halts pending1))])
            (loop pending2 seen2 (cons block res-blocks))))))
  (reverse (loop (list init-state) '() '())))

;; IN  : in-block, store
;; OUT : states, res-block
;; states (list of new states) (one or two -- depending on a jump)
(define (online-block bl store)
  (let*-values 
      ([(new-store res-assigns) (online-assigns (block-assigns bl) store)]
       [(new-labels res-jump) (online-jump (block-jump bl) new-store)])
    (values (map (λ (label) (state label new-store)) new-labels)
            (block (state->label (state (block-label bl) store))
                   res-assigns
                   res-jump))))

;; IN  : in-assigns,  store
;; OUT : out-assigns, store
(define (online-assigns assigns store)
  (for/fold ([st store] [res-assigns '()]) 
    ([asg assigns])    
    (let-values ([(store1 delta) (online-assign asg st)])
      (values store1 (append res-assigns delta)))))

;; IN  : in-assign,   sd-store
;; OUT : out-assigns, sd-store
;; out-assigns is either an empty list or one-element list
(define (online-assign asg store)
  (match-let ([(assign v e) asg])
    (match (online-exp e store)
      [(static obj)
       (values (hash-set store v (static obj)) '())]
      [(dynamic obj)
       (values (hash-set store v (dynamic (varref v))) (list (assign v obj)))])))

;; IN  : jump sd-store
;; OUT : labels, jump
;; where
;; * labels - a list of (original) labels in the residual jump
;; * jump - residual jump
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

;; IN  :  an expression `exp` and a sd-store
;; OUT : a corresponding residual expression
;; the logic is that all sub-expressions are "residualized" recursively
(define (online-exp exp sd-store)
  (match exp
    [(const datum) (static datum)]
    [(varref var)  (hash-ref sd-store var)]
    [(app op es) (online-op op (map (λ (e) (online-exp e sd-store)) es))]
    [_ (error "Unrecognized exp in online-exp: " exp)]))

;; IN  :  an operations `op` and its arguments `pe-vals`
;; OUT : a residual expression
;; if all args are static, evaluated, otherwise, - residualized
(define (online-op op pe-vals)
  (if (andmap static? pe-vals)
      (static (eval-op op (map pe-val->object pe-vals)))
      (dynamic (app op (map lift pe-vals)))))

;; pe-val -> exp     
(define (lift pe-val)
  (match pe-val
    [(static obj) (const obj)]
    [(dynamic obj) obj]))

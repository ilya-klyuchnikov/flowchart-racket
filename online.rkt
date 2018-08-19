#lang racket/base

(require racket/list)
(require racket/match)

(require "parse.rkt" "eval.rkt" "pe.rkt" "util.rkt")

(provide online-prog)

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
         [res-blocks      (pe-state init-state blockmap)])
    (program d-params (block-label (first res-blocks)) res-blocks)))

;; the main point
;; takes an initial state and a program (= blockmap)
;; and produces a list of residual blocks
;; the first block will be an initial one
(define (pe-state init-state b-map)
  (define (loop pending seen res-blocks)
    (if (empty? pending) res-blocks
        (match-let* ([(cons st pending) pending]
                     [(state lbl store)  st])
          (let*-values
              ([(bl)           (hash-ref b-map lbl)]
               [(res-label)    (state->label (state (block-label bl) store))]
               [(states block) (pe-block bl store b-map '() res-label)]
               [(non-halts)    (filter-not halt-state? states)]
               [(seen)         (add-set st seen)]
               [(pending)      (remove* seen (add-pending* non-halts pending))])
            (loop pending seen (cons block res-blocks))))))
  (reverse (loop (list init-state) '() '())))

(define (compress? _) #f)

;; IN  : in-block, store
;; OUT : states, res-block
(define (pe-block bl store b-map acc res-label)
  (let*-values
      ([(next-store res-assigns) (pe-assigns (block-assigns bl) store)]
       [(next-labels res-jump)   (pe-jump (block-jump bl) next-store)])
    (match res-jump
      ; transition compression
      [(goto (? compress? lb))
       (pe-block (hash-ref b-map lb) next-store b-map (append acc res-assigns) res-label)]
      [_ (values (map (λ (l) (state l next-store)) next-labels)
                 (block res-label (append acc res-assigns) res-jump))])))

;; IN  : in-assigns,  store
;; OUT : out-assigns, store
(define (pe-assigns assigns store)
  (for/fold ([st store] [res-assigns '()]) ([asg assigns])
    (let-values ([(st delta) (pe-assign asg st)])
      (values st (append res-assigns delta)))))

;; IN  : in-assign,   sd-store
;; OUT : out-assigns, sd-store
;; out-assigns is either an empty list or one-element list
;; this is the big difference!
(define (pe-assign asg store)
  (match-let ([(assign v e) asg])
    (match (pe-exp e store)
      [(static obj)
       (values (hash-set store v (static obj)) '())]
      [(dynamic obj)
       (values (hash-set store v (dynamic (varref v))) (list (assign v obj)))])))

;; IN  : jump sd-store
;; OUT : labels, jump
;; where
;; * labels - a list of (original) labels in the residual jump
;; * jump - residual jump
(define (pe-jump jump store)
  (match jump
    [(goto label)
     (define r-l (if (compress? label) label (state->label (state label store))))
     (values (list label) (goto r-l))]
    [(return exp)
     (values (list (halt '())) (return (lift (pe-exp exp store))))]
    [(if-jump exp l1 l2)
     (define r-l1 (state->label (state l1 store)))
     (define r-l2 (state->label (state l2 store)))
     (define g-l1 (if (compress? l1) l1 (state->label (state l1 store))))
     (define g-l2 (if (compress? l2) l2 (state->label (state l2 store))))
     (match (pe-exp exp store)
       [(static e)
        (if e (values (list l1) (goto g-l1)) (values (list l2) (goto g-l2)))]
       [(dynamic e)
        (values (list l1 l2) (if-jump e r-l1 r-l2))])]))

;; IN  :  an expression `exp` and a store
;; OUT : a corresponding residual expression
;; the logic is that all sub-expressions are "residualized" recursively
(define (pe-exp exp store)
  (match exp
    [(const datum) (static datum)]
    [(varref var)  (hash-ref store var)]
    [(app op es)   (pe-op op (map (λ (e) (pe-exp e store)) es))]
    [_ (error "Unrecognized exp in pe-exp: " exp)]))

;; IN  : an operations `op` and its arguments `pe-vals`
;; OUT : a residual expression
;; if all args are static, evaluated, otherwise, - residualized
(define (pe-op op pe-vals)
  (if (andmap static? pe-vals)
      (static (eval-op op (map pe-val->object pe-vals)))
      (dynamic (app op (map lift pe-vals)))))

(define (pe-val->object pe-val)
  (match pe-val
    [(static obj) obj]
    [(dynamic obj) obj]))

(define (lift pe-val)
  (match pe-val
    [(static obj) (const obj)]
    [(dynamic obj) obj]))

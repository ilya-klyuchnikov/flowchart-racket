#lang racket

(provide mix-prog)
(require "parse.rkt" "eval.rkt" "pe.rkt" "util.rkt")

;; this is very similar to online, but it accepts BTA!
;; it assumes that s-vars are always s-vars
;; (so it is a limited form of BTA, I guess)

;; prog - a program for specialization
;; s-vars - a list of "static vars"
;; s-vals - a list of corresponding values of s-vars
;; returns a residual program
(define (mix-prog prog s-vars s-vals)
  (let* ([params          (program-params prog)]
         [blocks          (program-blocks prog)]
         [vars            (collect-vars prog)]
         [d-params        (remove* s-vars params)]
         [init-store      (hash-set-kv* (hash) s-vars s-vals)]
         (init-state      (state (program-init-label prog) init-store))
         [_               (state->label-reset)]
         [blockmap        (hash-kv (map block-label blocks) blocks)]
         [res-blocks      (mix-state init-state blockmap s-vars)])
    (program d-params (block-label (first res-blocks)) res-blocks)))

;; the main point
;; takes an initial state and a program (= blockmap)
;; and produces a list of residual blocks
;; the first block will be an initial one
(define (mix-state init-state b-map s-vs)
  (define (loop pending seen blocks)
    (if (empty? pending) blocks
        (match-let* ([(cons st pending) pending]
                     [(state lbl store)  st])          
          (let*-values 
              ([(bl)           (hash-ref b-map lbl)]
               [(res-label)    (state->label (state (block-label bl) store))]
               [(states block) (mix-block bl store b-map '() res-label s-vs)]
               [(non-halts)    (filter-not halt-state? states)]
               [(seen)         (add-set st seen)]
               [(pending)      (remove* seen (add-pending* non-halts pending))])
            (loop pending seen (cons block blocks))))))
  (reverse (loop (list init-state) '() '())))

(define (compress? _) #f)

;; IN  : in-block, s-store
;; OUT : states, res-block
(define (mix-block bl s-store b-map acc res-label s-vs)
  (let*-values 
      ([(next-store res-assigns) (mix-assigns (block-assigns bl) s-store s-vs)]
       [(next-labels res-jump)  (online-jump (block-jump bl) next-store s-vs)])
    (match res-jump
      ; transition compression
      [(goto (? compress? lb)) 
       (mix-block (hash-ref b-map lb) next-store b-map (append acc res-assigns) res-label s-vs)]
      [_ (values (map (λ (l) (state l next-store)) next-labels)
                 (block res-label (append acc res-assigns) res-jump))])))

;; IN  : in-assigns,  store
;; OUT : out-assigns, store
(define (mix-assigns assigns s-store s-vs)
  (for/fold ([st s-store] [res-assigns '()]) ([asg assigns])    
    (let-values ([(st delta) (mix-assign asg st s-vs)])
      (values st (append res-assigns delta)))))

;; IN  : in-assign,   sd-store
;; OUT : out-assigns, sd-store
;; out-assigns is either an empty list or one-element list
;; this is the big difference!
;; if v is dynamic by BTA then then we perform assignmtent
;; and do not propagate possible information!
(define (mix-assign asg store s-vs)
  (match-let ([(assign v e) asg])
    (if (member v s-vs)
        (values (hash-set store v (eval-exp e store)) '())
        (values store (list (assign v (mix-exp e store s-vs)))))))

;; IN  : jump sd-store
;; OUT : labels, jump
;; where
;; * labels - a list of (original) labels in the residual jump
;; * jump - residual jump
(define (online-jump jump s-store s-vs)
  (match jump
    [(goto label)
     (define r-l (if (compress? label) label (state->label (state label s-store))))
     (values (list label) (goto r-l))]
    [(return exp)      
     (values (list (halt '())) (return (mix-exp exp s-store s-vs)))]
    [(if-jump exp l1 l2)
     (define r-l1 (state->label (state l1 s-store)))
     (define r-l2 (state->label (state l2 s-store)))
     (define g-l1 (if (compress? l1) l1 (state->label (state l1 s-store))))
     (define g-l2 (if (compress? l2) l2 (state->label (state l2 s-store))))
     (match (mix-exp exp s-store s-vs)
       [(const e)
        (if e (values (list l1) (goto g-l1)) (values (list l2) (goto g-l2)))]
       [e
        (values (list l1 l2) (if-jump e r-l1 r-l2))])]))

;; IN  :  an expression `exp` and an s-store
;; OUT : a corresponding residual expression
;; the logic is that all sub-expressions are "residualized" recursively
(define (mix-exp exp s-store s-vs)
  (match exp
    [(const datum) (const datum)]
    [(varref var) (if (member var s-vs) (const (hash-ref s-store var)) (varref var))]
    [(app op es) (mix-op op (map (λ (e) (mix-exp e s-store s-vs)) es))]
    [_ (error "Unrecognized exp in mix-exp: " exp)]))

;; IN  : an operations `op` and its arguments `pe-vals`
;; OUT : a residual expression
;; if all args are const, evaluated, otherwise, - residualized
(define (mix-op op pe-vals)
  (if (andmap const? pe-vals)
      (const (eval-op op (map pe-val->object pe-vals)))
      (app op pe-vals)))

;; pe-val -> exp     
#;(define (lift pe-val)
    (match pe-val
      [(static obj) (const obj)]
      [(dynamic obj) obj]))

(define (pe-val->object pe-val)
  (match pe-val
    [(const obj) obj]
    [obj obj]))

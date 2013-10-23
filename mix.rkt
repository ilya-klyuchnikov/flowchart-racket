#lang racket

(provide mix-prog)
(require "parse.rkt" "eval.rkt" "pe.rkt" "util.rkt")

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

;; the main loop
;; takes a state and a blocks and produces a list of residual blocks
(define (mix-state st blockmap s-vs)
  (define (loop pending seen blocks)
    (if (empty? pending) blocks
        (match-let* ([(cons st pending1) pending]
                     [(state lbl store)  st])
          (if (member st seen)
              (loop pending1 seen blocks)
              (let*-values 
                  ([(bl)           (hash-ref blockmap lbl)]
                   [(states block) (mix-block bl store blockmap '() (state->label (state (block-label bl) store)) s-vs)]
                   [(non-halts)    (filter-not halt-state? states)]
                   [(pending2)     (add-pending* non-halts pending1)]
                   [(seen2)        (add-set st seen)])
                (loop pending2 seen2 (cons block blocks)))))))
  (reverse (loop (list st) '() '())))

; TODO when compress? is set to #f tests are not passed
(define (compress? _) #t)

(define (mix-block bl store blockmap acc res-label s-vs)
  (let*-values 
      ([(new-store res-assigns) (mix-assigns (block-assigns bl) store s-vs)]
       [(new-labels res-jump) (online-jump (block-jump bl) new-store s-vs)])
    (match res-jump
      ; transition compression
      [(goto (? compress? lb)) 
       (mix-block (hash-ref blockmap lb) new-store blockmap (append acc res-assigns) res-label s-vs)]
      [_ (values
          (map (λ (l) (state l new-store)) new-labels)
          (block res-label (append acc res-assigns) res-jump))])))

(define (mix-assigns assigns store s-vs)
  (for/fold ([st store] [res-assigns '()]) 
    ([asg assigns])    
    (let-values ([(store1 delta) (mix-assign asg st s-vs)])
      (values store1 (append res-assigns delta)))))

(define (mix-assign asg store s-vs)
  (match-let ([(assign v e) asg])
    (if (member v s-vs)
        (values (hash-set store v (eval-exp e store)) '())
        (values store (list (assign v (reduce e store s-vs)))))))

(define (online-jump jump store s-vs)
  (match jump
    [(goto label)
     (values (list label) (goto label))]
    [(return exp)      
     (values (list (halt '())) (return (reduce exp store s-vs)))]
    [(if-jump exp l1 l2)     
     (match (reduce exp store s-vs)
       [(const e)
        (if e (values (list l1) (goto l1)) (values (list l2) (goto l2)))]
       [e
        (define r-l1 (state->label (state l1 store)))
        (define r-l2 (state->label (state l2 store)))
        (values (list l1 l2) (if-jump e r-l1 r-l2))])]))

(define (reduce exp store s-vs)
  (match exp
    [(const datum) (const datum)]
    [(varref var) (if (member var s-vs) (const (hash-ref store var)) (varref var))]
    [(app op es) 
     (let ([es (map (λ (e) (reduce e store s-vs)) es)])
       (if (andmap const? es) (const (eval-exp (app op es) store)) (app op es)))]))


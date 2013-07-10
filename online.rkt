#lang racket
;;====================================================================
;;
;; File: pe-online.scm
;; Author: John Hatcliff and Shawn Laubach
;; Last modified: June 28, 1998
;;
;;
;;  Implements an online partial evaluator for FCL.
;;
;;====================================================================

(provide (all-defined-out))

(require "parse.rkt" "eval.rkt" "pe.rkt" "util.rkt" "debug.rkt")

;;--------------------------------------------------------------
;;  Top-level calls
;;
;; - online-fcl takes a list giving the concrete syntax of an FCL
;;     program, a list of static parameters, and a list of values
;;     for static parameters.  It is assumed that these two
;;     lists have the same length.
;;
;; - online-fcl-file takes the program argument from a file
;;     and writes residual program to a file.
;;
;;
;;  Example call: specialize power program to n = 2.
;;
;;  > (define power (file->value "examples/power.fcl"))
;;  > (online power '(n) '(2))
;;
;;  > (online-file "examples/power.fcl"
;;                 '(n)
;;                 '(2)
;;                 "outputs/power-2.fcl")
;;
;;--------------------------------------------------------------

(define (online prog static-params static-vals)
  (pretty-print
   (unparse-program
    (online-prog
     (parse-program prog) static-params static-vals))))

(define (online-file in-file-name static-params static-vals out-file-name)
  (define prog (file->value in-file-name))
  (define result (unparse-program (online-prog (parse-program prog)
                                               static-params
                                               static-vals)))
  (pretty-print->file out-file-name result)
  (pretty-print result))

;;--------------------------------------------------------------
;;  make-initial store
;;
;;  First makes a store where all program variables have
;;  static value init-store-val.  Then initializes all
;;  static parameter values, then initializes all
;;  dynamic parameter values to dynamic
(define (make-store vars s-params s-vals d-params)
  (define s 
    (hash-set-kv* (hash-init vars (static '())) s-params (map static s-vals)))
  (hash-set-kv* s d-params (map (λ (v) (dynamic (varref v))) d-params)))

;;--------------------------------------------------------------
;;  Online PE Procedures
;;
;;  There is one procedure for each syntactic category in FCL.
;;  The structure closely follows the operational semantics
;;  definition.
;;--------------------------------------------------------------

;;(prog x (static)vars x (static)vals) -> (residual)prog
(define (online-prog prog s-params s-vals)
  (let* (;;
         ;; get the program pieces
         (params          (program-params prog))
         (blocks          (program-blocks prog))
         ;; set up to form initial store
         (vars            (collect-vars prog))   
         (d-params        (remove* s-params params))
         (init-store      (make-store vars s-params s-vals d-params))
         ;;
         ;; make initial state
         (init-state      (state (program-init-label prog) init-store))
         (_               (state->label-reset))
         (res-init-label  (state->label init-state))
         ;; make blockmap, pending list, seen set
         (blockmap        (hash-kv (map block-label blocks) blocks))
         (pending         (list init-state))
         (seen            '())
         ;; Do Online PE transitions until pending list is empty.
         ;; We get residual blocks as a result.
         (res-blocks (transitions blockmap pending seen '())))
    (program d-params res-init-label (reverse res-blocks))))

;; the main loop
(define (transitions blockmap pending seen blocks)
  (define (loop pending seen blocks)
    (if (empty? pending)
        blocks
        (let* ([state      (car pending)]
               [pending1   (cdr pending)]
               [lbl        (state-label state)]
               [store      (state-store state)]
               [temp       (online-debug state pending1 seen blocks)])
          (if (member state seen)
              (loop pending1 seen blocks)
              (let*-values 
                  ([(states block) (online-block (hash-ref blockmap lbl) store)]
                   [(non-halts)    (filter-not halt-state? states)]
                   [(pending2)     (add-pending* non-halts pending1)]
                   [(seen2)        (add-set state seen)])
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
  (let* ((var    (assign-var asg))
         (exp    (assign-exp asg))
         (pe-val (online-exp exp store)))
    (match pe-val
      [(static obj)
       (values (hash-set store var pe-val) '())]
      [(dynamic obj)
       (values (hash-set store var (dynamic (varref var)))
               (list (assign var obj)))]
      [_ (error "Invalid pe-value in online-assign: " pe-val)])))


;;(jump x store) -> (labels x jump)
;; todo - return values
(define (online-jump jump store)
  (match jump
    [(goto label) (values (list label)
                          (goto (state->label 
                                 (state label
                                        store))))]
    [(return exp) (let* ((pe-val (online-exp exp store))
                         (exp1   (lift pe-val)))
                    (values (list (halt '()))
                            (return exp1)))]
    [(if-jump exp then-label else-label)
     (let ((pe-val (online-exp exp store)))
       (match pe-val
         [(static obj)
          (if obj
              (let ((new-then-label (state->label
                                     (state
                                      then-label
                                      store))))
                (values (list then-label)
                        (goto new-then-label)))
              (let ((new-else-label (state->label 
                                     (state
                                      else-label
                                      store))))
                (values (list else-label)
                        (goto new-else-label))))]
         [(dynamic obj)
          (let ((new-then-label (state->label 
                                 (state
                                  then-label
                                  store)))
                (new-else-label (state->label
                                 (state
                                  else-label
                                  store))))
            (values (list then-label else-label)
                    (if-jump obj
                             new-then-label
                             new-else-label)))]
         (else (error "Unrecognized pe-val in online-jump: " pe-val))))]
    [else (error "Unrecognized jump in online-jump: " jump)]))

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


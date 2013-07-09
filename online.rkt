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

(require "parse.rkt")             ;; parsing procedures
(require "lib-fcl-shared.rkt")    ;; misc helper procedures
(require "lib-table.rkt")         ;; table procedures (for store, blockmap)
(require "lib-pending.rkt")       ;; pending list 
(require "lib-set.rkt")           ;; set operations (for seen set)
(require "eval.rkt")
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
;;  > (define power (get-file-object "examples/power.fcl"))
;;  > (online power '(n) '(2))
;;
;;  > (online-file "examples/power.fcl"
;;                 '(n)
;;                 '(2)
;;                 "outputs/power-2.fcl")
;;
;;  Note: get-file-object is in lib-fcl-shared.sc0m
;;
;;--------------------------------------------------------------

(define online
  (lambda (prog static-params static-vals)
    (pretty-print
     (unparse-program
      (online-prog
       (parse-program prog) static-params static-vals)))))

(define online-file
  (lambda (in-file-name static-params static-vals out-file-name)
    (let* ((prog   (get-file-object in-file-name))
           (result (unparse-program (online-prog (parse-program prog)
                                                 static-params
                                                 static-vals))))
      (begin
        (put-file-object out-file-name result)
        (pretty-print result)))))

;;--------------------------------------------------------------
;;  make-initial store
;;
;;  First makes a store where all program variables have
;;  static value init-store-val.  Then initializes all
;;  static parameter values, then initializes all
;;  dynamic parameter values to dynamic, i.e.,  (D, var-name).
;;
;;--------------------------------------------------------------

(define make-init-store
  (lambda (vars static-params static-vals dynamic-params)
    (let* ((store          (hash-init vars
                                             (static init-store-val)))
           (store-w/sta    (hash-set-kv* store static-params
                                          (map static static-vals)
                                          ))
           (store-w/sta/dyn (hash-set-kv* store-w/sta dynamic-params
                                           (map (lambda (var)
                                                  (dynamic
                                                   (varref var)))
                                                dynamic-params)
                                           )))
      store-w/sta/dyn)))



;;--------------------------------------------------------------
;;  Online PE Procedures
;;
;;  There is one procedure for each syntactic category in FCL.
;;  The structure closely follows the operational semantics
;;  definition.
;;--------------------------------------------------------------

;;(prog x (static)vars x (static)vals) -> (residual)prog
(define online-prog
  (lambda (prog static-params static-vals)
    (let* (;;
           ;; get the program pieces
           ;;
           (params          (program-params prog))
           (init-label      (program-init-label prog))
           (blocks     (program-blocks prog))
           ;;
           ;; set up to form initial store
           ;;  ..collect-vars is defined in parse.scm
           ;;  it retreives all variables (including parameters)
           ;;  used in a program
           ;;
           (vars            (collect-vars prog))   
           (dynamic-params  (diff-set params static-params))
           (init-store      (make-init-store vars
                                             static-params
                                             static-vals
                                             dynamic-params))
           ;;
           ;; make initial state
           ;;  ..see lib-fcl-shared.scm for a discussion of
           ;;  state-to-label! (it's non-trivial)
           ;;
           (init-state      (state init-label init-store))
           (tmp             (state->label-reset))
           (res-init-label  (state->label init-state))
           ;;
           ;; make blockmap, pending list, seen set
           ;;
           (blockmap (hash-kv (map block-label blocks)
                                    blocks
                                    ))
           (pending         (add-pending init-state empty-pending))
           (seen            empty-set)
           ;;
           ;; Do Online PE transitions until pending list is empty.
           ;; We get residual blocks as a result.  Then make the 
           ;; residual program from the residual blocks
           ;;
           (res-blocks (compute-transitions
                        blockmap
                        pending
                        seen
                        '())))
      (program
       dynamic-params
       res-init-label
       (reverse res-blocks)))))


;;(state x blockmap) -> value
(define compute-transitions
  (lambda (blockmap pending seen res-blocks)
    (letrec ((transition
              (lambda (pending seen res-blocks)
                (if (not (empty-pending? pending))
                    (let* ((state/new-pending (remove-pending pending))
                           (state             (car state/new-pending))
                           (new-pending       (cdr state/new-pending))
                           ;; debugging (see bottom of file for def)
                           (temp              (online-debug state
                                                            new-pending
                                                            seen
                                                            res-blocks)))
                      (if (not (in-set? state seen))
                          ;; not seen, so set up for next transition
                          (let* ((new-states/res-block
                                  (online-block
                                   (hash-ref blockmap (state-label state))
                                   (state-store state)))
                                 (new-states (car new-states/res-block))
                                 (res-block  (cdr new-states/res-block)))
                            ;; next transition with
                            ;;   ...non-halt states in pending list
                            ;;   ...previous stated added to seen
                            ;;   ...previous residual block added on
                            (transition (add-pending* (filter-not
                                                       halt-state?
                                                       new-states)
                                                      new-pending)
                                        (add-set state seen)
                                        (cons res-block res-blocks)))
                          ;; if state was seen before, don't process
                          (transition new-pending
                                      seen
                                      res-blocks)))
                    ;; if pending list is empty, return residual blocks.
                    res-blocks))))
      (transition pending seen res-blocks))))

;;(block x store) -> (states x block)
(define online-block
  (lambda (bl store)
    (let* ((new-store/res-assigns (online-assigns (block-assigns bl)
                                                  store))
           (new-store   (car new-store/res-assigns))
           (res-assigns (cdr new-store/res-assigns))
           (new-labels/res-jump (online-jump (block-jump bl) new-store))
           (new-labels  (car new-labels/res-jump))
           (res-jump    (cdr new-labels/res-jump)))
      (cons (map (lambda (label)
                   (state label new-store))
                 new-labels)
            (block (state->label 
                                    (state
                                     (block-label bl)
                                     store))
                   res-assigns
                   res-jump)))))

;;(assigns x store) -> (store x assigns)
(define online-assigns
  (lambda (assigns store)
    (if (null? assigns)
        (cons store '())
        (let* ( [store!/assigns!   (online-assign (car assigns) store)]
                [store!            (car store!/assigns! )]
                [assigns!          (cdr store!/assigns!)]
                (store!!/assigns!! (online-assigns (cdr assigns) store!))
                (store!!           (car store!!/assigns!!))
                (assigns!!         (cdr store!!/assigns!!)))
          (cons store!! (append assigns! assigns!!))))))


;;(assign x store) -> (store x assigns)
(define online-assign
  (lambda (asg store)
    (let* ((var    (assign-var asg))
           (exp    (assign-exp asg))
           (pe-val (online-exp exp store)))
      (match pe-val
        [(static obj)
         (cons (hash-set store var pe-val)
               '())]
        [(dynamic obj)
         (cons (hash-set store var
                             (dynamic (varref var)))
               (list (assign var
                             obj)))]
        [else (error "Invalid pe-value in online-assign: " pe-val)]))))


;;(jump x store) -> (labels x jump)
(define online-jump
  (lambda (jump store)
    (match jump
      [(goto label) (cons (list label)
                          (goto (state->label 
                                                 (state label
                                                        store))))]
      [(return exp) (let* ((pe-val (online-exp exp store))
                           (exp1   (lift pe-val)))
                      (cons (list (halt '()))
                            (return exp1)))]
      [(if-jump exp then-label else-label)
       (let ((pe-val (online-exp exp store)))
         (match pe-val
           [(static obj)
            (if (is-true? obj)
                (let ((new-then-label (state->label
                                                       (state
                                                        then-label
                                                        store))))
                  (cons (list then-label)
                        (goto new-then-label)))
                (let ((new-else-label (state->label 
                                                       (state
                                                        else-label
                                                        store))))
                  (cons (list else-label)
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
              (cons (list then-label else-label)
                    (if-jump obj
                             new-then-label
                             new-else-label)))]
           (else (error "Unrecognized pe-val in online-jump: " pe-val))))]
      [else (error "Unrecognized jump in online-jump: " jump)])))

;;(exp x store) -> pe-val
(define online-exp
  (lambda (exp store)
    (match exp
      [(const datum) (static datum)]
      [(varref var)  (hash-ref store var)]
      [(app op exps) (online-op op (map (lambda (exp)
                                          (online-exp exp store))
                                        exps))]
      [else (error "Unrecognized exp in online-exp: " exp)])))

(define all-static?
  (lambda (pe-vals)
    (not (memq #f (map static? pe-vals)))))

;;(op x pe-vals) -> pe-val
(define online-op
  (lambda (op pe-vals)
    (if (all-static? pe-vals)
        (static (eval-op op (map pe-val->object pe-vals)))
        (dynamic (app op (map lift pe-vals))))))

;; pe-val -> exp     
(define lift
  (lambda (pe-val)
    (match pe-val
      [(static obj)
       (const obj)]
      [(dynamic obj)
       obj]
      [else (error "Unrecognized pe-val in lift: " pe-val)])))


;;------------------------------------------------------------
;;  Debugging and Tracing
;;
;;    -- assumes the the store, pending, seen, and res-block
;;    structures can be printed directly.
;;------------------------------------------------------------

(define debug-level 0)
;; 0 = no debug info
;; > 0 = print out contents of state on each transition
;; > 1 = print out above plus pending and seen
;; > 2 = print out above plus residual blocks
;; > 3 = print out above plus label management table
(define online-debug
  (lambda (state pending seen res-blocks)
    (if (> debug-level 0)
        (begin
          (display "-------------------------------")
          (newline)
          (display "LABEL: ")
          (pretty-print (state-label state))
          (newline)
          (display "STORE: ")
          (pretty-print (state-store state))
          (newline)
          (if (> debug-level 1)
              (begin
                (display "...............")
                (newline)
                (display "Pending: ")
                (newline)
                (pretty-print pending)
                (newline)
                (newline)
                (display "...............")
                (newline)
                (display "Seen: ")
                (newline)
                (pretty-print seen)
                (newline)
                (if (> debug-level 2)
                    (begin
                      (display "...............")
                      (newline)
                      (display "Residual blocks: ")
                      (newline)
                      (pretty-print (reverse res-blocks))
                      (newline)
                      (newline))
                    '()))
              '()))
        '())))


#lang racket
;;====================================================================
;;
;; File: pe-offline.scm
;; Author: John Hatcliff and Shawn Laubach
;; Last modified: March 1, 1999
;;
;;
;;  Implements an offline partial evaluator for FCL.
;;  The first part of the file contains the binding-time analysis
;;   (consisting of division computing + annotation).
;;  The second part of the file is the specializer.
;;
;;====================================================================

(provide offline-prog)

(require "parse.rkt" "eval.rkt" "pe.rkt" "util.rkt")

;;----------------------------------------------------------------
;; Two-level syntax
;;
;;   For the two-level syntax, the we will reuse the conventional
;;   AST as the eliminable constructs.  So here we just define
;;   new AST records for the residual (underlined) constructs.
;;
;;----------------------------------------------------------------

(struct d-assign (var exp) #:transparent)
(struct d-const  (datum) #:transparent)
(struct d-varref (var) #:transparent)
(struct d-app    (op exps) #:transparent)
(struct d-goto   (label) #:transparent)
(struct d-if     (exp then-label else-label) #:transparent)
(struct d-return (exp) #:transparent)
(struct lift     (exp) #:transparent)


;;--------------------------------------------------------------
;;  Top-level calls  
;;
;;  Currently, these work just like the online PE.  Specifically,
;;  one does not pass a binding-time signature as an argument
;;  to the PE.  Instead, it is inferred from the input.  Also,
;;  annotated programs are only held as intermediate data structures
;;  --- they are not written to a file.
;;
;;  As a result of this, the current implementation is monolithic.
;;  That is, there is no user-friendly way to run the BTA and
;;  specializer as separate phases.  Of course, it is relatively
;;  easy to do this by calling some of the lower level procedures
;;  directly.
;;
;; - offline-fcl takes a list giving the concrete syntax of an FCL
;;     program, a list of static parameters, and a list of values
;;     for static parameters.  It is assumed that these two
;;     lists have the same length.
;;
;; - offline-fcl-file takes the program argument from a file
;;     and writes residual program to a file.
;;
;;
;;  Example call: specialize power program to n = 2.
;;
;;  > (define power (get-file-object "examples/power.fcl"))
;;  > (offline power '(n) '(2))
;;
;;  > (offline-file "examples/power.fcl"
;;                 '(n)
;;                 '(2)
;;                 "outputs/power-2.fcl")
;;
;;  Note: get-file-object is in lib-fcl-shared.scm
;;
;;  Note: some input parameters that were originally declared as
;;  static may become dynamic in order to obtain a uniform division.
;;  This complicates setting up the initial store and final residual
;;  program somewhat.
;;
;;  There are several solutions for this problem.  Currently,
;;  if some of the static parameters do become dynamic, we
;;  create a special initial block in the residual program.  This block
;;  contains assignments that assignment each formally static parameter
;;  with the static value given when the PE was run.  This is
;;  implemented by make-lift-block.
;; 
;;--------------------------------------------------------------

(define offline
  (lambda (prog static-params static-vals)
    (pretty-print
     (unparse-program
      (offline-prog
       (parse-program prog) static-params static-vals)))))

(define offline-file
  (lambda (in-file-name static-params static-vals out-file-name)
    (let* ((prog   (file->value in-file-name))
           (result (unparse-program (offline-prog (parse-program prog)
                                                  static-params
                                                  static-vals))))
      (begin
        (pretty-print->file out-file-name result)
        (pretty-print result)))))

;;====================================================================
;;
;;  Binding-time analysis
;;
;;  Binding-time types S and D are represented using the static/dynamic
;;  records defined in lib-fcl-shared.scm.  A bt type S, is created by
;;
;;   (make-static ())
;;
;;  Similarly, for D.
;;  The point is that we ignore the field of the 'static' record
;;  (we give it the dummy value ()).

;;====================================================================


(define bta
  (lambda (prog init-div)
    (let* ((div    (div-prog prog init-div))
           (prog-2 (ann-prog prog div)))
      (cons div prog-2))))


;;================================================================
;;
;;  Computing a division
;;
;;================================================================

;;
;; Computes a congruent uniform division
;;

;(prog x div) -> div
; ...takes an initial division...loops until no changes
(define div-prog
  (lambda (prog div)
    (let ((blocks (program-blocks prog)))
      (letrec ((loop (lambda (div)
                       (let ((new-div (div-blocks blocks div)))
                         (if (equal? new-div div)
                             new-div              ;; no change, so return
                             (loop new-div))))))  ;; change, so continue
        (loop div)))))

;(blocks x div) -> div
(define div-blocks
  (lambda (blocks div)
    (if (null? blocks)
        div
        (let ((new-div (div-block (car blocks) div)))
          (div-blocks (cdr blocks) new-div)))))

;(block x div) -> div
(define div-block
  (lambda (block div)
    (div-assigns (block-assigns block) div)))
;; note: no need to look at jumps (only assignments affect division)

;(assigns x div) -> div
(define div-assigns
  (lambda (assigns div)
    (if (null? assigns)
        div
        (let ((new-div (div-assign (car assigns) div)))
          (div-assigns (cdr assigns) new-div)))))

;(assign x div) -> div
(define div-assign
  (lambda (assign div)
    (let* ((var         (assign-var assign))
           (var-bt-type (hash-ref div var))
           (exp-bt-type (div-exp (assign-exp assign)
                                 div)))
      (hash-set div
                var
                (bt-type-lub var-bt-type exp-bt-type)
                ))))

;(bt-type x bt-type) -> bt-type
;;  computes least upper bound on bt-types (order S < D)
(define bt-type-lub
  (lambda (bt-type1 bt-type2)
    (if (and (static? bt-type1)
             (static? bt-type2))
        (static '())
        (dynamic '()))))


;bt-types -> bt-type
;;  computes supremum on list of bt-types (order S < D)
(define bt-type-lub*
  (lambda (bt-types)
    (foldr bt-type-lub (static '()) bt-types)))

;(exp x div) -> bt-type
(define div-exp
  (lambda (exp div)
    (match exp
      [(const datum) (static '())]
      [(varref var) (hash-ref div var)]
      [(app op exps) (div-op op exps div)]
      [else (error "Unrecognized exp in div-exp: " exp)])))


;(op x args x div) -> bt-type
; ...assumes no partially static structures
(define div-op
  (lambda (op exps div)
    (bt-type-lub* (map (lambda (exp) (div-exp exp div)) exps))))


;;================================================================
;;
;;  Annotating to form a two-level program
;;
;;================================================================

(define ann-object-untag
  (lambda (tagged-exp-2)
    (match tagged-exp-2
      [(static obj) obj]
      [(dynamic obj) obj]
      [else (error "Unrecognized pe-val in ann-object-untag: "
                   tagged-exp-2)])))

(define all-static?
  (lambda (ann-objects)
    (not (memq #f (map static? ann-objects)))))

;;(prog x args) -> prog-2
(define ann-prog
  (lambda (prog div)
    (let* ((blocks     (program-blocks prog))
           (blocks-2   (map (lambda (block) (ann-block block div))
                            blocks)))
      (program (program-params prog)
               (program-init-label prog)
               blocks-2))))

;; (block x store) -> block-2
(define ann-block
  (lambda (blk div)
    (let* ((assigns-2 (ann-assigns (block-assigns blk) div))
           (jump-2    (ann-jump (block-jump blk) div)))
      (block (block-label blk)
             assigns-2
             jump-2))))


;;(assigns x div) -> assigns-2
(define ann-assigns
  (lambda (assigns div)
    (if (null? assigns)
        '()
        (let ((assign-2  (ann-assign  (car assigns) div))
              (assigns-2 (ann-assigns (cdr assigns) div)))
          (cons assign-2 assigns-2)))))

;;(assign x div) -> assign-2
(define ann-assign
  (lambda (asg div)
    (let* ((var          (assign-var asg))
           (exp          (assign-exp asg))
           (tag          (hash-ref div var))
           (tagged-exp-2 (ann-exp exp div)))
      (match tag
        [(static  '()) (assign   var (ann-object-untag tagged-exp-2))]
        [(dynamic '()) (d-assign var (ann-lift tagged-exp-2))]
        [else (error "Unrecognized tag in ann-assign: " tag)]))))

;;(jump x div) -> jump-2
(define ann-jump
  (lambda (jump div)
    (match jump
      [(goto label) (d-goto label)]
      [(return exp) (d-return (ann-lift (ann-exp exp div)))]
      [(if-jump exp then-label else-label)
       (let ((tagged-exp-2 (ann-exp exp div)))
         (match tagged-exp-2
           [(static  obj) (if-jump   obj then-label else-label)]
           [(dynamic obj) (d-if obj then-label else-label)]
           (else (error "Unrecognized tagged exp-2 in ann-jump: "
                        tagged-exp-2))))]
      [else (error "Unrecognized jump in ann-jump: " jump)])))

;;(exp x div) -> tagged exp-2
(define ann-exp
  (lambda (exp div)
    (match exp
      [(const datum) (static (const datum))]
      [(varref var) (let ((tag (hash-ref div var)))
                      (match tag
                        [(static  '()) (static  (varref var))]
                        [(dynamic '()) (dynamic (d-varref var))]
                        [else (error "Unrecognized tag in ann-exp: "
                                     tag)]))]
      [(app op exps) (let ((tagged-exp-2s (map (lambda (exp)
                                                 (ann-exp exp div))
                                               exps)))
                       (if (all-static? tagged-exp-2s)
                           (static
                            (app   op (map ann-object-untag
                                           tagged-exp-2s)))
                           (dynamic
                            (d-app op (map ann-lift
                                           tagged-exp-2s)))))]
      [else (error "Unrecognized exp in ann-exp: " exp)])))

(define ann-lift
  (lambda (tagged-exp-2)
    (match tagged-exp-2
      [(static obj)
       (match obj
         [(const datum) (d-const datum)]
         [else (lift obj)])] ;; TODO: was: (lift exp-2), so maybe we need to have (lift tagged-exp-2) here
      [(dynamic obj) obj]
      [else (error "Unrecognized tagged-exp-2 in ann-lift: "
                   tagged-exp-2)])))


;;================================================================
;;
;;  Offline specializer
;;
;;================================================================

;;(prog x (static)vars x (static)vals) -> (residual)prog
(define offline-prog
  (lambda (prog static-params static-vals)
    (let* (;;
           ;; split program into pieces
           ;;
           (params          (program-params prog))
           (init-label      (program-init-label prog))
           ;;
           ;; make initial division
           ;;
           (dynamic-params  (remove* static-params params))
           (vars            (collect-vars prog))
           (init-div        (make-init-div vars
                                           static-params
                                           dynamic-params))
           ;;
           ;; binding-time analysis returns division and two-level prog
           ;;
           (div/prog-2      (bta prog init-div))
           (div             (car div/prog-2))
           (prog-2          (cdr div/prog-2))
           (blocks-2        (program-blocks prog-2))
           ;;
           ;; set up to make initial store
           ;;
           (dynamic-vars    (hash-keys (hash-filter-by-val div dynamic?)))
           (static-vars     (remove* dynamic-vars vars))
           (param-store     (hash-kv static-params
                                     static-vals
                                     ))
           (lifted-params   (intersect static-params dynamic-vars))
           ;;
           ;; make initial store, state, blockmap, pending, seen structures
           ;;
           (init-store      (make-init-store static-vars
                                             static-params
                                             param-store))
           (init-state      (state init-label init-store))
           (tmp             (state->label-reset))
           (res-init-label  (state->label init-state))
           (blockmap (hash-kv (map block-label blocks-2)
                              blocks-2
                              ))
           (pending         (list init-state))
           (seen            '())
           ;;
           ;;  run offline pe transitions until done, res blocks returned
           ;;
           (res-blocks (compute-transitions
                        blockmap
                        pending
                        seen
                        '())))
      ;;
      ;; If any static parameters were made dynamic, then we
      ;; must call make-lift-block to make the special block
      ;; of assignments. Else, we make the residual program as usual.
      ;;
      (if (null? lifted-params)
          (program
           dynamic-params
           res-init-label
           (reverse res-blocks))
          (let ((lift-block (make-lift-block lifted-params
                                             param-store
                                             res-init-label)))
            (program
             dynamic-params
             (block-label lift-block)
             (cons lift-block (reverse res-blocks))))))))

(define make-lift-block
  (lambda (lifted-params param-store res-init-label)
    (letrec ((make-assigns
              (lambda (vars)
                (if (null? vars)
                    '()
                    (cons (assign (car vars)
                                  (const (hash-ref
                                          param-store
                                          (car vars))))
                          (make-assigns (cdr vars)))))))
      (block
       'system-block
       (make-assigns lifted-params)
       (goto res-init-label)))))



;; notice how this parallels make initial store in online case
(define make-init-div
  (lambda (vars static-params dynamic-params)
    (let* ((div-w/sta     (hash-init vars (static '())))
           (div-w/sta/dyn (hash-set-kv* div-w/sta dynamic-params
                                        (map (lambda (var)
                                               (dynamic '()))
                                             dynamic-params)
                                        )))
      div-w/sta/dyn)))


(define get-dynamic-vars
  (lambda (vars div)
    (foldr 
     (lambda (var dyn-vars)
       (if (dynamic? (hash-ref div var))
           (cons var dyn-vars)
           dyn-vars)) '()
                      vars)))

(define make-init-store
  (lambda (static-vars static-params param-store)
    (hash-kv static-vars 
             (map (lambda (var)
                    (if (memq var static-params)
                        (hash-ref
                         param-store
                         var)
                        init-val))
                  static-vars)
             )))

;; -- note identical to online
;;(state x blockmap) -> value
(define compute-transitions
  (lambda (blockmap pending seen res-blocks)
    (letrec ((transition
              (lambda (pending seen res-blocks)
                (if (not (empty? pending))
                    (let* ((state/new-pending pending)
                           (state             (car state/new-pending))
                           (new-pending       (cdr state/new-pending))
                           (temp              (offline-debug state
                                                             new-pending
                                                             seen
                                                             res-blocks)))
                      (if (not (member state seen))
                          (let* ((new-states/res-block
                                  (offline-block
                                   (hash-ref blockmap (state-label state))
                                   (state-store state)))
                                 (new-states (car new-states/res-block))
                                 (res-block  (cdr new-states/res-block)))
                            (transition (add-pending* (filter-not
                                                       halt-state?
                                                       new-states)
                                                      new-pending)
                                        (add-set state seen)
                                        (cons res-block res-blocks)))
                          (transition new-pending
                                      seen
                                      res-blocks)))
                    res-blocks))))
      (transition pending seen res-blocks))))

;; identical to online 
;;(block x store x blocks x assign code) -> intermediate code
(define offline-block
  (lambda (blk store)
    (let* ((new-store/res-assigns (offline-assigns (block-assigns blk)
                                                   store))
           (new-store   (car new-store/res-assigns))
           (res-assigns (cdr new-store/res-assigns))
           (new-labels/res-jump (offline-jump (block-jump blk) new-store))
           (new-labels  (car new-labels/res-jump))
           (res-jump    (cdr new-labels/res-jump)))
      (cons (map (lambda (label)
                   (state label new-store))
                 new-labels)
            (block (state->label
                    (state
                     (block-label blk)
                     store))
                   res-assigns
                   res-jump)))))

;; --- note identical to online
;;(assigns x store) -> (store x assigns)
(define offline-assigns
  (lambda (assigns store)
    (if (null? assigns)
        (cons store '())
        (let* ((store1/assigns1   (offline-assign (car assigns) store))
               (store1            (car store1/assigns1))
               (assigns1          (cdr store1/assigns1))
               (store2/assigns2   (offline-assigns (cdr assigns) store1))
               (store2            (car store2/assigns2))
               (assigns2          (cdr store2/assigns2)))
          (cons store2 (append assigns1 assigns2))))))


;;(assign-2 x store) -> (store x assigns)
(define offline-assign
  (lambda (assign-2 store)
    (match assign-2
      [(assign var exp)
       (cons (hash-set store var (offline-exp exp store))
             '())]
      [(d-assign var exp)
       (cons store
             (list (assign var (offline-exp exp store))))]
      [else (error "Invalid assign-2 in offline-assign: " assign-2)])))

;;(jump-2 x store) -> (labels x jump)
(define offline-jump
  (lambda (jump-2 store)
    (match jump-2
      [(d-goto label) (cons (list label)
                            (goto (state->label
                                   (state label
                                          store))))]
      [(d-return exp) (let* ((exp1 (offline-exp exp store)))
                        (cons (list (halt '()))
                              (return exp1)))]
      [(if-jump exp then-label else-label)
       (let ((val (offline-exp exp store)))
         (cond
           (val
            (let ((new-then-label (state->label
                                   (state then-label
                                          store))))
              (cons (list then-label)
                    (goto new-then-label))))
           ((not val)
            (let ((new-else-label (state->label
                                   (state else-label
                                          store))))
              (cons (list else-label)
                    (goto new-else-label))))
           (else (error "Invalid exp val in offline-jump: " val))))]
      [(d-if exp then-label else-label)
       (let ((new-then-label (state->label
                              (state then-label
                                     store)))
             (new-else-label (state->label
                              (state else-label
                                     store)))
             (exp'           (offline-exp exp store)))
         (cons (list then-label else-label)
               (if-jump exp'
                        new-then-label
                        new-else-label)))]
      [else (error "Incorrect jump in offline-jump: " jump-2)])))


;;(exp-2 x store) -> pe-val
(define offline-exp
  (lambda (exp-2 store)
    (match exp-2
      [(const datum) datum]
      [(d-const datum) (const datum)]
      [(varref var)  (hash-ref store var)]
      [(d-varref var) (varref var)]
      [(app op exps) (eval-op op (map (lambda (exp)
                                        (offline-exp exp store))
                                      exps))]
      [(d-app op exps) (app op (map (lambda (exp)
                                      (offline-exp exp store))
                                    exps))]
      ;; lift only works for numbers now
      [(lift exp) (const (offline-exp exp store))]
      [else (error "Unrecognized exp in offline-exp: " exp)])))


;;------------------------------------------------------------
;;  Debugging and Tracing
;;
;;    -- assumes the the store, pending, seen, and res-block
;;    structures can be printed directly.
;;
;;   (this is currently identical to online)
;;------------------------------------------------------------

(define debug-level 0)
;; 0 = no debug info
;; > 0 = print out contents of state on each transition
;; > 1 = print out above plus pending and seen
;; > 2 = print out above plus residual blocks
;; > 3 = print out above plus label management table
(define offline-debug
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


;;------------------------------------------------------------
;;
;; Other debugging stuff
;;
;;  Use these procedures to see the results of computing a division, and
;;  the results of annotation.
;;
;;  They should be called with parsed programs. E.g.,
;;
;;   > (define power (get-file-object "examples/power.fcl"))
;;
;;   > (offline-debug-div (parse power) '(n))
;;
;;   > (offline-debug-ann (parse power) '(n))
;;------------------------------------------------------------

(define offline-debug-div
  (lambda (prog static-params)
    (let* ((params          (program-params prog))
           (dynamic-params  (remove* static-params params))
           (vars            (collect-vars prog))
           (init-div        (make-init-div vars
                                           static-params
                                           dynamic-params))
           (div             (div-prog prog init-div)))
      (pretty-print div))))

(define offline-debug-ann
  (lambda (prog static-params)
    (let* ((params          (program-params prog))
           (dynamic-params  (remove* static-params params))
           (vars            (collect-vars prog))
           (init-div        (make-init-div vars
                                           static-params
                                           dynamic-params))
           (div             (div-prog prog init-div))
           (ann-prog        (ann-prog prog div)))
      (pretty-print ann-prog))))

;(define power (get-file-object "examples/power.fcl"))
;(offline power '(n) '(2))

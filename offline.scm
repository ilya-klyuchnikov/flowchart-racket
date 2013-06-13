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

(require 'struct)

(load "parse.scm")
(load "lib-fcl-shared.scm")    ;; misc helper procedures
(load "lib-table.scm")         ;; table procedures (for store, blockmap)
(load "lib-pending.scm")       ;; pending list 
(load "lib-set.scm")           ;; set operations (for seen set)

;;----------------------------------------------------------------
;; Two-level syntax
;;
;;   For the two-level syntax, the we will reuse the conventional
;;   AST as the eliminable constructs.  So here we just define
;;   new AST records for the residual (underlined) constructs.
;;
;;----------------------------------------------------------------

(define-record d-assign (var exp))
(define-record d-const  (datum))
(define-record d-varref (var))
(define-record d-app    (op exps))
(define-record d-goto   (label))
(define-record d-if     (exp then-label else-label))
(define-record d-return (exp))
(define-record lift     (exp))


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
      (unparse
	(offline-prog
	  (parse prog) static-params static-vals)))))

(define offline-file
  (lambda (in-file-name static-params static-vals out-file-name)
    (let* ((prog   (get-file-object in-file-name))
	   (result (unparse (offline-prog (parse prog)
					  static-params
					  static-vals))))
      (begin
	(put-file-object out-file-name result)
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
    (let ((blocks (program->blocks prog)))
      (letrec ((loop (lambda (div)
		       (let ((new-div (div-blocks blocks div)))
			 (if (equal-tables? new-div div)
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
    (div-assigns (block->assigns block) div)))
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
    (let* ((var         (assign->var assign))
	   (var-bt-type (lookup-table var div))
	   (exp-bt-type (div-exp (assign->exp assign)
				 div)))
      (update-table var
		  (bt-type-lub var-bt-type exp-bt-type)
		  div))))

;(bt-type x bt-type) -> bt-type
;;  computes least upper bound on bt-types (order S < D)
(define bt-type-lub
  (lambda (bt-type1 bt-type2)
    (if (and (static? bt-type1)
	     (static? bt-type2))
	(make-static ())
	(make-dynamic ()))))


;bt-types -> bt-type
;;  computes supremum on list of bt-types (order S < D)
(define bt-type-lub*
  (lambda (bt-types)
    ((foldr (make-static ())
	    bt-type-lub)
     bt-types)))


;(exp x div) -> bt-type
(define div-exp
  (lambda (exp div)
    (variant-case exp
      (const (datum) (make-static ()))
      (varref (var) (lookup-table var div))
      (app (op exps) (div-op op exps div))
      (else (error "Unrecognized exp in div-exp: " exp)))))


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
    (variant-case tagged-exp-2
      (static (obj) obj)
      (dynamic (obj) obj)
      (else (error "Unrecognized pe-val in ann-object-untag: "
		   tagged-exp-2)))))

(define all-static?
  (lambda (ann-objects)
    (not (memq #f (map static? ann-objects)))))

;;(prog x args) -> prog-2
(define ann-prog
  (lambda (prog div)
    (let* ((blocks     (program->blocks prog))
	   (blocks-2   (map (lambda (block) (ann-block block div))
			    blocks)))
      (make-program (program->params prog)
		    (program->init-label prog)
		    blocks-2))))

;; (block x store) -> block-2
(define ann-block
  (lambda (block div)
    (let* ((assigns-2 (ann-assigns (block->assigns block) div))
           (jump-2    (ann-jump (block->jump block) div)))
      (make-block (block->label block)
		  assigns-2
		  jump-2))))


;;(assigns x div) -> assigns-2
(define ann-assigns
  (lambda (assigns div)
    (if (null? assigns)
	()
	(let ((assign-2  (ann-assign  (car assigns) div))
	      (assigns-2 (ann-assigns (cdr assigns) div)))
	  (cons assign-2 assigns-2)))))

;;(assign x div) -> assign-2
(define ann-assign
  (lambda (assign div)
    (let* ((var          (assign->var assign))
	   (exp          (assign->exp assign))
	   (tag          (lookup-table var div))
	   (tagged-exp-2 (ann-exp exp div)))
      (variant-case tag
	(static  () (make-assign   var (ann-object-untag tagged-exp-2)))
	(dynamic () (make-d-assign var (ann-lift tagged-exp-2)))
	(else (error "Unrecognized tag in ann-assign: " tag))))))

;;(jump x div) -> jump-2
(define ann-jump
  (lambda (jump div)
    (variant-case jump
      (goto (label) (make-d-goto label))
      (return (exp) (make-d-return (ann-lift (ann-exp exp div))))
      (if (exp then-label else-label)
	  (let ((tagged-exp-2 (ann-exp exp div)))
	    (variant-case tagged-exp-2
	      (static  (obj) (make-if   obj then-label else-label))
	      (dynamic (obj) (make-d-if obj then-label else-label))
	      (else (error "Unrecognized tagged exp-2 in ann-jump: "
			   tagged-exp-2)))))
      (else (error "Unrecognized jump in ann-jump: " jump)))))

;;(exp x div) -> tagged exp-2
(define ann-exp
  (lambda (exp div)
    (variant-case exp
      (const (datum) (make-static (make-const datum)))
      (varref (var) (let ((tag (lookup-table var div)))
		      (variant-case tag
			(static  () (make-static  (make-varref var)))
			(dynamic () (make-dynamic (make-d-varref var)))
			(else (error "Unrecognized tag in ann-exp: "
				     tag)))))
      (app (op exps) (let ((tagged-exp-2s (map (lambda (exp)
						 (ann-exp exp div))
					       exps)))
		       (if (all-static? tagged-exp-2s)
			   (make-static
			     (make-app   op (map ann-object-untag
						 tagged-exp-2s)))
			   (make-dynamic
			     (make-d-app op (map ann-lift
						 tagged-exp-2s))))))
      (else (error "Unrecognized exp in ann-exp: " exp)))))

(define ann-lift
  (lambda (tagged-exp-2)
    (variant-case tagged-exp-2
      (static (obj)
		  (variant-case obj
		    (const (datum) (make-d-const datum))
		    (else (make-lift exp-2))))
      (dynamic (obj) obj)
      (else (error "Unrecognized tagged-exp-2 in ann-lift: "
		   tagged-exp-2)))))


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
	   (params          (program->params prog))
	   (init-label      (program->init-label prog))
	   ;;
	   ;; make initial division
	   ;;
	   (dynamic-params  (diff-set params static-params))
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
	   (blocks-2        (program->blocks prog-2))
	   ;;
	   ;; set up to make initial store
	   ;;
	   (dynamic-vars    (domain-table (restrict-table dynamic? div)))
	   (static-vars     (diff-set vars dynamic-vars))
	   (param-store     (update-table* static-params
					   static-vals
					   empty-table))
	   (lifted-params   (inter-set static-params dynamic-vars))
	   ;;
	   ;; make initial store, state, blockmap, pending, seen structures
	   ;;
	   (init-store      (make-init-store static-vars
					     static-params
					     param-store))
	   (init-state      (make-state init-label init-store))
	   (tmp             (state-to-label! 'reset))
	   (res-init-label  (state-to-label! 'convert init-state))
	   (blockmap (update-table* (map block->label blocks-2)
				    blocks-2
				    empty-table))
	   (pending         (add-pending init-state empty-pending))
	   (seen            empty-set)
	   ;;
	   ;;  run offline pe transitions until done, res blocks returned
	   ;;
	   (res-blocks (compute-transitions
			      blockmap
			      pending
			      seen
			      ())))
      ;;
      ;; If any static parameters were made dynamic, then we
      ;; must call make-lift-block to make the special block
      ;; of assignments. Else, we make the residual program as usual.
      ;;
      (if (null? lifted-params)
	  (make-program
	    dynamic-params
	    res-init-label
	    (reverse res-blocks))
	  (let ((lift-block (make-lift-block lifted-params
					     param-store
					     res-init-label)))
	    (make-program
	      dynamic-params
	      (block->label lift-block)
	      (cons lift-block (reverse res-blocks))))))))
	    
(define make-lift-block
  (lambda (lifted-params param-store res-init-label)
    (letrec ((make-assigns
	       (lambda (vars)
		     (if (null? vars)
			 ()
			 (cons (make-assign (car vars)
					    (make-const (lookup-table
							  (car vars)
							  param-store)))
			       (make-assigns (cdr vars)))))))
      (make-block
	sys-lift-block-label
	(make-assigns lifted-params)
	(make-goto res-init-label)))))
				
			       

;; notice how this parallels make initial store in online case
(define make-init-div
  (lambda (vars static-params dynamic-params)
    (let* ((div-w/sta     (initialize-table vars (make-static ()) empty-table))
	   (div-w/sta/dyn (update-table* dynamic-params
					   (map (lambda (var)
						  (make-dynamic ()))
						dynamic-params)
					   div-w/sta)))
      div-w/sta/dyn)))


(define get-dynamic-vars
  (lambda (vars div)
    ((foldr ()
	    (lambda (var dyn-vars)
	      (if (dynamic? (lookup-table var div))
		  (cons var dyn-vars)
		  dyn-vars)))
     vars)))

(define make-init-store
  (lambda (static-vars static-params param-store)
    (update-table* static-vars 
		   (map (lambda (var)
			  (if (memq var static-params)
			      (lookup-table
				var
				param-store)
			      init-store-val))
			static-vars)
		   empty-table)))
	   
;; -- note identical to online
;;(state x blockmap) -> value
(define compute-transitions
  (lambda (blockmap pending seen res-blocks)
    (letrec ((transition
	       (lambda (pending seen res-blocks)
		 (if (not (empty-pending? pending))
		     (let* ((state/new-pending (remove-pending pending))
			    (state             (car state/new-pending))
			    (new-pending       (cdr state/new-pending))
			    (temp              (offline-debug state
							   new-pending
							   seen
							   res-blocks)))
		       (if (not (in-set? state seen))
			   (let* ((new-states/res-block
				    (offline-block
				      (lookup-table (state->label state)
						    blockmap)
				      (state->store state)))
				  (new-states (car new-states/res-block))
				  (res-block  (cdr new-states/res-block)))
			     (transition (add-pending* (filter
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
  (lambda (block store)
    (let* ((new-store/res-assigns (offline-assigns (block->assigns block)
				     store))
	   (new-store   (car new-store/res-assigns))
	   (res-assigns (cdr new-store/res-assigns))
	   (new-labels/res-jump (offline-jump (block->jump block) new-store))
	   (new-labels  (car new-labels/res-jump))
	   (res-jump    (cdr new-labels/res-jump)))
      (cons (map (lambda (label)
		   (make-state label new-store))
		 new-labels)
	    (make-block (state-to-label! 'convert
					      (make-state
						(block->label block)
						store))
			     res-assigns
			     res-jump)))))

;; --- note identical to online
;;(assigns x store) -> (store x assigns)
(define offline-assigns
  (lambda (assigns store)
    (if (null? assigns)
	(cons store ())
	(let* ((store'/assigns'   (offline-assign (car assigns) store))
	       (store'            (car store'/assigns'))
	       (assigns'          (cdr store'/assigns'))
	       (store''/assigns'' (offline-assigns (cdr assigns) store'))
	       (store''           (car store''/assigns''))
	       (assigns''         (cdr store''/assigns'')))
	  (cons store'' (append assigns' assigns''))))))


;;(assign-2 x store) -> (store x assigns)
(define offline-assign
  (lambda (assign-2 store)
    (variant-case assign-2
      (assign (var exp)
	      (cons (update-table var (offline-exp exp store) store)
		    ()))
      (d-assign (var exp)
	      (cons store
		    (list (make-assign var (offline-exp exp store)))))
      (else (error "Invalid assign-2 in offline-assign: " assign-2)))))

;;(jump-2 x store) -> (labels x jump)
(define offline-jump
  (lambda (jump-2 store)
    (variant-case jump-2
      (d-goto (label) (cons (list label)
			    (make-goto (state-to-label! 'convert
							(make-state label
								    store)))))
      (d-return (exp) (let* ((exp' (offline-exp exp store)))
			(cons (list (make-halt ()))
			      (make-return exp'))))
      (if (exp then-label else-label)
	  (let ((val (offline-exp exp store)))
	    (cond
	      ((is-true? val)
	       (let ((new-then-label (state-to-label! 'convert
						(make-state then-label
							    store))))
		 (cons (list then-label)
		       (make-goto new-then-label))))
	      ((is-false? val)
	       (let ((new-else-label (state-to-label! 'convert
						(make-state else-label
							    store))))
		 (cons (list else-label)
		       (make-goto new-else-label))))
	      (else (error "Invalid exp val in offline-jump: " val)))))
      (d-if (exp then-label else-label)
	  (let ((new-then-label (state-to-label! 'convert
						(make-state then-label
							    store)))
		(new-else-label (state-to-label! 'convert
						(make-state else-label
							    store)))
		(exp'           (offline-exp exp store)))
	    (cons (list then-label else-label)
		  (make-if exp'
			   new-then-label
			   new-else-label))))
      (else (error "Incorrect jump in offline-jump: " jump-2)))))


;; value -> #t | #f
(define is-true?
  (lambda (val)
    (not (equal? val 0))))

;; value -> #t | #f
(define is-false?
  (lambda (val)
    (equal? val 0)))

(define make-FCL-boolean
  (lambda (boolean)
    (if boolean
	1
	0)))

;;(exp-2 x store) -> pe-val
(define offline-exp
  (lambda (exp-2 store)
    (variant-case exp-2
      (const (datum) datum)
      (d-const (datum) (make-const datum))
      (varref (var)  (lookup-table var store))
      (d-varref (var)  (make-varref var))
      (app (op exps) (eval-op op (map (lambda (exp)
					(offline-exp exp store))
				      exps)
			      store))
      (d-app (op exps) (make-app op (map (lambda (exp)
					   (offline-exp exp store))
					 exps)))
      ;; lift only works for numbers now
      (lift (exp) (make-const (offline-exp exp store)))
      (else (error "Unrecognized exp in offline-exp: " exp)))))


;;(op x args x store) -> value 
(define eval-op
  (lambda (op vals store)
    (case op
      ((+) (+ (car vals)
	      (cadr vals)))
      ((-) (- (car vals)
	      (cadr vals)))
      ((*) (* (car vals)
	      (cadr vals)))
      ((/) (/ (car vals)
	      (cadr vals)))
      ((=) (equal? (car vals)
		   (cadr vals)))
      ((<) (make-FCL-boolean (< (car vals) (cadr vals))))
      ((hd) (car (car vals)))
      ((tl) (cdr (car vals)))
      ((cons) (cons (car vals)
		    (cadr vals)))
      ((list) vals)
      ((test) (if (is-true? (car vals))
		  (cadr vals)
		  (caddr vals)))
      (else (error "Undefined operation:" op)))))
	      

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
	  (pretty-print (state->label state))
	  (newline)
	  (display "STORE: ")
	  (pretty-print (state->store state))
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
		      (newline)
		      (if (> debug-level 3)
			  (begin
			    (display "...............")
			    (newline)
			    (display "State-Label Table: ")
			    (newline)
			    (state-to-label! 'print)
			    (newline))
			  ()))
		    ()))
	      ()))
	())))

      
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
    (let* ((params          (program->params prog))
	   (dynamic-params  (diff-set params static-params))
	   (vars            (collect-vars prog))
	   (init-div        (make-init-div vars
					   static-params
					   dynamic-params))
	   (div             (div-prog prog init-div)))
      (pretty-print div))))

(define offline-debug-ann
  (lambda (prog static-params)
    (let* ((params          (program->params prog))
	   (dynamic-params  (diff-set params static-params))
	   (vars            (collect-vars prog))
	   (init-div        (make-init-div vars
					   static-params
					   dynamic-params))
	   (div             (div-prog prog init-div))
	   (ann-prog        (ann-prog prog div)))
      (pretty-print ann-prog))))





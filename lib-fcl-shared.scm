;;====================================================================
;;
;; File: lib-fcl-shared.scm
;; Author: John Hatcliff and Shawn Laubach
;; Last modified: June 28, 1998
;;
;; Contains procedures that are shared between evaluator and
;; partial evaluators, e.g.,
;;
;;   - system constants (initial store value, etc.)
;;   - definitions of state data structures
;;   - procedures for evaluation of primitives
;;   - procedures that simply getting and putting objects to a file
;;   - common list functions (e.g., foldr and friends)
;;   - static/dynamic tags for PE's
;;   - label management for residual programs
;;
;;====================================================================


;;-----------------------------
;;
;; System constants
;;
;;-----------------------------

;;
;; Initial store value
;;
(define init-store-val 0)                 

;;
;; Block name for extra block added in offline PE when some of the
;; static parameters become dynamic when computing division.
;;
(define sys-lift-block-label 'SYS-lift-block)

;;-----------------------------
;;
;; State structure
;;
;;-----------------------------

(define-record state (label store))
(define-record halt (value))

;;
;;  *** warning ** : tremendous shortcut here using equal? to test
;;                   for equality of states.  Changes in representation
;;                   of states, stores, or tables may make this invalid.
;;
(define equal-state? equal?)

(define halt-state?
  (lambda (state)
    (halt? (state->label state))))


;;-----------------------------
;;
;; Evaluation of primitives
;;
;;-----------------------------

;;(op x args) -> value 
(define eval-op
  (lambda (op vals)
      (case op
        ((+) (+ (car vals)
	      (cadr vals)))
	((-) (- (car vals)
		(cadr vals)))
	((*) (* (car vals)
		(cadr vals)))
	((/) (quotient (car vals)
		       (cadr vals)))
	((%) (modulo    (car vals)
		        (cadr vals)))
	((=) (make-FCL-boolean (equal? (car vals)
				       (cadr vals))))
	((<) (make-FCL-boolean (< (car vals) (cadr vals))))
	((>) (make-FCL-boolean (> (car vals) (cadr vals))))
	;;
	;; list stuff
	;;
	((car) (car (car vals)))
	((cdr) (cdr (car vals)))
	((cons) (cons (car vals)
		      (cadr vals)))
	((null) ())
	((list) vals)
	((null?) (make-FCL-boolean (null? (car vals))))
	((pair?) (make-FCL-boolean (pair? (car vals))))
	((list?) (make-FCL-boolean (pair? (car vals))))
	;;
	;; misc
	;;
	((test) (if (is-true? (car vals))
		    (cadr vals)
		    (caddr vals)))
	(else (error "Undefined operation:" op)))))

;;
;; FCL booleans:
;;
;;   true  = non-zero
;;   false = 0
;;

;; converts Scheme boolean to FCL boolean
(define make-FCL-boolean
  (lambda (boolean)
    (if boolean
	1
	0)))

;; value -> #t | #f
(define is-true?
  (lambda (value)
    (not (equal? value 0))))

;; value -> #t | #f
(define is-false?
  (lambda (value)
    (equal? value 0)))

;;-----------------------------
;;
;; List functions
;;
;;-----------------------------

(define foldr
  (lambda (b f)
    (letrec ((loop (lambda (l)
		     (if (null? l)
			 b
			 (f (car l)
			    (loop (cdr l)))))))
      loop)))

(define foldl
  (lambda (b f)
    (letrec ((loop (lambda (l acc)
		     (if (null? l)
			 acc
			 (loop (cdr l)
			       (f (car l) acc))))))
      (lambda (l) (loop l b)))))

(define filter
  (lambda (p l)
    ((foldr () (lambda (a l) (if (p a) l (cons a l)))) l)))


;;-----------------------------
;;
;; PE tags
;;
;;   Note: we will use these tags in several different ways.
;;   So the field name for the tagged object is simply 'obj'
;;-----------------------------

(define-record static (obj))
(define-record dynamic (obj))

(define pe-val->object
  (lambda (pe-val)
    (variant-case pe-val
      (static (obj) obj)
      (dynamic (obj) obj)
      (else (error "Unrecognized pe-val in pe-val->object: " pe-val)))))


;;-----------------------------
;;
;; File I/O
;;
;;   Simply procedures to get and put a single object to a file
;;
;;-----------------------------

(define get-file-object
  (lambda (in-file-name)
    (let* ((in     (open-input-file in-file-name))
	   (obj    (read in)))
      (begin
	(close-input-port in)
	obj))))

(define put-file-object
  (lambda (out-file-name obj)
    (let ((out    (open-output-file out-file-name)))
      (begin
	(pretty-print obj out)
	(close-output-port out)))))

;;-----------------------------
;;
;; Label management
;;
;;   Idea:
;;
;;  It is somewhat difficult for us to use states as labels in Scheme.  So
;;  we will generate unique symbols and use those as labels in the
;;  residual program.  We need a unique label for each state encountered
;;  during PE.  Moreover, we need a mapping between states and generated
;;  labels so we can lookup the appropriate label when the state is
;;  encountered again.
;;
;;  The code below builds a little object with a local variable giving
;;  a "next label number" n, and a local table implementing the
;;  mapping between states and labels.  If we encounter state (foo, ..store..)
;;  then the label foo-n will become associated with the state.
;;
;;  The methods to the object are as follows:
;;
;;  'reset   - resets the local variable counter and clears the local table.
;;  'convert - generates a new label for the given state, and makes the
;;             association in the local table.
;;  'print   - dumps the state table for debugging purposes
;;
;;
;;-----------------------------


(define state-to-label!
  (let ((state-table       ())
	(next-label-number 0))
    (lambda (command . args)
      (case command
	((convert) (let* ((state (car args))
			  (label (state->label state))
			  (in-list (assoc state state-table)))
		     (if in-list
			 (cdr in-list)
			 (let ((new-label (string->symbol
					    (string-append
					      (symbol->string label)
					      "-"
					      (number->string
						next-label-number)))))
			   (begin
			     (set! next-label-number (+ next-label-number 1))
			     (set! state-table (cons (cons state new-label)
						     state-table))
			     new-label)))))
	((print) (pretty-print (reverse state-table)))
	((reset) (begin
		   (set! state-table '())
		   (set! next-label-number 0)))
	(else (error "Bad command to state-to-label-numbers: " command))))))



  

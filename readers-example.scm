(load "eval.scm")
(load "slicing.scm")

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
	((and) (make-FCL-boolean (and (is-true? (car vals))
				      (is-true? (cdr vals)))))
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

(define primitives (append primitives '(and)))

;;(define cri (list
;;	      (make-criterion (make-cfg-state-line 'raise-error '1)
;;			      '())))

(define cri (list
	      (make-criterion (make-cfg-state-line 'check-reqs '1)
			      '())))



(define readers-ast (parse (get-file-object "examples/readers.fcl")))


;(define ex-slice-set
;  '(#(cfg-state-line attempt-stop-write 1)
;   #(cfg-state-line attempt-stop-read 1)
;   #(cfg-state-line attempt-start-read 1)
;   #(cfg-state-line check-reqs 1)
;   #(cfg-state-line attempt-start-write 1)
;   #(cfg-state-line init 2)
;   #(cfg-state-line init 3)
;   #(cfg-state-line next-req 1)
;   #(cfg-state-line next-req 2)
;   #(cfg-state-line start-read 1)
;   #(cfg-state-line stop-read 1)
;   #(cfg-state-line start-write 1)
;   #(cfg-state-line stop-write 1)))

;(define ex-rel-table
;  '((#(cfg-state-line init 1) reqs)
;   (#(cfg-state-line init 2) reqs)
;   (#(cfg-state-line init 3) reqs activereaders)
;   (#(cfg-state-line init 4)
;    reqs
;    activereaders
;    writerpresent)
;   (#(cfg-state-line init 5)
;    reqs
;    activereaders
;    writerpresent)
;   (#(cfg-state-line check-reqs 1)
;    reqs
;    activereaders
;    writerpresent)
;   (#(cfg-state-line next-req 1)
;    reqs
;    activereaders
;    writerpresent)
;   (#(cfg-state-line next-req 2)
;    reqs
;    req
;    activereaders
;    writerpresent)
;   (#(cfg-state-line next-req 3)
;    req
;    reqs
;    activereaders
;    writerpresent)
;   (#(cfg-state-line attempt-start-read 1)
;    req
;    reqs
;    activereaders
;    writerpresent)
;   (#(cfg-state-line start-read 1)
;    activereaders
;    reqs
;    writerpresent)
;   (#(cfg-state-line start-read 2)
;    reqs
;    activereaders
;    writerpresent)
;   (#(cfg-state-line attempt-stop-read 1)
;    req
;    reqs
;    activereaders
;    writerpresent)
;   (#(cfg-state-line stop-read 1)
;    activereaders
;    reqs
;    writerpresent)
;   (#(cfg-state-line stop-read 2)
;    reqs
;    activereaders
;    writerpresent)
;   (#(cfg-state-line attempt-start-write 1)
;    req
;    reqs
;    activereaders
;    writerpresent)
;   (#(cfg-state-line start-write 1)
;    reqs
;    activereaders)
;   (#(cfg-state-line start-write 2)
;    reqs
;    activereaders
;    writerpresent)
;   (#(cfg-state-line attempt-stop-write 1)
;    reqs
;    activereaders
;    writerpresent
;    req)
;   (#(cfg-state-line stop-write 1)
;    reqs
;    activereaders)
;   (#(cfg-state-line stop-write 2)
;    reqs
;    activereaders
;    writerpresent)
;   (#(cfg-state-line raise-error 1)
;    reqs
;    activereaders
;    writerpresent)
;   (#(cfg-state-line raise-error 2)
;    reqs
;    activereaders
;    writerpresent)
;   (#(cfg-state-line end 1))))


;(define res-prog
;  (build-residual-program readers-ast ex-slice-set
;			  ex-rel-table ex-oblig-table))


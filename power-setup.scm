(load "slicing.scm")
(load "lib-fcl-shared.scm")

;; get program and build block map

(define power (get-file-object "examples/power.fcl"))
(define parsed-power (parse power))
(define bm (update-table* (map block->label
			       (program->blocks parsed-power))
			     (program->blocks parsed-power)
			     empty-table))

;; build block level and statement level cfg

(define power-cfg (cfg-build 'init empty-set empty-table bm))
(define power-cfg-state (cfg-state-build power-cfg bm))

;; compute control dependence information

(define power-state-hammock (build-hammock 'final power-cfg-state))
(define power-state-inv-hammock (inverse-hammock power-state-hammock))
(define power-state-inv-dom (build-dom 'final power-state-inv-hammock))
(define power-cd (build-cd power-state-hammock power-state-inv-dom))

;; compute def and ref information

(define power-defs (build-defs power-cfg-state bm))
(define power-refs (build-refs power-cfg-state bm))

;; example slicing criterion: block loop, line 2, variable n

(define p-n-slice (make-criterion (make-cfg-state-line 'loop '2)
				  '(n)))


;; build initial relevance table 

(define init-power-rel (initialize-table (domain-table power-cfg-state)
				      empty-set
				      empty-table))
(define power-rel
  (build-relevance-table p-n-slice power-defs power-refs power-cfg-state))

;; build initial slice set

(define slice-set (build-slice-set power-cfg-state power-rel power-defs))

(define final-slice-set (final-slice-set parsed-power p-n-slice))

;; get program and build block map

(define term (get-file-object "examples/term.fcl"))
(define parsed-term (parse term))

(define t-n-slice (make-criterion (make-cfg-state-line 'loop '2)
				  '(n)))
(define term-final-slice-set (final-slice-set parsed-term t-n-slice))
(build-relevance-case-1 (make-cfg-state-line 'init 1)
			p-n-slice
			init-power-rel)

(build-relevance-case-2-a (make-cfg-state-line 'loop 1)
			  (make-cfg-state-line 'loop 2)
			  power-defs
			  power-refs
			  (update-table (make-cfg-state-line 'loop 2)
					'(n)
					init-power-rel))

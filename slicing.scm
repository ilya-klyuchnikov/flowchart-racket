
(require 'struct)

(load "cfg.scm")
(load "lib-table.scm")
(load "lib-fcl-shared.scm")
(load "parse.scm")

(define-record criterion (line vars))

(define build-defs
  (lambda (cfg-state block-map)
    (letrec ((loop (lambda (lines def-table)
		     (if (null? lines)
			 def-table
			 (let* ((line      (car lines))
				(statement (cfg-state-get-state line
								block-map))
				(defs
				  (variant-case statement
				    (assign (var exp)
					    (singleton-set var))
				    (goto () empty-set)
				    (if () empty-set)
				    (return () empty-set)
				    (else (error "Invalid statement in build-defs: "
						 statement))))
				(new-def-table
				  (update-table line defs def-table)))
			   (loop (cdr lines) new-def-table))))))
      (loop (domain-table cfg-state)
	    empty-table))))


(define build-refs
  (lambda (cfg-state block-map)
    (letrec ((loop (lambda (lines ref-table)
		     (if (null? lines)
			 ref-table
			 (let* ((line      (car lines))
				(statement (cfg-state-get-state line
								block-map))
				(refs
				  (variant-case statement
				    (assign (var exp)
					    (collect-vars-exp exp))
				    (goto () empty-set)
				    (if (exp) (collect-vars-exp exp))
				    (return (exp) (collect-vars-exp exp))
				    (else (error "Invalid statement in build-refs: "
						 statement))))
				(new-ref-table
				  (update-table line refs ref-table)))
			   (loop (cdr lines) new-ref-table))))))
      (loop (domain-table cfg-state)
	    empty-table))))
			      
(define build-oblig-set
  (lambda (criterion-set)
    (list->set (map criterion->line (set->list criterion-set)))))

(define build-relevance-case-1
  (lambda (line criterion-set)
    (union*-set
      (map (lambda (criterion)
	     (let* ((criterion-line     (criterion->line criterion))
		    (criterion-vars     (criterion->vars criterion)))
	       (if (equal? line criterion-line)
		   criterion-vars
		   empty-set)))
	   (set->list criterion-set)))))

;;
;; n = line
;; m = succ
;; v is relevant at n if v is in REF(n) and there is a w in
;;  both DEF(n) and Rel(m)
;; ...so if intersect(DEF(n),REL(m)) is non-empty, then
;;    return REF(n)

(define build-relevance-case-2-a
  (lambda (line succ def-table ref-table rel-table)
    (let* ((relevant-defs (inter-set (lookup-table line def-table)
				     (lookup-table succ rel-table))))
      (if (empty-set? relevant-defs)
	  empty-set
	  (lookup-table line ref-table)))))

;;
;; n = line
;; m = succ
;; v is relevant at n if v is not in DEF(n) and v is in REL(m)
;; ...in other words, REL(m) - DEF(n) gives the relevant variables
;; that are not killed off by the def at line n
;;

(define build-relevance-case-2-b
  (lambda (line succ def-table ref-table rel-table)
    (let* ((not-killed (diff-set (lookup-table succ rel-table)
				 (lookup-table line def-table))))
      not-killed)))

;;
;; get successors of n and for each successor, do the tests above
;;

(define build-relevance-case-2
  (lambda (line def-table ref-table rel-table cfg-state)
    (letrec ((loop (lambda (succs relevant)
		     (if (empty-set? succs)
			 relevant
			 (let* ((succ           (car succs))
				(newly-relevant
				  (union-set
				    (build-relevance-case-2-a
				      line succ def-table ref-table rel-table)
				    (build-relevance-case-2-b
				      line succ def-table ref-table rel-table)
				    )))
			   (loop (cdr succs)
				 (union-set newly-relevant
					    relevant)))))))
      (loop (cfg-node->succs (lookup-table line cfg-state))
	    empty-set))))

(define build-relevance-line
  (lambda (line criterion-set def-table ref-table rel-table cfg-state)
    (let ((relevant-case-1 (build-relevance-case-1 line
						   criterion-set))
	  (relevant-case-2 (build-relevance-case-2 line
						   def-table
						   ref-table
						   rel-table
						   cfg-state)))
;      (display "line")
;      (newline)
;      (pretty-print line)
;      (display "c1")
;      (newline)
;      (pretty-print relevant-case-1)
;      (display "c2")
;      (newline)
;      (pretty-print relevant-case-2)
      (union relevant-case-1 relevant-case-2))))

;;
;; need to make sure that relevant table entries are always increasing
;;

(define build-relevance-table
  (lambda (criterion-set def-table ref-table cfg-state)
    (letrec ((loop (lambda (pending rel-table)
		     (if (null? pending)
			 rel-table
			 (let* ((line (car pending))
				(relevant (build-relevance-line
					    line
					    criterion-set
					    def-table
					    ref-table
					    rel-table
					    cfg-state))
				(old-relevant (lookup-table line
							    rel-table))
				(added (diff-set relevant old-relevant)))
			   (if (empty-set? added)
			       (loop (cdr pending) rel-table)
			       (let ((new-rel-table (update-table
						      line
						      relevant
						      rel-table))
				     (preds (cfg-node->preds
					      (lookup-table
						line
						cfg-state))))
				 (loop (append (cdr pending)
					       (set->list preds))
				       new-rel-table))))))))
      (let ((init-rel-table (initialize-table
			      (domain-table cfg-state)
			      empty-set
			      empty-table))
	    (pending (list->set (map criterion->line
				      (set->list criterion-set)))))
	(loop pending init-rel-table)))))

;; Slice-set = all lines that define variables that are relevant
;;             at at least one of the successors of the line.

(define build-slice-set
  (lambda (cfg-state rel-table def-table)
    (letrec ((loop (lambda (lines)
		     (if (null? lines)
			 empty-set
			 (let* ((line (car lines))
				(def-line (lookup-table line def-table))
				(succs (cfg-node->succs (lookup-table
							  line
							  cfg-state)))
				(rel-succs (union*-set (map (lambda (succ)
							      (lookup-table
								succ
								rel-table))
							    succs))))
			   (if (empty-set? (inter-set def-line rel-succs))
			       (loop (cdr lines))
			       (add-set line (loop (cdr lines)))))))))
      (loop (domain-table cfg-state)))))


(define build-branch-set
  (lambda (slice-set oblig-set cd)
    (union*-set (map (lambda (node)
		       (lookup-table node cd))
		     (set->list (union-set slice-set oblig-set))))))

(define build-branch-criterion
  (lambda (branch-node ref-table)
    (make-criterion branch-node (lookup-table branch-node ref-table))))

(define build-branch-relevance-table
  (lambda (branch-node def-table ref-table cfg-state)
    (let ((bc (build-branch-criterion branch-node ref-table)))
      (build-relevance-table (singleton-set bc)
			     def-table ref-table cfg-state))))

(define build-branch-relevance-table-table
  (lambda (branch-set def-table ref-table cfg-state)
    (let* ((branch-list (set->list branch-set))
	   (branch-rel-tables (map (lambda (branch-node)
				     (build-branch-relevance-table
				       branch-node
				       def-table
				       ref-table
				       cfg-state))
				   branch-list)))
      (update-table* branch-list
		     branch-rel-tables
		     empty-table))))
				    
(define iterate-relevance-table
  (lambda (cfg-state rel-table branch-set branch-relevance-table-table)
    (letrec ((loop (lambda (nodes rel-table)
		     (if (null? nodes)
			 rel-table
			 (let* ((node (car nodes))
				(new-rel-table
		           (update-table
			     node
			     (union
			       (lookup-table node rel-table)
			       (union*-set
				 (map (lambda (branch-node)
					(lookup-table
					  node
					  (lookup-table
					    branch-node
					    branch-relevance-table-table)))
				      (set->list branch-set))))
			     rel-table)))
			   (loop (cdr nodes) new-rel-table))))))
      (loop (domain-table cfg-state) rel-table))))

(define iterate-slice-set
  (lambda (branch-set cfg-state rel-table def-table)
    (union branch-set (build-slice-set cfg-state rel-table def-table))))

;;
;; really only need to iterate over rel-table and slice-set.
;; branch-set-k is computed from slice-set-k
;;

(define iterate
  (lambda (def-table ref-table cfg-state rel-table
		     oblig-set slice-set cd)
    (let* (;; calculate B^k_C
	   (branch-set (build-branch-set slice-set oblig-set cd))
	   ;; prepare to calculate R^{k+1}_C
	   ;;  ..first, calculuate branch-relevance table table
	   (brel-table-table (build-branch-relevance-table-table
			       branch-set def-table ref-table cfg-state))
	   ;;  ..now, get new relevance table
	   (new-rel-table (iterate-relevance-table
			    cfg-state rel-table branch-set
			    brel-table-table))
	   ;; ..now, calculate new slice set
	   (new-slice-set (iterate-slice-set branch-set
					     cfg-state
					     new-rel-table
					     def-table)))
      (cons new-slice-set new-rel-table))))

(define final-slice-set
  (lambda (prog-ast criterion-set)
    (let* ((block-map (update-table*
			(map block->label
			     (program->blocks prog-ast))
			(program->blocks prog-ast)
			empty-table))
	   (cfg       (cfg-build (program->init-label prog-ast)
				 empty-set
				 empty-table
				 block-map))
	   (cfg-state (cfg-state-build cfg block-map))
	   (defs      (build-defs cfg-state block-map))
	   (refs      (build-refs cfg-state block-map))
	   (init-rel  (initialize-table (domain-table cfg-state)
					empty-set
					empty-table))
	   (rel-table (build-relevance-table criterion-set
					     defs
					     refs
					     cfg-state))
	   (slice-set (build-slice-set cfg-state rel-table defs))
	   (oblig-set (build-oblig-set criterion-set))
	   (cd        (build-cd-from-scratch cfg-state)))
;      (display "DEFS")
;      (newline)
;      (pretty-print defs)
;      (display "REFS")
;      (newline)
;      (pretty-print refs)
;      (display "REL TABLE")
;      (newline)
;      (pretty-print rel-table)
;      (display "SLICE SET")
;      (newline)
;      (pretty-print slice-set)
;      (display "CD")
;      (newline)
;      (pretty-print cd)
;      (display "---------")
;      (newline)
      (letrec ((loop
		 (lambda (slice-set rel-table)
		   (let* ((result (iterate defs
					   refs
					   cfg-state
					   rel-table
					   oblig-set
					   slice-set
					   cd))
			  (new-slice-set (car result))
			  (new-rel-table (cdr result)))
		     (if (equal-sets? new-slice-set slice-set)
			 (vector new-slice-set new-rel-table oblig-set)
			 (loop new-slice-set new-rel-table))))))
	(loop slice-set rel-table)))))

(define build-residual-params
  (lambda (params init-node rel-table)
    ;; assumes params can be viewed as a set
    ;; init-node is the first node (line) in the cfg-state
    (inter-set params (lookup-table init-node rel-table))))

(define build-residual-assigns
  (lambda (assigns label number slice-set oblig-set)
    ;; assigns: working list of assigns from block 'label'
    ;; number: line number for car of assigns
    (if (null? assigns)
	()
	(let ((line (make-cfg-state-line label number)))
	  (cond ((in-set? line slice-set)
		 (cons (car assigns) (build-residual-assigns (cdr assigns)
							     label
							     (+ number 1)
							     slice-set
							     oblig-set)))
	        ((in-set? line oblig-set)
		 (cons (make-assign '**dummy**
				    (make-varref '**dummy**))
		       (build-residual-assigns (cdr assigns)
					       label
					       (+ number 1)
					       slice-set
					       oblig-set)))
	   	(else (build-residual-assigns (cdr assigns)
					      label
					      (+ number 1)
					      slice-set
					      oblig-set)))))))


(define build-residual-jump
  (lambda (jump label number slice-set)
    ;; jump: jump statement from block 'label'
    ;; number: line number of jump in block
    (let ((line (make-cfg-state-line label number)))
      (if (or (in-set? line slice-set) (return? jump) (goto? jump))
	  jump
	  ;; else we must have a conditional not in slice set
	  (make-goto (if->then-label jump))))))
	  ;; if conditional is not in the slice set, then
	  ;; nothing is control dependent on it. That means
	  ;; it doesn't matter if we jump to the true or false
	  ;; branch.
          ;; Note: oblig-set not needed here.  We always have to end
          ;; each block with a jump.

(define build-residual-block
  (lambda (block slice-set oblig-set)
    (let* ((label   (block->label block))
	   (assigns (block->assigns block))
	   (jump    (block->jump block))
	   (length-assigns (length assigns)))
      (make-block label
		  (build-residual-assigns assigns
					  label
					  1
					  slice-set
					  oblig-set)
		  (build-residual-jump jump
				       label
				       (+ 1 length-assigns)
				       slice-set)))))


(define build-residual-program
  (lambda (prog slice-set rel-table oblig-set)
    (let* ((params     (program->params prog))
	   (init-label (program->init-label prog))
	   (blocks     (program->blocks prog)))
      (make-program
	(build-residual-params params
			       (make-cfg-state-line init-label 1)
			       rel-table)
	init-label
	(map (lambda (block) (build-residual-block block
						   slice-set
						   oblig-set))
	     blocks)))))

(define slice
  (lambda (prog criterion-set)
    (let* ((prog-ast (parse prog))
	   (slice-set-rel-table (final-slice-set prog-ast criterion-set))
	   (slice-set (vector-ref slice-set-rel-table 0))
	   (rel-table (vector-ref slice-set-rel-table 1))
	   (oblig-set (vector-ref slice-set-rel-table 2))
	   (res-ast   (build-residual-program prog-ast
					      slice-set
					      rel-table
					      oblig-set)))
      (unparse res-ast))))
			
(define slice-file
  (lambda (in-file-name criterion-set)
    (let* ((prog (get-file-object in-file-name)))
      (slice prog criterion-set))))





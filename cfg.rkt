#lang racket

;; comments: consider dividing cfg up to distinguish forwards/backwards arcs.
(provide (all-defined-out))

(require "parse.rkt")
(require "lib-set.rkt")
(require "lib-table.rkt")

(define power (file->value "examples/power.fcl"))
(define parsed-power (parse-program power))
(define bm (update-table* (map block-label
                               (program-blocks parsed-power))
                          (program-blocks parsed-power)
                          empty-table))


(struct cfg-node (succs preds) #:transparent)
(struct cfg-state-line (label number) #:transparent)

;; block -> set(label)
(define cfg-get-succ-labels
  (lambda (blk)
    (match (block-jump blk)
      [(goto label) (set label)]
      [(return '()) (set)]
      [(if-jump exp then-label else-label) (set then-label else-label)]
      [else (error "Unrecognized jump in cfg-get-succ-labels: " (block-jump blk))])))

;;label x set(labels) x cfg x block-map --> cfg
(define cfg-build
  (lambda (label preds cfg block-map)
    (if (key-defined-table? label cfg)
        (let* ((node     (lookup-table label cfg))
               (new-node (cfg-node (cfg-node-succs node)
                                   (set-union preds
                                              (cfg-node-preds node)))))
          (update-table label new-node cfg))
        (let* ((succs  (cfg-get-succ-labels
                        (lookup-table label block-map)))
               (node   (cfg-node succs preds))
               (new-cfg (update-table label node cfg)))
          (cfg-build* succs
                      (singleton-set label)
                      new-cfg
                      block-map)))))

;;set(label) x set(labels) x cfg x block-map --> cfg
(define cfg-build*
  (lambda (labels preds cfg block-map)
    (letrec ((loop (lambda (pending cfg)
                     (if (null? pending)
                         cfg
                         (let ((new-cfg (cfg-build (car pending)
                                                   preds
                                                   cfg
                                                   block-map)))
                           (loop (cdr pending) new-cfg))))))
      (loop labels cfg))))


(define cfg-succ-labels
  (lambda (label cfg)
    (cfg-node-succs (lookup-table label cfg))))

(define cfg-pred-labels
  (lambda (label cfg)
    (cfg-node-preds (lookup-table label cfg))))


(define cfg-state-first-in-block
  (lambda (blk)
    (cfg-state-line (block-label blk) 1)))

(define cfg-state-last-in-block
  (lambda (blk)
    (let ((num-assigns (length (block-assigns blk))))
      (cfg-state-line (block-label blk)
                      (+ 1 num-assigns)))))

(define cfg-state-get-state
  (lambda (line block-map)
    (let* ((label   (cfg-state-line-label line))
           (number  (cfg-state-line-number line))
           (block   (lookup-table label block-map))
           (assigns (block-assigns block))
           (assigns-length (length assigns)))
      (cond ((equal? (+ 1 assigns-length) number)
             (block-jump block))
            ((<= number assigns-length)
             (list-ref assigns (- number 1)))
            (else (error "Bad line number in cfg-state-get-state: "
                         line
                         ))))))

(define cfg-state-build-for-assigns
  (lambda (number preds cfg-state label length-assigns)
    (if (> number length-assigns)
        cfg-state
        (let* ((line      (cfg-state-line
                           label number))
               (next-line (cfg-state-line
                           label
                           (+ number 1)))
               (new-cfg-state
                (update-table line
                              (cfg-node
                               (singleton-set
                                next-line)
                               preds)
                              cfg-state)))
          (cfg-state-build-for-assigns
           (+ number 1)
           (singleton-set line)
           new-cfg-state
           label
           length-assigns)))))


(define cfg-state-build-for-block
  (lambda (block cfg cfg-state block-map)
    (let* ((label   (block-label block))
           (assigns (block-assigns block))
           (jump    (block-jump block))
           (length-assigns (length assigns))
           (node        (lookup-table label cfg))
           (succ-labels (cfg-node-succs node))
           (pred-labels (cfg-node-preds node))
           (succ-states (map-set (lambda (label)
                                   (cfg-state-first-in-block
                                    (lookup-table label block-map)))
                                 succ-labels))
           (pred-states (map-set (lambda (label)
                                   (cfg-state-last-in-block
                                    (lookup-table label block-map)))
                                 pred-labels))
           (new-cfg-state (cfg-state-build-for-assigns
                           1
                           pred-states
                           cfg-state
                           label
                           length-assigns))
           (jump-pred-states (if (equal? length-assigns 0)
                                 pred-states
                                 (singleton-set
                                  (cfg-state-line
                                   label
                                   length-assigns))))
           (jump-line      (cfg-state-line label
                                           (+ 1 length-assigns))))
      (update-table jump-line
                    (cfg-node succ-states
                              jump-pred-states)
                    new-cfg-state))))

(define cfg-state-build
  (lambda (cfg block-map)
    (let* ((labels (domain-table block-map)))
      (letrec ((loop (lambda (labels cfg-state)
                       (if (null? labels)
                           cfg-state
                           (let ((new-cfg-state
                                  (cfg-state-build-for-block
                                   (lookup-table (car labels) block-map)
                                   cfg
                                   cfg-state
                                   block-map
                                   )))
                             (loop (cdr labels) new-cfg-state))))))
        (loop labels empty-table)))))

;; based on algorithm from Fig. 10.52 p. 671 in Dragon book
;; ..works on statement-level cfg or block-level cfg
;;
;;  ..slightly optimized by keeping a list of pending nodes.
;; Each iteration relies only on info about predecessor nodes.
;; So when a table entry for a node changes, we only need to enter
;; the successors in the pending list (I think this optimization works,
;; but I'm not 100% sure.
;;
;;
;;  Notes:
;;    - each node dominates itself
;;    - (lookup dom-table n) gives a set of the nodes that dominate node n.
;;    - much information is duplicated here (wasting space), instead
;;      we should hold the information in a dominator tree.
;;

(define build-dom
  (lambda (init-node cfg-state)
    (let* ((nodes          (domain-table cfg-state))
           (nodes-w/o-init (diff-set nodes (singleton-set init-node)))
           (init-dom-table (update-table init-node
                                         (singleton-set init-node)
                                         (initialize-table nodes
                                                           nodes
                                                           empty-table))))
      ;; init-dom-table has init-node only dominating itself,
      ;; and for every other node n, assume that n is dominated
      ;; all others.  We will gradually throw away those nodes
      ;; that cannot dominate n.
      ;;
      (letrec ((loop (lambda (pending dom-table)
                       (if (null? pending)
                           dom-table
                           (let* ((node (car pending))
                                  (cfg-node (lookup-table node cfg-state))
                                  (preds (cfg-node-preds cfg-node))
                                  (succs (cfg-node-succs cfg-node))
                                  ;; find the nodes m such that
                                  ;; for each predecessor p, m dominates p.
                                  (pred-doms (inter*-set
                                              (map (lambda (pred)
                                                     (lookup-table pred
                                                                   dom-table))
                                                   preds)))
                                  ;; dominators are pred-doms plus
                                  ;; node itself
                                  (node-doms (union-set (singleton-set node)
                                                        pred-doms))
                                  (old-node-doms (lookup-table node
                                                               dom-table)))
                             (if (equal-sets? node-doms old-node-doms)
                                 (loop (cdr pending) dom-table)
                                 (loop (append pending succs)
                                       (update-table node
                                                     node-doms
                                                     dom-table))))))))
        (loop (cfg-node-succs (lookup-table
                               init-node
                               cfg-state))
              init-dom-table)))))


(define build-hammock
  (lambda (final-node cfg)
    (letrec ((loop (lambda (nodes cfg final-preds)
                     (if (null? nodes)
                         (update-table final-node
                                       (cfg-node
                                        empty-set
                                        final-preds)
                                       cfg)
                         (let* ((node  (car nodes))
                                (node-data (lookup-table
                                            node
                                            cfg))
                                (succs (cfg-node-succs node-data))
                                (preds (cfg-node-preds node-data)))
                           (if (empty-set? succs)
                               (let* ((new-node-data
                                       (cfg-node
                                        (singleton-set final-node)
                                        preds))
                                      (new-cfg (update-table node
                                                             new-node-data
                                                             cfg))
                                      (new-final-preds (add-set
                                                        node
                                                        final-preds)))
                                 (loop (cdr nodes) new-cfg new-final-preds))
                               (loop (cdr nodes) cfg final-preds)))))))
      (loop (domain-table cfg) cfg empty-set))))


(define inverse-hammock
  (lambda (cfg)
    (letrec ((loop (lambda (nodes cfg)
                     (if (null? nodes)
                         cfg
                         (let* ((node (car nodes))
                                (node-info (lookup-table node cfg))
                                (new-node-info (cfg-node
                                                (cfg-node-preds node-info)
                                                (cfg-node-succs node-info)))
                                (new-cfg (update-table
                                          node
                                          new-node-info
                                          cfg)))
                           (loop (cdr nodes) new-cfg))))))
      (loop (domain-table cfg) cfg))))


(define make-cfg-state
  (lambda (prog-ast)
    (let* ((block-map (update-table*
                       (map block-label
                            (program-blocks prog-ast))
                       (program-blocks prog-ast)
                       empty-table))
           (cfg       (cfg-build (program-init-label prog-ast)
                                 empty-set
                                 empty-table
                                 bm))
           (cfg-state (cfg-state-build cfg block-map)))
      (pretty-print cfg-state))))

;; finds the 
;; inv-dom is the inverse dominator of node
(define on-paths
  (lambda (node inv-dom cfg)
    (if (equal? node inv-dom)
        empty-set
        (union*-set (map (lambda (node)
                           (on-paths node inv-dom cfg))
                         (cfg-node-succs (lookup-table node cfg)))))))

;; builds a set of nodes on which 'node' is control-dependent

(define cd-try-node
  (lambda (try-node-label node-label cd-node-labels visited
                          inv-dom-table cfg cont)
    (cond
      ((in-set? try-node-label visited)
       (cont cd-node-labels visited))
      (;; is this a conditional (2 or more succs)
       ;; that is not post-dominated by node?
       (let ((inv-doms (lookup-table try-node-label inv-dom-table)))
         (and (<= 2 (size-set (cfg-succ-labels try-node-label cfg)))
              (not (in-set? node-label inv-doms))))
       (cont (add-set try-node-label cd-node-labels)
             (add-set try-node-label visited)))
      (else (let ((pred-labels (cfg-pred-labels try-node-label cfg)))
              (cd-try-nodes pred-labels
                            node-label
                            cd-node-labels
                            (add-set try-node-label visited)
                            inv-dom-table
                            cfg
                            cont))))))

(define cd-try-nodes
  (lambda (try-node-labels node-label cd-node-labels visited
                           inv-dom-table cfg cont)
    ;;    (display "--------------")
    ;;    (newline)
    ;;    (display try-node-labels)
    ;;    (newline)
    ;;    (display node-label)
    ;;    (newline)
    ;;    (display cd-node-labels)
    ;;    (newline)
    (if (empty-set? try-node-labels)
        (cont cd-node-labels visited)
        (cd-try-node (car try-node-labels)
                     node-label
                     cd-node-labels
                     visited
                     inv-dom-table
                     cfg
                     (lambda (cd-node-labels visited)
                       (cd-try-nodes (cdr try-node-labels)
                                     node-label
                                     cd-node-labels
                                     visited
                                     inv-dom-table
                                     cfg
                                     cont))))))

;;
;; cd : node --> POWER(node)
;; ...maps each node to the set of nodes on which it is control
;; dependent
;;

;; For a cfg with hammock graph cfg and inv-dom table inv-dom,
;; builds the corresponding cd function

(define build-cd
  (lambda (cfg inv-dom-table)
    (let* ((node-labels   (domain-table cfg))
           (cd-info       (map (lambda (node-label)
                                 (cd-try-nodes (cfg-pred-labels
                                                node-label
                                                cfg)
                                               node-label
                                               empty-set
                                               empty-set
                                               inv-dom-table
                                               cfg
                                               (lambda (cd-node-labels
                                                        visited)
                                                 cd-node-labels)))
                               node-labels))
           (cd (update-table* node-labels cd-info empty-table)))
      cd)))

(define build-cd-from-scratch
  (lambda (cfg-state)
    (let* ((final-state           'final)
           (cfg-state-hammock     (build-hammock final-state
                                                 cfg-state))
           (cfg-state-inv-hammock (inverse-hammock cfg-state-hammock))
           (inv-dom               (build-dom final-state
                                             cfg-state-inv-hammock))
           (cd                    (build-cd cfg-state-hammock inv-dom)))
      cd)))

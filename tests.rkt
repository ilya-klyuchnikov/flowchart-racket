#lang racket

(require "parse.rkt")
(require "eval.rkt")
(require "online.rkt")
(require "offline.rkt")
(require rackunit)

;; testing
(check-not-false 
 (primitive? '*))

(check-false 
 (primitive? 'print))

(check-equal? 
 (parse-exp '1) 
 (const 1))

(check-equal? 
 (parse-exp 'a) 
 (varref 'a))

(check-equal? 
 (parse-exp '(* 1 2)) 
 (app '* (list (const 1) (const 2))))

(check-equal? 
 (parse-exp '*)
 (app '* '()))

(check-exn
 exn:fail?
 (lambda () (parse-exp '(1))))

(check-equal?
 (parse-assign '(result := (* result m)))
 (assign 'result (app '* (list (varref 'result) (varref 'm)))))

(check-equal? 
 (parse-jump '(goto test))
 (goto 'test))

(check-equal?
 (parse-jump '(return 1))
 (return (const 1)))

(check-equal?
 (parse-jump '(if n end loop))
 (if-jump (varref 'n) 'end 'loop))

(check-equal?
 (parse-block '(init ((a := 1) (b := 2)) (goto test)))
 (block 'init (list (assign 'a (const 1)) (assign 'b (const 2))) (goto 'test)))

(define s-prog
  '((m n)
    (init)
    {
     (init ((result := 1))
           (goto test))     
     (end ()
          (return result))
     }))

(define ast-prog
  (program 
   '(m n) 
   'init 
   (list 
    (block 'init (list (assign 'result (const 1))) (goto 'test)) 
    (block 'end '() (return (varref 'result))))))

(check-equal? (parse-program s-prog) ast-prog)
(check-equal? (unparse-program ast-prog) s-prog)

;; evaluation
(check-equal? (eval-fcl-file "examples/power.fcl" '(5 2)) 25)

;; online pe
(define power (file->value "examples/power.fcl"))
;(eval-fcl (unparse-program (online-prog (parse-program power) '(n) '(1))) '(5))
(check-equal? (eval-fcl (unparse-program (online-prog (parse-program power) '(n) '(1))) '(5)) 5)
(check-equal? (eval-fcl (unparse-program (online-prog (parse-program power) '(n) '(2))) '(5)) 25)

(check-equal? (eval-fcl (unparse-program (offline-prog (parse-program power) '(n) '(1))) '(5)) 5)
(check-equal? (eval-fcl (unparse-program (offline-prog (parse-program power) '(n) '(2))) '(5)) 25)
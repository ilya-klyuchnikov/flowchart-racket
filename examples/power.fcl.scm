(define power
(quote
((m n)
 (init)
 (
 (init ((result := 1))
       (goto test))

 (test ()
       (if (< n 1) 
            end
            loop))

 (loop (
        (result := (* result m))
        (n := (- n 1))
       )
        (goto test))

  (end ()
       (return result))
  ))
))
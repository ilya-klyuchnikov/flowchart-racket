((Q Right)
 (init)
 (
  {init 
   ((Qtail := Q)
    (Left := '() )) 
   (goto loop)}
  
  (loop 
   ()
   (if (= Qtail '()) stop cont))
  
  (cont 
   ((Instruction := (hd Qtail))
    (Qtail := (tl Qtail))
    (Operator := (hd (tl Instruction)))) 
   
   (if (= Operator 'right) do-right cont1))
  
  (cont1 
   ()
   (if (= Operator 'left) do-left cont2))
  
  (cont2 
   ()
   (if (= Operator 'write) do-write cont3))
  
  (cont3 
   ()
   (if (= Operator 'goto) do-goto cont4))
  
  (cont4 
   ()
   (if (= Operator 'if) do-if error))
  
  (do-right 
   ((Left := (cons (hd Right) Left))
    (Right := (tl Right)))
   (goto loop))
  
  (do-left 
   ((Right := (cons (hd Left) Right))
    (Left := (tl Left)))
   (goto loop))
  
  (do-write 
   ((Symbol := (hd (tl (tl Instruction))))
    (Right := (cons Symbol (tl Right))))
   (goto loop))
  
  (do-goto 
   ((Nextlabel := (hd (tl (tl Instruction))))
    (Qtail := Q))
   (goto new_tail))
  
  (do-if 
   ((Symbol := (hd (tl (tl Instruction))))
    (Nextlabel := (hd (tl (tl (tl (tl Instruction)))))))
   (if (= Symbol (hd Right)) jump loop))
  
  (jump 
   ((Qtail := Q))
   (goto new_tail))
  
  (new_tail 
   ()
   (if (= (hd (hd Qtail)) Nextlabel) loop loop2))
  
  (loop2 
   ((Qtail := (tl Qtail)))
   (goto new_tail))
  
  (error 
   ()
   (return Instruction))
  
  (stop 
   ()
   (return Right))
  
  ))
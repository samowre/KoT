;;resolves two ordered clauses to eliminate the first clashing pair of
;;literals
(defun resolve (cl1 cl2 accum clash?)
  (cond ((and (consp cl1)(consp cl2))
	 (if (< (abs (car cl1))(abs (car cl2)))
	     (resolve (cdr cl1) cl2 (cons (car cl1) accum) clash?)
	   (if (< (abs (car cl2))(abs (car cl1)))
	       (resolve cl1 (cdr cl2) (cons (car cl2) accum) clash?)
	     (if (eql (car cl1)(car cl2))
		 (resolve (cdr cl1)(cdr cl2) (cons (car cl1) accum) clash?)
	       (resolve (cdr cl1)(cdr cl2) accum t)))));clash occurred
	((consp cl1) (append (reverse accum) cl1))
	(t (append (reverse accum) cl2))))
   
;;process-line-proof builds a clause from a chain of resolution steps
(defun process-line-proof (line-string pos clause resolvent tr-array index)
  (multiple-value-bind (clausenum litpos)
       (read-from-string line-string nil nil :start pos)
      (cond ((eql clausenum 's)
	     (cond ((equalp clause resolvent)
		    (setf (aref tr-array index) resolvent)
		    tr-array)
		   ((or (not clause)(not resolvent))
		    (setf (aref tr-array index) (or clause resolvent))
		      tr-array)
		   (t nil)))
	    (t (let ((new-clause (resolve resolvent (aref tr-array clausenum) nil nil)))
		 (process-line-proof line-string litpos clause new-clause tr-array index))))))
				   
;;process-line-clause extracts the clause from the clause part of a line
(defun process-line-clause (line-string pos clause tr-array index)
  (multiple-value-bind (literal litpos)
       (read-from-string line-string nil nil :start pos)
      (cond ((or (eql literal 's)(eql literal '*))
             (process-line-proof line-string litpos (reverse clause) nil tr-array index))
	    (t (process-line-clause line-string litpos (cons literal clause)
					  tr-array index)))))

;;process-trace-line starts the processing of a trace line
(defun process-trace-line (line-string tr-array index)
 (multiple-value-bind (line-index pos)
 		      (read-from-string line-string nil nil)
     (cond ((eql index line-index)
            (process-line-clause line-string pos nil tr-array index))
	    (t (format t "Bad line index at ~d" index) nil))))
     
;;read-trace-line reads a single line till eof and invokes process-trace-line
(defun read-trace-line (stream tr-array index)
    (let ((tr-line (excl:simple-stream-read-line stream nil nil)))
       (if tr-line
	   (let* ((asize (array-total-size tr-array))
		  (tr-array (if (eql index asize)
			       (adjust-array tr-array (* 2 (1+ asize)) :initial-element 0)
			      tr-array)))
           (read-trace-line stream (process-trace-line tr-line tr-array index)
	                    (1+ index)))
	   tr-array)))
;;read-trace gets everything started
(defun read-trace (file-string)
  (with-open-file (trace file-string
			 :direction :input)
		  (let ((tr-array (make-array 100 :initial-element 0
						  :adjustable t)))
		    (read-trace-line trace tr-array 0))))
		  
		  

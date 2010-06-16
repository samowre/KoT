					; reads infile,
					; sorts lines by increasing line number
					; eliminates gaps
					; sorts the clause part of each line
(defun transform-pico-proof (infile outfile)
  (with-open-file (in infile :direction :input)
    (with-open-file (out outfile :direction :output
			 :if-exists :supersede)
      (dolist (clause (read-pico-proof* in))
	(format out "~%~{~a~^ ~}" clause)))))

(defun f (s) (string-to-number (subseq s 0 (position #\Space s))))

					; reads all the lines,
					; sorts them and then eliminates gaps
(defun read-pico-proof* (stream &optional (result nil))
					; line from the file
  (let ((line (read-line stream nil 'eof)))
    (if	(eq line 'eof)
					; end of file: sort the lines in result
	(eliminate-gaps (sort result #'< :key #'f))
      (read-pico-proof* stream
			  (cons line result)))))

(defun eliminate-gaps (list)
  (let* ((n (length list)) (offsets-hash (make-hash-table :size n) ))
    (eliminate-gaps* list offsets-hash)))
    

					; parse each line contained in the list
(defun eliminate-gaps* (list
			offsets-hash
			&optional (result nil)
			&optional (previous-line-number 0)
			&optional (last-line-association 0))
  (if list
      (multiple-value-bind (new-line
			    new-previous-line-number
			    last-line-association)
	  (process-line (car list)
			offsets-hash
			previous-line-number
			last-line-association
			)
	(eliminate-gaps* (cdr list)
			 offsets-hash
			 (cons new-line result)
			 new-previous-line-number))
    (nreverse result)))


(defun process-line (line
		     offsets-hash
		     previous-line-number
		     last-line-association)
					;read line number
  (multiple-value-bind (line-number npos)
      (read-from-string line nil 'eol :start 0)
  
    (let* ((last-line-association (+ last-line-association 1))
	  (new-line-number
	   (setf (get-hash line-number offsets-hash) last-line-association)))
      (values
       (cons (number-update line-number offsets-hash)
	     (rewrite-line* line npos offsets-hash))
       line-number
       last-line-association))))

(defun index_m (n) (if (< n 0) (- (* 2 (abs n)) 1) (* 2 (abs n)))) 

					;sort clause and update antecedents
(defun rewrite-line* (line pos
			   offsets-list
			   &optional (result nil)
			   &optional (mode 0)
			   &optional (clause nil))
  (multiple-value-bind (word npos)
      (read-from-string line nil 'eol :start pos)
    (if (eq word 'eol)
	(nreverse result)
      (if (= mode 0)
	  (if (or (eq word '*)
		  (eq word 0))
					;switch mode: begin replacing antecedents
	      (rewrite-line* line
			     npos
			     offsets-list
			     (cons (if (eq word 0) "s" "*") (sort result #'> :key 'index_m))
			     1)
					;continue to read the clause
	    (rewrite-line* line
			   npos
			   offsets-list
			   (cons word result)
			   mode))
	(rewrite-line* line
		       npos
		       offsets-list
		       (cons
			(if (eq word 0)
			    "s"
			  (number-update word offsets-list))
			result)
		       mode)))))
					;returns the new value of a line number
(defun number-update (line-number offsets-hash)
  (get-hash line-number offsets-hash))

(defun read-pico-proof (infile outfile)
  (with-open-file (in infile :direction :input)
    (with-open-file (out outfile :direction :output
			 :if-exists :supersede)
      (dolist (clause (read-pico-proof* in))
	(format out "~%~{~a~^ ~}" clause)))))

(defun f (s) (string-to-number (subseq s 0 (position #\Space s))))

					; reads all the lines, sorts them and then eliminates gaps
(defun read-pico-proof* (stream &optional (result nil))
					; line from the file
  (let ((line (read-line stream nil 'eof)))
    (if	(eq line 'eof)
					; end of file: sort the lines in result
	(eliminate-gaps* (sort result #'< :key #'f))
      (read-pico-proof* stream
			  (cons line result)))))

					; parse each line contained in the list
(defun eliminate-gaps* (list &optional (result nil)
			     &optional (offsets-list nil)
			     &optional (previous-line-number 0))
  (if list
      (multiple-value-bind (new-line
			    new-offsets-list
			    new-previous-line-number)
	  (process-line (car list)
			offsets-list previous-line-number)
	(eliminate-gaps* (cdr list)
			 (cons new-line result)
			 new-offsets-list
			 new-previous-line-number))
    (nreverse result)))

(defun process-line (line offsets-list previous-line-number)
					;read line number
  (multiple-value-bind (line-number npos)
      (read-from-string line nil 'eol :start 0)
    (let ((delta (- line-number previous-line-number)))
      (let ((new-offsets-list
					;add gap or not
	     (if (= delta 1)
		 offsets-list
	       (add-offset line-number delta offsets-list))))
	(values
	 (cons (number-update line-number new-offsets-list)
	       (rewrite-line* line npos new-offsets-list))
	 new-offsets-list
	 line-number)))))

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
			     (cons word (sort result #'>))
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
			    word
			  (number-update word offsets-list))
			result)
		       mode)))))
					;returns the new value of a line number
(defun number-update (line-number offsets-list)
  (- line-number (offset* line-number offsets-list)))

					; returns the offset for the specified line-number
(defun offset* (line-number offsets-list)
  (if offsets-list 
      (if (>=  line-number (caar offsets-list))
	  (cadar offsets-list)
	  (offset* line-number (cdr offsets-list)))
    0))

					; takes an index, an offset and the list of gaps
					; adds the pair (line#, offset) to the list of gaps
(defun add-offset (line-number delta offsets-list)
  (let ((delta (- delta 1)))
    (if offsets-list
	(cons 
	 (cons 
	  line-number 
	  (cons 
	   (+ delta (cadar offsets-list))
	   nil))
	 offsets-list)
      (cons 
       (cons line-number (cons delta nil)) nil))))
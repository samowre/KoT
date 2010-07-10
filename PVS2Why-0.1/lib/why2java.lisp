;; Release: PVS2Why-0.1 (11/10/07)
;; Prints Java representation of Why Code
;; --------------------------------------------------------------------
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
;; --------------------------------------------------------------------

(defun pvs2java (pvsfile theory version)
  (with-open-file 
   (file (format nil "~a.java" theory) 
	 :direction :output :if-exists :supersede)
   (format file 
	   "/* File: ~a.java 
*  Automatically generated from PVS theory ~a (~a.pvs) 
*  By: ~a 
*  Date: ~a 
*/~2%"
	   theory theory pvsfile version (now-today))
   (let ((translated-why-theory (pvs2why-theory theory)))
     (when *why2java-trace* 
       (format t "~%========== Processing tranlation from why to java ========== ~2%"))
     (why2java-modules file translated-why-theory))))

;;
;; Keep a global variable to denote a generic class
;;
(defvar *generic-module* nil)

;;
;; Definitions
;;

(defun abstract-definition? (def)
  (and (why-function? def)
       (null (body def))))

(defun abstract-class? (l)
  (loop for def in l 
	thereis (abstract-definition? def)))

(defun find-generic-record-defs (l)
  (remove nil (loop for def in l
	collect
	(when (generic-recordtype-definition? def)
	  def))))

(defun generic-recordtype-definition? (def)
  (and (why-record? def)
       (loop for field in (fields def)
	     thereis (is-generic-type?* (type field)))))

(defmethod is-generic-type?* ((type why-function-type))
  (or (is-generic-type?* (domain type))
      (is-generic-type?* (range type))))

(defmethod is-generic-type?* ((types list))
  (loop for type in types
	thereis (is-generic-type?* type)))
  
(defmethod is-generic-type?* ((type why-generic-type))
  t)

;; Default
(defmethod is-generic-type?* (type)
  false)

(defun generic-class? (module)
  (consp (type-parameters module)))

(defmethod why2java-modules ((file stream) modules)
  (dolist (module modules)
    (why2java* file module)))

(defmethod why2java* ((file stream) (def why-module) &optional noreturn )
  (when *why2java-trace* 
    (format t "Function: why2java*-why-module: ~a ~%" (identifier def)))
  (setq *generic-module* (consp (type-parameters def)))
  (indent file (format nil "import PVS2Java.*;~2%"))
  (when *generic-module*
    (indent file (format nil "@SuppressWarnings(\"unchecked\")~%")))
  (indent 
   file (format nil "public ~:[~;abstract ~]class ~a "
		(abstract-class? (definitions def))
		(identifier def))) ; TODO: make list
  (when *generic-module*
    (indent file (format nil "<~a> "
		(when *generic-module* (identifier (car (type-parameters def)))))))

  (block-java
   file 
   (when (parameters def)
     (format file "~%")
     (write-java-fields file (parameters def)))
   (when (imports def)
     (write-java-import-classes file (imports def)))
   (if *generic-module*
       (write-java-generic-header file (identifier def) (type-parameters def) (definitions def))
     (write-java-constructor file (identifier def) (parameters def) nil (imports def)))
;   (when (and (imports def) (not (parameters def)))
;     (write-java-imports file (imports def)))
;   (when *generic-module*
     
   (format file "~%")
    (dolist (definition (definitions def))
     (why2java* file definition noreturn)
     (format file "~%"))))

(defun objs2params (file params i)
  (when params
    (let ((param (car params)))
      (indent file
	      (format nil "~a ~a = (~a)obj__[~a];~%"
		      (why2java-type* (type param))
		      (identifier param)
		      (why2java-type* (type param) t)
		      i))
      (objs2params file (cdr params) (+ i 1)))))

; '("foo1" "foo2") ----> "foo1, foo2" 
(defun comma-list (lst)
    (format nil "~{~A~#[~:;, ~]~}" lst))

(defun currify (types return-type)
  (if (not types)
      return-type
    (format nil "Lambda<~a,~a>"
	      (why2java-type* (car types) t)
	      (currify (cdr types) return-type))))

; write-hof takes as arguments: a stream, a return-type at the end of parameters
; and the return expr, the return expr
; is an already translated string! The parameters are bindings
(defun write-hof (stream return-type parameters expr seq)
 (let* ((parameter (car parameters))
	(arg-type (why2java-type* (type parameter) t))
	(cdr-parameters (cdr parameters))
    	(result-type (currify (mapcar #'type cdr-parameters) return-type))
;	(lifted (lift-let* expr)) ; expr is already translated?
;	(lexpr (car lifted))
;	(seq-if (if (eq (length (cdr lifted)) 1)
;		    (car (cdr lifted))
;		  (mk-why-sequential-composition (cdr lifted))))
	(recursive-body (if (consp cdr-parameters)
 			    (with-output-to-string
			      (new-stream)
			      (write-hof new-stream return-type cdr-parameters expr seq))
			  expr)))
   (indent
    stream
    (format nil "new Lambda<~a,~a>()" arg-type result-type))
   (block-java
    stream
    (indent
     stream
     (format nil "public ~a curry(final ~a ~a) "
	     result-type
	     arg-type
	     (identifier parameter)))
    (block-java
     stream
     (indent
      stream
      (if seq 
	  (format nil "~a;~%return ~a;" (why2java-string* seq) recursive-body )
	(format nil "return ~a;" recursive-body)))))))

(defun write-java-hof-def (file def)
  (let* ((type    (why2java-type* (return-type def)))
	 (typeobj (why2java-type* (return-type def) t))
	 (rev-pars (reverse (parameters def)))
	 (arg-type (why2java-type* (type (car rev-pars)) t)))
    (format file "~%")
    (indent file
	    (format nil "// Higher order function ~a~%" (identifier def)))
    (indent file
	    (format nil "public Lambda<~a,~a> ~a = "
		    arg-type
		    (currify (mapcar #'type (cdr rev-pars)) typeobj)	
		    (identifier def)))
    (write-hof file typeobj rev-pars
	       (format nil "~a(~a)"
		       (identifier def)
		       (why2java-list 
			(mapcar #'(lambda (param) (identifier param))
				(parameters def)))) nil )
    (format file ";~%")))

(defun write-java-function (file def)
  (when (precondition def)
        (indent file (format nil "//@ requires ~a ~%" (why2java-string* (precondition def)))))
  (when (postcondition def)
        (indent file (format nil "//@ ensures ~a ~%" (why2java-string* (postcondition def)))))      
  (let ((header
	 (format nil "public ~:[abstract ~;~]~a ~a("
;		 (if (precondition def)
;		     (why2java* file (precondition def)); nil?
;		     nil)
		 (body def)
		 (why2java-type* (return-type def))
		 (identifier def))))
    (indent file header)
    (when (parameters def)
      (open-block (length header))
      (why2java* file (car (parameters def)))
      (dolist (param (cdr (parameters def)))
	(format file ",~%")
	(indent file)
	(why2java* file param))
      (close-block))
    (cond ((body def)
	   (format file ") ")
	   (block-java
	    file
	    (why2java* file  (body def))))
	  (t (format file ");~%")))))

(defmethod why2java* ((file stream) (def why-function) &optional noreturn )
  (when *why2java-trace* 
    (format t "Function: why2java*-why-function: ~a ~%" (identifier def)))
  (write-java-function file def)
  (when (and (parameters def)
	     (loop for param in (parameters def)
		   never (why-function-type? (type param))))
    (write-java-hof-def file def)))

; no higher order functions allowed as predicates
; translate the predicate into an ordinare why function
; mk-why-function turns a list of variable names in why-binders. Since they are already why-binders
; in the predicate we strip them first and then feed them to mk-why-function.
(defmethod why2java* ((file stream) (def why-logic-predicate) &optional noreturn )
  (when *why2java-trace* 
    (format t "Function: why2java*-why-logic-predicate: ~a ~%" (identifier def)))
  (let* ((stripped-binders (loop for var in (binders def) collect (identifier var)))
 	 (function (mk-why-function (identifier def) stripped-binders (predicate def) (type def) nil nil)))
        (write-java-function file function)))


;(defmethod why2java* ((file stream) (def why-declaration-adt))
;  (indent
;   file
;   (format nil "public abstract class ~a{};~%"
;	   (identifier def))))

(defun write-java-fields (file parameters)
  (dolist (field parameters)
    (indent
     file 
     (format nil "~a ~a;~%" 
	     (why2java-type* (type field))
	     (identifier field)))))

; Just one parameter, fix more later
(defun write-java-import-classes (file imports)
  (dolist (import imports)
    (if (and (consp (parameters import))
	     (why-generic-type? (car (parameters import))))
	(if (module (car (parameters import)))
	    (indent
	     file
	     (format nil "~a<~a.~a> ~(~a~);~%"
		     (identifier import)
		     (module (car (parameters import)))
		     (identifier (car (parameters import)))
		 (identifier import)))
	  (indent
	   file
	   (format nil "~a<~a> ~(~a~);~%"
		   (identifier import)
		   (identifier (car (parameters import)))
		   (identifier import))))
      (indent
       file 
       (format nil "~a ~(~a~);~%" 
	       (identifier import)
	       (identifier import))))))

;
; an import is handled as a new instantiation of the corresponding class
;
(defun write-java-imports (file imports)
  (dolist (import imports)
    (indent
     file 
     (format nil "~(~a~) = new ~a(~a);~%" 
	     (identifier import)
	     (identifier import)
	     (why2java-list (parameters import))))))

(defun write-java-generic-header (file class-name type-parameters defs)
  (dolist (parameter type-parameters)
    (indent
     file 
     (format nil "Array<~a> arr_~a;~%"
	     (why2java-type* parameter)
	     (why2java-type* parameter))))
  (format file "~%")
  (indent file 
	  (format nil "public ~a()" class-name
		  (if update " update" "")
		  (why2java-list parameters)))
  (block-java
   file
   (loop for def in (find-generic-record-defs defs)
	 do
	 (indent
	  file
	  (format nil "this.~a = new ~a();~%"
		  (identifier def)
		  (identifier def))))
   (dolist (parameter type-parameters)
     (indent
      file 
      (format nil "this.arr_~a = new Array<~a>();~%"
	      (why2java-type* parameter)
	      (why2java-type* parameter))))))

(defun write-java-constructor (file identifier
					   parameters &optional update imports)
  (format file "~%")
  (indent file 
	  (format nil "public ~a~a(~a) "
		  identifier
		  (if update " update" "")
		  (why2java-list parameters)))
  (block-java
   file
   (dolist (field parameters)
     (indent
      file 
      (format nil "this.~a = ~a;~%"
	      (identifier field)
	      (identifier field))))
   (when imports
     (write-java-imports file imports))
   (when update
     (indent file
	     (format nil "return this;~%")))))

(defun write-java-clone (file)
   (format file "~%")
   (indent file "public Object clone() ")
   (block-java
    file
    (indent file
	    (format nil "try { return super.clone(); }~%"))
    (indent file
	    (format nil "catch (CloneNotSupportedException e) {~%"))
    (indent file
	    (format nil "  return this;~%"))
    (indent file 
	    (format nil "}~%"))))

(defun write-java-array (file identifier)
   (format file "~%")
   (indent file (format nil
			"public static ~a[] new_~a(int size,Lambda<Integer,~a> lambda) "
			identifier identifier identifier identifier))
   (block-java
    file
    (indent file
	    (format nil "~a[] array~a = new ~a[size];~%"
		    identifier identifier identifier))
    (indent file
	    (format nil "for (int i=0;i<size;i++)~%"))
    (indent file
	    (format nil "  array~a[i] = lambda.curry(i);~%" identifier))
    (indent file
	    (format nil "return array~a;~%" identifier))))

(defmethod why2java* ((file stream) (def why-record) &optional noreturn)
  (when *why2java-trace* 
    (format t "Function: why2java*-why-record: ~a ~%" (identifier def)))
  (if *generic-module*
      (progn
	(indent file
		(format nil "~a ~a;"
			(identifier def) (identifier def)))
	(indent file
		(format nil "public class ~a implements Cloneable "
			(identifier def))))
    (indent file
	   (format nil  "static public class ~a implements Cloneable "
		   (identifier def))))
  (block-java
   file
   (write-java-fields file (fields def))
   (if (not *generic-module*)
       (write-java-array file (identifier def))
     (progn
       (indent file
	       (format nil "Array<~a> arr_~a;~%~%"
		       (identifier def)
		       (identifier def)))
       (indent file
	       (format nil "public ~a() " (identifier def)))
       (block-java file
		   (indent file
			   (format nil "this.arr_~a = new Array<~a>();~%"
				   (identifier def)
				   (identifier def))))))
   (write-java-constructor file (identifier def) (fields def))
   (write-java-constructor file (identifier def) (fields def) t)
   (write-java-clone file)))

;(defun write-generic-record-array (file record-identifier)
;  (indent file
;	  (format nil "~a ~a;~%"
;		  identifier identifier))
;  (indent file
;	  (format nil "public class ~a implements Cloneable "
;		  identifier))
;  (indent file
;	  (format nil "

;;
;; Expressions
;;

(defmethod why2java* ((file stream) (expr why-binding) &optional noreturn )
  (when *why2java-trace* 
    (format t "Function: why2java*-why-binding: ~a ~%" (identifier expr)))
  (format file "final ~a ~a" 
	  (why2java-type* (type expr))
	  (identifier expr)))

;  (let* ((dummy (format t "~a~%" (why2java-string* expr)))
;	 (lifted-expr (lift-let* expr))
;	 (dummy (format t "lifted-expr : ~a~%" lifted-expr))
;	 (lexpr (car lifted-expr))
;	 (dummy (format t "~a~%" (why2java-string* lexpr)))
;	 (seq (cdr lifted-expr))
;	 (dummy (format t "~a~%" seq)))
;    (progn
;      (when seq
;	(why2java* file (if (eq (length seq) 1)
;			    (car seq)
;			  (mk-why-sequential-composition seq)) t ))
;      (indent
;       file
;       (if noreturn
;	   (format nil "~a;~%" (why2java-string* lexpr))
;	   (format nil "return ~a;~%"
;	       (why2java-string* lexpr))))))) ;lexpr ;)


(defmethod why2java* ((file stream) (expr why-conditional) &optional noreturn)
  (when *why2java-trace* 
    (format t "Function: why2java*-why-conditional: ~a ~%" (identifier expr)))
  (if (false-branch expr)
      (let* ((lifted-if (lift-let* (branch-condition expr)))
	     (lexpr-if (car lifted-if))
	     (seq-if (if (cdr lifted-if)
			 (if (eq (length (cdr lifted-if)) 1)
			     (car (cdr lifted-if))
			   (mk-why-sequential-composition (cdr lifted-if)))
		       nil)))
;	     (lifted-false (lift-let* (false-branch expr)))
;	     (lexpr-false (car lifted-false))
;	     (seq-false (if (eq (length (cdr lifted-false)) 1)
;			    (car (cdr lifted-false))
;			  (mk-why-sequential-composition (cdr lifted-false))))
;	     (lifted-true (lift-let* (true-branch expr)))
;	     (lexpr-true (car lifted-true))
;	     (seq-true (if (eq (length (cdr lifted-true)) 1)
;			   (car (cdr lifted-true))
;			 (mk-why-sequential-composition (cdr lifted-true)))))
	(progn
	  (when seq-if
	    (why2java* file seq-if t))
	  (indent
	   file (format nil "if (~a) " (why2java-string* (branch-condition expr))))
	  (block-java
	   file
;	   (when seq-true
;	     (why2java* file seq-true t))
;	   (why2java* file lexpr-true noreturn)
	   (why2java* file (true-branch expr) noreturn)
	   (close-block)
	   (indent 
	    file 
	    (format nil "} else { ~%"))
	   (open-block)
;	   (when seq-false
;	     (why2java* file seq-false t))
;	   (why2java* file lexpr-false noreturn))))
	   (why2java* file (false-branch expr) noreturn))))
	(why2java* file (true-branch expr) noreturn)))
;	    (format nil "~a" (why2java-string* (branch-condition expr))))))

; This is not good! Should set the field expr to be the "seq; lexpr". Otherwise seq "disappears".
(defmethod why2java* ((file stream) (expr why-let) &optional noreturn)
  (when *why2java-trace* 
    (format t "Function: why2java*-why-let: ~a ~%" (identifier expr)))
  (let* ((lifted (lift-let* (expr expr)))
	 (lexpr (car lifted))
	 (seq (if (cdr lifted)
		  (if (eq (length (cdr lifted)) 1)
		      (car (cdr lifted))
		    (mk-why-sequential-composition (cdr lifted)))
		nil)))
    (progn
      (when seq
	(why2java* file seq t))
      (indent 
       file
       (format nil "final ~a ~a = ~a;~%" 
	       (why2java-type* (type (identifier expr))) ; note identifier is a why-binding!
	       (identifier (identifier expr)) ; confusing, I know
	       (why2java-string* lexpr)))
      (why2java* file (in_expr expr) noreturn))))

(defmethod why2java* ((file stream) (expr why-sequential-composition) &optional noreturn)
  (when *why2java-trace* 
    (format t "Function: why2java*-why-sequential-composition: ~a ~%" (identifier expr)))
  (dolist (e (exprs expr))
    (why2java* file e noreturn)))

; This should not be
(defmethod  why2java-string* ((expr why-sequential-composition) &optional mop lr type)
  (format nil "~{ ~a;~% ~}" (exprs expr)))

(defmethod why2java* ((file stream) (def why-adt-def) &optional noreturn)
  (when *why2java-trace* 
    (format t "Function: why2java*-why-adt-def: ~a ~%" (identifier def)))
  (indent
   file
   (format nil "public class ~a " (identifier def)))
  (block-java
   file
   (indent file (format nil "public ~a() {}~%" (identifier def))))
;   (write-adt-class file (constructors def)))
  (indent
   file
   (format nil "~%"))
  (write-adt-constructors file (constructors def) (identifier def))
  (write-adt-accessors file (constructors def) (identifier def))
;  (write-adt-accessors-hof file (constructors def) (identifier def))
  (write-adt-recognizers file (constructors def) (identifier def))
  (write-adt-recognizers-hof file (constructors def) (identifier def)))


(defun write-adt-class (file constructors)
  (dolist (cons constructors)
    (indent
     file
     (format nil "boolean ~a = false;~%" (recognizer cons)))
    (dolist (arg (constructor-arguments cons))
      (indent
       file
       (format nil "~a~a;~%" (why2java-string* arg) (recognizer cons))))))

(defun write-adt-recognizers (file constructors type-name)
  (dolist (cons constructors)
    (indent
     file
     (format nil "public boolean ~a(~a ~(~a~)) "
	     (recognizer cons) type-name type-name))
    (block-java
     file
     (indent file
	     (format nil "return ~(~a~).getClass().getName().equals(\"~a\");~%"
		     type-name
		     (constructor cons))))
    (indent
     file
     (format nil "~%"))))

(defun write-adt-recognizers-hof (file constructors type-name)
  (dolist (cons constructors)
    (indent
     file
     (format nil "public Lambda<~a,Boolean> ~a = new Lambda<~a,Boolean>() "
	     type-name (recognizer cons) type-name))
    (block-java
     file
     (indent file
	     (format nil "public Boolean curry(final ~a ~(~a~)) "
		     type-name type-name))
     (block-java
      file
      (indent file
	      (format nil "return ~a(~(~a~));~%"
		      (recognizer cons)
		      type-name))))
    (indent
     file
     (format nil ";~%"))))

(defun write-adt-accessors (file constructors type-name)
  (dolist (cons constructors)
    (dolist (arg (constructor-arguments cons))
      (indent
       file
       (format nil "public ~a ~a(~a ~(~a~)) "
	       (why2java-type* (type arg))
	       (identifier arg)
	       (constructor cons)
	       (constructor cons)))
      (block-java
       file
       (indent file
	       (format nil "return ~(~a~).~a;~%"
		       (constructor cons)
		       (identifier arg))))
      (indent file (format nil "~%")))))

(defun write-adt-constructors (file constructors type-name)
  (dolist (cons constructors)
    (indent
     file
      (format nil "public class ~a extends ~a "
	      (constructor cons)
	      type-name))
;	      (why2java-list (constructor-arguments cons))))
    (block-java
     file
     (dolist (arg (constructor-arguments cons))
       (indent
	file
	(format nil "~a ~a;~%"
		(why2java-type* (type arg))
		(identifier arg))))
     (indent file
	     (format nil "public ~a(~a) "
		     (constructor cons)
		     (why2java-list (constructor-arguments cons))))
     (block-java
      file
      (dolist (arg (constructor-arguments cons))
	(indent
	 file
	 (format nil "this.~(~a~) = ~(~a~);~%"
		 (identifier arg) (identifier arg))))))
    (indent
     file
     (format nil "~%"))))



;	     (format nil "~a ~a = new ~a();~%" type-name
;		     type-name
;		     type-name))
;     (indent file
;	     (format nil "~a.~a = true;~%" type-name
;		     (recognizer cons)))
;		(identifier arg)
;		(constructor cons)
;		(identifier arg))))
;     (indent file
;	     (format nil "return ~a;~%" type-name)))



(defmethod why2java* ((file stream) (expr why-array-assignment) &optional noreturn)
  (when *why2java-trace* 
    (format t "Function: why2java*-why-array-assignment: ~a ~%" (identifier expr)))
  (indent
   file
   (format nil "~a[~a] = ~a;~%"
	   (identifier expr)
	   (why2java-string* (index expr)) (expr expr))))

 (defmethod why2java-string* ((expr why-array-assignment)  
			     &optional mop lr type)
;  (format nil " ~a[~a] = ~a "
   (format nil " Prelude.update(~a,~a,~a) "
          (identifier expr)
	  (why2java-string* (index expr))
	  (why2java-string* (expr expr))))

; this is not correct
(defmethod why2java-string* ((expr why-record-assignment)  
			     &optional mop lr type)
  (format nil "~a.update(~a)"
	  (identifier expr)
	  (why2java-list (mapcar #'(lambda (a) (expr a)) 
				 (assignments expr)))))

(defmethod why2java-string* ((expr why-assignment)
			     &optional mop lr type)
  (format nil "final ~a = ~a " ; Let's assume they are final for the moment...otherwise make this optional
	  (why2java-string* (identifier expr))
	  (why2java-string* (expr expr))))
	  
(defmethod why2java-string* ((expr why-record-copy) &optional mop lr type)
  (format nil "(~a)~a.clone()" 
	  (why2java-type* (type expr))
	  (source expr)))

(defmethod why2java-string* ((expr why-record-literal) &optional mop lr type)
  (format nil "new ~a(~a)" 
	  (why2java-type* (type expr))
	  (why2java-list 
	   (mapcar #'(lambda (expr) (expr expr))
		   (assignments expr)))))

(defmethod why2java-string* ((expr why-array-literal) &optional mop lr type)
  (if (primitive-type? (element-type expr))
      (let ((typeobj (why2java-type* (element-type expr) t)))
	(format nil "Prelude.new_~a(~a-~a+1,~a)"
		(why2java-type* (element-type expr))
		(why2java-string* (upper-bound expr))
		(why2java-string* (lower-bound expr))
		(why2java-string* (init-function expr) nil nil typeobj)))
      (let ((typeobj (why2java-type* (element-type expr) t)))
	(if (why-generic-type? (element-type expr))
	    (if (module (element-type expr))
		(format nil "~(~a~).~a.arr_~a.new_array(~a-~a+1,~a)"
			(module (element-type expr))
			(identifier (element-type expr))
			(identifier (element-type expr))
			(why2java-string* (upper-bound expr))
			(why2java-string* (lower-bound expr))
			(why2java-string* (init-function expr) nil nil typeobj))
	      (format nil "arr_~a.new_array(~a-~a+1,~a)"
		      (why2java-type* (element-type expr))
		      (why2java-string* (upper-bound expr))
		      (why2java-string* (lower-bound expr))
		      (why2java-string* (init-function expr) nil nil typeobj)))
	  (if (and (not (why-generic-type? (element-type expr))) ; not necessary anymore
		 (module (element-type expr)))
	    (format nil "~a.~a.new_~a(~a-~a+1,~a)"
		(module (element-type expr))
		(identifier (element-type expr))
		(identifier (element-type expr))
		(why2java-string* (upper-bound expr))
		(why2java-string* (lower-bound expr))
		(why2java-string* (init-function expr) nil nil typeobj))
	    (format nil "~a.new_~a(~a-~a+1,~a)"
		(identifier (element-type expr))
		(identifier (element-type expr))
		(why2java-string* (upper-bound expr))
		(why2java-string* (lower-bound expr))
		(why2java-string* (init-function expr) nil nil typeobj)))))))

;    (format nil "*******")))

(defmethod why2java-string* ((expr why-quantifier) &optional mop lr type)
  (format nil "Prelude.~a(~a,~a,~a)"
	  (quantifier expr)
	  (why2java-string* (lower-bound expr))
	  (why2java-string* (upper-bound expr))
	  (why2java-string* (predicate expr) nil nil "Boolean")))


(defmethod why2java-string* ((expr why-lambda-abstraction) &optional mop lr type)
  (let* ((lifted (lift-let* (expr expr)))
	 (nexpr (append (cdr lifted) (list (car lifted))))
	 (nseq (if (eq (length nexpr) 1)
		   (car nexpr)
		 (mk-why-sequential-composition nexpr)))
	 (dummy (setf (slot-value expr 'expr) nseq))
	 (lexpr (if (why-sequential-composition? (expr expr))
		    (last (exprs (expr expr)))
		  (expr expr)))
	 (seq (if (why-sequential-composition? (expr expr))
		  (reverse (cdr (reverse (exprs (expr expr)))))
		nil)))
    (with-open-stream 
     (stream (make-string-output-stream))
     (write-hof stream 
		(if type
		    type ; type is already translated
		  (why2java-type* (if (why-function-type? (type expr))
				      (range (type expr))
				    (type expr)) t)) ; we always have a function type here?
		(reverse (bindings expr))
		(why2java-string* lexpr)
		seq ) ;(expr expr)))
     (get-output-stream-string stream))))

(defmethod why2java-string* ((expr why-record-selection) &optional mop lr type)
  (format nil "~a.~a"
	  (why2java-string* (identifier expr) mop lr type)
	  (field expr)))

(defmethod why2java-string* ((expr list) &optional mop lr type)
  (why2java-list-semi expr))

(defmethod why2java-string* ((expr why-array-subscription) &optional mop lr type)
  (format nil "~a[~a]"
	  (why2java-string* (identifier expr)) ; identifier can be an expression
	  (why2java-string* (index expr))))

(defmethod why2java-string* ((expr why-conditional) &optional mop lr type)
  (format nil "(~a ? ~a : ~a)"
	  (why2java-string* (branch-condition expr))
	  (why2java-string* (true-branch expr))
	  (if (false-branch expr)
	      (why2java-string* (false-branch expr))
	    "null")))

(defun why2java-list (l)
  (format nil "~{~a~#[~:;,~]~}" (mapcar #'why2java-string* l)))

(defun why2java-list-semi (l)
  (format nil "~{~a~#[~:;;~]~}" (mapcar #'why2java-string* l)))

(defun infix-operator-expr? (op)
  (and (why-name? op)
       (member (identifier op) 
	       '(== != + - * / < <= > >= && \|\| mod))))

(defun prefix-operator-expr? (op)
  (and (why-name? op)
       (member (identifier op) '(- !))))

(defun unary-expression? (expr)
  (and (why-function-application? expr)
       (prefix-operator-expr? (operator expr))
       (= (length (arguments expr)) 1)))

(defun logical-operator? (op)
  (member op '(== != < <= > >= && \|\|)))

;; Java priority of why operators
(defun priority (op)
  (cond ((member op '(* / mod)) 3)
	((member op '(+ -)) 4)
	((member op '(< <= > >=)) 6)
	((member op '(== !=)) 7)
	((equal op '&&) 11)
	((equal op '\|\|) 12)
	(t 0)))

(defun associative? (op)
  (member op '(* + == != && \|\|)))

;; op1 > op3
(defun java-precedence (op1 op2 lr)
  (let ((p1 (priority op1))
	(p2 (priority op2)))
    (or (and (= p1 p2) 
	     (equal lr 'r))
	(= p1 0)
	(< p1 p2))))

(defun need-parentheses? (expr mop lr)
  (and mop 
       (why-function-application? expr)
       (infix-operator-expr? (operator expr))
       (not (unary-expression? expr))
       (java-precedence mop (identifier (operator expr)) lr)))

;; mop is the main operator (nil if there is none)
;; lr is left or right
(defmethod why2java-string* ((expr why-function-application) 
			     &optional mop lr type)
  (let ((parentheses (need-parentheses? expr mop lr))
	(op (operator expr)))
    (cond ((unary-expression? expr)
	   (if (why-function-application? (car (arguments expr)))
	       (format nil "~a(~a)"
		   (why2java-string* op)
		   (why2java-string* (car (arguments expr))))
	     (format nil "~a~a" ; should only do this if 
		     (why2java-string* op)
		     (why2java-string* (car (arguments expr))))))
	  ((infix-operator-expr? op)
	   (format nil "~:[~;(~]~a~:[~; ~]~a~:[~; ~]~a~:[~;)~]"
		   parentheses
		   (why2java-string* (car (arguments expr)) 
				     (identifier op) 'l)
		   (logical-operator? (identifier op))
		   (why2java-string* op)
		   (logical-operator? (identifier op))
		   (why2java-string* (cadr (arguments expr)) 
				     (identifier op) 'r)
		   parentheses))
	  ((constructor expr)
	   (format nil "new ~a(~a)"
		   (why2java-string* op)
		   (why2java-list (arguments expr))))
	  (t 
	   (format nil "~a(~a)" 
		   (cond ((and (why-name? op)
			       (member (kind op)
				       '(parameter formal)))
			  (format nil "~a.curry" (identifier op)))
			 ((not (why-name? op))
			  (format nil "~a.curry" (why2java-string* op)))
			 (t (why2java-string* op)))
		   (why2java-list (arguments expr)))))))

(defmethod why2java-string* ((expr why-name) &optional mop lr type)
  (cond ((equal (identifier expr) '|True|) "true")
	((equal (identifier expr) '|False|) "false")
	((equal (identifier expr) 'abs) "Math.abs")
	((equal (identifier expr) 'max) "Math.max")
	((equal (identifier expr) '=>) "Prelude.implies")
	((equal (identifier expr) '<=>) "Prelude.iff")
	((equal (identifier expr) 'even?) "Prelude.even")
	((equal (identifier expr) 'odd?) "Prelude.odd")
	((equal (identifier expr) 'ceiling) "Prelude.ceiling")
	((equal (identifier expr) 'floor) "Prelude.floor")
	((equal (identifier expr) 'mod) "%")
	(t (if (module expr)
	       (format nil "~(~a~).~a" (module expr) (identifier expr))
	       (format nil "~a" (identifier expr))))))

(defmethod why2java-string* ((expr why-literal) &optional mop lr type)
  (format nil "~a" (value expr)))

(defmethod why2java-string* ((expr symbol) &optional mop lr type )
  (format nil "~a" expr))

(defmethod why2java-string* ((expr why-binding) &optional mop lr type)
  (format nil "~a ~a" 
	  (why2java-type* (type expr))
	  (identifier expr)))

(defmethod why2java-string* ((expr why-cast) &optional mop lr type)
  (format nil "(~a) ~a"
	  (why2java-type* (cast expr))
	  (why2java-string* (expr expr))))
;;
;; Types
;;

(defun primitive-type? (type)
  (and (why-primitive-type? type) 
       (or (equal (identifier type) 'int)
	   (equal (identifier type) 'real)
	   (equal (identifier type) 'bool))))

(defmethod why2java-type* ((type why-primitive-type) &optional object)
  (let ((id (identifier type)))
    (cond ((and object (equal id 'int))
	   "Integer")
	  ((and object (equal id 'real))
	   "Float")
	  ((equal id 'real)
	   "float")
	  ((and object (equal id 'bool))
	   "Boolean")
	  ((equal id 'bool)
	   "boolean")
	  (t (if (module type)
		 (format nil "~a.~a" (module type) id)
	       (format nil "~a" id))))))

(defmethod why2java-type* ((type why-array-type) &optional object)
  (format nil "~a[]" (why2java-type* (type type))))

; we assume one actual for the moment
(defmethod why2java-type* ((type why-generic-type) &optional object)
  (if (module type)
      (format nil "~a<~a>.~a" (module type)
	      (if (module (car (parameters type)))
		  (format nil "~a.~a"
			  (module (car (parameters type)))
			  (identifier (car (parameters type))))
		(identifier (car (parameters type))))
	      (identifier type)) ; should check for empty parameters here too
    (if (consp (parameters type))
	(format nil "~a<~a>"
		(identifier type)
		(if (module (car (parameters type)))
		  (format nil "~a.~a"
			  (module (car (parameters type)))
			  (identifier (car (parameters type))))
		(identifier (car (parameters type)))))
      (format nil "~a" (identifier type)))))

(defmethod why2java-type* ((type why-record-type) &optional object)
  (if (module type)
      (format nil "~a.~a" (module type) (identifier type))
    (format nil "~a" (identifier type))))


(defmethod why2java-type* ((type why-function-type) &optional object)
  (format nil "Lambda<~a,~a>" (why2java-type* (car (domain type)) t) (why2java-type* (range type) t)))


;; Defaults

;(defun lift-let-out expr
;  (let* ((lifted-expr (lift-let* expr))
;	 (expr (car lifted-expr))
;	 (seq (cdr lifted-expr)))
;    (if seq
;	(mk-why-sequential-composition (append seq (list expr)))
;      expr)))

(defmethod  why2java* ((file stream) expr &optional noreturn)
  (let* ((dummy (format t "Enter lifted expr: ~a~%" (why2java-string* expr)))
	 (lifted-expr (lift-let* expr))
	 (dummy (format t "Leave lifted-expr : ~a~%" lifted-expr))
	 (lexpr (car lifted-expr))
	 (dummy (format t "Translated lifted-expr: ~a~%" (why2java-string* lexpr)))
	 (seq (cdr lifted-expr))
	 (dummy (format t "Prefix: ~a~%" seq)))
    (progn
      (when seq
	(why2java* file (if (eq (length seq) 1)
			    (car seq)
			  (mk-why-sequential-composition seq)) t ))
      (indent
       file
       (if noreturn
	   (format nil "~a;~%" (why2java-string* lexpr))
	   (format nil "return ~a;~%"
	       (why2java-string* lexpr))))))) ;lexpr ;)

(defmethod  why2java-string* (expr &optional mop lr type)
  (format nil "/* ~a */" expr))


(defmethod why2java-type* (type &optional object)
  (format nil "/* type: ~a */" type))




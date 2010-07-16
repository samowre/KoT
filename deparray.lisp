;; --------------------------------------------------------------------
;; PVS
;; Copyright (C) 2006, SRI International.  All Rights Reserved.

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

(in-package :pvs)

(defvar *eqtype-obligations*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Defining Lisp representations for the types so that we can then
;;pass type parameters in the Lisp code.  These type parameters are
;;used for enumeration and destructive updates.  The main type structures
;;we need are:  subrange, scalar, subtype, and tuple/record.

(defstruct pvs-lisp-subrange  ;;represents [low, high)
  low high)

(defstruct pvs-lisp-scalar
  constructors)

(defstruct pvs-lisp-subtype
  supertype predicate)

(defstruct pvs-lisp-tuple
  elemtypes)

(defstruct pvs-lisp-dep-binding
  name type)

(defstruct pvs-lisp-array
  bound offset rangetype)

(defstruct pvs-lisp-funtype
  domain range)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *pvs-lisp-types-alist*
  (list (cons *boolean* 'boolean)
	(cons *naturalnumber*  'natural)
	(cons *integer* 'integer)
	(cons *rational* 'rational)
	(cons *real* 'real)
	(cons *number* 'number)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;converts PVS types into a Lisp representation that can be used
;;as arguments by late-binding functions.  The idea behind reifying
;;the types is to make certain definitions executable, e.g.,
;;quantification is executable when the type is bounded, or to make
;;them more efficiently executable, e.g., using an array representation. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod pvs-lisp-type ((type type-name) bindings)
  (if (formal-type-decl? (declaration type)) ;;must be in bindings
      (cdr (assoc type bindings :key #'declaration))
      (if (enum-adt? type)
	  (make-pvs-lisp-scalar
	   :constructors (pvs2cl-constructors (constructors type)
					      type))	  
	  `(quote ,(id type)))))

(defun get-pvs-lisp-subrange (type bindings)
  (let ((range (subrange? type)))
    (if range
	(make-pvs-lisp-subrange
	 :low (pvs2cl_up* (car range) bindings)
	 :high `(1+ ,(pvs2cl_up* (cdr range) bindings)))
	(let ((below (below? type)))
	  (if below
	      (make-pvs-lisp-subrange
	       :low 0
	       :high (pvs2cl_up* below bindings nil))
	      (let ((upto (upto? type)))
		(if upto
		    (make-pvs-lisp-subrange
		     :low 0
		     :high (1+ (pvs2cl_up* upto bindings nil)))
		    nil)))))))
					    

(defmethod pvs-lisp-type ((type subtype) bindings)
  (with-slots (supertype predicate) type
    (let ((bind (assoc type *pvs-lisp-types-alist*
			:test #'tc-eq)))
      (if bind
	  `(quote ,(cdr bind))
	  (let ((subrange (get-pvs-lisp-subrange type bindings)))
	    (or subrange
		(make-pvs-lisp-subtype
		 :supertype (pvs-lisp-type supertype bindings)
		 :predicate (pvs2cl_up* predicate bindings nil)))))))) ;;livevars?

(defmethod pvs-lisp-type ((type enumtype) bindings)
  (with-slots (constructors) type
    (make-pvs-lisp-scalar
     :constructors (pvs2cl-constructors constructors (adt type)))))

(defmethod pvs-lisp-type ((type tupletype) bindings)
  (make-pvs-lisp-tuple 
   :elemtypes (pvs-lisp-type (types type) bindings)))

(defmethod pvs-lisp-type ((type recordtype) bindings)
  (make-pvs-lisp-tuple 
   :elemtypes (pvs-lisp-type (fields type) bindings)))


(defmethod pvs-lisp-type ((type list) bindings)
  (cond ((null type) nil)
	(t (if (binding? (car type))
	       (let* ((ty1 (car type))
		      (car-binding (if (assoc ty1 bindings);;was rassoc
				       (pvs2cl-newid (id ty1) bindings)
				       (lisp-id (id ty1)))))
		 (cons (make-pvs-lisp-dep-binding :name car-binding
						  :type (pvs-lisp-type ty1 bindings))
		       (pvs-lisp-type (cdr type)
				(acons ty1
				       car-binding
				       bindings))))
		 (cons (pvs-lisp-type (car type) bindings)
		       (pvs-lisp-type (cdr type) bindings))))))

(defmethod pvs-lisp-type ((type field-decl) bindings)
  (pvs-lisp-type (type type) bindings))

(defmethod pvs-lisp-type ((type dep-binding) bindings)
  (pvs-lisp-type (type type) bindings))

(defmethod pvs-lisp-type ((type funtype) bindings)
  (with-slots (domain range) type
    (make-pvs-lisp-funtype
     :domain (pvs-lisp-type domain bindings)
     :range 
     (if (binding? domain)
	 (let ((id-binding (if (rassoc domain bindings)
			       (pvs2cl-newid (id domain) bindings)
			       (lisp-id (id domain)))))
	   (pvs-lisp-type range
			  (acons domain id-binding bindings)))
	 (pvs-lisp-type range bindings)))))
	
;;     (let ((subrange (get-pvs-lisp-subrange (if (binding? domain)
;; 					       (type domain)
;; 					       domain)
;; 					   bindings)))
;;       (if subrange
;; 	  (make-pvs-lisp-array
;; 	   :size `(- ,(pvs-lisp-subrange-low subrange)
;; 		     ,(pvs-lisp-subrange-high subrange))
;; 	   :offset (pvs-lisp-subrange-low subrange)
;; 	   :range (if (binding? domain)
;; 		      (pvs-lisp-type range (acons domain
;; 						  (pvs2cl-newid (id domain)
;; 								bindings)
;; 						  bindings))
;; 		      (pvs-lisp-type range bindings)))
;; 	  nil))


(defmethod pvs-lisp-type ((type t) bindings)
  (when (enum-adt? type)
    (make-pvs-lisp-scalar :constructors
			  (pvs2cl-constructors constructors (adt type)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;The next section introduces closure-hashes to allow function updates
;;even when the domain type is not indexable.  We don't keep the size
;;of the domain type since closure-hashes.  This size will have to be known
;;in the context when the representation is changed from a closure-hash
;;to an array.  
(defstruct pvs-closure-hash
  hash closure)

(defmacro mk-pvs-closure-hash (hash closure)
  `(make-pvs-closure-hash :hash ,hash
			  :closure ,closure))

;;The argument is first looked up in the hash table, and if it is not
;;found, the closure is used.  
(defmacro pvs-closure-hash-lookup (function argument)
  (let ((funval (gentemp))
	(argval (gentemp)))
  `(let ((,funval ,function)
	 (,argval ,argument))
     (multiple-value-bind (val found)
	 (gethash ,argval (pvs-closure-hash-hash ,funval))
       (if found val
	   (funcall (pvs-closure-hash-closure ,funval) ,argval))))))

(defmacro pvs-function-update (function argument value)
  (let ((funval (gentemp))
	(argval (gentemp))
	(val (gentemp)))
    `(let ((,funval ,function)
	   (,argval ,argument)
	   (,val ,value))
       (if (pvs-closure-hash-p ,funval)
	   (setf (gethash ,argval (pvs-closure-hash-hash ,funval))
		 ,val)
	   (let ((hash (make-hash-table :test #'pvs_equalp)))
	     (setf (gethash ,argval hash) ,val)
	     (mk-pvs-closure-hash hash ,funval)))))
;;check that funval in the above is always a closure. 

(defun pvs-outer-array-lookup (outer-array index)
  (let* ((arr outer-array)
	  (ind index)
	  (inner-array (pvs-outer-array-inner-array arr)))
     (or (and (null (pvs-outer-array-diffs arr))
	      (null (pvs-array-diffs inner-array))
	      (aref (pvs-array-contents inner-array) ind))
	 (let ((lookup-diffs (assoc ind (pvs-outer-array-diffs arr))))
	   (and lookup-diffs
		(cdr lookup-diffs)))
	 (let ((lookup-inner-diffs
		(assoc-last ind (pvs-array-diffs
				inner-array)
			    (- (pvs-array-size inner-array)
			       (pvs-outer-array-offset arr)))))
	   (when lookup-inner-diffs (cdr lookup-inner-diffs)))
	 (aref (pvs-array-contents inner-array) ind))))

(defmacro pvs-array-lookup (pvs-array val)
  (let ((arr (gentemp))
	(ind (gentemp)))
    `(let ((,arr ,pvs-array)
	   (,ind ,val))
     (let ((lookup-diffs (assoc ,ind (pvs-array-diffs ,arr))))
    (or (and lookup-diffs
	     (cdr lookup-diffs))
	(aref (pvs-array-contents ,arr) ,ind)))))

(defmacro pvs-funcall (fun &rest args)
  `(let ((funval ,fun))
     (if (arrayp funval)
	 (aref funval ,@args)
	 (if (pvs-outer-array-p funval)
	     (pvs-outer-array-lookup funval ,@args) 
	     (funcall funval ,@args)))))
;; ;; helpful macros
;; (defmacro in-info (decl)
;;   `(eval-info ,decl))

;; (defmacro in-defn (decl)
;;   `(unary (in-info ,decl)))

;; (defmacro in-defn-m (decl)
;;   `(multiary (in-info ,decl)))

;; (defmacro in-defn-d (decl)
;;   `(destructive (in-info ,decl)))

;; (defmacro in-name (decl)
;;   `(name (in-defn ,decl)))

;; (defmacro in-name-m (decl)
;;   `(name (in-defn-m ,decl)))

;; (defmacro in-name-d (decl)
;;   `(name (in-defn-d ,decl)))

;; (defmacro ex-name (decl)
;;   `(name (ex-defn ,decl)))

;; (defmacro ex-name-m (decl)
;;   `(name (ex-defn-m ,decl)))

;; (defmacro ex-name-d (decl)
;;   `(name (ex-defn-d ,decl)))


;; (defun make-eval-info (decl)
;;   (unless (eval-info decl)
;;     (setf (eval-info decl)
;; 	  (make-instance 'eval-defn-info
;; 			'unary (make-instance 'eval-defn)
;; 			'multiary (make-instance 'eval-defn)
;; 			'destructive (make-instance 'eval-defn)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;this setf macro performs the destructive update.  Now all the arrays
;;are regular, and only records and tuples are simple vectors.  
(defmacro pvs-setf (A i v);;use gentemp below
  (let ((AA (gentemp))
	(ii (gentemp))
	(vv (gentemp)))
  `(let ((,AA ,A)
	 (,ii ,i)
	 (,vv ,v))
     (cond ((stringp AA)
	    (setf (schar AA ii) vv))
	   ((> ii (fill-pointer AA)) nil)
	   (t (when (eq ii (fill-pointer AA))
		(vector-push-extend ii AA))
	      (setf (aref AA ii) vv)))
     AA)))

(defmacro setfval (expr lhs rhs);;lhs is the part of expr that is
                                ;;being updated
  `(progn (setf ,lhs ,rhs)
	  ,expr))

(defmethod pvs2cl_up* :around ((expr expr) bindings livevars)
	   (declare (ignore livevars bindings))
	   (if (variable? expr)
	       (call-next-method) ;;since variables are already declared
	   (let ((lisp-type (pvs2cl-lisp-type (type expr))))
	     (if lisp-type   ;;make sure lisp type expressions
		             ;;are used for subranges
		 `(the ,lisp-type
		    ,(call-next-method))
		 (call-next-method)))))


(defun pvs2cl-operator2 (op actuals arguments def-formals livevars bindings)
  (declare (ignore bindings))
  (pvs2cl-resolution2 op)
  (let ((decl (declaration op)))
    (if *destructive?*
	(let* (;;(decl (declaration op))
	       (module-formals
		(when actuals
		  (loop for x in (formals (module decl))
			when (or (formal-const-decl? x)
				 (formal-type-decl? x))
			collect x)))
	       ;; (defn (args2 (car (last (def-axiom decl)))))
	       ;; (def-formals (when (lambda-expr? defn)
	       ;;                (bindings defn)))
	       (formals (if actuals
			    (append module-formals
				    def-formals)
			    def-formals))
	       (eval-defn (if actuals
			      (ex-defn-d (declaration op))
			      (in-defn-d (declaration op))))
	       (output-vars (output-vars eval-defn))
	       (check (check-output-vars
		       output-vars 
		       (pairlis
			formals
			(append actuals
				arguments))
		       livevars)))
	  (when (and (null check)
		     *eval-verbose*)
	    (format t "~%Destructive update check failed on ~a[~{~a,~}](~{~a,~})" op actuals arguments))
	  (if check
	      (external-lisp-function-d (declaration op))
	      (external-lisp-function2 (declaration op))))
	    (external-lisp-function2 (declaration op)))))

;;External application.  Actuals, if any, are appended to the
;;front of argument list.  
(defun mk-fun2-application (op arguments arg-formals bindings livevars)
  (let* ((decl (declaration op))
	 (actuals (expr-actuals  (module-instance op)))
	 (internal-actuals
	  (or actuals
	      (and (eq (module decl) *external*)
		   (loop for x in (formals (module decl))
			 when (formal-const-decl? x)
			 collect (make-constant-from-decl x)))))
	 (args-free-formals (updateable-vars arguments))
	 (args-livevars (append args-free-formals livevars))
	 ;; (args (if (= (length (bindings (car (last (butlast (def-axiom decl))))))
	 ;; 		(length arguments))
	 ;; 	     arguments
	 ;; 	     (list (make!-tuple-expr* arguments))))
	 )
    (if internal-actuals
	(mk-funapp
	 (pvs2cl-operator2 op actuals arguments arg-formals livevars bindings)
	 (append (pvs2cl_up* internal-actuals
			     bindings
			     args-livevars)
		 (pvs2cl_up* arguments bindings
			     (append (mapcan #'updateable-free-formal-vars
				       actuals);;only updateable
				     ;;parameters of already evaluated
				     ;;exprs needed.
				     livevars))))
	(mk-funapp (pvs2cl-operator2 op nil arguments arg-formals
				     livevars bindings)
		   ;;(pvs2cl-resolution2 op)
		   (pvs2cl_up* arguments  bindings livevars)))))


;;mk-pvs-array-closure is needed to handle conversions to other array
;;formats
(defmethod pvs2cl_up* ((expr lambda-expr) bindings livevars)
  (declare (ignore livevars))
  (let ((bound (array-bound (find-supertype (type expr)))))
    (if bound
	`(mk-pvs-array-closure
	  ,bound
	  ,(pvs2cl-lambda (bindings expr) (expression expr) bindings))
	(pvs2cl-lambda (bindings expr) (expression expr) bindings))))

;;added clause for type-name to allow theory formals
(defmethod pvs2cl_up* ((expr forall-expr) bindings livevars)
  (let* ((binds (bindings expr))
	 (body (expression expr)))
    (if binds
	(let* ((bind1 (car binds))
	       (typ1 (type bind1))
	       (sub (simple-subrange? typ1))
	       (bind-rest (cdr binds))
	       (expr-rest (if bind-rest
			      (make!-forall-expr bind-rest body)
			      body)))
	  (cond ((enum-adt? (find-supertype typ1))
		 (if (subtype? typ1)
		     (pvs2cl_up* (lift-predicates-in-quantifier expr (list (find-supertype typ1)))
				 bindings livevars)
		     `(every ,(pvs2cl-lambda (list bind1)
					     expr-rest
					     bindings)
					     ;;(append (updateable-vars expr)
						;;     livevars)
			     (list ,@(pvs2cl_up* (constructors typ1)
						 bindings livevars)))))
		(sub
		 (if (and (subtype? typ1)
			  (simple-subrange? (supertype typ1)))
		     (pvs2cl_up* (lift-predicates-in-quantifier expr (list (supertype typ1)))
				 bindings livevars)
		     (let  ((lfrom (if (number-expr? (car sub))
				       (number (car sub))
				       (or (pvs2cl_up* (car sub) bindings livevars)
					   (let ((undef (undefined (car sub))))
					     `(funcall ',undef)))))
			    (lto (if (number-expr? (cdr sub))
				     (number (cdr sub))
				     (or (pvs2cl_up* (cdr sub) bindings livevars)
					 (let ((undef (undefined (cdr sub))))
					   `(funcall ',undef)))))
			    (i (gentemp)))
		       `(loop for ,i
			      from ,lfrom
			      to ,lto
			      always
			      (pvs-funcall ,(pvs2cl-lambda (list bind1)
							   expr-rest
							   bindings)
							   ;;(append (updateable-vars expr)
								;;   livevars)
					   ,i)))))
		((type-name? typ1)
		 (let ((lisp_typ1 (cdr (assoc (declaration typ1) bindings
					      :key #'declaration))))
		   `(pvs-lisp-every ,lisp_typ1
				    ,(pvs2cl-lambda (list bind1) expr-rest
						    bindings))))
		(t (let ((undef (undefined bind1 "Hit non-scalar/subrange quantifier in ~% ~a")))
		   `(funcall ',undef)))))
	(pvs2cl_up* body bindings livevars))))


(defun pvs-lisp-every (pvs-lisp-type function)
  (if (pvs-lisp-subrange-p pvs-lisp-type)
      (loop for i from (pvs-lisp-subrange-low pvs-lisp-type)
	    to (1- (pvs-lisp-subrange-high pvs-lisp-type))
	   always (funcall function i))
      (if (pvs-lisp-scalar-p pvs-lisp-type)
	  (loop for ctr in (pvs-lisp-scalar-constructors pvs-lisp-type)
		always (funcall function ctr))
	  (if (pvs-lisp-subtype-p pvs-lisp-type)
	      (pvs-lisp-every (pvs-lisp-subtype-supertype pvs-lisp-type)
			      #'(lambda (x) (or (not (funcall pvs-lisp-subtype-predicate x))
						(funcall function x))))
	      (funcall (throw 'undefined (values 'error (format nil "Non-enumerable type in range of quantifier: ~% ~a" pvs-lisp-type))))))))

(defun pvs-lisp-some (pvs-lisp-type function)
  (if (pvs-lisp-subrange-p pvs-lisp-type)
      (loop for i from (pvs-lisp-subrange-low pvs-lisp-type)
	    to (1- (pvs-lisp-subrange-high pvs-lisp-type))
	   thereis (funcall function i))
      (if (pvs-lisp-scalar-p pvs-lisp-type)
	  (loop for ctr in (pvs-lisp-scalar-constructors pvs-lisp-type)
		thereis (funcall function ctr))
	  (if (pvs-lisp-subtype-p pvs-lisp-type)
	      (pvs-lisp-some (pvs-lisp-subtype-supertype pvs-lisp-type)
			      #'(lambda (x) (or (not (funcall pvs-lisp-subtype-predicate x))
						(funcall function x))))
	      (funcall (undefined pvs-lisp-type "Non-enumerable type in range of quantifier: ~% ~a"))))))

(defmethod pvs2cl_up* ((expr exists-expr) bindings livevars)
  (let* ((binds (bindings expr))
	 (body (expression expr)))
    (if binds
	(let* ((bind1 (car binds))
	       (typ1 (type bind1))
	       (sub (simple-subrange? typ1))
	       (bind-rest (cdr binds))
	       (expr-rest (if bind-rest
			      (mk-forall-expr
				  bind-rest
				body)
			      body)))
	  (cond ((enum-adt? (find-supertype typ1))
		 (if (subtype? typ1)
		     (pvs2cl_up* (lift-predicates-in-quantifier expr (list (find-supertype typ1)))
				 bindings livevars)
		     `(some ,(pvs2cl-lambda (list bind1)
					    expr-rest
					    bindings)
					    ;;(append (updateable-vars expr)
						;;    livevars)
			    (list ,@(pvs2cl_up* (constructors typ1)
						bindings livevars)))))
		(sub
		 (if (and (subtype? typ1)
			  (simple-subrange? (supertype typ1)))
		     (pvs2cl_up* (lift-predicates-in-quantifier expr (list (supertype typ1)))
				 bindings livevars)
		     (let ((lfrom (if (number-expr? (car sub))
				      (number (car sub))
				      (or (pvs2cl_up* (cdr sub) bindings livevars)
					  (let ((undef (undefined (car sub))))
					    `(funcall ',undef)))))
			   (lto (if (number-expr? (cdr sub))
				    (number (cdr sub))
				    (or (pvs2cl_up* (cdr sub) bindings livevars)
					(let ((undef (undefined (cdr sub))))
					  `(funcall ',undef)))))
			   (i (gentemp)))
		       `(loop for ,i
			      from ,lfrom
			      to ,lto
			      thereis
			      (pvs-funcall ,(pvs2cl-lambda (list bind1)
							   expr-rest
							   bindings)
							   ;;(append (updateable-vars expr)
								;;   livevars)
					   ,i)))))
		((type-name? typ1)
		 (let ((lisp_typ1 (cdr (assoc (declaration typ1) bindings
					 :key #'declaration))))
		   `(pvs-lisp-some ,lisp_typ1
				    ,(pvs2cl-lambda (list bind1) expr-rest
						    bindings))))
		(t
		 (let ((undef (undefined expr "Hit non-scalar/subrange quantifier in ~% ~a")))
		   `(funcall ',undef)))))
	(pvs2cl_up* body bindings livevars))))



(defun expr-actuals (modinst) ;;changed to include type actuals
  (loop for act in (actuals modinst)
	collect
	(if (null (type-value act))
	    (expr act)  ;;should crash for actuals 
	    act)))      ;;that are not types or exprs

(defun decl-formals (decl)
  (loop for x in (formals (module decl))
	when (or (formal-const-decl? x)
		 (formal-type-decl? x))
	collect (make-constant-from-decl x)))

(defun eval-actuals (name-expr)
  (or (expr-actuals (module-instance name-expr))
      (let ((decl (declaration name-expr)))
	(and (eq (module decl) *external*)
	     (decl-formals decl)))))

(defun pvs2cl-actuals (actuals bindings livevars)
  (if (consp actuals)
      (if (type-value (car actuals))
	  (cons (pvs-lisp-type (type-value (car actuals)) bindings)
		(pvs2cl-actuals (cdr actuals) bindings livevars))
	  (cons (pvs2cl_up* (expr (car actuals)) bindings livevars)
		(pvs2cl-actuals (cdr actuals) bindings livevars)))
      nil))

(defun pvs2cl-constant (expr bindings livevars)
  (cond ((pvs2cl-primitive? expr)
	 (if (memq (id expr) *primitive-constants*) ;the only constants
	     (pvs2cl-primitive expr)	;else need to return closures.
	     `(function ,(pvs2cl-primitive expr))))
	((lazy-random-function? expr)
	 (generate-lazy-random-lisp-function expr))
	(t (pvs2cl-resolution expr)
	   (if (datatype-constant? expr)
	       (if (scalar-constant? expr)
		   (lisp-function (declaration expr))
		   (let ((fun (lisp-function (declaration expr))))
		     (if (not (funtype? (find-supertype (type expr))))
			 (mk-funapp fun nil)
			 `(function ,fun))));;actuals irrelevant for datatypes
	       (let* ((actuals (eval-actuals expr))
		      (decl (declaration expr))
		      (defns (def-axiom decl))
		      (defn (when defns(args2 (car (last (def-axiom decl))))))
		      (def-formals (when (lambda-expr? defn)
				     (bindings defn)))
		      (fun (if defns
			       (if def-formals
				   (external-lisp-function decl)
				   (pvs2cl-operator2 expr actuals nil nil
						     livevars bindings))
			       (external-lisp-function (declaration expr)))))
		 (assert fun)
		 (mk-funapp fun (pvs2cl-actuals actuals
					    bindings livevars)))))))

(defun pvs2cl-resolution2 (expr)
  (pvs2cl-resolution expr)
  (external-lisp-function2 (declaration expr)))

(defun lisp-function (decl)
  (name (unary (eval-info decl))))

(defun lisp-function2 (decl)
  (name (multiary (eval-info decl))))

(defun pvs2cl-resolution (expr)
  (let* ((decl (declaration expr))
	 (*current-context* (saved-context (module decl)))
	 (*current-theory* (theory *current-context*)))
    (unless (eval-info decl)
      (make-eval-info decl))
    (if (datatype-constant? expr)
	(or (lisp-function (declaration expr))
	    (pvs2cl-datatype expr))
	(or (external-lisp-function (declaration expr))
	    (pvs2cl-external-lisp-function (declaration expr))))))

(defmethod lambda-bindings* ((expr lambda-expr))
  (cons (bindings expr)(lambda-bindings* (expression expr))))

(defmethod lambda-bindings* ((expr t))
  nil)

(defun pvs2cl-external-lisp-function (decl)
  (let* ((defax (def-axiom decl))
	 (*external* (module decl))
	 (*current-theory* (module decl))
	 (undef (undefined decl)))
    (cond ((null defax)
	   (make-eval-info decl)
	   (setf (ex-name decl) undef
		 (ex-name-m decl) undef
		 (ex-name-d decl) undef)
	   undef)
	  (t (let ((formals (loop for x in (formals (module decl))
				  when (or (formal-const-decl? x)
					   (formal-type-decl? x))
				  collect x)))
	       (let* ((id (mk-newfsymb (format nil "~a_~a"
					 (id (module decl))
					 (pvs2cl-id decl))))
		      (id-d (mk-newfsymb (format nil "~a!~a"
					   (id (module decl))
					   (pvs2cl-id decl))))
		      (formal-ids (loop for x in formals
					collect (lisp-id (id x))))
		      (bindings (pairlis formals formal-ids))
		      (defn (args2 (car (last defax))))
		      (defn-bindings (when (lambda-expr? defn)
				       (loop for bnd in
					     (lambda-bindings* defn)
					     append bnd)))
		      (defn-expr (body* defn))
		      (defn-binding-ids
			(make-binding-ids-without-dups defn-bindings nil))
		      (formal-ids2 (append formal-ids
					   defn-binding-ids))
		      (declarations
		       (pvs2cl-declare-vars formal-ids2
					    (append formals defn-bindings))))
		 (make-eval-info decl)
		 (setf (ex-name decl) id)
		 (let ((id2 (mk-newfsymb (format nil "~a__~a"
					   (id (module decl))
					   (pvs2cl-id decl)))))
		   (setf (ex-name-m decl) id2)
		   (let ((*destructive?* nil))
		     (setf (definition (ex-defn-m decl))
			   `(defun ,id2 ,formal-ids2
			      ,@(append (when declarations
					  (list declarations))
					(list 
					 (pvs2cl_up* defn-expr
						     (append (pairlis
							      defn-bindings
							      defn-binding-ids)
							     bindings)
						     nil))))))
		   (eval (definition (ex-defn-m decl)))
		   (assert id2)
		   (compile id2))
		 (setf (ex-name-d decl) id-d)
		 (let ((*destructive?* t)
		       (*output-vars* nil))
		   (setf (definition (ex-defn-d decl))
			 `(defun ,id-d ,formal-ids2
			    ,declarations
			    ,@(append (when declarations
					(list declarations))
				      (list 
				       (pvs2cl-till-output-stable
					(ex-defn-d decl)
					defn-expr
					(append (pairlis defn-bindings
							 defn-binding-ids)
						bindings)
					nil)))))
		   ;;setf output-vars already in
		   ;;pvs2cl-till-output-stable
		   (setf (output-vars (ex-defn-d decl)) *output-vars*))
		 (eval (definition (ex-defn-d decl)))
		 (assert id-d)
		 (compile id-d)
		 (let ((*destructive?* nil)
		       (declarations (pvs2cl-declare-vars formal-ids formals)))
		   (setf (definition (ex-defn decl))
			 `(defun ,id ,formal-ids
			    ,@(append (when declarations
					(list declarations))
				      (list 
				       (pvs2cl_up* defn  bindings nil))))))
		 (eval (definition (ex-defn decl)))
		 (assert id)
		 (compile id)))))))

(defun pvs2cl-theory (theory &optional force?)
  (let* ((theory (get-theory theory))
	 (*current-theory* theory)
	 (*current-context* (context theory)))
    (cond ((datatype? theory)
	   (let ((adt (adt-type-name theory)))
	     (pvs2cl-constructors (constructors adt) adt))
	   (pvs2cl-theory (adt-theory theory))
	   (let ((map-theory (adt-map-theory theory))
		 (reduce-theory (adt-reduce-theory theory)))
	     (when map-theory (pvs2cl-theory (adt-map-theory theory)))
	     (when reduce-theory (pvs2cl-theory (adt-reduce-theory theory)))))
	  (t (loop for decl in (theory theory)
		   do (cond ((type-eq-decl? decl)
			     (let ((dt (find-supertype (type-value decl))))
			       (when (adt-type-name? dt)
				 (pvs2cl-constructors (constructors dt) dt))))
			    ((datatype? decl)
			     (let ((adt (adt-type-name decl)))
			       (pvs2cl-constructors (constructors adt) adt)))
			    ((const-decl? decl)
			     (unless (eval-info decl)
			       (progn
				 (make-eval-info decl)
				 (or (external-lisp-function decl)
				     (pvs2cl-external-lisp-function decl))
				 )))
			    (t nil)))))))

(defun get-field-num (id typ)
  (position id (sort-fields (fields typ) t)
            :test #'(lambda (x y) (eq x (id y)))))

(defmethod pvs2cl_up*  ((expr field-application) bindings livevars)
  (let* ((clarg (pvs2cl_up* (argument expr) bindings livevars))
	 (argtype (find-supertype (type (argument expr))))
	 (nonstr `(project ,(1+ (get-field-num (id expr) argtype)) ,clarg)))
    nonstr))

(defun pvs2cl-add-field-bindings (index field-types field-num expr bindings)
  (if (>= index field-num)
      bindings
      (if (binding? (car field-types))
	  (pvs2cl-add-field-bindings (1+ index) (cdr field-types)
				     field-num expr
				     (acons (car field-types)
					    `(svref ,expr ,index) bindings))
	  (pvs2cl-add-field-bindings (1+ index) (cdr field-types) field-num
				     expr bindings))))

(defmethod pvs2cl-update-nd-type* ((type recordtype) expr arg1 restargs
				   assign-expr bindings livevars)
  (let* ((id (id (car arg1)))
	 (fields  (fields type))
	 (field-num (position  id fields :test #'(lambda (x y) (eq x (id y)))))
	 (new-expr `(svref ,expr ,field-num))	 
	 (field-type (type (find id fields :key #'id) ))
	 (new-bindings (pvs2cl-add-field-bindings 0 fields field-num
						  expr bindings))
	 (newval (pvs2cl-update-nd-type field-type new-expr
					restargs assign-expr new-bindings
					livevars)))
    `(nd-rec-tup-update ,expr ,field-num ,newval)))

(defmethod pvs2cl-update-assign-args ((type recordtype) cl-expr args rhs
					 bindings livevars)
  (let* ((args1 (car args))
	 (id (id (car args1)))
	 (fields (fields type))
	 (field-num (position  id fields :test #'(lambda (x y) (eq x (id y)))))
	 (cl-expr-var (gentemp "E"))	 
	 (newexpr `(svref ,cl-expr-var ,field-num))
	 (field-type (type (find id fields :key #'id) ))
 	 (new-bindings (pvs2cl-add-field-bindings 0 fields field-num
						  cl-expr-var bindings))
	 (other-updateable-types
	  (loop for fld in (fields type)
		when (not (eq id (id fld)))
		nconc (top-updateable-types (type fld) nil)))
	 (newrhs  (if (null (cdr args))
		      rhs
		      (if (member field-type
				  other-updateable-types
				  :test #'compatible?)
			  (pvs2cl-update-nd-type
			   field-type newexpr (cdr args) rhs
			   new-bindings livevars)
			  (pvs2cl-update-assign-args
			   field-type newexpr (cdr args) rhs
			   new-bindings livevars)))))
    `(let ((,cl-expr-var ,cl-expr))
       (setf (svref ,cl-expr-var ,field-num) ,newrhs)
       ,cl-expr-var)))

(defmethod pvs2cl_up* ((expr record-expr) bindings livevars)
  ;;add special case for strings
  (let ((args (pvs2cl_up* (mapcar #'expression
			    (sort-assignments (assignments expr)))
			  bindings livevars)))
    `(pvs2cl_record ,@args)))

(defun mk-fun-array (expr size) ;;okay to use aref here instead of pvs-setf
  (if (or (simple-vector-p expr) (vectorp expr)
	  (hash-table-p expr) (null size))
      expr
      (cond ((pvs-outer-array-p expr)
	     (let* ((arr (make-array size :initial-element 0
				     :fill-pointer size
				     :adjustable t))
		    (inner-array (pvs-outer-array-inner-array expr))
		    (contents (pvs-array-contents inner-array))
		    (inner-size (pvs-array-size inner-array))
		    (offset (pvs-outer-array-offset expr))
		    (outer-diffs (pvs-outer-array-diffs expr))
		    (inner-diffs (pvs-array-diffs inner-array)))
	       (break "mk-fun-array")
	       (loop for i from 0 to (1- size) do
		     (setf (aref arr i)(aref contents i)))
	       (loop for (x . y) in inner-diffs
		     as i from (1+ offset) to inner-size
		     do (setf (aref arr x) y))
	       (insert-array-diffs outer-diffs arr)))
	    (t (let ((arr (make-array size :initial-element 0
				      :fill-pointer size
				     :adjustable t)))
		 (loop for i from 0 to (1- size) do
		       (setf (aref arr i)(funcall expr i)))
		 arr)))))

(defmethod pvs2cl-update-assign-args ((type funtype) cl-expr args rhs
					 bindings livevars)
      (let* ((args1 (car args))
	     (cl-args1 (pvs2cl_up* (car args1) bindings
					   livevars))
	     (lhsvar (gentemp "LHS"))
	     (bound (array-bound type))
	     (cl-bound (pvs2cl_up* bound
				   bindings livevars))
	     (cl-expr (if (symbolp cl-expr)
			  cl-expr
			  `(mk-fun-array ,cl-expr ,cl-bound)))
	     (cl-expr-var (gentemp "E"))
	     (newexpr `(aref ,cl-expr-var ,lhsvar))
	     (newrhs (if (null (cdr args))
		      rhs
		      (pvs2cl-update-nd-type
		      (range type) newexpr (cdr args) rhs
		      bindings livevars))))
	(push (list lhsvar cl-args1) *lhs-args*)
	`(pvs-setf ,cl-expr ,lhsvar ,newrhs)))

(defmethod pvs2cl_up* ((expr record-expr) bindings livevars)
  (let ((args (pvs2cl_up* (mapcar #'expression
			    (assignments expr))
			  bindings livevars)))
    `(pvs2cl_record ,@args)))

(defun pvs2cl-update* (type cl-expr assigns bindings livevars)
    (if (consp assigns)
      (let* ((args (arguments (car assigns)))
	    (rhs-expr (expression (car assigns)))
	    (rhsvar (gentemp "RHS"))
	    (rhs (pvs2cl_up* rhs-expr bindings
			     livevars))
	    (cl-expr (if (funtype? type)
			 (let* ((bound (array-bound type))
				(cl-bound (pvs2cl_up* bound
						      bindings livevars)))
			   `(mk-fun-array ,cl-expr ,cl-bound))
			 cl-expr))
	    (exprvar (gentemp "E")))
	(let* ((*lhs-args* nil)
	       (new-cl-expr
		(pvs2cl-update-assign-args type exprvar args rhsvar
					   bindings
					   (append (updateable-vars rhs-expr)
						   livevars)))
	       (lhs-bindings (nreverse *lhs-args*))
	       (new-expr-body `(let ((,rhsvar ,rhs)
				     (,exprvar ,cl-expr))
				 ;;(declare ((simple-array t) ,exprvar))
				 ,new-cl-expr
				 ,exprvar))
	       (newexpr-with-let
		(if lhs-bindings 
		    `(let ,lhs-bindings ,new-expr-body)
		    new-expr-body))
		   )
	  (pvs2cl-update*
	   type newexpr-with-let (cdr assigns) bindings
	   ;;because later assignments are evaluated first.
	   (append (updateable-vars (car assigns))
			   livevars))))
      cl-expr))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;modified lambda-exprs to return an array when it is only being
;;domain restricted.
(defmethod pvs2cl_up* ((expr lambda-expr) bindings livevars)
  (with-slots ((expr-bindings bindings) expression) expr
    (break "lambda")
      (if (and *destructive?*
	       (singleton? expr-bindings)
	       (array-bound (type expr))
	       (application? expression)
	       (same-declaration (car expr-bindings)
			  (argument expression))
	       (not (member (car expr-bindings)
			    (freevars (operator expression))
			    :test #'same-declaration)))
	  (pvs2cl_up* (operator expression) bindings livevars)
	  (pvs2cl-lambda (bindings expr) (expression expr) bindings))))



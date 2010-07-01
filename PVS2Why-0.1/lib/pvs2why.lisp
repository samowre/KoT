;; Release: PVS2Why-0.1 (11/10/07)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PVS to Why Translator
;; Based on the PVS to Lisp and PVS to Clean translators
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;; Transformation from PVS object to Why objects

(defstruct why-info id type definition analysis)

(defvar *livevars-table*)
(defvar *why-record-defns*)
(defvar *why-annotations* nil)
(defvar *why-renamings*)
(defvar *why-nondestructive-hash* (make-hash-table :test #'eq))
(defvar *why-destructive-hash* (make-hash-table :test #'eq))
(defvar *why-type-defns*)

(defmacro why-hashtable ()
  `(if *destructive?* *why-destructive-hash* *why-nondestructive-hash*))

(defun clear-why-hash ()
  (clrhash *why-nondestructive-hash*)
  (clrhash *why-destructive-hash*))

(defun clear-why-renamings ()
  (setq *why-renamings* nil))

(defun clear-why-record-defns ()
  (setq *why-record-defns* nil))

(defun clear-why-type-defns ()
  (setq *why-type-defns* nil))

(defun reset-variables ()
  (clear-why-hash)
  (clear-why-renamings)
  (clear-why-record-defns)
  (clear-why-type-defns))

(defun why-id (op)
  (let ((hashentry (gethash (declaration op) (why-hashtable))))
    (when hashentry (why-info-id hashentry))))

(defun why-nondestructive-id (op)
  (let ((hashentry (gethash (declaration op) *why-nondestructive-hash*)))
    (when hashentry (why-info-id hashentry))))

(defun why-type (op)
  (let ((hashentry (gethash (declaration op) (why-hashtable))))
    (when hashentry (why-info-type hashentry))))

(defun why-definition (op)
  (let ((hashentry (gethash (declaration op) (why-hashtable))))
    (when hashentry (why-info-definition hashentry))))

(defun why-analysis (op)
  (let ((hashentry (gethash (declaration op) (why-hashtable))))
    (when hashentry (why-info-analysis hashentry))))


(defun test (expr &optional safe)
  (let* ((pr-input	
	  (cond ((stringp expr)	(pc-parse expr 'expr))
		(safe           (pc-parse (format nil "~a" expr) 'expr))
		(t              expr)))
	 (*tccforms* nil)
	 (*generate-tccs* 'all))
    (pc-typecheck pr-input)))

;; These are all the primitives defined within pvs and their why counterparts
;; they are paired against the pvs pairs so no deletions may be made.
(defparameter *why-primitives* 
  '(==  != True False => => <=> && && \|\|
	! pvsWhen <=> + - * / pvsNumberFieldPred < <= >
	>= pvsRealPred pvsIntegerPred pvsIntegerPred pvsRationalsPred 
	floor ceiling rem / even? odd? pvsCons hd tl isCons 
	[!!] isNull pvsRestrict length isMember (!!) ++ reverse))

;; Map from why primitives to pvs primitives
(defparameter *pvs2why-primitives-map*
  (pairlis *pvs2cl-primitives* *why-primitives*))

;; search the associative list of lisp to clean primitives under the
;; test of same-primitive?(name) = key. if the result (entry) is not nil
;; return the associated primitive
(defun pvs2why-primitive-op (name type)
  (let ((entry (assoc name *pvs2why-primitives-map* :test #'same-primitive?)))
    (when entry (mk-why-constant (cdr entry) (pvs2why-type type)))))

;; Here primitive operations are bound to their why equivalents
;; convert the operator and the arguments.
(defun pvs2why-primitive-app (expr bindings livevars declared-type-args declared-type)
  (when *pvs2why-trace*
    (format t "Function: pvs2why-primitive-app: ~{ ~a ~} ~{ ~a ~} ~{ ~a ~} ~a ~%" (arguments expr) livevars declared-type-args declared-type))
  (let ((args (pvs2why* (arguments expr) bindings livevars declared-type-args))
	(why-type (pvs2why-type declared-type)) ; (pvs2why-type (type expr))) ; otherwise 1 + 1 -> real
	(oper (pvs2why-primitive-op (operator expr) (type (operator expr)))))
    (mk-why-function-application oper args (why-name? oper) nil why-type)))

;; entry function
(defun pvs2why (expr &optional context)
  (let* ((*current-context* (if context context *current-context*))
	(*current-theory* (theory *current-context*))
	(*generate-tccs* 'none))
    (pvs2why* expr nil nil nil)))
;;

; Declare supresses compiler warnings
(defmethod pvs2why* ((expr number-expr) bindings livevars declared-type)
  (declare (ignore bindings livevars))
  (when *pvs2why-trace*
    (format t "Function: pvs2why*-number-expr: ~a ~a ~%" expr declared-type))
  (mk-why-literal (number expr) 'number))

; no tuples yet; should turn this into anonymous records
; (defmacro pvs2clean_tuple (args)
;  `(format nil "(~{~a~^, ~})" ,args))

;;
;; Tuples should be translated into records with
;; named positions and selection functions
;;
;;(defmethod pvs2clean* ((expr tuple-expr) bindings livevars)
;;  (let ((args (pvs2clean* (exprs expr) bindings livevars)))
;;    (pvs2clean_tuple args)))

;;
;; Records : A record-expr consists of a list of assignments. An assignment
;; is a class with arguments and an expression. Seems only the first argument
;; of the arguments list is used. Weird. Also, why are they sorted?
;; LL: They are sorted because they are translated into lisp arrays so they
;; need a fixed order.
;; This function translates and translates all of the assignments.

; we have to find an identifier somewhere; this just creates a new record
; we have no corresponding why expression.

; A renaming is a binding of variable and type
(defun rename-expr (expr renamings)
(when *pvs2why-trace*
    (format t "Function: rename-expr: ~a ~%" 
	    expr))
  expr)
; LL : turned this off.. it is not working correctly at the moment. 
;(if (null renamings)
 ;   expr
 ;   (let* ((binding (car renamings))
;	   (type (cdr binding))
;	   (oldvar (id (car binding)))
;	   (newvar (gentemp (format nil "~a" oldvar)))
;	   (rename (rename-expr (rename* oldvar newvar expr) (cdr renamings))) ;renaming happens on the why-objects as a side effect
;	   (copy-var (if (why-record-type? type) ; only alternative atm is array
;			 (mk-why-record-copy newvar oldvar type)
;			 (mk-why-array-copy oldvar newvar type))))
;	  (mk-why-let newvar copy-var expr type))))

;(defun rename-exprs (exprs renamings)
;    (if (null exprs)
;	exprs
;	(cons (rename-expr (car exprs) renamings) (rename-exprs (cdr exprs) renamings)))
;)

; Renamings are bindings of a variable and type to be renamed
; The result of rename-expr 
; the declared type of the record expression should be a type-name
(defmethod pvs2why* ((expr record-expr) bindings livevars declared-type)
  (when *pvs2why-trace*
    (format t "Function: pvs2why*-record-expr: ~a  ~{ ~{ ~a ~a ~} ~} ~a ~%" 
	    expr bindings declared-type))
  (let* ((*why-renamings* nil) ; list of pairs (id,type) 
	 (decl-type-assignments (fields (find-supertype declared-type))) ; was reverse
	 (sorted-assignments (sort-assignments (assignments expr))) ; was reverse
	 (zipped-assignments (pairlis decl-type-assignments sorted-assignments)) ; pairlis reverses the assignments
	 (assignments 
	  (reverse
	   (loop for entry in zipped-assignments collect
		 (mk-why-assignment
			  (caar (arguments (cdr entry)))
			  (pvs2why* (expression (cdr entry)) bindings 
				    (append (updateable-vars (cdr entry)) livevars)
				    (type (car entry))))))) ; or declared-type?
	 (dummy (format t "## ~a ##" declared-type)) ; we should lookup the type...
;	 (lookup-typedef (assoc (type expr) *why-record-defns* :test 'strict-compatible?))
;	 (dummy (format t "lookupType expr: ~a " lookup-typedef))
;	 (why-type (if lookup-typedef
;		       lookup-typedef
;		       (pvs2why-type declared-type))))
	 (why-type (pvs2why-type declared-type)))
      (mk-why-record-literal assignments why-type)))
;	 (renamed (if *why-renamings*
;		      (rename-expr assignments *why-renamings*)
;		      assignments))
;	 (renamed-assignments (mapcar #'car assignments))
;	 (let-assignments (mapcar #'cdr assignments))
;	 (dummy (format t "Here: ~{ ~{ ~a ~a ~} ~} ~%" *why-record-defns*))
;	 (lookup-typedef (assoc (type expr) 
;				*why-record-defns* :test 'compatible?)) ; Look up a compatible type, or should we use the expected type to use strict-compatible? ??
;	 (dummy (format t "Next: ~a ~%" (type expr)))
; If the record types are defined in another file, we look them up.
; 
;	 (lookup-type (if lookup-typedef
;			  (mk-why-record-type (identifier (cdr lookup-typedef)))
;			  (let* ((recdef (pvs2why-record-definition (type expr)))
;				 (typedef (assoc (type expr)
;						*why-record-defns* :test 'strict-compatible?)))
;				(mk-why-record-type (identifier (cdr typedef)))))) 
;         (dummy (format t "~a" *why-record-defns*))
; What if the typedef is outside the module??
;	 (lookup-type (mk-why-record-type (identifier (cdr lookup-typedef))))
;	 (dummy (format t "Here 3!"))
 ;        (record-lit (mk-why-record-literal assignments why-type)))
 ;   (if *why-renamings*
	;(rename-expr record-lit *why-renamings*)
;      record-lit)))
;     (pvs2why-type (type expr)))))

					; (let* ((assigns (loop for entry in (assignments expr) ; get rid of the sorting?
;			collect (cons (caar (arguments entry)) (pvs2why* (expression entry) bindings livevars))))
;        (pairs (unzip (car assigns) (cdr assigns))))
;       (mk-why-record-assignment identifier (car pairs) (cdr pairs))))
;

;; (defun matchlist (index length dummy)
;;  (if (eql index 0)
;;	(if (eql length 0)
;;	    (list dummy)
;;	    (cons dummy (enlist (1- length))))
;;     (cons '_ (matchlist (1- index)(1- length) dummy))))

;;(defun enlist (n)
;;  (if (eql n 0)
;;      nil
;;      (cons '_ (enlist (1- n)))))


;;
;; This is a tuple projection. We should translate those as field
;; applications where the index is replaced with a field selector
;;
;(defmethod pvs2why* ((expr projection-application) bindings livevars)
;  (let* ((ll (length (exprs expr)))
;	 (dummy (gentemp 'ddd))
;	 (match-list (pvs2clean_tuple (matchlist (index expr) ll dummy)))
;	 (expr-list (pvs2clean* expr bindings livevars)))
;    `(let ,match-list = ,expr-list in ,dummy)))
	

; Make a field application a la  variable.field
; A field application is an id with an argument
; Actually, it is an expression with an argument
(defmethod pvs2why*  ((expr field-application) bindings livevars declared-type)
  (when *pvs2why-trace*
    (format t "Function: pvs2why*-field-application: ~a ~%" expr))
  (let* ((record (pvs2why* (argument expr) bindings livevars declared-type))
	 (field (id expr))
	 (why-type (pvs2why-type (type expr))))
    (mk-why-record-selection record field why-type))) ; this clearly is NOT always an identifier...

;
; This is not finished
;
;(defmethod pvs2why* ((expr exists-expr) bindings livevars declared-type)
;  (when *pvs2why-trace*
;    (format t "Function: pvs2why*-exists-expr: ~a ~a ~%" expr declared-type))
;  (let* ((xpr (expression expr))
;	 (bnd (bindings expr))
;	 (bind-ids (pvs2why-make-bindings bnd bindings))
;	 (bind-types (pvs2why-binding-types bind-decls))
;	 (why-bindings (list-bindings bind-ids bind-types)))
;	(pvs2why* xpr
;			   (append (pairlis (bindings expr) bind-ids) bindings)
;			   livevars declared-type)))



(defmethod pvs2why* ((expr exists-expr) bindings livevars declared-type)
(when *pvs2why-trace* ;declared type should be boolean
   (format t "Function: pvs2why*-exists-expr: ~a ~a ~%" expr declared-type))
    (when (simple-subrange? (type (car (bindings expr))))
	(let* ((bind-decls (bindings expr))
	       (bind-ids (pvs2why-make-bindings bind-decls bindings))
	       (bind-types (pvs2why-binding-types bind-decls))
	       (why-bindings (list-bindings bind-ids bind-types))
	       (why-expr (pvs2why* (expression expr) 
			       (append (pairlis (bindings expr) bind-ids ) 
				       bindings) nil declared-type)) ;  nil is ok?
	       (type (type (car (bindings expr))))
	       (range-index (simple-subrange? type))
	       (lower-bound (pvs2why* (car range-index)
				      (append (pairlis (bindings expr) bind-ids) bindings)
				      nil declared-type))
	       (upper-bound (pvs2why* (cdr range-index)
				      (append (pairlis (bindings expr) bind-ids) bindings)
				      nil declared-type))
;	       (range-index (subrange-index type))
;	       (lower-bound (car range-index))
;	       (upper-bound (cadr range-index))
	       (why-lambda (mk-why-lambda-abstraction why-bindings why-expr)))
	  (mk-why-exists lower-bound upper-bound why-lambda))))
;; mk-why-exists lower-bound upper-bound expt
;; mk-why-stuff

;;
;; This translates a list of expressions into a list of why expressions
;;
(defmethod pvs2why* ((expr list) bindings livevars declared-types)
(when *pvs2why-trace*
    (format t "Function: pvs2why*-list: ~{ ~a ~} ~{ ~a ~} ~%" expr declared-types))
  (if (consp expr)
      (cons (pvs2why* (car expr) bindings
			(append (updateable-vars (cdr expr)) livevars) (car declared-types))  ; (type (car expr))) ; this is cheating
	    (pvs2why* (cdr expr) bindings  ;;need car's freevars
			(append (updateable-vars (car expr)) ;;f(A, A WITH ..)
				livevars) (cdr declared-types)))
      nil))

(defmethod pvs2why* ((expr application) bindings livevars declared-type)
  (when *pvs2why-trace*
    (format t "Function: pvs2why*-application: ~a ~a ~%" expr declared-type))
  (with-slots 
   (operator argument) expr
   (let* ((decl-type (type expr))
	  (domain-type (domain (type operator)))
	  (domain-type1 (if (dep-binding? domain-type)
			    (type domain-type)
			  domain-type))
	  (domain-types (if (tupletype? domain-type1)
			    (types domain-type1)
			  (list domain-type1)))
	  (dummy (format t "decl-type ~a ~%" decl-type)))
     (if (constant? operator)
	 (if (pvs2cl-primitive? operator)
	     (pvs2why-primitive-app expr bindings livevars domain-types declared-type) ; declared-type)
	   (if (datatype-constant? operator)
	       (let ((why-args (pvs2why* (arguments expr) bindings livevars domain-types));declared-type))
		     (is-why-constructor (constructor-name-expr? operator))
		     (why-type (pvs2why-type declared-type)) ; (type expr)));  declared-type)) ; (pvs2why-type (type expr)))
		     (why-op (pvs2why-resolution operator))) ; check for destructive?
		 (mk-why-function-application why-op why-args nil nil why-type is-why-constructor))
					;		(mk-funapp (pvs2why-resolution operator)
					;			   (pvs2why* (arguments expr) bindings livevars))
	     (pvs2why-defn-application  expr bindings livevars declared-type)))	; hier gaan we in
       (let* ((why-op (pvs2why* operator bindings
				(append (updateable-vars
					 argument)
					livevars) (type operator)));declared-type))
	      (why-arg (pvs2why* (arguments expr) bindings
				 (append
				  (updateable-free-formal-vars operator)
				  livevars) domain-types))
	      (why-type (pvs2why-type (range (type operator))))
	      (why-arg-list (if (listp why-arg) why-arg (list why-arg))))
	 (if (why-updateable? (type operator))  ; This is not correct
	     (mk-why-array-subscription why-op (car why-arg-list) why-type)
;	     (if (why-name? why-op) ; if we have a variable		 
;		 (mk-why-array-subscription (identifier why-op) (car why-arg-list))
; In why there can only be an array subscription from a variable. However, in java we can easily
; have a`field[10]. So let us not be picky and extend why to allow subscriptions on expressions
; we can always transform the code later if need be.
;
;	       (let* ((temp-var (gentemp "tmp"))
;		      (why-var (mk-why-binding temp-var (pvs2why-type (type operator))))
;		      (subscr (mk-why-array-subscription temp-var (car why-arg-list) why-type)))
;		 (mk-why-let why-var why-op subscr why-type)))
	   (if (and (why-lambda-abstraction? why-op)
		    (isID why-op))  ; if why-op equals (lambda x.x), then just take the argument
	       (car why-arg-list)
	     (mk-why-function-application why-op why-arg-list nil nil (pvs2why-type declared-type))))))))) ; (type expr))))))))))


(defun constant-formals (module)
  (when *pvs2why-trace*
    (format t "Function: constant-formals~%"))
;  (loop for x in (formals module)
;			 when (formal-const-decl? x)
;			 collect (make-constant-from-decl x))
     nil
)


;(defun pvs2why-list (exprs bindings livevars)
;(when *pvs2why-trace*
;  (format t "Function: pvs2why-list: ~a~%" exprs))
; (loop for entry in exprs collect (pvs2why* entry bindings livevars))
; (let ((args (loop for entry in exprs collect (pvs2why* entry bindings livevars))))
;      args)
;)

(defun pvs2why-defn-application (expr bindings livevars declared-type)
  (when *pvs2why-trace*
    (format t "Function: pvs2why-defn-application: ~a ~a ~%" expr declared-type))
  (with-slots 
   (operator argument) expr
;   (pvs2why-resolution operator)  Do not use resolution anymore
   (let* ((actuals (expr-actuals (module-instance operator)))
	  (op-decl (declaration operator))
	  (declared-type1 (type op-decl))
	  (domain-type0 (domain declared-type1))
	  (domain-type1 (if (dep-binding? domain-type0)
			    (type domain-type0)
			  domain-type0))
	  (domain-type2 (if (tupletype? domain-type1)
			    (types domain-type1)
			  (list domain-type1)))
;	  (range-type (range declared-type1))
;	  (declared-type (if (type-name? (declared-type op-decl))
;			     (type-expr (declaration (declared-type op-decl)))
;			   (range (declared-type op-decl))))
	  (dummy (format t "Declared type: ~a ~%" declared-type1))
	  (dummy (format t "Domain type: ~{ ~a ~} ~%" domain-type2))
	  (args (arguments expr))
	  (why-args (pvs2why* args bindings livevars domain-type2)) ; declared-type can be a declared type
	  (why-op (pvs2why* operator bindings livevars (range declared-type1))); declared-type))
	  (module (if (eq *current-theory* (module op-decl))
		      nil
		      (id (module op-decl))))
;	  (why-op (pvs2why* operator bindings livevars))) ; append args
	  (dummy (format t "Before Declared type: ~a ~%" declared-type))
          (why-type (pvs2why-type declared-type))
	  (dummy (format t "Why Declared type: ~a ~%" why-type)))
	  (mk-why-function-application why-op why-args nil module why-type)))) ; translate id-op decl?
;	  (why-args (pvs2why* (append actuals args) bindings livevars))) do not use the module instance actuals
; let's simplify this: We have a defined application, do not try to look it up, but simply translate
; it into a why-application with arguments, the operator is 
; 	  


;     (if *destructive?*
;	 (let* ((defns (def-axiom op-decl))
;		(defn (when defns (args2 (car (last (def-axiom op-decl))))))
;		(def-formals (when (lambda-expr? defn) (bindings defn)))
;		(module-formals nil) ; (constant-formals (module op-decl)))
;		(alist (append (pairlis module-formals actuals)
;			       (pairlis def-formals args)))
;		(alist 	       (if (null defn) nil 
;				   (pairlis def-formals args)))
;		(analysis (why-analysis operator))
;		(check (check-output-vars analysis alist livevars)))
;	   (mk-why-function-application (if check (mk-why-constant (why-id operator))
;					  (why-nondestructive-id operator))
;					why-args)
;	  (format nil "(~a ~{ ~a~})" (if check (why_id operator)
;				       (why_nondestructive_id operator))
;			    why-args)
;	   )
;       (mk-why-function-application (why-id operator) why-args)))))


(defun pvs2why-resolution (op)
  (when *pvs2why-trace*
    (format t "Function: pvs2why-resolution: ~a~%" op))
  (let* ((op-decl (declaration op)))
	(pvs2why-declaration op-decl)))

(defun pvs2why-declaration (op-decl)
  (when *pvs2why-trace*
    (format t "Function: pvs2why-declaration: ~a~%" op-decl)) ; is this ok?
  (if (not (eq (module op-decl) *current-theory*)) ; if the function call is to a different theory
      (mk-why-constant (id op-decl) (pvs2why-type (declared-type op-decl)) (id (module op-decl)))
    (if (or (formal-const-decl? op-decl) (adt-constructor-decl? op-decl)) ; ugly hack
    (mk-why-constant (id op-decl) (pvs2why-type (declared-type op-decl)))
    (let ((d-hashentry (gethash op-decl *why-destructive-hash*)))
    (if (null d-hashentry)
	(let ((op-id (gentemp (format nil "~a" (id op-decl))))
	      (op-d-id (if *pvs2why-unique-names*
			   (gentemp (format nil "d_~a" (id op-decl)))
			 (id op-decl))))
	  (setf (gethash op-decl *why-nondestructive-hash*)
		(make-why-info :id op-id))
	  (setf (gethash op-decl *why-destructive-hash*)
		(make-why-info :id op-d-id));          (if (lamda-expr? defn) (type defn)
	  (let* ((defns (def-axiom op-decl))
		 (defn (when defns (args2 (car (last defns)))))
		 (def-formals (cond ((formals op-decl) (car (formals op-decl)))
				    ((lambda-expr? defn) (bindings defn))))
		 (def-body (if (lambda-expr? defn) (expression defn) defn))
		 (declared-type (declared-type op-decl))
		 (module-formals nil) ;(constant-formals (module op-decl)))
		 (range-type (if (or (formals op-decl)
				     (lambda-expr? defn))
				 (range (type op-decl)) 
			       (type op-decl))))
	    (if (and (arraytype? (type op-decl)) (null (formals op-decl)))
		(pvs2why-resolution-destructive op-decl module-formals (definition op-decl) (type op-decl))
	      (pvs2why-resolution-destructive op-decl (append module-formals def-formals) def-body range-type))))
      (mk-why-constant (why-info-id d-hashentry) (pvs2why-type (declared-type op-decl))))))))

;;
;(defmethod pvs2why* ((expr lambda-expr) bindings livevars)
;  (when *pvs2why-trace*
;    (format t "Function: pvs2why*-lambda-expr: ~a~%" expr))
;    (if (and (why-updateable? (type expr)) (funtype? (type expr)))
;	(let* ((bind-ids (pvs2why-make-bindings (bindings expr) bindings))
;	       (body (pvs2why* (expression expr) (append (pairlis (bindings expr) bind-ids) bindings) nil)) ;  nil is ok?
;	       (index-var (car bind-ids))
;	       (array-cons (pvs2why-array-construction (type expr))) ; new variable??
;	       (array-assign (pvs2why-mk-loop index-var array-cons body))
;	       (seq (list array-cons array-assign)))
;	      (mk-why-sequential-composition seq))
; otherwise -> function definition
;))




;;
;; This is the actual construction of the why function 
;; 
;; For now never called
(defun pvs2why-resolution-nondestructive (op-decl formals body range-type)
  (when *pvs2why-trace*
    (format t "Function: pvs2why-resolution-nondestructive ~a ~a ~a ~a ~%" op-decl formals body range-type))
  (let* ((*destructive?* nil)
	 (bind-ids (pvs2why-make-bindings formals nil))
	 (declared-type (declared-type op-decl))
	 (cl-body (pvs2why* body (pairlis formals bind-ids) nil declared-type))
	 (cl-type (let*	((domain (loop for var in formals
					collect (pvs2why-type (type var))))
			 (range	 (pvs2why-type range-type)))
			(mk-why-function-type domain range)))
	 (hash-entry (gethash op-decl *why-nondestructive-hash*))
	 (precondition (if *why-annotations*
			   (pvs2why-preconditions formals (pairlis formals bind-ids))
			   nil))						
	 (postcondition (if *why-annotations*
			    (pvs2why-postcondition cl-body range-type)
			    nil))
	 (cl-defn (mk-why-function (why-info-id hash-entry) bind-ids cl-body cl-type precondition postcondition))
; function definitions should be followed by a precondition derived from the variables
;	 (cl-defn (mk-why-letrec (id op-decl) bind-ids cl-body cl-type))
	 )
;    (format t "~%Defining (nondestructively) ~a with ~%type ~a ~%as ~a" (id op-decl) cl-type cl-defn)
;    (when *pvs2why-trace*
;      (format t "Defining XML: ~a~%" (why2String* cl-defn)))
    (setf (why-info-type hash-entry) cl-type
	  (why-info-definition hash-entry) cl-defn
	  )
    cl-defn
    )
)

;
; Construct a list of bind variables out of the PVS bind declarations. If a variable
; already exists in the bindings list, generate a new variable for it.
;
(defun pvs2why-make-bindings (bind-decls bindings)                             
  (if (consp bind-decls)                                                      
      (let* ((bb (car bind-decls))                                            
	     (id (id bb))                                                     
	     (newid (if (null (rassoc id bindings))                          
			id
       		      (pvs2cl-newid (id bb) bindings))))
	(cons newid (pvs2why-make-bindings (cdr bind-decls) bindings)))    
    nil))  
  
;; destructive variant. Main difference is the analysis of the *output-vars*
(defun pvs2why-resolution-destructive (op-decl formals body range-type)
  (when *pvs2why-trace*
    (format t "Function: pvs2why-resolution-destructive ~a ~{ ~a ~} ~a~%" op-decl formals range-type))
  (let* ((*destructive?* t)
	 (*output-vars* nil)
	 (bind-ids (pvs2why-make-bindings formals nil))
	 (declared-type range-type)
	 (cl-type ;(pvs2why-type (type op-decl)))
	  (let ((domain 
		 (loop for var in formals
		       collect 
		       (if (assoc (declaration var) 
				  *output-vars* :key #'declaration)
			   (pvs2why-type (type var)) ; unique!
			 (pvs2why-type (type var)))))
		(range (pvs2why-type range-type)))
	    (mk-why-function-type domain range)))
	 (cl-body (if (not body)
		      nil ; abstract function
		      (pvs2why* body (pairlis formals bind-ids) nil declared-type)))
;	 (dummy (format t "Done with body"))

	 (hash-entry (gethash op-decl *why-destructive-hash*))
	 (precondition (if *why-annotations*
			   (pvs2why-preconditions formals (pairlis formals bind-ids))
			   nil))
	 (postcondition (if *why-annotations*
			    (pvs2why-postcondition cl-body range-type)
			    nil))
	 (cl-defn  (mk-why-function (why-info-id hash-entry) bind-ids cl-body cl-type precondition postcondition))
;	 (cl-defn  (mk-why-letrec (id op-decl) bind-ids cl-body cl-type))
	 (old-output-vars (why-info-analysis hash-entry)))
;        (format t "~%Defining (destructively) ~a with ~%type ~a ~%as ~a" (id op-decl) cl-type cl-defn)
;   (when *pvs2why-trace*
;      (format t "Defining XML: ~a~%" (why2String* cl-defn)))
    (setf (why-info-type hash-entry) cl-type
	  (why-info-definition hash-entry) cl-defn
	  (why-info-analysis hash-entry) *output-vars*)
    cl-defn
))
;    (unless (equalp old-output-vars *output-vars*)
;      (pvs2why-resolution-destructive op-decl formals body range-type))))


;;
;; Named expressions are identified by a declaration. Identify if it is found
;; within the passed bindings; Is this necessary? We can just assume they are
;; correct otherwise it would not have passed inspection, right?
;;	  
;
; named expressions can have resolutions, or _do_ have resolutions?
(defmethod pvs2why* ((expr name-expr) bindings livevars declared-type)
  (when *pvs2why-trace*
    (format t "Function: pvs2why*-name-expr: ~a ~{ ~{ ~a ~a ~} ~}  ~a ~%" expr bindings declared-type))
  (let* ((decl (declaration expr))
	 (module (module decl))
	 (super-type (find-supertype (type decl)))
	 (why-type (pvs2why-type (type decl)))
	 (bnd (assoc decl bindings :key #'declaration)))
;    (assert (not (and bnd (const-decl? decl))))
    (if bnd
	(if (consp (free-variables expr)) ; formals are in the free-variables list
	    (mk-why-formal (cdr bnd) why-type)
	    (mk-why-variable (cdr bnd) why-type)) ; type-information is useful
	(if (const-decl? decl)	
	    (if (funtype? super-type)	; We know we have no arguments
;		(if (and (consp (resolutions expr)) (full-modname? (module-instance (car (resolutions expr)))))  ; a resolution is present
;		    (let*  ((reso  (car (resolutions expr)))
;			    (dummy (format t "~a" reso))
;			    (thr-id (id (theory *current-theory*)))
;			    (mod-id (id (module-instance reso))))
;			   (if (eq thr-id mod-id)   ; call function in a different theory?
;			       (mk-why-constant (id expr) )
;			       (mk-why-constant (id expr) mod-id)))
		(if (eq module *current-theory*)		 
		    (mk-why-constant (id expr) why-type )
		    (mk-why-constant (id expr) why-type (id module)))  ; yet we have a funtype then we create a constant
	    	(pvs2why-constant expr decl bindings livevars))
	    (if (formal-const-decl? decl)
		(mk-why-parameter (id expr))
           	(let ((undef (undefined expr "Hit untranslateable expression ~a")))
	             `(funcall ',undef)))))))
; not the most elegant construction
; Use the special why operator for undefined operations?

;; Should redo the latter part

(defun pvs2why-constant (expr op-decl bindings livevars)	
  (when *pvs2why-trace*
    (format t "Function: pvs2why-constant: ~a ~a~%" expr op-decl))
  (let* ((defns (def-axiom op-decl))
	 (defn (when defns (args2 (car (last (def-axiom op-decl))))))
	 (def-formals (when (lambda-expr? defn) (bindings defn)))
					;         (why-expr (pvs2why-resolution expr))
	 (why-expr (if (pvs2cl-primitive? expr)
		       (pvs2why-primitive-op expr (type expr))
		       (pvs2why-resolution expr)))
;    why-expr))

; scrapped the last part
	 (actuals (expr-actuals (module-instance expr)))	; op-decl?
;	 (why-actuals (pvs2why* actuals bindings livevars)))
	 (why-actuals nil))
    (if (and (eq nil why-actuals) (pvs2cl-primitive? expr)) 	; FALSE and TRUE
	why-expr ; we won't find curried functions here, will we?
      (mk-why-function-application why-expr why-actuals))))



;        (if def-formals ; create an explicit lambda expression out of defn and the bindings
;	    (let ((eta-expansion ; expr with bindings a b is converted into lambda a b.expr a b
;		   (mk-lambda-expr def-formals
;		     (mk-application expr
;		       (loop for bd in def-formals
;			     collect (change-class (copy bd) 'name-expr))))))
;	  (pvs2why* eta-expansion bindings livevars)) ; after expansion, translate!
;	(let* ((actuals (expr-actuals (module-instance expr)))
;	       (why-actuals (pvs2why* actuals bindings livevars))) ; translate the actuals
;	      (mk-why-function-application (why_nondestructive_id expr) why-actuals )))


; type?
;	(format nil "(~a ~{ ~a~})" (why_nondestructive_id expr)
		
;;
;; Get the identifier of the nondestructive function
;;


(defun pvs2why-binding-types (bind-decls)
  (loop for bind-decl in bind-decls collect (pvs2why-type (type bind-decl)))
)

;(defun pvs2why* ((expr lambda-expr) bindings livevars)
;
;pvs2why-lambda (bind-decls expr bindings) ;;removed livevars
;  (when *pvs2why-trace*
;    (format t "Function: pvs2why-lambda: ~a ~a~%" bind-decls expr))
;  (let* ((*destructive?* nil)
;	 (bind-ids (pvs2why-make-bindings bind-decls bindings))
;	 (bind-types (pvs2why-binding-types bind-decls))
;	 (why-bindings (list-bindings bind-ids bind-types))
;	 (why-body (pvs2why* expr (append (pairlis bind-decls bind-ids) bindings) nil)))
;        (mk-why-lambda-abstraction why-bindings cl-body))
;)

(defun pvs2why-coerce-types (type1 type2)
  (when *pvs2why-trace*
    (format t "Recordtype? t1 t2 ~a ~a ~%" (print-type type1) (print-type type2))
    (format t "Function: pvs2why-coerce-types ~a ~a ~%" type1 type2))
  (if (recordtype? type1) ; Recordtypes can be record expressions or defined types, we distinguish by print-type
      (if (and (print-type type1) (not (print-type type2)))
	  type1
	(if (subtype-of? type1 type2)
	    type2
	  type1))
    (if (subtype-of? type1 type2)
	type1
      type2)))

(defmethod pvs2why* ((expr lambda-expr) bindings livevars declared-type)
  (declare (ignore livevars))
  (when *pvs2why-trace*
    (format t "Function: pvs2why*-lambda-expr: ~a ~a~%" expr declared-type))
  (let* ((bind-decls (bindings expr))
	 (bind-ids (pvs2why-make-bindings bind-decls bindings))
	 (bind-types (pvs2why-binding-types bind-decls))
	 (why-bindings (list-bindings bind-ids bind-types))
	 (range-decltype (range declared-type))
	 (range-type (type (expression expr)))
	 (coerced-range-type (pvs2why-coerce-types range-decltype range-type))
	 (dummy (format t "Coerceresult ~a ~%" (pvs2why-coerce-types range-decltype range-type)))
;	 (dummy (format t "Is supertype ~a" (subtype-of? range-type range-decltype)))
	 (coerced-type (find-supertype (pvs2why-coerce-types declared-type (type expr)))) ; this doesnt make much sense...
	 (why-expr (pvs2why* (expression expr) 
			     (append (pairlis (bindings expr) bind-ids) 
				     bindings) nil coerced-range-type));  ;(range declared-type)))
	 (dummy (format t "??????? ~%"))
	 (why-type (pvs2why-type coerced-type)) ;declared-type)) ; type
	 (why-lambda (mk-why-lambda-abstraction why-bindings why-expr why-type)))

    (if (and (why-updateable? coerced-type) (funtype? coerced-type)) ; extra conditions??
	(let* ((range-index (simple-subrange? (domain coerced-type)))
	       (dummy (format t "lower ~a upper ~a" (car range-index) (cdr range-index)))
	       (lower-bound (pvs2why* (car range-index)
				      (append (pairlis (bindings expr) bind-ids) bindings)
				      nil (type (car range-index))))
	       (upper-bound (pvs2why* (cdr range-index)
				      (append (pairlis (bindings expr) bind-ids) bindings)
				      nil (type (cdr range-index)))))
	  (mk-why-array-literal lower-bound upper-bound why-lambda (pvs2why-type coerced-type)))
      why-lambda)))

;(defun pvs2why-actuals (actuals)
;  (when *pvs2why-trace*
;    (format t "Function: pvs2why-actuals: ~{ ~a ~} ~%" actuals))
;  (remove nil
;	  (loop for actual in actuals collect
;		(when (and (expr actual) (not (type-eq-decl? (declaration (expr actual)))))
;		  (pvs2why* (expr actual) nil nil nil)))
;	  ))

(defun pvs2why-actuals (actuals)
  (when *pvs2why-trace*
    (format t "Function: pvs2why-actuals: ~{ ~a ~} ~%" actuals))
  (remove nil
	  (loop for actual in actuals collect
		(if (type-value actual)
		    (pvs2why-type (type-value actual))
		  (when (and (expr actual) (not (type-eq-decl? (declaration (expr actual)))))
		    (pvs2why* (expr actual) nil nil nil))))))


(defun pvs2why-type-parameters (actuals)
  (when *pvs2why-trace*
    (format t "Function: pvs2why-type-parameters: ~{ ~a ~} ~%" actuals))
  (format t "Result: ~a ~%" 
  (remove nil
	  (loop for actual in actuals when (formal-type-decl? actual) collect
	;	(when (formal-type-decl? actual)  ********** dummy?
		(pvs2why-type (type-value actual)))))
  (format t "here1")
  (remove nil
	  (append
	   (loop for actual in actuals collect
		 (when (and (not (formal-type-decl? actual))
			    (and (not (formal-const-decl? actual))
				 (and (expr actual)
				      (type-eq-decl? (declaration (expr actual))))))
		   (format t "here")
		   (pvs2why-type (type-value actual))))
	   (loop for actual in actuals collect
		(when (formal-type-decl? actual)
		  (format t "here?")
		  (pvs2why-type (type-value actual)))))))

(defmethod pvs2why* ((expr forall-expr) bindings livevars declared-type)
(when *pvs2why-trace*
   (format t "Function: pvs2why*-forall-expr: ~a~%" expr))
    (when (simple-subrange? (type (car (bindings expr))))
	(let* ((bind-decls (bindings expr))
	       (bind-ids (pvs2why-make-bindings bind-decls bindings))
	       (bind-types (pvs2why-binding-types bind-decls))
	       (why-bindings (list-bindings bind-ids bind-types))
	       (why-expr (pvs2why* (expression expr) 
			       (append (pairlis (bindings expr) bind-ids ) 
				       bindings) nil declared-type)) ;  nil is ok?
	       (type (type (car (bindings expr))))
	       (range-index (simple-subrange? type))
	       (lower-bound (pvs2why* (car range-index)
				      (append (pairlis (bindings expr) bind-ids) bindings)
				      nil declared-type))
	       (upper-bound (pvs2why* (cdr range-index)
				      (append (pairlis (bindings expr) bind-ids) bindings)
				      nil declared-type))
;	       (range-index (subrange-index type))
;	       (lower-bound (car range-index))
;	       (upper-bound (cadr range-index))
	       (why-lambda (mk-why-lambda-abstraction why-bindings why-expr)))
	  (mk-why-forall lower-bound upper-bound why-lambda))))

;(defmethod pvs2why* ((expr forall-expr) bindings livevars)
;(when *pvs2why-trace*
;   (format t "Function: pvs2why*-forall-expr: ~a~%" expr))
;    (if (simple-subrange? (type (car (bindings expr))))
;	(let* ((bind-ids (pvs2why-make-bindings (bindings expr) bindings))
;	       (body (pvs2why* (expression expr) (append (pairlis (bindings expr) bind-ids) ;bindings) nil)) ;  nil is ok?
;	       (index-var (car bind-ids))
;	       (why-index-var (mk-why-variable index-var t))
;	       (aggregator-var (mk-why-variable (gentemp "aggregate") t))
;	       (why-loop (pvs2why-mk-simple-loop why-index-var aggregator-var body ;(subrange-index (type (car (bindings expr))))))
;	       (why-init (mk-why-constant 'true))
;;	       (return-var (mk-why-variable (identifier array-cons) t))
;	       (why-seq (list why-loop aggregator-var)))
;	      (mk-why-let aggregator-var why-init why-seq t 'boolean))
; otherwise -> function definition
;))

;(defun pvs2why-mk-simple-loop (index-var aggregator-var why-body range)
;(when *pvs2why-trace*
;   (format t "pvs2why-mk-simple-loop: ~a ~a ~a ~a%" index-var aggregator-var why-body range))
;  (let* ((why-init-value (mk-why-constant (car range)))
;	 (why-var (mk-why-variable index-var t))
;	 (why-incr-value (mk-why-constant '1))
;	 (why-incr (mk-why-function-application '+ (list why-var why-incr-value)))
;	 (why-bound (mk-why-constant (cdr range)))
;;	 (why-test-bound (mk-why-function-application '< (list why-var why-bound)))
;	 (aggregator (identifier aggregator-var))
;;	 (why-aggregate (mk-why-function-application '&& (list aggregator-var why-body
;	 (why-expr (mk-why-assignment aggregator why-body))
;	 (why-loop-expr (mk-why-sequential-composition (list why-body why-incr)))
;	 (why-while (mk-why-while aggregator-var why-loop-expr nil))) ; variant is unknown atm
;	(mk-why-let index-var why-init-value why-while t 'int))
;)


;(defun pvs2why-mk-loop (index-var array-cons why-body)
;(when *pvs2why-trace*
;   (format t "pvs2why-mk-loop: ~a ~a ~a~%" index-var array-cons why-body))
;  (let* ((why-init-value (mk-why-constant (lower-bound array-cons)))
;	 (why-var (mk-why-variable index-var t))
;	 (why-incr-value (mk-why-constant '1))
;	 (why-incr (mk-why-function-application '+ (list why-var why-incr-value)))
;	 (why-bound (mk-why-constant (upper-bound array-cons)))
;	 (why-test-bound (mk-why-function-application '< (list why-var why-bound)))
;	 (why-array (identifier array-cons))
;	 (why-expr (mk-why-array-assignment why-array why-var why-body))
;	 (why-loop-expr (mk-why-sequential-composition (list why-expr why-incr)))
;	 (why-while (mk-why-while why-test-bound why-loop-expr nil))) ; variant is unknown atm
;	(mk-why-let index-var why-init-value why-while t 'int))
;)


;(defun pvs2why-array-construction(type)
;(when *pvs2why-trace*
;   (format t "pvs2why-array-construction: ~a~%" type))
;  (let* ((range-index (subrange-index (domain type)))
;	 (array-var (gentemp "array"))
;	 (lower-bound (car range-index))
;	 (upper-bound (cadr range-index))
;	 (why-range (pvs2why-type (range type))))
;	 (mk-why-array-construction array-var upper-bound why-range lower-bound))
;)


;(defun pvs2why-multiarray-construction (types)	; this is the domain already
;(when *pvs2why-trace*
;    (format t "pvs2why-multiarray-construction: ~{ ~a ~} ~%" types))
;  (let* ((array-type (car types))
;	 (array-cons (mk-why-array (array-type)))
;	 (why-array-var (car array-cons))
;	 (why-array-cons (cadr array-cons))
;	 (rest-types (cdr types))
;	 (if (consp (rest-types))
;	     (let* (range (subrange-index type)
;	  (
;
;	(loop for entry in (types type) collect
;		(let* ((range (subrange-index entry))
;		       (lower-bound (car entry))
;		       (upper-bound (cadr entry))
;		       (array-var (gentemp "temp_array"))
;		       (array-cons (mk-why-array-construction array-var upper-bound lower-bound))
;			
;		
;
;    
;)



;	     (isArray-type (and (why-updateable? defn-type) and (funtype? type)))
;	     (def-formals (when (lambda-expr? defn) (bindings defn)))
;	     (def-body (if (lambda-expr? defn) (expression defn) defn))


;loop for entry in (assignments expr) ; get rid of the sorting?
;		       collect (caar (arguments entry))))


;;
;; 
(defmethod pvs2why* ((expr if-expr) bindings livevars declared-type)
  (when *pvs2why-trace*
    (format t "Function: pvs2why*-if-expr: ~a ~a ~%" expr declared-type))
  (cond ((branch? expr)
	 (let ((condition (condition expr))
	       (then-part (then-part expr))
	       (else-part (else-part expr)))
	  (mk-why-conditional (pvs2why* condition bindings
			      (append (updateable-vars then-part)
				   (append (updateable-vars else-part)
					   livevars)) declared-type)
	                     (pvs2why* (then-part expr) bindings livevars declared-type)
	                     (pvs2why* (else-part expr) bindings livevars declared-type)
			     (pvs2why-type declared-type))))
	(t (call-next-method))))

;;
;; case Constructor1 v1 v2 of y1 -> e1, y2 -> e2
;; translates to: if x = y1 then e1 else (if x = y2 then e2 .. etc)
;; How do we translate into why??

(defmethod pvs2why* ((expr cases-expr) bindings livevars declared-type)
  (when *pvs2why-trace*
    (format t "Function: pvs2why*-cases-expr: ~a ~a ~%" expr declared-type))
  (let* ((comparison (pvs2why* (expression expr) bindings livevars (type (expression expr)))))
;         (fresh-var 'newvar)
        (pvs2why-cases comparison (selections expr) (else-part expr) bindings livevars declared-type)))
;        (mk-why-let fresh-var comparison why-cases t)
;
; I am always confused about this:
; LET X = Y IN Z = (LAMBDA x: Z(x)) Y
;
; Operator = Z
; Argument = Y
; Bindings operator = X
;
(defmethod pvs2why* ((expr let-expr) bindings livevars declared-type)
  (when *pvs2why-trace*
    (format t "Function: pvs2why*-let-expr: ~a ~a ~%" expr declared-type))
  (let* ((operator (operator expr))
	 (why-argument (pvs2why* (argument expr) bindings livevars (type (argument expr)))) ; instead of livevars use (append (updateable-vars why-expr) livevars)?
	 (dummy (format t "AQUI? : ~a ~a"(argument expr) (type why-argument)))
	 (dummy (format t "Variable type : ~a ~a " (id (car (bindings operator)))
			(pvs2why-type (type (car (bindings operator))))))
;         (why-argument-type (pvs2why-type (type (argument expr))))
	 (bind-decls (bindings operator))
	 (bind-ids (pvs2why-make-bindings bind-decls bindings))
	 (why-var (mk-why-binding (id (car (bindings operator)))
;				  (type why-argument)))
				  (if (pvs2why-type (type (car (bindings operator))))
				      (pvs2why-type (type (car (bindings operator))))
				      (type why-argument))))
	 (dummy (format t "Let-var: ~a" why-var))
; (pvs2why* (car (bindings operator)) bindings livevars declared-type))
;	 (why-var-type (pvs2why-type (type (car (bindings operator)))))
	 (why-type (type why-argument))
	 (why-declared-type (pvs2why-type declared-type))
	 (why-expr (pvs2why* (expression operator) (append (pairlis bind-decls bind-ids) bindings) livevars declared-type))) ; append (bindings operator)
        (mk-why-let why-var why-argument why-expr why-type))	;? should have 2 types!!
)



(defun pvs2why-cases (var selections else-part bindings livevars declared-type)
  (when *pvs2why-trace*
    (format t "Function: pvs2why-cases: ~a ~{ ~a ~} ~a ~%" var selections else-part))
  (if (eq (length selections) 0)	; consp?
      (format t "Error: Case without selection")
      (let* ((entry (car selections))
	     (module (module (type var)))
	     (dummy (format t "Type ~a, Module ~a ~%" (type var) (module (type var))))
	     (recognizer (filterID (id (recognizer (constructor entry)))))
	     (rec-type (mk-why-function-type (type var) (mk-why-primitive-type 'boolean)))
	     (recognizes (mk-why-function-application (mk-why-constant (if module
									   (format nil "~(~a~).~a" module recognizer)
									 (format nil "~a" recognizer))
								       rec-type) (list var)))
	     (bind-decls (args entry))
	     (bind-ids (pvs2why-make-bindings bind-decls bindings))
	     (expression (pvs2why* (expression entry) (append (pairlis bind-decls bind-ids) bindings) livevars declared-type))
	     (why-type (pvs2why-type (type (expression entry))))
	     (accessors (when (consp bind-decls) (accessors (constructor entry))))
	     (let-expression (if (consp bind-decls)
			       (pvs2why-cases-let var bind-decls accessors expression why-type declared-type (constructor entry) module)
			       expression))
	     (alternative (if (eq (length selections) 1)
			    else-part
			    (pvs2why-cases var (cdr selections) else-part bindings livevars declared-type))))
            (mk-why-conditional recognizes let-expression alternative why-type))))

; this is not correct : TODO fix it

; var is not a var, can be an expression!
(defun pvs2why-cases-let (var bindings accessors why-expr why-type declared-type constructor module)
  (when *pvs2why-trace*
    (format t "Function: pvs2why-cases-let ~a ~{ ~a ~} ~{ ~a ~} ~a ~a ~%" var bindings accessors why-expr constructor))
  (let* ((accessor (mk-why-function-application (mk-why-constant (if module
								     (format nil "~(~a~).~a" module (car accessors))
								   (format nil "~a" (car accessors)))
								 (pvs2why-type (type (car bindings))))
						(list (mk-why-cast (mk-why-primitive-type constructor t module)
								   var))
						(type why-expr)))
	 (dummy (format t "Type why-expr: ~a~%" (type why-expr)))
	 (why-var (mk-why-binding (id (car bindings)) (pvs2why-type (type (car bindings)))))
	 (expression (if (eq (length bindings) 1)
			 why-expr
		         (pvs2why-cases-let var (cdr bindings) (cdr accessors) why-expr why-type declared-type constructor module))))
	(mk-why-let why-var accessor expression (type expression)))) ; This should be the subtype!

;;
;; Update expression
;; 
(defmethod pvs2why* ((expr update-expr) bindings livevars declared-type)
  (when *pvs2why-trace*
    (format t "Function: pvs2why*-update-expr ~a ~a ~%" expr declared-type))
  (if (why-updateable? type (expression expr)))
;      (if (and *destructive?*
;	       (not (some #'maplet? (assignments expr))))
;	  (let* ((expression (expression expr))
;		 (assignments (assignments expr))
;		 (*livevars-table* 
;		  (no-livevars? expression livevars assignments))
;		 )
	    ;;very unrefined: uses all
	    ;;freevars of eventually updated expression.
;	    (cond (*livevars-table* ;; check-assign-types
;		   (push-output-vars (car *livevars-table*)
;				     (cdr *livevars-table*))
;		   (pvs2why-update expr bindings livevars))
;		  (t
;		   (when (and *eval-verbose* (not *livevars-table*))
;		     (format t "~%Update ~s translated nondestructively. Live variables ~s present" expr livevars))
      (pvs2why-update expr bindings livevars declared-type)     ;(type (expression expr)))
      (pvs2why* (translate-update-to-if! expr)	; Can we use this elsewhere? 
		  bindings livevars declared-type)))

(defun pvs2why-update (expr bindings livevars declared-type)
  (when *pvs2why-trace*
    (format t "Function: pvs2why-update ~a ~a ~%" expr declared-type))
  (with-slots (type expression assignments) expr
    (let* ((why-expr (pvs2why* expression bindings livevars declared-type))	; why-expr can be a variable.. or not
;				(append (updateable-free-formal-vars
;					 assignments)
					;;assign-args can be ignored
;					livevars)))
;	   (dummy (format t "#HERE? ~%"))
;	   (why-type (pvs2why-type type)) ; u
	   (type-expr (type expression)) ; use the updated expression for type determination
	   (coerced-type-expr (pvs2why-coerce-types type-expr declared-type))
	   (isVariable (typep why-expr 'why-name)) ;?
	   (exprvar (if (not isVariable) ; Should become a real variable
                        (gentemp "E")
			(identifier why-expr)))
;	   (dummy (format t "#LOOK# : ~a ~%" coerced-type-expr))
	   (why-update (pvs2why-update* (type expression)
					expression
					assignments
					bindings
					(append (updateable-vars expression) livevars)
					coerced-type-expr
					)))
;	   (dummy (format t "#THERE ~%")))
; Only if why-expr is  a variable we can directly update the variable,
; otherwise we have to create a variable (let exprvar = why-expr in assignm
; let exprvar = why-expr in (exprvar[index1 = newexpr1, index2 = newexpr2,  ..])
	(if (not isVariable)
	    (mk-why-let exprvar why-expr why-update (pvs2why-type coerced-type-expr))
	    (progn (push (cons (identifier why-expr) (find-supertype type)) *why-renamings*)
	           why-update))
    )
  )
)

;(defun pvs2why-assign-rhs (assignments bindings livevars)
;(when *pvs2why-trace*
;  (format t "Function: pvs2why-assign-rhs ~a~%" assignments))
;  (when (consp assignments)
;      (let ((why-assign-expr (pvs2why* (expression (car assignments))
;					   bindings
;					   (append (updateable-vars
;						    (arguments (car assignments)))
;						   (append (updateable-vars (cdr assignments))
;					   livevars))))
;	    (*lhs-args* nil))
;	(cons why-assign-expr
;	      (pvs2why-assign-rhs (cdr assignments) bindings
;				    (append (updateable-free-formal-vars
;					     (expression (car assignments)))
;					    livevars))))))

;; assignments: a sorted list of assignments, for instance ((t):= 1 (y):= 7)
;; types: a sorted list of types which contains at least each elements of assignemts, for instance (t:real x: nat y:upto(x))
;; pairlis_update-type-assignments removes the useless type declarations in types and do a pairlis
;; in our instance, pairlis will return ((t:real.(t):=1)(y:upto(x).(y):= 7))
(defun pairlis_update-types-assignments (types assignments) 
  (if assignments  
      (if (equalp (id (caar (arguments (car assignments)))) (id (car types))) 
	  (cons (cons (car types) (car assignments)) (pairlis_update-types-assignments (cdr types) (cdr assignments)))
	  (pairlis_update-types-assignments (cdr types) assignments))
      nil))

;; Different one for records?
;; type should be type declaration, not type name!!
(defmethod pvs2why-update* ((type recordtype) exprvar assignments bindings livevars declared-type)
  (when *pvs2why-trace*
    (format t "Function: pvs2why-record-update* ~a ~a ~{ ~a ~} ~a ~%" type exprvar assignments declared-type))
  (setq *why-renamings* '0)
  (let* ((decl-type-assignments (fields (find-supertype declared-type)))
	 (sorted-assignments (sort-assignments assignments))
;	 (dummy (format t "#ICI# ~%"))
;	 (dummy (format t "#decl-type-assignments: ~{ ~a ~} ~%" decl-type-assignments))
;	 (dummy (format t "#sorted-assignments: ~{ ~a ~} ~%" sorted-assignments))
	 (zipped-assignments (pairlis_update-types-assignments decl-type-assignments sorted-assignments)) ;see pairlis_update... def above
	 (assignments (reverse (loop for entry in zipped-assignments collect ; entry = (x:nat.(x):=1)
			 (let* ((assign-expr (expression (cdr entry))) ; assign-expr = 1
				(assign-arg  (caar (arguments (cdr entry)))) ; assign-arg = x
				(upd-vars (mapcar #'declaration (updateable-vars assign-expr)))
; (updateable-vars assign-expr) returns a list of name-exprs.
				(why-assign-expr (pvs2why* assign-expr
					   		   bindings
					   		   (append (updateable-vars assign-expr) livevars) (type (car entry))))
				(why-assign-var (id assign-arg)))
			       (setq *why-renamings* (if (member (declaration exprvar) upd-vars) (+ *why-renamings* 1) *why-renamings*))
			       (mk-why-assignment why-assign-var why-assign-expr)))))
;	 (lookup-typedef (assoc type *why-record-defns* :test 'compatible?)) ; Use compatible? Instead of strict-compatible.
;	 (lookup-type (mk-why-record-type (identifier (cdr lookup-typedef))))
	 
	 (record-assign (mk-why-record-assignment (id exprvar) assignments
						  (pvs2why-type (find-supertype declared-type))))) ; find-supertype not necessary
;    record-assign))
	(if (> *why-renamings* 1) ; what if we have nested updates? or a renamings from a literal?
	    (rename-expr record-assign (list (cons exprvar declared-type)))
	    record-assign)))


(defmethod pvs2why-update* ((type funtype) 
			    exprvar assignments bindings livevars declared-type)
  (when *pvs2why-trace*
    (format t "Function: pvs2why-array-update* ~a ~a ~{ ~a ~} ~%" type exprvar assignments))
  (let* ((seq (loop for entry in assignments collect
			 (let* ((*lhs-args* nil)
				(assign-expr (expression entry))
				(assign-arg  (car (arguments entry)))
				(why-assign-expr (pvs2why* assign-expr
					   		   bindings
					   		   (append (updateable-vars assign-arg)
							     (append (updateable-vars (cdr assignments))
							       livevars)) declared-type))
				(why-assign-var (pvs2why* (car assign-arg)
					   		  bindings
;			  				  (append ;(updateable-vars assign-arg)
;				  	 		     (append ;(updateable-vars (cdr assignments))
					  		       livevars declared-type))
				)
			       (mk-why-array-assignment exprvar why-assign-var why-assign-expr (pvs2why-type type))))))
        (if (> (length assignments) 1)
	    (mk-why-sequential-composition seq)
	    (car seq))))

(defmethod pvs2why-update* ((type subtype) exprvar assignments bindings livevars declared-type)
  (pvs2why-update* (find-supertype type) exprvar assignments bindings livevars declared-type)
)


;;recursion over nested update arguments in a single update.
;(defun pvs2why-update-nd-type (type expr newexprvar args assign-expr
;					   bindings livevars accum)
;(when *pvs2why-trace*
;  (format t "Function: pvs2why-update-nd-type ~a ~a ~a ~a ~a~%" type expr newexprvar args assign-expr))
;  (if (consp args)
;      (pvs2why-update-nd-type* type expr newexprvar (car args) (cdr args) assign-expr
;			      bindings livevars accum)
;      (cons (list newexprvar assign-expr) accum)))


;(defmacro pvswhy_update (array index value)
;   (if  (and *destructive?* *livevars-table*)
;	(mk-why-array-assignment array index value)
;	(mk-why-array-assignment array index value) ; TODO: Should do a copy first
;   ))

 ; `(let ((update-op (if (and *destructive?* *livevars-table*)
;			(mk-why-array-assignment array
;		(format nil "pvsDestructiveUpdate")
;		(format nil "pvsNonDestructiveUpdate"))))
 ;     (format nil  "~a ~a ~a ~a" update-op ,array ,index ,value)))

;(defmethod pvs2why-update-nd-type* ((type funtype) expr newexprvar arg1 restargs
;				   assign-expr bindings livevars accum)
;(when *pvs2why-trace*
;  (format t "Function: pvs2why-update-nd-fun-type ~a ~a ~a ~a ~a~%" expr newexprvar arg1 restargs assign-expr))
;  (let* ((arg1var (gentemp "L"))
;	 (why-arg1 (pvs2why*  (car arg1) bindings
;				 (append (updateable-vars restargs)
;					 livevars)))
;	 (why-assign (mk-why-assignment )
;    (push (list arg1var why-arg1) *lhs-args*)
;    (if (consp restargs)
;	(let* (
;	       (exprvar (gentemp "E"))
;	       (exprval (format nil "pvsSelect ~a ~a" expr arg1var))
;	       (newexprvar2 (gentemp "N"))
;	       (newaccum
;		(pvs2why-update-nd-type 
;		 (range type) exprvar newexprvar2
;		 restargs assign-expr bindings livevars
;		 (cons (list exprvar exprval) accum))))
;	  (cons (list newexprvar (pvswhy_update expr arg1var newexprvar2))
;		newaccum)
;	  )
;	(cons (list newexprvar (pvswhy_update expr arg1var assign-expr))
;	      accum))))



;(defmethod pvs2why-update-nd-type* ((type recordtype) expr newexprvar arg1 restargs
;				   assign-expr bindings livevars accum)
;(when *pvs2why-trace*
;  (format t "Function: pvs2why-update-nd-record-type ~a ~a ~a ~a ~a~%" expr newexprvar arg1 restargs assign-expr))
;  (let ((id (id (car arg1))))
;	(if (consp restargs)
;	    (let* (
;		   (exprvar (gentemp "E"))
;		   (new-expr (format nil "~a.~a" expr id))
;		   (field-type (type (find id (fields type) :key #'id) ))
;		   (newexprvar2 (gentemp "N"))
;		   (newaccum (pvs2why-update-nd-type field-type exprvar newexprvar2
;						       restargs assign-expr bindings
;						       livevars
;						       (cons (list exprvar new-expr) accum))))
;	      (cons (list newexprvar (format nil "{~a & ~a = ~a}" expr id newexprvar2)) newaccum))
;	    (cons (list newexprvar (format nil "{~a & ~a = ~a}" expr id assign-expr))
;		  accum))))

;(defmethod pvs2why-update-nd-type* ((type subtype)  expr newexprvar arg1 restargs
;				   assign-expr bindings livevars accum)
;  (pvs2why-update-nd-type* (find-supertype type) expr newexprvar arg1 restargs
;			  assign-expr bindings livevars accum))

;;
;; typing
;; make a difference between declarations and references?
(defmethod pvs2why-type ((type recordtype))
  (when *pvs2why-trace*
    (format t "Function: pvs2why-type-record: ~a ~%" (print-type type))) ; compatible? instead of strict-compatible?
  (when (print-type type)
    (let*  ((print-type (print-type type)) ; actual can either be a type parameter or an expression, if it is a type parameter it has a type-value
	    (type-parameters (remove nil (loop for actual in (actuals print-type) collect
					       (when (type-value actual)
						 (pvs2why-type (type-value actual))))))
	    (decl (declaration print-type)))
      (if type-parameters
	  (mk-why-generic-type (id decl) (mk-why-record-type (id decl) (id (module decl))) type-parameters (id (module decl)))
	(if (not (eq (module decl) *current-theory*))
	    (mk-why-record-type (id decl) (id (module decl)))
	  (mk-why-record-type (id decl)))))))

;(defmethod pvs2why-type ((type adt-type-name)) ; what if we haven parametrized ADT's?
;  (when *pvs2why-trace*
;    (format t "Function: pvs2why-type-adt-name: ~a~%" type))
;  (if (actuals type)
;      (let* ((type-parameters (loop for actual in (actuals type) collect (pvs2why-type (type-value actual))))
;	     (module (id (adt-theory (adt type)))))
;	(mk-why-generic-type (id type) (mk-why-primitive-type (id type) t) type-parameters module))
;    (mk-why-primitive-type (id type) t)))

   
;   (let ((entry (assoc type *why-record-defns* :test #'compatible?))) ;  (assoc name *pvs2why-primitives-map* :test #'same-primitive?))) 
;	(if entry
;            (mk-why-record-type (identifier (cdr entry)))
;	    (mk-why-record-type (id (print-type type))))))
;	    (format t "~%Record type ~a must be declared." type))))
;   (if (type-name? print-type)
;       (mk-why-record-type print-type)


(defmethod pvs2why-type ((type type-decl))
  (when *pvs2why-trace*
    (format t "Function: pvs2why-type-decl: ~a ~%" type ))
  (mk-why-primitive-type (id type) t))

(defmethod pvs2why-type ((type dep-binding))
  (when *pvs2why-trace*
    (format t "Function: pvs2why-type-dep-binding: ~a ~%" type))
  (pvs2why-type (type type))
)

(defmethod pvs2why-type ((type type-application))
  (when *pvs2why-trace*
    (format t "Function: pvs2why-type-application: ~a ~%" type))
  (pvs2why-type (car (parameters type)))) ; hack, what if we have more pars?

(defmethod pvs2why-type ((type name-expr))
  (when *pvs2why-trace*
    (format t "Function: pvs2why-type-name-expr: ~a ~%" type))
  (pvs2why-type (declaration type)))

(defmethod pvs2why-type ((type formal-const-decl))
  (when *pvs2why-trace*
    (format t "Function: pvs2why-type-formal-const-decl: ~a ~%" type))
  (pvs2why-type (type type)))

(defmethod pvs2why-type ((type formal-type-decl))
  (when *pvs2why-trace*
    (format t "Function: pvs2why-type-formal-type-decl: ~a ~%" type))
  (mk-why-generic-type (id type)))
; or use (pvs2why-type (type type)) ?

(defun pvs2why-record-definition (type) 
(when *pvs2why-trace*
    (format t "Function:pvs2why-record-definition: ~a ~%" type))
  (with-slots (print-type) type
  (let ((entry (assoc type *why-record-defns* :test #'compatible?)))
       (if entry
	   (when *pvs2why-trace* (format t "Already declared ~a" (id print-type))) ; Only declare strict compatible recordtypes once!
	   (let* ((fields (reverse (loop for fld in (reverse (sort-fields (fields (find-supertype type)))) collect 
				(mk-why-binding (id fld) 
					(pvs2why-type (type fld))))))
		  (rectype-name (id print-type))
		  (rectype (mk-why-record rectype-name fields)))
		 (push (cons type rectype) *why-record-defns*)
;		 (format t "Declaring ~a " type)
		 rectype)))))

;compatible?*
;compatible-type*
;strict-compatible?*


;		  (rectype-name (id print-type))

;       (let ((entry (assoc (declaration print-type) *why-record-defns*)))
;	 (if entry
;	     (cadr entry)
;	   (let* ((fields (loop for fld in (fields type) collect 
;				(mk-why-binding (id fld) 
;						(pvs2why-type (type fld)))))
;		  (rectype-name (id print-type))
;		  (rectype (mk-why-record rectype-name fields))
;		  (push (list (declaration print-type) rectype-name rectype) 
;			*why-record-defns*))
;	     rectype)))	; was rectype-name..


(defmethod pvs2why-type ((type tupletype))
  (when *pvs2why-trace*
    (format t "Function: pvs2why-type-tuple: ~a~%" type))
  (loop for elemtype in (types type)
	collect
	(pvs2why-type elemtype)))

(defmethod pvs2why-type ((type funtype))
  (when *pvs2why-trace*
    (format t "Function: pvs2why-type-funtype ~a~%" type)) 
  (if (why-updateable? type)
      (let* ((range-index (simple-subrange? (domain type)))
	     (lower-bound (mk-why-literal '0 'int))
	     (upper-bound (mk-why-literal '0 'int)))
;	     (lower-bound (pvs2why* (car range-index) nil nil)) ; should we have bindings in here?
;	     (upper-bound (pvs2why* (cdr range-index) nil nil))) ; should we have bindings in here?
	  (mk-why-array-type lower-bound upper-bound (pvs2why-type (range type))))
; (pvs2why-type type)))
    (mk-why-function-type 
     (pvs2why-type (domain type)) 
     (pvs2why-type (range type)))))

(defmethod pvs2why-type ((type adt-type-name)) ; what if we haven parametrized ADT's?
  (when *pvs2why-trace*
    (format t "Function: pvs2why-type-adt-name: ~a~%" type))
  (if (actuals type)
      (let* ((type-parameters (remove nil (loop for actual in (actuals type) collect
						(when (type-value actual)
						  (pvs2why-type (type-value actual))))))
	     (module (id (adt-theory (adt type)))))
	(if type-parameters
	    (mk-why-generic-type (id type) (mk-why-primitive-type (id type) t) type-parameters module)
	  (mk-why-primitive-type (id type) t module)))
    (mk-why-primitive-type (id type) t)))

(defmethod pvs2why-type ((type type-name))
  (when *pvs2why-trace*
    (format t "Function: pvs2why-type-name: ~a~%" type))
 (cond ((eq (print-type type) 'void)	;?
        (mk-why-primitive-type 'unit))
       ((eq (id type) 'boolean)
	(mk-why-primitive-type 'bool))
       ((eq (id type) 'bool)
	(mk-why-primitive-type 'bool))
       ((eq (id type) 'nat)
	(mk-why-primitive-type 'int))
       ((eq (id type) 'real)
	(mk-why-primitive-type 'real))
       ((eq (id type) 'number)
	(mk-why-primitive-type 'real))
       (t (pvs2why-type (declaration type)))))

(defmethod pvs2why-type ((type type-eq-decl))
 (when *pvs2why-trace*
    (format t "Function: pvs2why-type-eq-decl: ~a~%" type))
 (let* ((why-id (id type))
	(why-module (if (not (eq (module type) *current-theory*))
			(id (module type))
		      nil)))
   (cond ((recordtype? (type-expr type))
	  (mk-why-record-type why-id why-module))
	 (t (pvs2why-type (type-expr type))))))

(defmethod pvs2why-type ((type subtype))
  (when *pvs2why-trace*
    (format t "Function: pvs2why-type-subtype: ~a~%" type))
  (let ((fs (find-supertype type)))
       (if (eq fs *number*)
	(if (subtype-of? type *integer*)
	    (mk-why-primitive-type 'int)
	    (mk-why-primitive-type 'real))
;	(format nil "int");;Generates nonsense if type is not subtype of int.
	(pvs2why-type (find-supertype type)))))

;;why-updateable? denotes the types that might be destructively updated

(defmethod why-updateable? ((texpr tupletype))
  (why-updateable? (types texpr)))

(defmethod why-updateable? ((texpr arraytype))	;; enums and subrange too .. 
  (or (simple-below? (domain texpr))(simple-upto? (domain texpr))) ; for subrange we need a phase difference
)

;;this is the only case where clean-updateable? can be false, because
;;the given function type is not an array.  
(defmethod why-updateable? ((texpr funtype)) ;;add enum types, subrange.
  (or (simple-below? (domain texpr))(simple-upto? (domain texpr)))
 ; and      (why-updateable? (range texpr))))	; Is this ok???
)

(defmethod why-updateable? ((texpr recordtype))
   t;  used to be t
)
;  (why-updateable? (mapcar #'type (fields texpr)))) ; I don't get this

(defmethod why-updateable? ((texpr subtype)) ; neither do I get this...
  (why-updateable? (find-supertype texpr)))

(defmethod why-updateable? ((texpr list))
  (or (null texpr)
      (and (why-updateable? (car texpr))
	   (why-updateable? (cdr texpr)))))

;;This is subsumed by fall-through case.
;(defmethod clean-updateable? ((texpr type-name))
;  (not (or (eq texpr *boolean*)
;	   (eq texpr *number*))))



;(defmethod clean-updateable? ((texpr actual))
;  (clean-updateable? (type-value texpr)))

(defmethod why-updateable? ((texpr t))
  nil) ;;It is okay to say why-updateable? for uninterpreted
;;or actuals since these will not be updated destructively or otherwise.



; The pvs2why-type-expr methods return the translation of the type-expression
; combined with the parameters, so we can construct a function


; These functions should return the translated type expression
; the extra argument is the variable x that is bound to the type
; predicate
(defmethod pvs2why-type-expr* ((expr type-application) binding bindings)
  (when *pvs2why-trace*
    (format t "Function: pvs2why-type-application-expr*: ~a ~a ~{ ~{ ~a ~a ~} ~} ~%" expr binding bindings))
  (let* ((args (cons (mk-why-formal (car binding) (type expr)) (pvs2why* (parameters expr) bindings nil declared-type))))
        (cons (mk-why-function-application (id (type expr)) args) (list (car binding)))))


; this function is only called when we are constructing the types
; from within type-eq-decl. At the other occasions, bind-decls, ...
; the type-predicate is called with the expression as argument. i.e. (n:idx) => idx?(n) 
(defmethod pvs2why-type-expr* ((expr subtype) binding bindings)
  (when *pvs2why-trace*
    (format t "Function: pvs2why-type-subtype-expr*: ~a ~a ~{ ~{ ~a ~a ~} ~} ~%" expr binding bindings))
  (pvs2why-predicate (predicate expr) binding bindings))

; we ignore the binding when we have a set-function, instead we return the set variable
(defun pvs2why-predicate (set-expr binding bindings)
  (when *pvs2why-trace*
    (format t "Function: pvs2why-predicate: ~a ~a ~{ ~{ ~a ~a ~} ~} ~%" set-expr binding bindings))
  (let* ((bind-decls (bindings set-expr)) ;(append (bindings set-expr) (free-parameters set-expr)))
         (bind-ids (pvs2why-make-bindings bind-decls nil))
 	 (bind-types (loop for var in bind-decls collect (pvs2why-type (type var))))
;	 (bindings (loop for var in bind-ids collect (mk-why-formal var)))
	 (expr (pvs2why* (expression set-expr) (append (pairlis bind-decls bind-ids) bindings) nil declared-type)))
        (cons expr bind-ids))); (pairlis bind-ids bind-types))))

(defun pvs2why-type-eq-decl (type-eq-decl)
  (when *pvs2why-trace*
    (format t "Function: pvs2why-type-eq-decl: ~a ~%" type-eq-decl))
  (let* ((pvs-expr (type-expr type-eq-decl))
	 (newid 'x)
	 (newtype (pvs2why-type (type-value type-eq-decl)))
	 (t_expr (pvs2why-type-expr* pvs-expr (cons newid newtype) nil))
	 (expr (car t_expr))
	 (subtypecall (mk-why-function-application (mk-why-constant (print-type (supertype (type-value type-eq-decl))) newtype)
						   (list (mk-why-formal newid newtype))))
	 (new_expr (mk-why-function-application (mk-why-constant '&& (mk-why-primitive-type 'boolean)) (list subtypecall expr) true))
	 (binders (cdr t_expr))
	 (range (mk-why-primitive-type 'bool nil))	 
	 (type (mk-why-function-type (list newtype) range)))
        (mk-why-logic-predicate (id type-eq-decl) binders new_expr type)))

; What if we have a subtype instead of a type-name?
(defun pvs2why-preconditions (bind-decls bindings)
  (when *pvs2why-trace*
    (format t "Function: pvs2why-preconditions: ~a ~{ ~a ~} ~{ ~{ ~a ~a ~} ~} ~%" *why-annotations* bind-decls bindings))
  (if (consp bind-decls)                                                      
      (let* ((bb (car bind-decls)) ; this is the binding
	     (aa (cdr bind-decls))
	     (id (id bb)) ; id of the variable
	     (type (declared-type bb)) ; type of the variable
  	     (body (if (subtype? type)
		       (pvs2why-precondition-subtype type id bindings)
		       (mk-why-function-application (mk-why-constant type (pvs2why-type type))
						    (list (mk-why-formal id (pvs2why-type type)))))))
	    (if (consp aa)
		(mk-why-function-application (mk-why-constant '&& (mk-why-primitive-type 'boolean))
					     (list body (pvs2why-preconditions aa bindings)))
		body))
      nil))
;
(defun pvs2why-precondition-subtype (type newvar bindings)
  (when *pvs2why-trace*
    (format t "Function: pvs2why-type-precondition-subtype: ~a ~a ~{ ~{ ~a ~a ~} ~} ~%" type newvar bindings))
  (let* ((pred (predicate type))
	 (newvar_type (print-type (supertype type)))
	 (t_expr (pvs2why-type-expr* type newvar bindings))
	 (expr (car t_expr))
	 (binders (cdr t_expr))
	 (oldvar (car binders))
;	 (dummy (format t ":: ~a ~a" oldvar newvar))
	 (t_new_expr (rename* oldvar newvar expr))
;	 (dummy (format t "there!"))
	 (app (mk-why-function-application (mk-why-constant newvar_type t_expr)
					   (list (mk-why-formal newvar newvar_type)))))
        (mk-why-function-application (mk-why-constant '&& (mk-why-primitive-type 'boolean))
				     (list app t_new_expr))))


; 	 (postcondition (pvs2why-postcondition body range-type))
(defun pvs2why-postcondition (body range-type)
  (when *pvs2why-trace*
    (format t "Function: pvs2why-postcondition ~a ~a ~%" body range-type))
  (mk-why-function-application (mk-why-constant range-type (pvs2why-type range-type)) (list body)))


(defun pvs2why-theory (theoryname)
  (when *pvs2why-trace*
    (format t "Function: pvs2why-theory: ~a ~%" theoryname))
  (reset-variables)
  (let* ((theory (get-theory theoryname))
	 (*current-theory* theory)
	 (*current-context* (context theory))
;	 (usings (mapcar #'car (all-usings theory)))
;	 (modules (loop for module in usings collect
;			(pvs2why-one-theory module (id module))))
	 (main (pvs2why-one-theory theory theoryname)))
	(cons main nil))) ;  modules)))

(defun pvs2why-one-theory (theory theoryname)
 (when *pvs2why-trace*
    (format t "Function: pvs2why-one-theory: ~a ~%" (id theory)))
    (cond ((datatype? theory)
	   nil)
;	   (let ((adt (adt-type-name theory)))
;	     (pvs2why-constructors (constructors adt) adt))
;	   (pvs2why-theory (adt-theory theory))
;	   (let ((map-theory (adt-map-theory theory))
;		 (reduce-theory (adt-reduce-theory theory)))
;	     (when map-theory (pvs2why-theory (adt-map-theory theory)))
;	     (when reduce-theory (pvs2why-theory (adt-reduce-theory theory)))))
	  (t (let* ((bindingscollect (remove nil (loop for bind in (formals theory) collect
				    (if (formal-type-decl? bind)
					nil
				      (mk-why-binding (id bind) (pvs2why-type (type bind)))))))
		    (bindings (if (and (consp bindingscollect) (not (car bindingscollect)))
				  (cdr bindingscollect); ****** cdr was car
				  bindingscollect)) 
		    (type-variables (pvs2why-type-parameters (formals theory)))
		    (imports (pvs2why-imports (all-usings theory))) ; (mapcar #'cadr (all-usings theory)))) ; add parameters later
;		    (imports (pvs2why-imports (immediate-usings theory)))
		    (decls (remove nil (loop for decl in (theory theory) collect
			(cond ((type-eq-decl? decl)
			       (let* ((dt (find-supertype (type-value decl)))
				      (pre (if *why-annotations*
					       (pvs2why-type-eq-decl decl)
					       nil)))
				     (cond ;((adt-type-name? dt)
				           ; (pvs2why-constructors (constructors dt) dt))
					   ((recordtype? dt)
					    (pvs2why-record-definition dt))
;					   ((arraytype? dt)
;					    (pvs2why-type dt))
					   (t pre)))) ; otherwise return the preconditionfun
			      ((datatype? decl)
			       (let ((adt (adt-type-name decl)))
				 (pvs2why-constructors (constructors decl) adt)))
;			      ((type-decl? decl)
;			       (mk-why-declaration-adt (id decl)))
			      ((const-decl? decl)
			       (unless (or (eval-info decl)
					   (generated-by decl))  
				 (pvs2why-declaration decl)))
			      (t nil))))))
		    (mk-why-module theoryname bindings decls imports type-variables)))))

; If the imported theory has a (type) parameter, it can happen that it is _not_ mentioned.
; This 
(defun pvs2why-imports (usings)
  (when *pvs2why-trace*
    (format t "Function: pvs2why-imports: ~{ ~{ ~a ~} ~} ~%" usings))
  (loop for cimport in usings collect
	(let* ((import (cadr cimport))
	       (imported-theory (car cimport))
;	       (dummy (format t "Imported actuals: ~a ~%" import))
;	       (dummy (format t "Imported theory: ~a ~%" imported-theory))
	       (why-name (id import))
	       (why-type-parameters (pvs2why-type-parameters (actuals import)))
;	       (why-other-parameters (when (formals imported-theory)
;				       (find-extra-parameters (formals imported-theory))))
;	       (why-type-parameters2 (if why-type-parameters
;					 why-type-parameters
;				       why-other-parameters))
	       (dummy (format t "why-type-parameters: ~{ ~a ~} ~%" why-type-parameters))
	       (why-parameters (pvs2why-actuals (actuals import))))
	  (mk-why-module-import why-name why-parameters why-type-parameters2))))

(defun find-extra-parameters (parameters)
  (when *pvs2why-trace*
    (format t "Function: find-extra-parameters: ~{ ~a ~} ~%" parameters))
  (remove nil (loop for parameter in parameters
		    collect (when (and (type-decl? parameter) (type-value parameter))
			      pvs2why-type (type-value parameter)))))
	

(defun pvs2why-constructors (constructors type-name)
  (when *pvs2why-trace*
    (format t "Function: pvs2why-constructors: ~a : ~{ ~a ~} ~%" type-name constructors))
  (let* ((why-constructors
	  (remove nil
		  (loop for cons in constructors collect
			(let* ((recognizer (recognizer cons))
			       (dummy (format t "~a" (simple-constructor? cons)))
			       (constructor (id cons))
			       (decls (mapcar #'bind-decl (arguments cons)))
			       (bind-ids (pvs2why-make-bindings decls nil))
			       (bind-types (pvs2why-binding-types decls))
			       (arguments (list-bindings bind-ids bind-types)))
			  (mk-why-constructor recognizer constructor arguments))))))
	 (mk-why-adt-def type-name why-constructors)))

;; Release: PVS2Why-0.1 (11/10/07)
;; 
;; Definition of Why Language
;; 
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

(defcl why-type () ; Types
  (identifier :type symbol)
  (module :type symbol))

(defcl why-expr () ; Expressions
  (type :type why-type))

(defcl why-def () ; Definitions
  (identifier :type symbol))

(defcl why-declaration ()) ; Declarations

(defcl why-logic-declaration ()) ; Logic declarations

;;
;; Expressions
;; 

(defcl why-lambda-abstraction (why-expr)
  (bindings :type list)
  (expr :type why-expr))

(defcl why-binding (why-expr) ; Parameters of lambda expressions 
  (identifier :type symbol))

(defcl why-name (why-expr)
  (identifier :type symbol)
  (module :type symbol)
  (kind :type symbol)) ; constant (defined), parameter (module), 
                       ; formal (function),
                       ; variable (let-in)

(defcl why-literal (why-expr)
  (value :type string)
  (kind  :type symbol)) ; number, string

(defcl why-array-subscription (why-expr) ; officially not in the why language
  (identifier :type symbol)		 ; but defined in the logic
  (index :type why-expr))

(defcl why-array-literal (why-expr)
  (upper-bound :type number)
  (lower-bound :type number)
  (init-function :type why-lambda-abstraction)
  (element-type :type why-type))

(defcl why-conditional (why-expr)
  (branch-condition :type why-expr)	; constructions like assignments
  (true-branch :type why-expr)		; in conditionals are allowed
  (false-branch :type why-expr))	; the false branch may be nil

(defcl why-label (why-expr)
  (identifier :type symbol)
  (expr :type why-expr))

; while is internally translated into a try catch with a loop
; inside that raises an exception when the branch condition
; is true
(defcl why-while (why-expr)
  (branch-condition :type why-expr)
  invariant	; optional (assertion)
  variant	; optional (measure)
  (loop-body :type why-expr))


(defcl why-quantifier (why-expr)
  (quantifier :type symbol) ; forall, exists
  (lower-bound :type number)
  (upper-bound :type number)
  (predicate :type why-lambda-abstraction))

;(defcl why-assert (why-expr)
;  assertions
;  expr
;)

;(defcl why-postcondition (why-expr)
;  expr
;  postcondition
;)

;(defcl why-try(why-expr)
;  expr
;  handler = (identifier [identifier] expr)
;)

;(defcl why-raise(why-expr)
; identifier
; epxr  ; optional
; type  ; value-type
;)

;(defcl loop(why-expr)
; expr
; invariant	; isn't this redundant (see why-letrec)
; variant	; measure
;)
;
;(defcl absurd (why-expr)) ; to denote unreachable code

(defcl why-let (why-expr)
  (identifier :type symbol)
  (expr :type why-expr)
  (in_expr :type why-expr))

(defmethod lift-let* ((expr why-let))
  (when *why-types-trace*
    (format t "lift-let*-why-let: ~a ~%" expr))
  (let* ((lift (lift-let* (expr expr)))
	 (nxpr (car lift))
	 (seq (cdr lift)))
    (cons (in_expr expr)
	  (append seq
		  (list (mk-why-assignment (identifier expr)
					   nxpr))))))

(defcl why-sequential-composition (why-expr)
  (exprs :type list))

; Although assignments and updates are not really defined in the why
; language, they are defined in the prelude as an ADT and accessor
; and update functions. In order to have construction functions we
; need a memory model to work with. We define the constructor function
; anyway.
(defcl why-assignment (why-expr)
  (identifier :type symbol)
  (expr :type why-expr))

(defmethod lift-let* ((expr why-assignment))
  (when *why-types-trace*
    (format t "lift-let*-why-assignment: ~a ~%" expr))
  (let* ((lift (lift-let* (expr expr)))
	 (nxpr (car lift))
	 (seq (cdr lift)))
    (progn
      (setf (slot-value expr 'expr) nxpr)
      (cons expr seq))))

(defcl why-array-assignment (why-expr)
  (identifier :type symbol)	
  (index :type why-expr)
  (expr :type why-expr))

(defmethod lift-let* ((expr why-array-assignment))
  (when *why-types-trace*
    (format t "lift-let*-why-array-assignment: ~a ~%" expr))
  (let* ((lift (lift-let* (expr expr)))
	 (nxpr (car lift))
	 (seq (cdr lift)))
    (progn
      (setf (slot-value expr 'expr) nxpr)
      (cons expr seq))))

; Should we do lift-let in upper and lower bound as well? 
(defmethod lift-let* ((expr why-array-literal))
  (when *why-types-trace*
    (format t "lift-let*-why-array-literal: ~a ~%" expr))
  (let* ((lift (lift-let* (init-function expr)))
	 (nxpr (car lift))
	 (seq (cdr lift)))
    (progn
      (setf (slot-value expr 'init-function) nxpr)
      (cons expr seq))))

;  (lift-let* (init-function expr)))
    

;(defun mk-why-array-literal (lower-bound upper-bound init-function type)
;  (when *why-decls-trace*
;    (format t "mk-why-array-literal ~a ~a ~a ~a~%" 
;	    upper-bound lower-bound init-function type))
;  (make-instance 'why-array-literal
;		 :lower-bound lower-bound
;		 :upper-bound upper-bound
;		 :init-function init-function
;		 :element-type (type type)
;		 :type type))

(defcl why-array-copy (why-expr)
  id_copy
  id_original)

(defun mk-why-array-copy (id_original id_copy type)
(when *why-decls-trace*
    (format t "mk-why-array-copy ~a ~a~%" id_copy id_original))
  (make-instance 
   'why-array-copy
   :id_copy id_copy
   :type type
   :id_original id_original))

;;

(defcl why-function-application (why-expr)
  (operator :type why-expr)
  (arguments :type list)
  (module :type symbol)	
  (builtin :type boolean)
  (constructor :type boolean))

(defmethod lift-let* ((expr why-function-application))
  (when *why-types-trace*
    (format t "lift-let*-why-function-application: ~a ~%" expr))
  (let* ((lift1 (lift-let* (operator expr)))
	 (nxpr1 (car lift1))
	 (seq1 (cdr lift1))
	 (lifts2 (lift-let* (arguments expr)))
	 (nxprs2 (car lifts2))
	 (seq2 (cdr lifts2)))
    (progn
      (setf (slot-value expr 'operator) nxpr1)
      (setf (slot-value expr 'arguments) nxprs2)
      (cons expr (append seq1 seq2)))))


; Records are not within the why language. They must be
; represented by abstract datatypes with accessor and update
; functions. [and presumably construction functions as well]

(defcl why-record-assignment (why-expr)
  (identifier :type symbol)
  (assignments :type list)) 

(defmethod lift-let* ((expr why-record-assignment))
  (when *why-types-trace*
    (format t "lift-let*-why-record-assignment: ~a ~%" expr))
  (let* ((lifts (lift-let* (assignments expr)))
	 (nxprs (car lifts))
	 (seq (cdr lifts)))
    (progn 
      (setf (slot-value expr 'assignments) nxprs)
      (cons expr seq))))

(defcl why-record-selection (why-expr)
  (identifier :type symbol)
  (field :type symbol))

; was removed? field too!
(defmethod lift-let* ((expr why-record-selection))
  (when *why-types-trace*
    (format t "lift-let*-why-record-selection: ~a ~%" expr))
  (let* ((lift (lift-let* (identifier expr)))
	 (nxpr (car lift))
	 (seq (cdr lift)))
    (progn
      (setf (slot-value expr 'identifier) nxpr)
      (cons expr seq))))

(defcl why-record-literal (why-expr)
  (assignments :type list))

(defmethod lift-let* ((expr why-record-literal))
  (when *why-types-trace*
    (format t "lift-let*-why-record-literal: ~a ~%" expr))
   (let* ((lifts (lift-let* (assignments expr)))
	 (nxprs (car lifts))
	 (seq (cdr lifts)))
     (setf (slot-value expr 'assignments) nxprs)
     (cons expr seq)))

(defmethod lift-let* ((exprs list))
  (when *why-types-trace*
    (format t "lift-let*-list: ~a ~%" exprs))
  (let* ((lifts (loop for expr in exprs collect
		      (lift-let* expr)))
	 (nxprs (mapcar #'car lifts))
	 (seq (reduce #'append (mapcar #'cdr lifts))))
    (cons nxprs seq)))
	 
(defcl why-record-copy (why-expr)
  (target :type symbol)
  (source :type symbol))

(defun mk-why-record-copy (target source type)
  (when *why-decls-trace*
    (format t "mk-why-record-copy ~a ~a ~a~%" target source type))
  (make-instance 
   'why-record-copy
   :target target
   :source source
   :type type))

;;
;; Definitions
;; 

(defcl why-module-import (why-expr)
  (identifier :type symbol)
  (parameters :type list) ; list of why-binding
  (type-parameters :type list) ; list of type parameters
)

(defun mk-why-module-import (identifier parameters &optional type-parameters)
 (when *why-decls-trace*
    (format t "mk-why-module-imports ~a ~a ~a ~%" identifier parameters type-parameters))
 (make-instance
  'why-module-import
  :identifier identifier
  :parameters parameters
  :type-parameters type-parameters))

(defcl why-module (why-def)
  (parameters :type list)
  (type-parameters :type list) ; to implement generics
  (imports :type list) ; list of why-module-import
  (definitions :type list)) ; List of why-binding

(defun mk-why-module (identifier parameters definitions &optional imports type-parameters)
  (when *why-decls-trace*
    (format t "mk-why-module ~a ~a ~a ~a ~a ~%" identifier parameters definitions imports type-parameters))
  (make-instance 
   'why-module
   :identifier identifier
   :imports imports
   :parameters parameters ; List of why-binding
   :type-parameters type-parameters
   :definitions definitions))

(defcl why-function (why-def)
  (parameters :type list) ; List of why-binding
  (return-type :type why-type)
  (type :type why-type)
  (body :type why-expr)
  (precondition :type why-expr)
  (postcondition :type why-expr))

(defun mk-why-function (identifier parameters expr type precondition postcondition)
  (when *why-decls-trace*
    (format t "mk-why-function ~a ~a ~a ~a~%" identifier parameters expr type))
  (make-instance 
   'why-function
   :identifier identifier
   :parameters (list-bindings parameters (domain type))
   :return-type (range type)
   :type type
   :precondition precondition
   :postcondition postcondition
   :body expr))

(defcl why-record (why-def)
  (fields :type list)) ; List of why-binding

(defun mk-why-record (identifier fields)
  (when *why-decls-trace*
    (format t "mk-why-record ~a ~a~%" identifier fields))
  (make-instance 
   'why-record
   :identifier identifier
   :fields fields))

;;
;; Logic declarations
;; 

(defcl why-logic-type (why-logic-declaration)
  (identifier :type symbol)
  type-parameters
  (external :type boolean)
)

(defcl why-logic-statement (why-logic-declaration)
  (identifiers :type list)
  (external :type boolean)
)

(defcl why-logic-function (why-logic-declaration)
  (identifier :type symbol)
  logic-binders
  primitive-type
  term
)

(defcl why-logic-predicate (why-logic-declaration)
  (identifier :type symbol)
  binders
  predicate
  type
)

(defun mk-why-logic-predicate (identifier binders expr type)
  (when *why-decls-trace*
    (format t "mk-why-logic-predicate ~a ~{ ~a ~} ~a ~a ~%" identifier binders expr type))
  (make-instance 
   'why-logic-predicate
   :identifier identifier
   :binders (list-bindings binders (domain type))
   :predicate expr
   :type type))
  
(defcl why-logic-axiom (why-logic-declaration)
  (identifier :type symbol)
  predicate
)

(defcl why-logic-goal (why-logic-declaration)
  (identifier :type symbol)
  predicate
)

;;
;; Declarations
;;

(defcl why-declaration-parameter (why-declaration)
  (external :type boolean)
  (identifiers :type list)
  value-type	; is simple type in our description
)

; why declaration constant is not in the why specification
; use why-declaration-function with a "constant" flag?
(defcl why-declaration-constant (why-declaration)
  (identifier :type symbol)
  (expression :type why-expr)	; ofcourse the expression has no free variables and
)

(defcl why-declaration-function (why-declaration)
  (identifier :type symbol)
  bindings
  (expression :type why-expr)
)

(defcl why-declaration-recursive-function (why-declaration-function)
  variant	; measure
)

(defcl why-declaration-exception (why-declaration)
  (identifier :type symbol)
  primitive-type	; optional
)

(defcl why-declaration-logic (why-declaration)
  (why-logic-declaration :type why-logic-declaration)
)

;;
;; Array and record type declarations are not in the
;; why language. They should be translated to a suitable
;; abstract datatype by means of a why-logic-declaration
;;
(defcl why-declaration-record (why-declaration)
  fields	; list of pairs of identifiers and types
)

(defcl why-declaration-array (why-declaration)
  array-type
)

(defcl why-declaration-adt (why-declaration)
  identifier
)

(defun filterID (identifier)
  (let* ((id-string (symbol-name identifier))
	 (replaced-string (replace-all id-string "?" "Questionmark")))
    (intern replaced-string)))

(defun mk-why-declaration-adt (identifier)
  (make-instance 'why-declaration-adt
    :identifier identifier)
)

;;
;; Make functions
;;

(defun mk-why-while (while do variant &optional invariant)
  (make-instance 'why-while
    :branch-condition while
    :variant variant
    :invariant invariant
    :loop-body do
  )
)

(defmethod rename* (oldvar newvar (expr why-while))
  (when *why-decls-trace*
    (format t "rename* ~a ~a ~a ~%" oldvar newvar expr))
  (rename* oldvar newvar (branch-condition expr))
  (rename* oldvar newvar (loop-body expr)))

(defun mk-why-let (id expr1 expr2 &optional type)
  (when *why-decls-trace*
    (format t "mk-why-let ~a ~a ~a ~a ~%" id expr1 expr2 type))
  (make-instance 
   'why-let
   :identifier id
   :expr expr1
   :in_expr expr2
   :type type))

(defmethod rename* (oldvar newvar (expr why-let))
  (when *why-decls-trace*
    (format t "rename* ~a ~a ~a ~%" oldvar newvar expr))
  (rename* oldvar newvar (expr expr))
  (rename* oldvar newvar (in_expr expr)))


(defun mk-why-sequential-composition (exprs)
  (when *why-decls-trace*
    (format t "mk-why-sequential-composition: ~{ ~a ~} ~%" exprs))
  (make-instance 'why-sequential-composition
		 :exprs exprs))

(defmethod rename* (oldvar newvar (expr why-sequential-composition))
  (when *why-decls-trace*
    (format t "rename* ~a ~a ~a ~%" oldvar newvar expr))
  (mapcar #'(lambda (expr) (rename* oldvar newvar expr)) 
	  (exprs expr)))

(defun mk-why-assignment (id expr)
  (when *why-decls-trace*
    (format t "mk-why-assignment ~a ~a ~%" id expr))
  (make-instance 'why-assignment
    :identifier id
    :expr expr))

(defmethod rename* (oldvar newvar (expr why-assignment))
  (when *why-decls-trace*
    (format t "rename* ~a ~a ~a ~%" oldvar newvar expr))
  (rename* oldvar newvar (expr expr)))

(defun mk-why-function-application (operator arguments
					     &optional builtin module type constructor)
  (when *why-decls-trace*
    (format t "mk-why-function-application ~a ~a # ~a ~a ~a  ~%" operator arguments builtin module type))
  (make-instance 
   'why-function-application
   :module module
   :operator operator
   :arguments arguments
   :type type
   :builtin builtin
   :constructor constructor))

(defmethod rename* (oldvar newvar (expr why-function-application))
  (when *why-decls-trace*
    (format t "rename* ~a ~a ~a ~%" oldvar newvar expr))
  (let ((new-arguments (mapcar #'(lambda (expr)
			       (rename* oldvar newvar expr)) 
			       (arguments expr))))
       (setf (slot-value expr 'arguments) new-arguments))
   expr)

(defun mk-why-record-assignment (id assignments type)
  (when *why-decls-trace*
    (format t "mk-why-record-assignment ~a ~{ ~a ~} ~a~%" id 
	    assignments type))
  (make-instance 
   'why-record-assignment
   :identifier id
   :assignments assignments
   :type type))

(defmethod rename* (oldvar newvar (expr why-record-assignment))
  (when *why-decls-trace*
    (format t "rename* ~a ~a ~a ~%" oldvar newvar expr))
  (rename* oldvar newvar (assignments expr)))

(defmethod rename* (oldvar newvar (expr list))
  (when *why-decls-trace*
    (format t "rename* ~a ~a ~a ~%" oldvar newvar expr))
  (when (consp expr)
    (progn (rename* oldvar newvar (car expr))
	   (rename* oldvar newvar (cdr expr)))))

(defun mk-why-name (identifier kind &optional type module)
  (when *why-decls-trace*
    (format t "mk-why-name ~a ~a ~a~%" identifier kind type))
  (make-instance 
   'why-name
   :identifier identifier
   :module module
   :type type
   :kind kind))

(defmethod rename* (oldvar newvar (expr why-name))
  (when *why-decls-trace*
    (format t "rename* ~a ~a ~a ~%" oldvar newvar expr))
  (when (;and (eq (slot-value expr 'kind) 'variable)
	     eq (slot-value expr 'identifier) oldvar)
    (setf (slot-value expr 'identifier) newvar))
  expr)

(defmethod rename* (oldvar newvar expr)
  (declare (ignore oldvar newvar))
  expr)

(defun mk-why-constant (identifier type &optional module) ; identifier is a user defined name
  (mk-why-name identifier 'constant type module))

(defun mk-why-variable (identifier type) ; identifier is a let-in variable
  (mk-why-name identifier 'variable type))

(defun mk-why-formal (identifier type) ; identifier is a formal parameter of a func.
  (mk-why-name identifier 'formal type))

(defun mk-why-parameter (identifier)  ; identifier is a parameter of a module
  (mk-why-name identifier 'parameter))

(defun mk-why-literal (value kind)
  (when *why-decls-trace*
    (format t "mk-why-literal ~a ~a~%" value kind))
  (make-instance 
   'why-literal
   :value value
   :kind  kind))

(defun mk-why-record-selection (id field &optional type)
  (when *why-decls-trace*
    (format t "mk-why-record-selection ~a ~a~%" id field))
  (make-instance 'why-record-selection
    :identifier id
    :type type
    :field field))

(defmethod rename* (oldvar newvar (expr why-record-selection))
(when *why-decls-trace*
    (format t "rename* ~a ~a ~a ~%" oldvar newvar expr))
    (when (eq oldvar (identifier expr))
	  (setf (slot-value expr 'identifier) newvar))
)

(defmethod rename* (oldvar newvar (expr why-literal))
  (when *why-decls-trace*
    (format t "rename* ~a ~a ~a ~%" oldvar newvar expr))
  expr)
;  (when (eq oldvar (identifier expr))
;	  (setf (slot-value expr 'value) newvar))
;  expr)

(defun mk-why-record-literal (assignments type)
  (when *why-decls-trace*
    (format t "mk-why-record-literal ~{ ~a ~} ~a~%" assignments type))
  (make-instance 'why-record-literal
		 :assignments assignments
		 :type type))

(defun mk-why-lambda-abstraction (bindings expr &optional type)
  (when *why-decls-trace*
    (format t "mk-why-lambda-abstraction ~a ~a ~a ~%" bindings expr type))
  (make-instance 'why-lambda-abstraction
    :bindings bindings 
    :expr expr
    :type type))

;(defmethod lift-let* ((expr why-lambda-abstraction))
;  (when *why-decls-trace*
;    (format t "lift-let*-why-lambda-abstraction ~a ~%" expr))
;  (let* ((lift (lift-let* (expr expr)))
;	 (nxpr (car lift))
;	 (seq (cdr lift)))
;    (progn
;      (setf (slot-value expr 'expr) nxpr)
;      (cons expr seq))))


; function to recognize lambda x.x (created by conversions in PVS)
(defun isID (lambda-abstraction)
  (when *why-decls-trace*
    (format t "isID ~a ~%" lambda-abstraction))
  (let  ((var (identifier (car (bindings lambda-abstraction))))
	 (xpr (expr lambda-abstraction)))
    (and (why-name? xpr) (eq (identifier xpr) var))))

(defun mk-why-quantifier (quantifier lb ub expr)
  (when *why-decls-trace*
    (format t "mk-why-quantifier ~a ~a ~a ~a~%" quantifier lb ub expr))
  (make-instance 
   'why-quantifier 
   :quantifier quantifier
   :lower-bound lb
   :upper-bound ub
   :predicate expr))

(defmethod lift-let* ((expr why-quantifier))
  (when *why-decls-trace*
    (format t "lift-let*-why-quantifier ~a ~%" expr))
  (let* ((lift (lift-let* (predicate expr)))
	 (nxpr (car lift))
	 (seq (cdr lift)))
    (progn
      (setf (slot-value expr 'predicate) nxpr)
      (cons expr seq))))

(defun mk-why-forall (lower-bound upper-bound expr)
  (mk-why-quantifier 'forall lower-bound upper-bound expr))

(defun mk-why-exists (lower-bound upper-bound expr)
  (mk-why-quantifier 'exists lower-bound upper-bound expr))


(defmethod rename* (oldvar newvar (expr why-lambda-abstraction))
(when *why-decls-trace*
    (format t "rename* ~a ~a ~a ~%" oldvar newvar expr))
    (rename* oldvar newvar (expr expr))
)

(defun mk-why-array-subscription (id index &optional type)
  (when *why-decls-trace*
    (format t "mk-why-array-subscription ~a ~a ~%" id index))
  (make-instance 'why-array-subscription
    :identifier id
    :type type
    :index index
  )
)

(defun mk-why-array-assignment (array index expr &optional type)
(when *why-decls-trace*
 (format t "mk-why-array-assignment ~a ~a ~a ~a~%" array index expr type))
  (make-instance 'why-array-assignment
     :identifier array
     :index index
     :type type
     :expr expr
  )
)

(defmethod rename* (oldvar newvar (expr why-array-assignment))
(when *why-decls-trace*
    (format t "rename* ~a ~a ~a ~%" oldvar newvar expr))
    (rename* oldvar newvar (expr expr))
)

;(defcl why-array-assignment (why-expr)
;  (identifier :type symbol)	; or use type why-array-subscription?
;  (index :type why-expr)
;  (expr :type why-expr)
;)

(defun mk-why-conditional (if then else &optional type)
  (when *why-decls-trace*
    (format t "mk-why-conditional ~a ~a ~a ~%" if then else))
  (make-instance 'why-conditional
    :branch-condition if
    :true-branch then
    :false-branch else
    :type type
  )
)

(defmethod lift-let* ((expr why-conditional))
  (when *why-types-trace*
    (format t "lift-let*-why-conditional: ~a ~%" expr))
  (let* ((lift1 (lift-let* (branch-condition expr)))
	 (nxpr1 (car lift1))
	 (seq1 (cdr lift1))
	 (lift2 (lift-let* (true-branch expr)))
	 (nxpr2 (car lift2))
	 (seq2 (cdr lift2))
	 (lift3 (lift-let* (false-branch expr)))
	 (nxpr3 (car lift3))
	 (seq3 (cdr lift3)))
    (progn
      (setf (slot-value expr 'branch-condition) nxpr1)
      (setf (slot-value expr 'true-branch) nxpr2)
      (setf (slot-value expr 'false-branch) nxpr3)
      (cons expr (append (append seq1 seq2) seq3)))))


(defmethod rename* (oldvar newvar (expr why-conditional))
(when *why-decls-trace*
    (format t "rename* ~a ~a ~a ~%" oldvar newvar expr))
    (rename* oldvar newvar (branch-condition expr))
    (rename* oldvar newvar (true-branch expr))
    (rename* oldvar newvar (false-branch expr))
)

(defun mk-why-binding (identifier type)
  (when *why-decls-trace*
    (format t "mk-why-binding ~a ~a ~%" identifier type))
  (make-instance 
   'why-binding
   :identifier identifier
   :type type))

(defun list-bindings (bindings types)
  (when bindings
    (cons (mk-why-binding (car bindings) (car types))
	  (list-bindings (cdr bindings) (cdr types)))))

(defun mk-why-array-literal (lower-bound upper-bound init-function type)
  (when *why-decls-trace*
    (format t "mk-why-array-literal ~a ~a ~a ~a~%" 
	    upper-bound lower-bound init-function type))
  (make-instance 'why-array-literal
		 :lower-bound lower-bound
		 :upper-bound upper-bound
		 :init-function init-function
		 :element-type (type type)
		 :type type))

;;
;; Print the datastructure for debugging purposes
;;

(defmethod why2String* ((expr why-conditional))
  (format nil "IF ~a THEN ~a ELSE ~a ENDIF"
    (why2String* (branch-condition expr))
    (why2String* (true-branch expr))
    (why2String* (false-branch expr))))

(defmethod why2String* ((expr why-function))
  (format nil "FUNCTION ~a ~{ ~a ~} : ~a = ~a"
    (identifier expr)
    (bindings expr)
    (why2String* (type expr))
    (why2String* (body expr))))

(defmethod why2String* ((expr why-name))
  (format nil "~d" (identifier expr)))

(defmethod why2String* ((expr why-literal))
  (format nil "~d" (value expr)))

(defmethod why2String* ((expr why-record-literal))
  (format nil "(# ~:{ ~a := ~a, ~} #)"
	  (pairlis (cdr (assignments expr))
		   (mapcar #'why2String* (car (assignments expr))))))

(defmethod why2String* ((expr why-record-selection))
  (format nil "~a.~a"
	  (identifier expr)
	  (field expr)))

(defmethod why2String* ((expr why-function-application))
  (format nil "(~a ~{ ~a ~})"
	  (why2String* (operator expr))
	  (mapcar #'why2String* (arguments expr))))

(defmethod why2String* ((expr why-let))
  (format nil "~a = ~a IN ~a"
	 (identifier expr)
	 (expr expr)
	 (why2String* (in_expr expr))))

(defmethod why2String* ((expr why-sequential-composition))
  (format nil "{ ~{ ~a;~% ~} }" (exprs expr)))

(defmethod why2String* ((expr why-module))
  (format nil "MODULE ~a:~% ~{ ~a~% ~}"
	  (identifier expr)
	  (mapcar #'why2String* (definitions expr))))

(defmethod why2String* (expr)
  (format nil "<<~a>>" expr))

;;
;; Types
;;

;; Quick hack to make things work in Java. Have to drop this later.
(defcl why-cast (why-expr)
  (cast :type why-type)
  (expr :type why-expr))

(defun mk-why-cast (type expr)
  (when *why-types-trace*
    (format t "mk-why-cast: ~a ~a ~%" type expr))
  (make-instance 'why-cast
    :expr expr
    :cast type))

(defcl why-primitive-type (why-type)
  adt ; boolean that is true iff the type is an abstract data type
)

; A generic type can be just a "regular" type variable. Then it will not have
; parameters. Or it can be another type with type variables.
(defcl why-generic-type (why-type)
  (type :type why-type)
  (parameters :type why-generic-type))

(defcl why-array-type (why-type)
  (lower-bound :type number)
  (upper-bound :type number)
  (type :type why-type))

(defcl why-record-type (why-type)
;  module % module in general type definition
) ; Reference to a record definition

(defcl why-function-type (why-type)  
  (domain	:type list) 		
  (range	:type why-type))

(defcl why-constructor ()
  (recognizer :type symbol)
  (constructor :type symbol)
  (constructor-arguments :type list)) ; list of why-bindings

(defun replace-all (string part replacement &key (test #'char=))
"Returns a new string in which all the occurences of the part 
is replaced with replacement."
    (with-output-to-string (out)
      (loop with part-length = (length part)
            for old-pos = 0 then (+ pos part-length)
            for pos = (search part string
                              :start2 old-pos
                              :test test)
            do (write-string string out
                             :start old-pos
                             :end (or pos (length string)))
            when pos do (write-string replacement out)
            while pos))) 

(defun mk-why-constructor (recognizer constructor arguments)
  (when *why-types-trace*
    (format t "mk-why-constructor: ~a ~a ~a ~%" recognizer constructor arguments))
  (make-instance 'why-constructor
    :recognizer (filterID recognizer)
    :constructor (filterID constructor)
    :constructor-arguments arguments))

(defcl why-adt-def (why-def)
  (identifier :type symbol)
  (constructors :type list))

(defun mk-why-adt-def (identifier constructors)
  (when *why-types-trace*
    (format t "mk-why-adt-def: ~a ~a ~%" identifier constructors))
  (make-instance 'why-adt-def
    :identifier identifier
    :constructors constructors))

(defcl why-effect ()
  reads		; list of identifiers
  writes	; list of identifiers
  raises	; we have not yet defined exceptions
)

(defcl why-computation-type (why-type)
  precondition
  type 		; only a reference to a function or simple type is allowed here
  (effect :type why-effect)
  postcondition
)

;;
;; Make functions
;;

(defun mk-why-primitive-type (id &optional adt module)
  (when *why-types-trace*
    (format t "mk-why-primitive-type: ~a ~a ~a ~%" id adt module))
  (make-instance 'why-primitive-type
    :identifier id
    :adt adt
    :module module
  )
)

(defun mk-why-generic-type (id &optional type actuals module)
  (when *why-types-trace*
    (format t "mk-why-generic-type: ~a ~a ~a ~a ~%" id type actuals module))
  (make-instance 'why-generic-type
    :module module
    :type type
    :parameters actuals
    :identifier id))

(defun mk-why-array-type (lower-bound upper-bound type)
  (when *why-types-trace*
    (format t "mk-why-array-type: ~a ~a ~a~%" 
	    lower-bound upper-bound type))
  (make-instance 'why-array-type
		 :lower-bound lower-bound
		 :upper-bound upper-bound
		 :type type))

(defun mk-why-function-type (domain range &optional id)
  (let ((listdomain (if (listp domain) domain (list domain))))
    (when *why-types-trace*
      (format t "mk-why-function-type: domain ~a range ~a~%" listdomain range))
    (make-instance 'why-function-type
		   :identifier id
		   :domain listdomain
		   :range range)))

(defun mk-why-computation-type (type effect &optional pre post id)
  (when *why-types-trace*
    (format t "mk-why-computation-type: ~a ~a ~%" type effect))
  (make-instance 'why-computation-type
    :identifier id
    :precondition pre
    :why-type type	
    :effect effect
    :postcondition post
  )
)


(defun mk-why-effects (reads writes raises)
  (make-instance 'why-effects-type
    :reads reads
    :writes writes
    :raises raises
  )
)

(defun mk-why-record-type (identifier &optional module)
  (when *why-types-trace*
    (format t "mk-why-record-type: ~a ~a ~%" identifier module))
  (make-instance 'why-record-type
		 :identifier identifier
		 :module module ))

;;
;;
;;

(defmethod why2String* ((type why-function-type))
  (format nil "~{ ~a ~} -> ~a"
	  (mapcar #'why2String* (domain type))	
	  (why2String* (range type))))


;;
;; default
;;

(defmethod lift-let* (expr)
  (when *why-types-trace*
    (format t "lift-let*-default: ~a ~%" expr))
  (cons expr nil))

(defpackage :ideal-pvs
  (:use :common-lisp :esrap))

(in-package :ideal-pvs)

(defclass sym ()
  ((name :initarg :name)
   (index :initarg :index)))

(defclass dot ()
  ((module :initarg :module)
   (expr :initarg :expr)))

(defclass interpretation ()
  ((module :initarg :module)
   (interp :initarg :interp :initform (make-hash-table))
   (names :initarg :names :initform (make-hash-table))
   (constructors :initarg :constructors :initform (make-hash-table))
   (length :initarg :length)))

(defclass theory ()
  ((decls :initarg :decls)))

(defclass fun ()
  ((domain :initarg :domain)
   (range :initarg :range)))

(defclass prod ()
  ((left :initarg :left)
   (right :initarg :right)))

(defclass subtype ()
  ((supertype :initarg :supertype)
   (predicate :initarg :predicate)))

(defclass app ()
  ((fun :initarg :fun)
   (arg :initarg :arg)))

(defclass lam ()
  ((type :initarg :type)
   (body :initarg :body)))

(defclass pair ()
  ((left :initarg :left)
   (right :initarg :right)))

(defclass proj ()
  ((expr :initarg :expr)
   (side :initarg :side)))

(defclass decl ()
  ((var :initarg :var)
   (type :initarg :type)
   (def :initarg :def)))

;;;; Parsing
(defrule ws
    (+ (or #\Space #\Tab #\Newline #\Return))
  (:constant nil))

(defun nonkeyword (s)
  (let ((txt (string-upcase (text s))))
    (not (member txt
		 '("BEGIN" "END" "LAMBDA" "TYPE")
		 :test #'string-equal))))

(defrule symbol
    (nonkeyword (+ (or #\_ #\=
		       (alphanumericp character))))
  (:lambda (s)
    (intern (text s))))

(defrule name
    (or symbol
	(and #\# (+ (character-ranges (#\0 #\9)))))
  (:lambda (s)
    (if (listp s)
	(make-instance 'sym :index (parse-integer (text (cadr s)) :radix 10))
	(make-instance 'sym :name s))))

;; decl
(defrule decl
    (and (? (and symbol (? ws) #\: (? ws)))
	 (or (and "THEORY" (? (and (? ws) theory)))
	     (and "TYPE" (? (and (? ws) #\= (? ws) (or "EXTERNAL" type))))
	     (and type (? (and (? ws) #\= (? ws) (or "EXTERNAL" term))))))
  (:destructure (name decl)
		(apply #'make-instance 'decl
		       (append (when name (list :var (car name)))
			       (if (stringp (car decl))
				   (when (string-equal (car decl) "THEORY")
				     (list :type nil))
				   (list :type (car decl)))
			       (when (cadr decl)
				 (if (and (stringp (car decl))
					  (string-equal (car decl) "THEORY"))
				     (list :def (cadr (cadr decl)))
				     (destructuring-bind (w1 eq w2 def) (cadr decl)
				       (declare (ignore w1 eq w2))
				       (if (stringp def)
					   (list :def nil)
					   (list :def def)))))))))

(defun is-decl? (d)
  (not (slot-boundp d 'def)))

(defun is-var? (d)
  (and (slot-boundp d 'def)
       (not (slot-value d 'def))))

(defun is-def? (d)
  (and (slot-boundp d 'def)
       (slot-value d 'def)))

(defun type-decl? (d)
  (not (slot-boundp d 'type)))

(defun theory-decl? (d)
  (and (slot-boundp d 'type)
       (not (slot-value d 'type))))

(defun term-decl? (d)
  (and (slot-boundp d 'type)
       (slot-value d 'type)))

(defun is-term-decl? (d)
  (and (is-decl? d)
       (term-decl? d)))

;; dot
(defrule partial-dot
    (and #\. 
	 (or (and (? ws) name)
	     (and (and #\( (? ws)) expr (and (? ws) #\)))))
  (:destructure (d n)
		(declare (ignore d))
		#'(lambda (module)
		    (make-instance 'dot :expr (cadr n) :module module))))

;; interpretation
(defrule partial-interp
    (and "{{" (* (and (? ws) def (? #\,))) (and (? ws) "}}"))
  (:destructure (ob defs cb)
		(declare (ignore ob cb))
		(let ((itbl (make-hash-table))
		      (ntbl (make-hash-table)))
		  (loop for (ws (name . def) comma) in defs
		     when (slot-boundp name 'index) do
		       (setf (gethash (slot-value name 'index) itbl) def)
		     when (slot-boundp name 'name) do
		       (setf (gethash (slot-value name 'name) ntbl) def))
		  #'(lambda (module)
		      (make-instance 'interpretation
				     :interp itbl
				     :names ntbl
				     :module module)))))

(defrule def

    (and name (and (? ws) ":=" (? ws)) (or type term))
  (:destructure (name eq def)
		(declare (ignore eq))
		(cons name def)))

;; theory
(defrule theory-body
    (and "BEGIN" (* (and (? ws) decl (? #\;))) (? ws) "END")
  (:destructure (begin decls ws end)
		(declare (ignore begin ws end))
		(make-instance 'theory :decls (reverse (mapcar #'cadr decls)))))
;; (loop for (ws decl sc) in decls collect decl)
;; 		(let ((dtbl (make-hash-table)))
;; 		  (loop for (ws decl sc) in decls
;; 		     counting t into nb do
;; 		       (setf (gethash (1- nb) dtbl) decl))
;; 		  (make-instance 'theory :decls dtbl))))

;; fun/prod
(defrule funprod
    (and (and #\[ (? ws)) (is-term-decl? decl)
	 (and (? ws) (or "," "->") (? ws))
	 type (and (? ws) #\]))
  (:destructure (open left arr right close)
		(declare (ignore open close))
		(if (string-equal (cadr arr) ",")
		    (make-instance 'prod :left left :right right)
		    (make-instance 'fun :domain left :range right))))

;; subtype
(defrule subtype
    (and (and #\{ (? ws)) (is-term-decl? decl) (and (? ws) #\| (? ws)) term (and (? ws) #\}))
  (:destructure (open supertype st predicate close)
		(declare (ignore open st close))
		(make-instance 'subtype :supertype supertype :predicate predicate)))

;; app
(defrule partial-app
    tuple-term
  (:lambda (arg)
    #'(lambda (f)
	(make-instance 'app :fun f :arg arg))))

;; lam
(defrule lam
    (and (and "LAMBDA" (? ws) #\( (? ws)) (is-term-decl? decl)
	 (and (? ws) #\) (? ws) #\: (? ws))
	 term)
  (:destructure (open typ colon body)
		(declare (ignore open colon))
		(make-instance 'lam :type typ :body body)))

;; pair
(defrule tuple-term
    (and (and #\( (? ws)) term (? ws) (? (and (and #\, (? ws)) term (? ws))) #\))
  (:destructure (open left ws right close)
		(declare (ignore open ws close))
		(if right
		    (make-instance 'pair :left left :right (cadr right))
		    left)))

;; proj
(defrule partial-proj
    (and (and #\` (? ws)) (or #\1 #\2))
  (:destructure (field side)
		(declare (ignore field))
		(if (string-equal side "1")
		    #'(lambda (expr)
			(make-instance 'proj :expr expr :side 'left))
		    #'(lambda (expr)
			(make-instance 'proj :expr expr :side 'right)))))

;; Toplevels
(defrule longname
    (or (and (or lam tuple-term name theory-body)
	     (+ (and (? ws) (or partial-interp partial-app partial-dot partial-proj))))
	name)
  (:lambda (x)
    (if (listp x)
	(destructuring-bind (f args) x
	  (loop for (ws arg) in args do
	       (setf f (funcall arg f)))
	  f)
	x)))

(defrule theory
    (or theory-body longname))

(defrule term
    (or longname lam tuple-term))

(defrule type
    (or longname funprod subtype))

(defrule expr
    (or term type theory))

;;;; Display
(defun pp-context (l &key (display #'write) (begin "BEGIN") (end "END") (comma ";"))
  (pprint-logical-block (nil nil)
    (write-string begin)
    (write-char #\space)
    (pprint-logical-block (nil l)
      (pprint-exit-if-list-exhausted)
      (pprint-newline :linear)
      (funcall display (pprint-pop))
      (loop
	 (pprint-exit-if-list-exhausted)
	 (write-string comma)
	 (write-char #\space)
	 (pprint-newline :linear)
	 (funcall display (pprint-pop))))
    (write-char #\space)
    (pprint-newline :linear)
    (write-string end)))

(defvar *pp-ambiguous* nil)
(defvar *pp-noparens* nil)
(defvar *pp-debruijn* nil)

(defgeneric pp* (expr)
  (:method ((v sym))
    "Displays the symbol's name.
    Falls back to De Bruijn representation prefixed by '#' if it has no name."
    (when (and (slot-boundp v 'name)
	     (or (not *pp-debruijn*)
		 (not (slot-boundp v 'index))))
	(princ (slot-value v 'name)))
    (when (slot-boundp v 'index)
	  (write-char #\#)
	  (princ (slot-value v 'index))))
  (:method ((d dot))
    "Displays the lhs and rhs of the dot separated by '.'"
    (with-slots (module expr) d
      (pprint-logical-block (nil nil)
	(let ((*pp-ambiguous* t)
	      (*pp-noparens* nil))
	  (pprint-indent :current 2)
	  (pp* module)
	  (pprint-newline :fill)
	  (write-char #\.)
	  (unless (typep expr 'sym)
	    (write-char #\())
	  (pp* expr)
	  (unless (typep expr 'sym)
	    (write-char #\)))))))
  (:method ((i interpretation))
    "Displays the lhs, then the substitution between '{{' and '}}'."
    (with-slots (module interp names) i
      (pprint-logical-block (nil nil)
	(pprint-indent :current 2)
	(let ((*pp-ambiguous* t)
	      (*pp-noparens* nil))
	  (pp* module))
	(pprint-newline :linear)
	(let ((*pp-ambiguous* nil)
	      (*pp-noparens* nil))
	  (pp-context
	   (append
	    (loop for k being the hash-keys of interp
	       using (hash-value v) collect
		 (cons (make-instance 'sym :index k) v))
	    (loop for k being the hash-keys of names
	       using (hash-value v) collect
		 (cons (make-instance 'sym :name k) v)))
	   :display #'(lambda (x)
			(pprint-indent :current 2)
			(pp* (car x))
			(write-string " := ")
			(pprint-newline :linear)
			(pp* (cdr x)))
	   :begin " {{"
	   :end "}}"
	   :comma ",")))))
  (:method ((th theory))
    (with-slots (decls) th
      (let ((*pp-ambiguous* nil)
	    (*pp-noparens* nil))
	(pp-context
	 (reverse decls)
	 ;; (mapcar #'cdr 
	 ;; (sort (loop for k being the hash-keys of decls using (hash-value v) collect
	 ;; 	    (cons k v))
	 ;;       #'(lambda (x y) (< (car x) (car y))))))
	 :display #'pp*))))
  (:method ((f fun))
    (with-slots (domain range) f
      (pprint-logical-block (nil nil :prefix "[" :suffix "]")
	(let ((*pp-ambiguous* nil)
	      (*pp-noparens* nil))
	  (pprint-indent :block 2)
	  (pp* domain)
	  (write-string " -> ")
	  (pprint-newline :linear)
	  (pp* range)))))
  (:method ((p prod))
    (with-slots (left right) p
      (pprint-logical-block (nil nil :prefix "[" :suffix "]")
	(let ((*pp-ambiguous* nil)
	      (*pp-noparens* nil))

	  (pprint-indent :block 2)
	  (pp* left)
	  (write-string " , ")
	  (pprint-newline :linear)
	  (pp* right)))))
  (:method ((s subtype))
    (with-slots (supertype predicate) s
      (pprint-logical-block (nil nil :prefix "{ " :suffix " }")
	(let ((*pp-ambiguous* nil)
	      (*pp-noparens* nil))
	  (pp* supertype)
	  (pprint-indent :current 0)
	  (write-char #\space)
	  (pprint-newline :fill)
	  (write-char #\|)
	  (write-char #\space)
	  (pp* predicate)))))
  (:method ((a app))
    (with-slots (fun arg) a
      (pprint-logical-block (nil nil)
	(pprint-indent :current 2)
	(let ((*pp-ambiguous* t)
	      (*pp-noparens* nil))
	  (pp* fun))
	(write-char #\()
	(pprint-newline :linear)
	(let ((*pp-ambiguous* nil)
	      (*pp-noparens* t))
	  (pp* arg))
	(pprint-newline :linear)
	(write-char #\)))))
  (:method ((expr lam))
    (with-slots (type body) expr
      (pprint-logical-block (nil nil
				 :prefix (if *pp-ambiguous* "(" "")
				 :suffix (if *pp-ambiguous* ")" ""))
	(pprint-indent :current 2)
	(write-string "LAMBDA(")
	(pp* type)
	(write-string "): ")
	(pprint-newline :fill)
	(pp* body))))
  (:method ((expr pair))
    (with-slots (left right) expr
      (pprint-logical-block (nil nil
				 :prefix (if (and *pp-noparens* (not *pp-ambiguous*)) "" "(")
				 :suffix (if (and *pp-noparens* (not *pp-ambiguous*)) "" ")"))
	(let ((*pp-noparens* nil))
	  (pprint-indent :current 2)
	  (let ((*pp-ambiguous* t))
	    (pp* left))
	  (write-string ", ")
	  (pprint-newline :fill)
	  (let ((*pp-ambiguous* nil))
	    (pp* right))))))
  (:method ((p proj))
    (with-slots (expr side) p
      (pprint-logical-block (nil nil)
	(let ((*pp-ambiguous* t)
	      (*pp-noparens* nil))
	  (pp* expr))
	(write-char #\`)
	(if (eq side 'left)
	    (write-char #\1)
	    (write-char #\2)))))
  (:method ((d decl))
    (pprint-logical-block (nil nil)
      (let ((*pp-noparens* nil))
	(pprint-indent :current 1)
	(when (and (slot-boundp d 'var)
		   (not *pp-debruijn*))
	  (princ (slot-value d 'var))
	  (write-string ": ")
	  (pprint-newline :fill)
	  (pprint-indent :block 3))
	(cond
	  ((theory-decl? d) (write-string "THEORY"))
	  ((type-decl? d) (write-string "TYPE"))
	  ((term-decl? d)
	   (let ((*pp-ambiguous* t))
	     (pp* (slot-value d 'type)))))
	(when (and (not (is-decl? d))
		   (not (theory-decl? d)))
	  (write-string " ="))
	(pprint-newline :linear)
	(cond
	  ((is-var? d) (write-string " EXTERNAL"))
	  ((is-def? d)
	   (let ((*pp-ambiguous* nil))
	     (write-char #\space)
	     (pp* (slot-value d 'def))))))))
  )

(defun pp (expr &key
		  (stream *standard-output*)
		  (ambiguous? nil)
		  (noparens? nil))
  (let ((*pp-ambiguous* ambiguous?)
	(*pp-noparens* noparens?)
	(*standard-output* stream))
    (pp* expr)))

(defun pprint-ideal (&optional (flag t))
  (set-pprint-dispatch
   '(or sym dot interpretation theory fun prod subtype app lam pair proj decl)
   (when flag #'(lambda (s x)
		  (when *print-readably*
		    (error 'print-not-readable :object x))
		  (pp x
		      :stream s
		      :ambiguous? *pp-ambiguous*
		      :noparens? *pp-noparens*)))))

(defun find-rest (ctx name)
  (when (consp ctx)
    (if (and (slot-boundp (car ctx) 'var)
	     (eq (slot-value (car ctx) 'var) name))
	(cdr ctx)
	(find-rest (cdr ctx) name))))

(defun find-index (ctx name)
  (when (consp ctx)
    (if (and (slot-boundp (car ctx) 'var)
	     (eq (slot-value (car ctx) 'var) name))
	0
	(let ((ind (find-index (cdr ctx) name)))
	  (when ind
	    (1+ ind))))))

(defgeneric undebruijn* (ctx expr)
  (:method (ctx (v sym))
    (if (and (slot-boundp v 'index)
	     (< (slot-value v 'index) (length ctx)))
	(let ((decl (undebruijn* (nthcdr (1+ (slot-value v 'index)) ctx)
				 (elt ctx (slot-value v 'index)))))
	  (when (typep decl 'decl)
	    (when (slot-boundp decl 'var)
	      (setf (slot-value v 'name) (slot-value decl 'var)))
	    decl))))
  (:method (ctx (d dot))
    (let ((decl (undebruijn* ctx (slot-value d 'module))))
      (when (and (typep decl 'decl)
		 (slot-boundp decl 'def)
		 (typep (slot-value decl 'def) 'theory))
	(undebruijn* (append (slot-value (slot-value decl 'def) 'decls) ctx)
		     (slot-value d 'expr)))))
  (:method (ctx (i interpretation))
    (let ((decl (undebruijn* ctx (slot-value i 'module))))
      (when (and (typep decl 'decl)
		 (slot-boundp decl 'def)
		 (typep (slot-value decl 'def) 'theory))
	(setf (slot-value i 'length) (length (slot-value (slot-value decl 'def) 'decls)))
	(setf (slot-value i 'constructors) (make-hash-table))
	(loop for d in (slot-value (slot-value decl 'def) 'decls) counting t into nb do
	     (when (type-decl? d)
	       (setf (gethash (1- nb) (slot-value i 'constructors)) "itype"))
	     (when (term-decl? d)
	       (setf (gethash (1- nb) (slot-value i 'constructors)) "iterm")))
	(let ((mctx (append (slot-value (slot-value decl 'def) 'decls) ctx)))
	  (maphash #'(lambda (k v)
		       (undebruijn* (find-rest mctx k) v))
		   (slot-value i 'names))
	  (maphash #'(lambda (k v)
		       (undebruijn* (nthcdr (1+ k) mctx) v)
		       (let ((decl (elt mctx k)))
			 (when (slot-boundp decl 'var)
			   (setf (gethash (slot-value decl 'var) (slot-value i 'names)) v)
			   (remhash k (slot-value i 'interp)))))
		   (slot-value i 'interp)))
	decl)))
  (:method (ctx (th theory))
    (loop for decl in (reverse (slot-value th 'decls)) do
	 (push (undebruijn* ctx decl) ctx))
    (make-instance 'decl :type nil :def th))
  (:method (ctx (fn fun))
    (undebruijn* ctx (slot-value fn 'domain))
    (undebruijn* (cons (slot-value fn 'domain) ctx) (slot-value fn 'range))
    nil)
  (:method (ctx (p prod))
    (undebruijn* ctx (slot-value p 'left))
    (undebruijn* (cons (slot-value p 'left) ctx) (slot-value p 'right))
    nil)
  (:method (ctx (s subtype))
    (undebruijn* ctx (slot-value s 'supertype))
    (undebruijn* (cons (slot-value s 'supertype) ctx) (slot-value s 'predicate))
    nil)
  (:method (ctx (a app))
    (undebruijn* ctx (slot-value a 'fun))
    (undebruijn* ctx (slot-value a 'arg))
    nil)
  (:method (ctx (l lam))
    (undebruijn* ctx (slot-value l 'type))
    (undebruijn* (cons (slot-value l 'type) ctx) (slot-value l 'body))
    nil)
  (:method (ctx (p pair))
    (undebruijn* ctx (slot-value p 'left))
    (undebruijn* ctx (slot-value p 'right))
    nil)
  (:method (ctx (p proj))
    (undebruijn* ctx (slot-value p 'expr))
    nil)
  (:method (ctx (d decl))
    (when (and (slot-boundp d 'type) (slot-value d 'type))
      (undebruijn* ctx (slot-value d 'type)))
    (when (and (slot-boundp d 'def) (slot-value d 'def))
      (undebruijn* ctx (slot-value d 'def)))
    d)
  )
(defun undebruijn (expr &optional (ctx nil))
  (undebruijn* ctx expr)
  expr)

(defgeneric debruijn* (ctx expr)
  (:method (ctx (v sym))
    (let ((index (and (slot-boundp v 'name)
		      (find-index ctx (slot-value v 'name)))))
      (when index
	(setf (slot-value v 'index) index)
	(elt ctx index))))
  (:method (ctx (d dot))
    (let ((decl (debruijn* ctx (slot-value d 'module))))
      (when (and (typep decl 'decl)
		 (slot-boundp decl 'def)
		 (typep (slot-value decl 'def) 'theory))
	(debruijn* (append (slot-value (slot-value decl 'def) 'decls) ctx)
		   (slot-value d 'expr)))))
  (:method (ctx (i interpretation))
    (let ((decl (debruijn* ctx (slot-value i 'module))))
      (when (and (typep decl 'decl)
		 (slot-boundp decl 'def)
		 (typep (slot-value decl 'def) 'theory))
	(setf (slot-value i 'length) (length (slot-value (slot-value decl 'def) 'decls)))
	(setf (slot-value i 'constructors) (make-hash-table))
	(loop for d in (slot-value (slot-value decl 'def) 'decls) counting t into nb do
	     (when (type-decl? d)
	       (setf (gethash (1- nb) (slot-value i 'constructors)) "itype"))
	     (when (term-decl? d)
	       (setf (gethash (1- nb) (slot-value i 'constructors)) "iterm")))
	(let ((mctx (append (slot-value (slot-value decl 'def) 'decls) ctx)))
	  (maphash #'(lambda (k v)
		       (debruijn* (nthcdr (1+ k) mctx) v))
		   (slot-value i 'interp))
	  (maphash #'(lambda (k v)
		       (debruijn* (find-rest mctx k) v)
		       (when (find-index mctx k)
			 (setf (gethash (find-index mctx k) (slot-value i 'interp)) v)
			 (remhash k (slot-value i 'names))))
		   (slot-value i 'names)))
	decl)))
  (:method (ctx (th theory))
    (loop for decl in (reverse (slot-value th 'decls)) do
	 (push (debruijn* ctx decl) ctx))
    (make-instance 'decl :type nil :def th))
  (:method (ctx (fn fun))
    (debruijn* ctx (slot-value fn 'domain))
    (debruijn* (cons (slot-value fn 'domain) ctx) (slot-value fn 'range))
    nil)
  (:method (ctx (p prod))
    (debruijn* ctx (slot-value p 'left))
    (debruijn* (cons (slot-value p 'left) ctx) (slot-value p 'right))
    nil)
  (:method (ctx (s subtype))
    (debruijn* ctx (slot-value s 'supertype))
    (debruijn* (cons (slot-value s 'supertype) ctx) (slot-value s 'predicate))
    nil)
  (:method (ctx (a app))
    (debruijn* ctx (slot-value a 'fun))
    (debruijn* ctx (slot-value a 'arg))
    nil)
  (:method (ctx (l lam))
    (debruijn* ctx (slot-value l 'type))
    (debruijn* (cons (slot-value l 'type) ctx) (slot-value l 'body))
    nil)
  (:method (ctx (p pair))
    (debruijn* ctx (slot-value p 'left))
    (debruijn* ctx (slot-value p 'right))
    nil)
  (:method (ctx (p proj))
    (debruijn* ctx (slot-value p 'expr))
    nil)
  (:method (ctx (d decl))
    (when (and (slot-boundp d 'type) (slot-value d 'type))
      (debruijn* ctx (slot-value d 'type)))
    (when (and (slot-boundp d 'def) (slot-value d 'def))
      (debruijn* ctx (slot-value d 'def)))
    d)
  )
(defun debruijn (expr &optional (ctx nil))
  (debruijn* ctx expr)
  expr)

;; Initial theory
(defparameter init-theory
  (debruijn (parse 'theory "BEGIN
  bool_def: THEORY
  BEGIN
    bool: TYPE = EXTERNAL
    TRUE: bool = EXTERNAL
    FALSE: bool = EXTERNAL
    boolop: TYPE = [[bool, bool] -> bool]
    not: [bool -> bool] = EXTERNAL
    and: boolop = EXTERNAL
    or: boolop = EXTERNAL
  END

  equalities: THEORY
  BEGIN
    T: TYPE
    =: [[T, T] -> bool_def.bool] = EXTERNAL
  END
END")))

(defparameter bool-theory
  (with-slots (decls) init-theory
    (slot-value (elt decls (find-index decls '|bool_def|)) 'def)))

(defparameter equalities-theory
  (with-slots (decls) init-theory
    (slot-value (elt decls (find-index decls '|equalities|)) 'def)))

;; Release: PVS2Why-0.1 (11/10/07)
;; Prints XML representation of Why Code
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

(defun pvs2xml (pvsfile theory version)
 (with-open-file 
   (file (format nil "~a.xml" theory) 
	 :direction :output :if-exists :supersede)
   (format file 
	   "<!-- File: ~a.xml 
           Automatically generated from PVS theory ~a (~a.pvs)
           By: ~a
           Date: ~a -->~2%"
	   theory theory pvsfile version (now-today))
   (why2xml-list file (pvs2why-theory theory))))

(defun block-list-xml (file tag l &optional attr)
  (if l
      (block-xml 
       file tag attr
       (dolist (e l)
	 (why2xml* file e)))
    (line-xml file tag attr)))

;;
;; Definitions
;;

;; LL Allow for the translation of a list of why-modules
(defmethod why2xml-list ((file stream) modules)
  (dolist (module modules)
    (why2xml* file module)))

(defmethod why2xml* ((file stream) (def why-module))
  (block-xml 
   file "module" (list "identifier" (identifier def))
   (block-list-xml
    file "parameters" (parameters def))
   (block-list-xml
    file "definitions" (definitions def))))

(defmethod why2xml* ((file stream) (def why-function))
  (let ((abstract (when (null (body def)) (list "abstract" "true"))))
    (block-xml 
     file "function" 
     (append (list "identifier" (identifier def))
	     abstract)
     (block-xml 
      file "returnType" nil 
      (why2xml* file (return-type def)))
     (block-xml
      file "precondition" nil
      (why2xml* file (precondition def)))
     (block-list-xml 
      file "parameters" (parameters def))
     (when (body def)
       (block-xml 
	file "body" nil
	(why2xml* file (body def)))))))
  
(defmethod why2xml* ((file stream) (def why-record))
  (block-xml
   file "record" (list "identifier" (identifier def))
   (dolist (field (fields def))
     (block-xml file "field" (list "identifier" (identifier field))
		(why2xml* file (type field))))))

(defmethod why2xml* ((file stream) (def why-logic-predicate))
  (block-xml
   file "predicate" 
   (list "identifier" (identifier def))
   (block-list-xml file "parameters" (binders def))
   (block-xml file "body" nil
     (why2xml* file (predicate def)))))

;;
;; Expressions
;;

(defmethod why2xml* ((file stream) (expr why-binding))
  (block-xml file "parameter" (list "identifier" (identifier expr))
	     (why2xml* file (type expr))))

(defmethod why2xml* ((file stream) (expr why-conditional))
  (block-xml 
   file "if" nil
   (block-xml 
    file "condition" nil
    (why2xml* file (branch-condition expr)))
   (block-xml
    file "then" nil
    (why2xml* file (true-branch expr)))
   (block-xml
    file "else" nil
    (why2xml* file (false-branch expr)))))

(defmethod why2xml* ((file stream) (expr why-function-application))
  (block-xml 
   file "application"
   nil
   (if (builtin expr)
       (line-xml file "operator"
		 (list "builtin" (identifier (operator expr))))
     (block-xml 
      file "operator" nil 
      (why2xml* file (operator expr))))
   (block-list-xml
    file "arguments" (arguments expr))))

(defmethod why2xml* ((file stream) (expr why-sequential-composition))
  (dolist (e (exprs expr))
    (why2xml* file e)))

(defmethod why2xml* ((file stream) (expr why-array-assignment))
  (block-xml
   file "arrayAssignment"
   (list 'identifier  (identifier expr))
   (block-xml
    file "index" nil
    (why2xml* file (index expr)))
   (block-xml
    file "rhs" nil
    (why2xml* file (expr expr)))))

(defmethod why2xml* ((file stream) (expr why-array-subscription))
  (block-xml
   file "arraySubscription"
   (list 'identifier  (identifier expr))
   (why2xml* file (index expr))))

(defmethod why2xml* ((file stream) (expr why-literal))
  (line-xml 
   file 
   (format nil "~aLiteral" (kind expr))
   (list "value" (value expr))))

(defmethod why2xml* ((file stream) (expr why-name))
  (line-xml 
    file "name"
    (list "identifier" (identifier expr)
	  "kind" (kind expr))))
  
(defmethod why2xml* ((file stream) (expr why-let))
  (block-xml
   file "let" (list "identifier" (identifier expr))
   (block-xml
    file "type" nil (why2xml* file (type expr)))
   (block-xml 
    file "expression" nil (why2xml* file (expr expr)))
   (block-xml
    file "in"  nil(why2xml* file (in_expr expr)))))

(defmethod why2xml* ((file stream) (expr why-record-assignment))
  (block-list-xml
   file "recordAssignment"
   (assignments expr)
   (list "identifier" (identifier expr))))

(defmethod why2xml* ((file stream) (expr why-record-selection))
  (line-xml
   file "recordSelection" 
   (list "identifier" (identifier expr)
	 "field" (field expr))))

(defmethod why2xml* ((file stream) (expr why-record-literal))
  (block-list-xml
   file "recordLiteral" 
   (assignments expr)))

(defmethod why2xml* ((file stream) (expr why-assignment))
  (block-xml 
   file "assignment"
   (list "identifier" (identifier expr))
   (why2xml* file (expr expr))))

(defmethod why2xml* ((file stream) (expr why-lambda-abstraction))
  (block-xml 
   file "lambda" nil
   (block-list-xml
    file "parameters"
    (bindings expr))
   (why2xml* file (expr expr))))

(defmethod why2xml* ((file stream) (expr why-array-literal))
  (block-xml
   file
   "arrayLiteral" 
   (list "lowerBound" (lower-bound expr)
	 "upperBound" (upper-bound expr))
   (block-xml 
    file "elementType" nil
    (why2xml* file (element-type expr)))
   (block-xml
    file "inititalization" nil
    (why2xml* file (init-function expr)))))

(defmethod why2xml* ((file stream) (expr why-quantifier))
  (block-xml
   file
   (quantifier expr)
   (list "lowerBound" (lower-bound expr)
	 "upperBound" (upper-bound expr))
   (why2xml* file (predicate expr))))

;;
;; Types
;;

(defmethod why2xml* ((file stream) (type why-primitive-type))
  (line-xml file "primitiveType" 
	    (list "identifier" (identifier type))))

(defmethod why2xml* ((file stream) (type why-array-type))
  (block-xml 
   file "arrayType"
   nil
   (why2xml* file (type type))))

(defmethod why2xml* ((file stream) (type why-record-type))
  (line-xml 
   file "recordType" (list "identifier" (identifier type))))

(defmethod why2xml* ((file stream) (type why-function-type))
  (block-xml
   file "functionType"
   nil
   (block-xml
    file "domain" nil
    (dolist (type (domain type))
      (why2xml* file type)))
   (block-xml
    file "range" nil
    (why2xml* file (range type)))))


;; Default

(defmethod  why2xml* ((file stream) type)
  (format file "<!-- ~a -->~%" type))

;; Release: PVS2Why-0.1 (11/10/07)
;; Utility functions
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
(defvar *pvs2why-indent* nil)
(defvar *pvs2why-default-indent* 2)

(defun now-today ()
  (multiple-value-bind (s mi h d mo y dow dst tz) 
		       (get-decoded-time)
		       (declare (ignore tz dst dow))
		       (format nil "~a:~a:~a ~a/~a/~a" h mi s mo d y)))

(defun get-indent ()
  (or (car *pvs2why-indent*) 0))

(defun open-block (&optional (n *pvs2why-default-indent*))
  (setq *pvs2why-indent* 
	(cons (+ n (get-indent))
	      *pvs2why-indent*)))

(defun close-block ()
 (setq *pvs2why-indent*  (cdr *pvs2why-indent*)))

(defun indent (file &optional (str ""))
  (format file "~a~a"
	  (make-string (get-indent) :initial-element #\Space)
	  str))

(defun bool2str (b)
  (format nil "~:[false~;true~]" b))

;; XML

(defun line-xml (file tag &optional attr expr)
  (if expr
      (indent file 
	      (format nil "<~a~{ ~a=\"~a\"~}>~a</~a>~%" tag attr expr tag))
      (indent file 
	      (format nil "<~a~{ ~a=\"~a\"~}/>~%" tag attr))))

(defun open-xml (file tag attr)
  (indent file
	  (format nil "<~a~{ ~a=\"~a\"~}>~%" tag attr))
  (open-block))

(defun close-xml (file tag)
  (close-block)
  (indent file (format nil "</~a>~%" tag)))

(defmacro block-xml (file tag attr &rest l)
  (append 
   (cons 'progn (list `(open-xml ,file ,tag ,attr)))
   l
   (list  `(close-xml ,file ,tag))))

;; Java
(defun open-java (file)
  (format file "{~%")
  (open-block))

(defun close-java (file)
  (close-block)
  (indent file (format nil "}~%")))

(defmacro block-java (file &rest l)
  (append 
   (cons 'progn (list `(open-java ,file)))
   l
   (list  `(close-java ,file))))











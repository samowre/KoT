;; Release: PVS2Why-0.1 (11/10/07)
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
(in-package :pvs)

(defvar *pvs2why-version* "")
(defvar *why-decls-trace* nil)
(defvar *why-types-trace* nil)
(defvar *pvs2why-trace* nil)
(defvar *pvs2why-unique-names* nil)

(let* ((filename   (environment-variable "PVS2WHYFILENAME"))
       (filedir    (environment-variable "PVS2WHYFILEDIR"))
       (pvsfile    (format nil "~a/~a.pvs" filedir filename))
       (packlist   (read-from-string (environment-variable "PVS2WHYPACK")))
       (debug      (read-from-string (environment-variable "PVS2WHYDEBUG")))
       (xml        (read-from-string (environment-variable "PVS2WHYXML")))
       (java       (read-from-string (environment-variable "PVS2WHYJAVA")))
       (*pvs2why-version* (environment-variable "PVS2WHYVERSIONMSG"))
       (*pvs2why-trace* debug)
       (*why-decls-trace* debug)
       (*why-types-trace* debug)
       (*noninteractive* t)
       (current-prefix-arg t))
  (multiple-value-bind 
      (val err)
      (ignore-errors
	(change-context (probe-file filedir))
	(dolist (pack packlist) (load-prelude-library pack))
	(typecheck-file filename nil nil nil t)
	(let* ((theories (theories-in-file filename)))
	  (format t "~%***~%*** Processing ~a via ~a~%***~%" pvsfile 
		  *pvs2why-version*)
	  (dolist (theory theories)
	    (when (or xml (not java)) 
	      (pvs2xml filename theory *pvs2why-version*)
	      (format t "*** ~a (~a) --> ~a/~a.xml~%" 
		      theory pvsfile filedir theory))
	    (when java 
	      (pvs2java filename theory *pvs2why-version*)
	      (format t "*** ~a (~a) --> ~a/~a.java~%" 
		      theory pvsfile filedir theory))))
	t)
    (when err 
      (format t "~%*** ~a (~a)~%" err pvsfile)
      (bye -1)))
  (fresh-line)
  (bye))


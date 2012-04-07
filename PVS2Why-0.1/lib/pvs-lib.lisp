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

(load-prelude-library "ProofLite")

(let* ((package-files '("util" "why" "pvs2why" "why2xml" "why2java"))
       (*load-verbose* t)
       (*compile-verbose* nil)
       (lisp-files (loop for f in package-files
                         collect (format nil "~a.lisp" f)))
       (fasl-files (loop for f in package-files
                         collect (format nil "~a.~a" f 
                                         sys::*fasl-default-type*))))
  (loop for lisp-file in lisp-files
        for fasl-file in fasl-files
        unless (and (probe-file fasl-file)
                    (<= (file-write-date lisp-file)
                        (file-write-date fasl-file)))
        do (compile-file lisp-file)
        do (libload fasl-file)))


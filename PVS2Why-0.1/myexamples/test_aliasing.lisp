(in-package 'PVS)

(defun tab_3 () #'(lambda (i) (the fixnum i)))
(defun _tab_3 (i) (declare (type fixnum i)) (the fixnum i))
(defun tab!_3 (i) (declare (type fixnum i)) (the fixnum i))
(defun plus_3 ()
  #'(lambda (|lamvar#0|)
      (let ((tab1 (project 1 |lamvar#0|))
            (tab2 (project 2 |lamvar#0|)))
        #'(lambda (i)
            (pvs__+ (the (integer 0) (pvs-funcall tab1 (the fixnum i)))
                    (the (integer 0)
                         (pvs-funcall tab2 (the fixnum i))))))))
(defun _plus_3 (tab1 tab2 i)
  (declare (type fixnum i))
  (pvs__+ (the (integer 0) (pvs-funcall tab1 (the fixnum i)))
          (the (integer 0) (pvs-funcall tab2 (the fixnum i)))))
(defun plus!_3 (tab1 tab2 i)
  (declare (type fixnum i))
  (pvs__+ (the (integer 0) (pvs-funcall tab1 (the fixnum i)))
          (the (integer 0) (pvs-funcall tab2 (the fixnum i)))))
(defun res_1 ()
  (pvs-funcall (plus_3)
               (the (simple-array t)
                    (pvs2cl_tuple (let
                                   ((A114 0))
                                   (let
                                    ((N113 2) (E112 (tab_3)))
                                    (pvs-outer-array-update
                                     E112
                                     A114
                                     N113
                                     3)))
                                  (tab_3)))))
(defun _res_0 ()
  (pvs-funcall (plus_3)
               (the (simple-array t)
                    (pvs2cl_tuple (let
                                   ((A107 0))
                                   (let
                                    ((N106 2) (E105 (tab_3)))
                                    (pvs-outer-array-update
                                     E105
                                     A107
                                     N106
                                     3)))
                                  (tab_3)))))
(defun res!_0 ()
  (pvs-funcall (plus_3)
               (the (simple-array t)
                    (pvs2cl_tuple (let
                                   ((LHS110 0))
                                   (let
                                    ((RHS108 2)
                                     (E109 (mk-fun-array (tab_3) 3)))
                                    (declare ((simple-array t) E109))
                                    (setf (svref E109 LHS110) RHS108)
                                    E109))
                                  (tab_3)))))
(defun n () (the (integer 0) (pvs-funcall (_res_0) 0)))
(defun _n () (the (integer 0) (pvs-funcall (_res_0) 0)))
(defun n! () (the (integer 0) (pvs-funcall (res!_0) 0)))
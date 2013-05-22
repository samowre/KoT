#! /usr/bin/clisp 
(format t "in")
(load "pico2")
(format t "in")
(if (< (length *args*) 2) 
    (format t "usage: ./lisp input_file output_file")
    (transform-pico-proof (car *args*) (cadr *args*)))
(format t "done")

(defpackage asdarf
  (:use :cl))
(in-package :asdarf)

(defstruct foo
  hp)

(defparameter *foo-num* 5)
(defparameter *foo-hp-max* 25)

(defun foo-print (foo)
  (format t "Foo (HP: ~d)~c" (foo-hp foo) #\newline))

(defun init-foos (*random-state*)
  (mapcar (lambda (hp) (make-foo :hp hp))
          (loop for x to (1- *foo-num*)
                collect (random *foo-hp-max*))))

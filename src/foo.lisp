(defpackage foo
  (:use :cl))
(in-package :foo)

(defstruct foo
  hp)

(defparameter *foo* (make-foo :hp 1234))

(format t "~a~c" (foo-hp *foo*) #\newline)
(setf (foo-hp *foo*) 123)
(format t "~a~c" (foo-hp *foo*) #\newline)

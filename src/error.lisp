(defpackage asdarf
  (:use :cl))
(in-package :asdarf)

(define-condition asdarf-error (condition) ())

(define-condition invalid-argument-type
  (asdarf-error)
  ((expected-type :initarg :expected-type
                  :reader invalid-argument-type-expected-type)
   (value :initarg :value
          :reader invalid-argument-type-value))
  (:report (lambda (condition stream)
             (format stream "invalid argument type given, expected ~a, value '~a'"
                     (invalid-argument-type-expected-type condition)
                     (invalid-argument-type-value condition)))))

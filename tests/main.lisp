(defpackage asdarf/tests/main
  (:use :cl
        :asdarf
        :rove))
(in-package :asdarf/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :asdarf)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))

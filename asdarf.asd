(defsystem "asdarf"
  :version "0.0.1"
  :author "crowdigit"
  :license "MIT"
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description ""
  :in-order-to ((test-op (test-op "asdarf/tests"))))

(defsystem "asdarf/tests"
  :author "crowdigit"
  :license "MIT"
  :depends-on ("asdarf"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for asdarf"
  :perform (test-op (op c) (symbol-call :rove :run c)))

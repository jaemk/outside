(defsystem "outside"
  :version "0.0.0"
  :author "James Kominick"
  :license "MIT"
  :depends-on (
      "uuid"
      "ironclad"
      "arrow-macros"
      "log4cl"
      "str"
      "hunchentoot"
      "drakma"
      "cl-json"
      "metabang-bind"
      "cl-interpol"
      "cl-ppcre"
      "local-time"
      "trivial-backtrace"
      "bordeaux-threads"
  )
  :serial t
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "utils")
                 (:file "web")
                 (:file "main")
                 )))
  :description ""
  :in-order-to ((test-op (test-op "outside/tests"))))

(defsystem "outside/tests"
  :author "James Kominick"
  :license "MIT"
  :depends-on ("outside"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for outside"
  :perform (test-op (op c) (symbol-call :rove :run c)))


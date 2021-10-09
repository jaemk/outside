(defconstant +deps+ '(
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
                      ))

(loop for d in +deps+ do
      (ql:quickload d))

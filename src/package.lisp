(defpackage outside.utils
  (:use :cl :arrow-macros :metabang-bind)
  (:export
    :now-millis
    :get-error-backtrace
    :get-log-level
    :aget))

(defpackage outside.web
  (:use :cl :arrow-macros :metabang-bind)
  (:export
    :start-server
    :stop-server
    :restart-server))

(defpackage outside
  (:use :cl :arrow-macros :metabang-bind)
  (:export
    :main))

(in-package :outside)


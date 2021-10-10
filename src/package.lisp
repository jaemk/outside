(defpackage outside.utils
  (:use :cl :arrow-macros :metabang-bind)
  (:export
    :now-millis
    :get-error-backtrace
    :aget
    :trim-to-nil))

(defpackage outside.config
  (:use :cl :arrow-macros :metabang-bind)
  (:export
    :value
    :*values*
    :load-values))

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


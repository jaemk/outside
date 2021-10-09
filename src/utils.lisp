(in-package :outside.utils)

(named-readtables:in-readtable :interpol-syntax)


(defun now-millis ()
  (bind ((now (local-time:now)))
    (+ (* 1000 (local-time:timestamp-to-unix now))
     (local-time:timestamp-millisecond now))))


(defmacro get-error-backtrace (e)
  (bind ((s (gensym)))
    `(bind ((,s (make-string-output-stream)))
       (progn
         (trivial-backtrace:print-backtrace ,e :output ,s)
         (get-output-stream-string ,s)))))


(defun s->log-level (s)
  (cond
    ((string-equal s "TRACE") :trace)
    ((string-equal s "DEBUG") :debug)
    ((string-equal s "INFO") :info)
    ((string-equal s "WARN") :warn)
    ((string-equal s "ERROR") :error)
    ((string-equal s "FATAL") :fatal)
    (t :info)))


(defun get-log-level ()
  (-> (sb-ext:posix-getenv "LOG_LEVEL")
      ((lambda (e) (if e (string-upcase e) nil)))
      (str:trim)
      (s->log-level)))


(defun aget (key alist)
  (->> (assoc key alist) rest))


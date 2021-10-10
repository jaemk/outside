(in-package :outside.config)

(named-readtables:in-readtable :interpol-syntax)


(defun s->log-level (s)
  (alexandria:eswitch (s :test #'equal)
    ("TRACE" :trace)
    ("DEBUG" :debug)
    ("INFO" :info)
    ("WARN" :warn)
    ("ERROR" :error)
    ("FATAL" :fatal)))


(defun env (var &key (default nil))
  (or
    (some->
      (sb-ext:posix-getenv var)
      (outside.utils:trim-to-nil))
    default))


(defun parse-boolean (s)
  (bind ((b (some-> s str:trim string-downcase)))
    (or
      (equal b "true")
      (equal b "t"))))


(defmacro get-version ()
  (when (uiop:probe-file* "commit_hash.txt")
    (->
      (str:from-file "commit_hash.txt")
      (str:trim)
      ((lambda (s)
         (format t "~&WARNING: loaded commit hash: ~a - should only be at build time!~%" s)
         s)))))


(defvar *values* nil)


(defun load-values ()
  (setf *values* (make-hash-table))
  (setf (gethash :log-level *values*)
        (some-> (env "LOG_LEVEL" :default "info") string-upcase s->log-level))
  (setf (gethash :version *values*)
        (get-version))
  (setf (gethash :host *values*)
        (-> (env "PUBLIC") parse-boolean ((lambda (p) (if p "0.0.0.0" "127.0.0.1")))))
  (setf (gethash :port *values*)
        (some-> (or (env "PORT") "3003") parse-integer)))


(defun value (key)
  (when (null *values*)
    (load-values))
  (gethash key *values*))


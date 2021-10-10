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
  (->
    (list
      (list :log-level
        (some-> (env "LOG_LEVEL" :default "info") string-upcase s->log-level))
      (list :version
        (get-version))
      (list :host
        (-> (env "PUBLIC") parse-boolean ((lambda (p) (if p "0.0.0.0" "127.0.0.1")))))
      (list :port
        (some-> (or (env "PORT") "3003") parse-integer)))
    ((lambda (vals)
       (setf *values* (make-hash-table))
       (loop for (k v) in vals do
         (setf (gethash k *values*) v))))))


(defun value (key)
  (when (null *values*)
    (load-values))
  (gethash key *values*))


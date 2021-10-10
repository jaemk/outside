(in-package :outside)

(named-readtables:in-readtable :interpol-syntax)


;; handle any errors if they aren't cause by the catch-all handler in 'main
(setf
  *debugger-hook*
  (lambda (c old-hook)
    (declare (ignore old-hook))
    (format *error-output* "~&Unhandled error: ~a~%" c)
    (sb-ext:quit :unix-status 1)))


(defun main (argvs)
  (handler-case
    (progn
      (log:config (outside.config:value :log-level))
      (log:config :sane2)
      (log:config :nofile)
      (log:debug "args: ~a" argvs)
      (outside.web:start-server
        :address (outside.config:value :host)
        :port (outside.config:value :port))
      (sb-thread:join-thread
        (find-if (lambda (th)
                   (str:starts-with-p "hunchentoot-listener" (sb-thread:thread-name th)))
                 (sb-thread:list-all-threads))))

    ;; C-c
    (sb-sys:interactive-interrupt
      ()
      (progn
        (format t "~&Aborting...~%")
        (outside.web:stop-server)
        (sb-ext:quit :unix-status 1)))

    ;; everything else
    (error
      (e)
      (progn
        (format *error-output* "~&Error: ~a~%" e)
        (sb-ext:quit :unix-status 1)))))


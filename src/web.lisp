(in-package :outside.web)

(named-readtables:in-readtable :interpol-syntax)
(setq ppcre:*allow-named-registers* t)


;; Create our own acceptor so we can customize logging and error handling
(defclass accptr (hunchentoot:acceptor)
  ((dispatch-table
    :initform '()
    :accessor dispatch-table
    :documentation "List of dispatch functions"))
  (:default-initargs
   :address "127.0.0.1"))


;; Implement dispatch for our acceptor
(defmethod hunchentoot:acceptor-dispatch-request ((acceptor accptr) request)
  (mapc
    (lambda (dispatcher)
	  (let ((handler (funcall dispatcher request)))
	    (when handler
          (progn
            (setf (hunchentoot:aux-request-value :request-start request) (outside.utils:now-millis))
	        (return-from hunchentoot:acceptor-dispatch-request (funcall handler))))))
	(dispatch-table acceptor))
  (call-next-method))


;; Customize request access log format
(defmethod hunchentoot:acceptor-log-access ((acceptor accptr)
                                            &key return-code)
  (bind ((method (hunchentoot:request-method*))
         (uri (hunchentoot:request-uri*))
         (request-start (hunchentoot:aux-request-value :request-start hunchentoot:*request*))
         (now (outside.utils:now-millis))
         (request-time-millis (- now request-start)))
    (hunchentoot:acceptor-log-message
      acceptor
      :info
      "~a ~a ~a ~ams"
      return-code method uri request-time-millis)))


;; Generate a response for unexpected handler errors.
;; Handler errors should generally be caught and formatted
;; by the `defhandler` macro's handler-cases before "bad"
;; http-return-codes make it here, but if we do something
;; that causes an error in hunchentoot code, then we may end up here.
(defmethod hunchentoot:acceptor-status-message ((acceptor accptr)
                                                http-return-code
                                                &key
                                                &allow-other-keys)
  ;; look for an error/message in the request data, or return a placeholder
  (bind ((handler-error (hunchentoot:aux-request-value :handler-error hunchentoot:*request*))
         (handler-error-msg (hunchentoot:aux-request-value :handler-error-msg hunchentoot:*request*))
         (error-msg (or handler-error-msg "Something went wrong..."))
         )
    (log:error "http-code: ~a, handler error: ~a, message: ~a" http-return-code handler-error error-msg)
    error-msg))


;; Override the acceptor's logger to use our logger
(defmethod hunchentoot:acceptor-log-message ((acceptor accptr)
                                             log-level
                                             format-string
                                             &rest format-arguments)
  (bind ((fargs (append (list nil format-string) format-arguments))
         (s (apply 'format fargs)))
    (case log-level
      (:trace (log:trace "~a" s))
      (:debug (log:debug "~a" s))
      (:info  (log:info  "~a" s))
      (:warn  (log:warn  "~a" s))
      (:error (log:error "~a" s))
      (:fatal (log:fatal "~a" s))
      (t      (log:error "~a" s)))))


(defmacro defhandler (name &rest sexps)
  "Define a handler function that handles errors"
  `(defun ,name ()
     (handler-case
        (progn
          ,@sexps)
        ;; ---
        ;; handle known errors here
        (sb-int:simple-parse-error (e)
          (progn
            (log:error "number parse error: ~a~&~a" e (outside.utils:get-error-backtrace e))
            (setf (hunchentoot:return-code*) 400)
            (format nil "invalid number")))

        ;; ---
        ;; everything else
        (error (e)
          (progn
            (log:error "unexpected handler error: ~a~&~a" e (outside.utils:get-error-backtrace e))
            (setf (hunchentoot:return-code*) 500)
            (format nil "Something went wrong..."))))))


;;; Define handlers
(defhandler index ()
  (setf (hunchentoot:content-type*) "text/plain")
  (bind (((:values s cached-p) (get-weather-cached "41.351691" "-71.718995")))
    (log:info "returning weather info, cached: ~a" cached-p)
    s))


(defmacro get-version ()
  (when (uiop:probe-file* "commit_hash.txt")
    (->
      (str:from-file "commit_hash.txt")
      (str:trim)
      ((lambda (s) (format t "~&WARNING: loaded commit hash: ~a - should only be at build time!~%" s) s)))))


(defhandler status ()
  (cl-json:encode-json-to-string (list (cons :ok "ok") (cons :version (get-version)))))


(defhandler hello ()
  (bind ((uri (hunchentoot:request-uri*))
         ((:values s caps) (ppcre:scan-to-strings #?r"/hello/(?<name>\w+)/?$" uri))
         (#(name) (or caps #(nil))))
    (declare (ignore s))
    (format nil "hello~:[~;, ~@(~a~)~]!" name name)))


;; routes listed in most to least specific order
(defvar *routes*
  (list
    (hunchentoot:create-regex-dispatcher #?r"/hello(/\w+)?/?$" 'hello)
    (hunchentoot:create-regex-dispatcher #?r"/status/?$" 'status)
    (hunchentoot:create-static-file-dispatcher-and-handler "/favicon.ico" "static/favicon.ico")
    (hunchentoot:create-prefix-dispatcher "/" 'index)))


(defun set-routes (server)
  ;; reverse the routes list so the last entry of *routes* is
  ;; pushed first and is at the lowest match priority
  (loop for r in (reverse *routes*) do
    (push
      r
     (dispatch-table server)))
  server)


(defvar *server* nil)

(defun start-server (&key (port nil) (address nil))
  (bind ((address (or address "127.0.0.1"))
         (port (or port 3003)))
    (log:info "starting server on ~a:~a" address port))
    (if (null *server*)
        (progn
          (->> (make-instance 'accptr :port (or port 3003) :address (or address "127.0.0.1"))
               (set-routes)
               (setf *server*)
               (hunchentoot:start))
          (log:info "server started"))
        (log:warn "server already started")))


(defun stop-server ()
  (if (null *server*)
    (log:warn "server already stopped")
    (progn
      (log:info "stopping server")
      (hunchentoot:stop *server*)
      (setf *server* nil)
      (log:info "server stopped"))))


(defun restart-server ()
  (log:info "restarting server")
  (stop-server)
  (start-server))


(defvar *cache-lock* (bt:make-lock "cache-lock"))
(defvar *cache* (make-hash-table :test 'equal))
(defparameter cache-lifespan-ms #.(* 60 60 1000))
(defparameter *user-agent* "https://outside.kominick.com")


(defun get-weather-cached (lat long)
  "Check for and return existing weather data or fetch fresh data"
  (bt:with-lock-held (*cache-lock*)
    (bind ((key #?"${lat},${long}")
           (exists (gethash key *cache*))
           (#(value created-ms) (or exists (vector nil 0)))
           (now-ms (outside.utils:now-millis)))
      (if (and exists
               (< (- now-ms created-ms) cache-lifespan-ms))
        (values value t)
        (bind ((new-val (get-weather lat long))
               (now-ms (outside.utils:now-millis))
               (result (vector new-val now-ms)))
          (progn
            (setf (gethash key *cache*) result)
            (values new-val nil)))))))


(defun get-weather (lat long)
  (bind ((latlong #?"${lat},${long}"))
    (->>
      (drakma:http-request
        #?"https://api.weather.gov/points/${latlong}"
        :additional-headers (list (cons "user-agent" *user-agent*)))
      (flexi-streams:octets-to-string)
      (cl-json:decode-json-from-string)
      (outside.utils:aget :properties)
      (outside.utils:aget :forecast)
      ((lambda (uri)
         (drakma:http-request
           uri
           :additional-headers (list (cons "user-agent" *user-agent*)))))
      (flexi-streams:octets-to-string)
      (cl-json:decode-json-from-string)
      (outside.utils:aget :properties)
      (outside.utils:aget :periods)
      (mapcar
        (lambda (period)
          (bind ((name (outside.utils:aget :name period))
                 (temp (outside.utils:aget :temperature period))
                 (temp-unit (outside.utils:aget :temperature-unit period))
                 (wind (outside.utils:aget :wind-speed period))
                 (wind-dir (outside.utils:aget :wind-direction period))
                 (detailed (outside.utils:aget :detailed-forecast period)))
            #?"${name}: ${temp} ${temp-unit}, ${wind} ${wind-dir}\n${detailed}")))
      (str:join #?"\n\n"))))


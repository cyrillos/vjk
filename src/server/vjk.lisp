;;;
;;; For SBCL
;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :usocket :silent t)
  (ql:quickload :flexi-streams :silent t)
  (ql:quickload :sqlite :silent t)
  (ql:quickload :cl-ppcre :silent t)
  (ql:quickload :bordeaux-threads :silent t)
  (ql:quickload :yason :silent t))

;;;
;;; Printing helpers
;;;
(defmacro pr-func (prefix fmt &rest body)
  `(format t (concatenate 'string ,prefix ,fmt "~%") ,@body))

(defmacro pr-err (fmt &rest body)
  `(pr-func "error: " ,fmt ,@body))

(defmacro pr-info (fmt &rest body)
  `(pr-func "info:  " ,fmt ,@body))

(defmacro pr-warn (fmt &rest body)
  `(pr-func "warn:  " ,fmt ,@body))

(defmacro pr-exit (fmt &rest body)
  `(progn
     (pr-info ,fmt ,@body)
     #+sbcl
     (sb-ext:exit :code 0)
     #+clisp
     (ext:exit 0)
     ))

(defmacro pr-fatal (fmt &rest body)
  `(progn
     (pr-err ,fmt ,@body)
     #+sbcl
     (sb-ext:exit :code 1)
     #+clisp
     (ext:exit 1)
     ))

;;;
;;; Date and time conversion
;;; http://lisptips.com/post/11649360174/the-common-lisp-and-unix-epochs
;;;
(defparameter *unix-epoch-difference*
  (encode-universal-time 0 0 0 1 1 1970 0))

(defun universal-to-unix-time (universal-time)
  (- universal-time *unix-epoch-difference*))

(defun unix-to-universal-time (unix-time)
  (+ unix-time *unix-epoch-difference*))

(defun get-unix-time ()
  (universal-to-unix-time (get-universal-time)))

(defun today-starts-unix-time ()
  (multiple-value-bind
    (second minute hour date month year day-of-week dst-p tz)
    (get-decoded-time)
    (universal-to-unix-time (encode-universal-time 0 0 0 date month year))))

(defun today-ends-unix-time ()
  (multiple-value-bind
    (second minute hour date month year day-of-week dst-p tz)
    (get-decoded-time)
    (universal-to-unix-time (encode-universal-time 59 59 23 date month year))))

;;;
;;; Database
;;;
(defparameter *db-lock* (bordeaux-threads:make-lock))
(defun db-lock () (bordeaux-threads:acquire-lock *db-lock*))
(defun db-unlock () (bordeaux-threads:release-lock *db-lock*))

;;;
;;; Main code
;;;

(defmacro make-vjk-key (val)
  `(concatenate 'string "vjk-" ,val))

(defmacro hash-vjk-key (key value htable)
  `(setf (gethash (make-vjk-key ,key) ,htable) ,value))

(defun vjk-encode-reply (stream status &optional msg)
  (yason:with-output (stream)
    (yason:with-object ()
      (yason:encode-object-element "status" status)
      (when msg
        (yason:encode-object-element "message" msg)))))

(defun vjk-encode-reply-ok (stream)
  (vjk-encode-reply stream "OK"))

(defun vjk-encode-reply-err (stream msg)
  (vjk-encode-reply stream "ERROR" msg))

(defun vjk-handle-start (db jhash) t)
(defun vjk-handle-stop (db jhash)
  (pr-exit "Bye!"))

(defun vjk-handle-list (db jhash) t)

(defun vjk-read-line (stream)
  (let* ((flexi (flexi-streams:make-flexi-stream
                  stream :external-format :utf-8))
         (str (read-line flexi)))
    (close flexi)
    str))

(defun vjk-write-line (stream)
  (let* ((flexi (flexi-streams:make-flexi-stream
                  stream :external-format :utf-8))
         (str (write-line flexi)))
    (close flexi)
    str))

(defun vjk-handle-request (db sk stream)
  (let ((json (vjk-read-line stream)))
    (pr-info "vjk-handle-request: ~a" json)
    (let* ((jhash (yason:parse json))
           (cmd (if jhash (gethash "cmd" jhash) nil)))
      (when cmd
        (cond ((string= cmd "start")
               (vjk-handle-start db jhash))
              ((string= cmd "stop")
               (vjk-handle-stop db jhash))
              ((string= cmd "list")
               (vjk-handle-list db jhash))
              (t (pr-fatal "Unknown command ~a")))))))

(defun vjk-server (hash-conf)
  (let ((sk-server (usocket:socket-listen
                     (gethash (make-vjk-key "address") hash-conf)
                     (gethash (make-vjk-key "port") hash-conf)
                     :reuse-address t
                     :element-type '(unsigned-byte 8)))
        (db (sqlite:connect (gethash "path" (gethash "database" hash-conf)))))
    (unwind-protect
      (loop do
            (let* ((sk-client (usocket:socket-accept sk-server))
                   (client-stream (usocket:socket-stream sk-client)))
              (sb-thread:make-thread
                (lambda ()
                  (funcall #'vjk-handle-request db sk-client client-stream)
                  (sqlite:disconnect db)
                  (close client-stream)
                  (usocket:socket-close sk-client))))))))

(defun file-get-content (filename)
  (when (probe-file filename)
    (with-open-file (stream filename)
      (let ((content (make-string (file-length stream))))
        (read-sequence content stream)
        content))))

(defun conf-validate-addr (val htable)
  (let ((addrport (cl-ppcre:split ":" val)))
    (when (>= (list-length addrport) 2)
      (ignore-errors
        (multiple-value-bind
          (address port)
          (values (first addrport)
                  (parse-integer (second addrport)))
          (hash-vjk-key "address" address htable)
          (hash-vjk-key "port" port htable)
          (return-from conf-validate-addr t))))))

(defun conf-validate-db-type (val htable)
  (declare (ignore htable))
  (when (string= val "sqlite3") t))

(defun conf-validate-db-path (val htable)
  (declare (ignore htable))
  (when (probe-file val) t))

(defparameter conf-template
  (list
    '("address"         conf-validate-addr)
    '("database"
      ("type"           conf-validate-db-type)
      ("path"           conf-validate-db-path))))

;
; Walk over template and json parsed hash
; to validate values
(defun conf-validator (template htable)
  (when (and template htable)
    (dolist (elem template)
      (let* ((head (car elem))
             (hash (gethash head htable))
             (tail (car (cdr elem)))
             (res
               (if (symbolp tail)
                   (funcall tail hash htable)
                   (conf-validator (cdr elem) hash))))
        (if (not res)
              (progn
                (pr-err "Invalid json input: \"~a\":\"~a\"" head hash)
                (return-from conf-validator nil)))))
    t))

(defun validate-conf (path hash-conf)
  (let ((res (conf-validator conf-template hash-conf)))
    (if (not res)
        (pr-err "Invalid json conf in ~a" path) nil)
        hash-conf))

(defun vjk-parse-conf (path)
    (pr-info "vjk-parse-conf: ~a" path)
    (handler-case
      (let ((hash-conf (yason:parse (file-get-content path))))
        (when (validate-conf path hash-conf)
          hash-conf))
      (error (c)
             (declare (ignore c))
             (pr-err "Can't parse ~s~%" path)
             nil)))

(defun handle-argv (argv)
  (when argv
    (if (string= (first argv) "--conf")
        (let ((hash-conf (vjk-parse-conf (second argv))))
          (when hash-conf
            (vjk-server hash-conf)))
        (handle-argv (cdr argv)))))

(handle-argv sb-ext:*posix-argv*)
(pr-fatal "No '--conf path-to-conf' option provided")

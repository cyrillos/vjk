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
(defmacro panic (message &rest body)
  `(error ,message ,@body))

(defmacro pr-func (prefix fmt &rest body)
  `(format t (concatenate 'string ,prefix ,fmt "~%") ,@body))

(defmacro pr-msg (fmt &rest body)
  `(format t (concatenate 'string ,fmt "~%") ,@body))

(defmacro pr-sep ()
  `(format t "-----------------------------------------------~%"))

(defmacro pr-err (fmt &rest body)
  `(pr-func "error: " ,fmt ,@body))

(defmacro pr-info (fmt &rest body)
  `(pr-func "info:  " ,fmt ,@body))

(defmacro pr-warn (fmt &rest body)
  `(pr-func "warn:  " ,fmt ,@body))

(defmacro pr-report (header &rest body)
  `(progn
     (pr-msg ,header)
     (pr-sep) ,@body (pr-sep)))

(defmacro pr-exit (fmt &rest body)
  `(progn
     (pr-info ,fmt ,@body)
     (sb-ext:exit :code 0)))

(defmacro pr-fatal (fmt &rest body)
  `(progn
     (pr-err ,fmt ,@body)
     (sb-ext:exit :code 1)))

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
           (cmd (gethash "cmd" jhash)))
      (when cmd
        (cond ((string= cmd "start")
               (vjk-handle-start db jhash))
              ((string= cmd "stop")
               (vjk-handle-stop db jhash))
              ((string= cmd "list")
               (vjk-handle-list db jhash))
              (t (pr-fatal "Unknown command ~a")))))))

(defun vjk-server (conf)
  (let* ((addrport (cl-ppcre:split ":" (gethash "address" conf)))
         (addr (first addrport))
         (port (parse-integer (second addrport)))
         (sk-server
           (usocket:socket-listen addr port
                                  :reuse-address t
                                  :element-type '(unsigned-byte 8)))
         (db (sqlite:connect (gethash "path" (gethash "database" conf)))))
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
  (with-open-file (stream filename)
    (let ((content (make-string (file-length stream))))
      (read-sequence content stream)
      content)))

(defun vjk-validate-conf (conf)
  (let ((db (gethash "database" conf))
        (addr (gethash "address" conf)))
        (if (not (and db addr))
            (pr-fatal "No database/address"))
        (if (not (and
                   (gethash "type" db)
                   (gethash "path" db)))
            (pr-fatal "No type/path")))
  conf)

(defun vjk-parse-conf (path)
    (pr-info "vjk-parse-conf: ~a" path)
    (let ((conf (yason:parse (file-get-content path))))
          (vjk-validate-conf conf)))

(defun handle-argv (argv)
  (when argv
    (if (string= (first argv) "--conf")
        (let ((conf (vjk-parse-conf (second argv))))
          (when conf
            (vjk-server conf)))
        (handle-argv (cdr argv)))))

(handle-argv sb-ext:*posix-argv*)
(pr-fatal "No '--conf path-to-conf' option provided")

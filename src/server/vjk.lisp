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
;;; Main code
;;;

(defun vjk-handle-start (jhash) t)
(defun vjk-handle-stop (jhash)
  (pr-exit "Buy!"))

(defun vjk-handle-list (jhash) t)

(defun vjk-handle-request (conf stream)
  (let* ((flexi (flexi-streams:make-flexi-stream stream
                                                :external-format :utf-8))
         (json (read-line flexi)))
    (pr-info "vjk-handle-request: ~a" json)
    (let* ((jhash (yason:parse json))
           (cmd (gethash "cmd" jhash)))
      (when cmd
        (cond ((string= cmd "start")
               (vjk-handle-start jhash))
              ((string= cmd "stop")
               (vjk-handle-stop jhash))
              ((string= cmd "list")
               (vjk-handle-list jhash))
              (t (pr-fatal "Unknown command ~a")))))))

(defun vjk-server (conf)
  (let* ((addrport (cl-ppcre:split ":" (gethash "address" conf)))
         (addr (first addrport))
         (port (parse-integer (second addrport)))
         (sk-server
           (usocket:socket-listen addr port
                                  :reuse-address t
                                  :element-type '(unsigned-byte 8))))
    (unwind-protect
      (loop do
            (let* ((sk-client (usocket:socket-accept sk-server))
                   (client-stream (usocket:socket-stream sk-client)))
              (sb-thread:make-thread
                (lambda ()
                  (let ((res
                          (funcall #'vjk-handle-request conf client-stream)))
                    (close client-stream)
                    (usocket:socket-close sk-client)
                    res))))))))

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

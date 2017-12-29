;;;
;;; For SBCL only
;;;
(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :usocket :silent t)
  (ql:quickload :yason :silent t)
  (ql:quickload :sqlite :silent t)
  (ql:quickload :cl-ppcre :silent t)
  (ql:quickload :trivial-utf-8 :silent t))

;;;
;;; Time helpers
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
    (seconds minute hour date month year day-of-week dst-p tz)
    (get-decoded-time)
    (declare (ignore seconds minute hour day-of-week dst-p tz))
    (universal-to-unix-time (encode-universal-time 0 0 0 date month year))))

(defun today-ends-unix-time ()
  (multiple-value-bind
    (seconds minute hour date month year day-of-week dst-p tz)
    (get-decoded-time)
    (declare (ignore seconds minute hour day-of-week dst-p tz))
    (universal-to-unix-time (encode-universal-time 59 59 23 date month year))))

;;;
;;; Various helpers
;;;
(defun cli-options ()
  #+:sbcl (rest sb-ext:*posix-argv*)
  #+:clisp (rest ext:*args*)
 )

(defmacro to-utf8 (b)
  `(trivial-utf-8:utf-8-bytes-to-string ,b))

(defmacro utf8-to (s)
  `(trivial-utf-8:string-to-utf-8-bytes ,s))

(defmacro ubytes (size &rest body)
  `(make-array ,size
               :element-type '(unsigned-byte 8)
               ,@body))

(defun read-utf8 (stream size)
  (ignore-errors
    (let* ((buf (ubytes size))
           (pos (read-sequence buf stream :start 0 :end size)))
      (when pos
        (to-utf8 (subseq buf 0 pos))))))

(defun read-file-utf8 (path)
  (when (probe-file path)
    (with-open-file (stream path :element-type '(unsigned-byte 8))
      (read-utf8 stream (file-length stream)))))

(defun get-ip-port (str)
  (let ((ipport (cl-ppcre:split ":" str)))
    (when (and (= (list-length ipport) 2)
               (not (string= "" (first ipport))))
      (values (first ipport)
              (parse-integer
                (second ipport) :junk-allowed t)))))

;;;
;;; JSON herlpers
;;;
(defun json-parse (s)
  (ignore-errors
    (case (array-element-type s)
      ((character) (yason:parse s :object-as :alist))
      (t (yason:parse (to-utf8 s) :object-as :alist)))))

(defmacro json-get (key jdata)
  `(cdr (assoc ,key ,jdata :test #'equalp)))

(defmacro json-add (key value jdata)
  `(setf ,jdata (nconc ,jdata (list (cons ,key ,value)))))

(defmacro json-leaf (key value)
  `(list (cons ,key ,value)))

;;;
;;; Stream helpers
;;;
(defun read-ustream (sk-ustream size)
  (ignore-errors
    (let*
      ((buf (ubytes size))
       (pos (read-sequence buf sk-ustream :start 0 :end size)))
      (when pos
        (json-parse (to-utf8 (subseq buf 0 pos)))))))

(defun read-cmd (sk-ustream)
  (let*
    ((jdata (read-ustream sk-ustream 16))
     (size (json-get "size" jdata)))
    (when size
      (read-ustream sk-ustream size))))

(defun json-encode-reply (status &key callback data)
  (yason:with-output-to-string* ()
    (yason:with-object ()
      (yason:encode-object-element "status" status)
      (if callback
        (yason:with-object-element ("data") (funcall callback data))
        (if data
            (yason:encode-object-element "data" data))))))

(defun json-encode-reply-ok (&key callback data)
  (json-encode-reply 200 :callback callback :data data))

(defun json-encode-reply-err (&key callback data)
  (json-encode-reply 400 :callback callback :data data))

(defun json-reply (sk-ustream data)
  (write-sequence (utf8-to data) sk-ustream)
  (force-output sk-ustream))

(defun json-reply-ok (sk-ustream &key callback data)
  (json-reply
    sk-ustream
    (json-encode-reply-ok :callback callback :data data)))

(defun json-reply-err (sk-ustream &key callback data)
  (json-reply
    sk-ustream
    (json-encode-reply-err :callback callback :data data)))

;;;
;;; Printing helpers
;;;
(defparameter *loglevels* '(("debug" . 4) ("info" . 3) ("warn" . 2) ("error" . 1)))

(defun loglevel-decode (name)
  (cdr (assoc name *loglevels* :test #'equalp)))

(defparameter *current-loglevel* (loglevel-decode "debug"))

(defun loglevel-get () (values *current-loglevel*))

(defun loglevel-set (lvl-or-name)
  (let ((lvl (if (stringp lvl-or-name)
                 (cdr (assoc lvl-or-name *loglevels* :test #'equalp))
                 (values lvl-or-name))))
    (when lvl
      (setf *current-loglevel* lvl))))

(defun loglevel-should-print (lvl)
  (when (<= lvl (loglevel-get)) t))

(defmacro print-on-level (lvl prefix fmt &rest body)
  `(when (loglevel-should-print ,lvl)
     (format t (concatenate 'string ,prefix ,fmt "~%") ,@body)))

(defmacro pr-err (fmt &rest body)
  `(print-on-level 1 "error: " ,fmt ,@body))

(defmacro pr-warn (fmt &rest body)
  `(print-on-level 2 "warn : " ,fmt ,@body))

(defmacro pr-info (fmt &rest body)
  `(print-on-level 3 "info : " ,fmt ,@body))

(defmacro pr-debug (fmt &rest body)
  `(print-on-level 4 "debug: " ,fmt ,@body))

(defmacro pr-exit (fmt &rest body)
  `(progn (pr-info ,fmt ,@body)
     #+sbcl (sb-ext:exit :code 0)
     #+clisp (ext:exit 0)
     ))

(defmacro pr-fatal (fmt &rest body)
  `(progn (pr-err ,fmt ,@body)
     #+sbcl (sb-ext:exit :code 1)
     #+clisp (ext:exit 1)
     ))

;;;
;;; Main code
;;;

(defparameter *conf* nil)

;;
;; Database structure and helpers
;; tables:
;;      activity
;;              @id
;;              @catid ---------+
;;              @name           |
;;              @comment        |
;;              @start          |
;;              @stop           |
;;      category                |
;;              @id     <-------+
;;              @name
;;              @comment

(defun db-fmt-conds (cols conds vals)
  (loop for x in cols for y in conds for z in vals
        collect (values (if (typep z 'string)
                            (format nil "~a~a~s" x y z)
                            (format nil "~a~a~a" x y z)))))

(defun db-fmt-vals (vals)
  (let ((res (mapcar #'(lambda (v)
                         (cond ((typep v 'string) (format nil "~s" v))
                               ((null v) (string "null"))
                               (t (format nil "~d" v))))
                         vals)))
    res))

(defun db-lookup (db table cols conds vals and-or)
  (ignore-errors
    (let* ((fmt (concatenate 'string "select * from ~a where "
                             "(~{~a~^ " and-or " ~});"))
           (req (format nil fmt table (db-fmt-conds cols conds vals))))
      (pr-debug "db-lookup: ~a" req)
      (sqlite:execute-to-list db req))))

(defun db-lookup-signle (db table col con val)
  (car (db-lookup db table (list col) (list con) (list val) nil)))

(defun db-insert (db table cols vals)
  (ignore-errors
    (let ((req (format nil
                       "insert into ~a (~{~a~^, ~}) values (~{~a~^, ~});"
                       table cols (db-fmt-vals vals))))
      (pr-debug "db-insert: ~a" req)
      (sqlite:execute-non-query db req)
      (sqlite:last-insert-rowid db))))

(defun db-update (db table cols conds vals id)
  (ignore-errors
    (let ((req (format nil "update ~a set ~{~a~^, ~} where id=~d;"
                       table (db-fmt-conds cols conds vals) id)))
      (pr-debug "db-update: ~a" req)
      (sqlite:execute-non-query db req)
      (sqlite:last-insert-rowid db))))

(defun db-last-id (db table)
  (ignore-errors
    (sqlite:execute-single
      db (format nil "select * from ~s order by id desc limit 1;" table))))

;;
;; Main code
;;

(defmacro ret-err(&optional msg)
  `(values (json-encode-reply-err :data ,msg) nil))

(defmacro ret-fatal(&optional msg)
  `(values (json-encode-reply-err :data ,msg) t))

(defmacro ret-ok(&key (should-exit nil))
  `(values (json-encode-reply-ok) ,should-exit))

(defun activity-start (db data)
  (pr-debug "activity-start: ~a" data)
  (multiple-value-bind (ts activity category comment)
    (values-list (mapcar #'(lambda(v) (json-get v data))
                         '("time" "activity" "category" "comment")))
    (when (not (and ts activity category))
      (return-from activity-start
                   (ret-err "Missing timestamp, name or category")))
    (let ((catid (db-lookup-signle db "category" "name" "=" category)))
      (when (not catid)
        (setf catid (db-insert db "category" '("name") (list category))))
      (when (not catid)
        (return-from activity-start
                     (ret-err "Unable to write category")))
      (let ((inserted (db-insert db "activity"
                                 '("catid" "name" "comment" "start")
                                 (list (car catid) activity comment ts))))
        (when (not inserted)
          (return-from activity-start
                       (ret-err "Unable to write activity")))
        (ret-ok)))))

(defun activity-stop (db data)
  (pr-debug "activity-stop: ~a" data)
  (let ((ts (json-get "time" data))
        (actid (db-last-id db "activity")))
    (when (not ts)
      (return-from activity-stop
                   (ret-err "Missing timestamp")))
    (when (not actid)
      (return-from activity-stop
                   (ret-err "No running activity found")))
    (let* ((rec (db-lookup-signle db "activity" "id" "=" actid))
           (id (car rec))
           (stopped (nth 5 rec)))
      (when stopped
        (return-from activity-stop
                     (ret-err "Activity already stopped")))
      (let ((updated (db-update db "activity" '("stop") '("=") (list ts) id)))
        (when (not updated)
           (return-from activity-stop
                       (ret-err "Failed to stop activity")))
        (ret-ok)))))

(defun get-category-name (db catid)
  (let ((rec (db-lookup-signle db "category" "id" "=" catid)))
    (if rec (nth 1 rec) "")))

(defun gen-activity-recs (args)
  (let ((enames (list "id" "category" "name" "comment" "start" "stop")))
    (yason:with-array ()
      (loop for x in (second args)
          collect (yason:with-object ()
                    (loop for y in x
                          for z in enames
                          collect (yason:encode-object-element
                                    z (if (eql z (nth 1 enames))
                                          (get-category-name (first args) y) y))))))))

(defun activity-list (db data)
  (pr-debug "activity-list: ~a" data)
  (let ((from (json-get "time-start" data))
        (to (json-get "time-stop" data)))
    (if (not from) (setf from (today-starts-unix-time)))
    (if (not to) (setf to (today-ends-unix-time)))
    (let ((recs (db-lookup db "activity"
                           '("start" "stop")
                           '(">=" "<=")
                           (list from to) "and")))
      (when (not recs)
        (return-from activity-list (ret-ok)))
      (values (json-encode-reply-ok
                :callback #'gen-activity-recs
                :data (list db recs)) nil))))

(defun handle-request (db sk-ustream)
  (let* ((jdata (read-cmd sk-ustream))
         (cmd (json-get "cmd" jdata))
         (data (json-get "data" jdata)))
    (pr-debug "handle-request: cmd ~a data ~a" cmd data)
    (cond ((string= "start" cmd)
           (activity-start db data))
          ((string= "stop" cmd)
           (activity-stop db data))
          ((string= "list" cmd)
           (activity-list db data))
          ((string= "exit" cmd)
           (ret-ok :should-exit t))
          (t (ret-err (format nil "Unknown command ~a" cmd))))))

(defun server (conf)
  (multiple-value-bind (ip port)
    (get-ip-port (json-get "address" conf))
    (pr-info "Listening: ~a:~a" ip port)
    (let ((sk-server (usocket:socket-listen ip port
                                            :reuse-address t
                                            :element-type '(unsigned-byte 8)))
          (db (sqlite:connect (json-get "path" (json-get "database" conf)))))
      (when (not db)
        (pr-exit "Can't connect to backend database"))
      (unwind-protect
        (loop do
              (let* ((sk-client (usocket:socket-accept sk-server))
                     (client-ustream (usocket:socket-stream sk-client)))
                (sb-thread:make-thread
                  (lambda ()
                    (multiple-value-bind (reply-data should-exit)
                      (funcall #'handle-request db client-ustream)
                      (when reply-data
                        (pr-debug "server: reply-data ~a" reply-data)
                        (json-reply client-ustream reply-data))
                      (close client-ustream)
                      (usocket:socket-close sk-client)
                      (when should-exit
                        (pr-exit "Exiting: ~a:~a" ip port)))))))))))

(defun process-args (argv)
  (when argv
    (let ((next (cdr argv)))
      (cond
        ((and (string= (first argv) "--conf")
              (>= (list-length argv) 2))
         (setf *conf* (json-parse (read-file-utf8 (second argv))))
         (setf next (cddr argv))))
      (process-args next))))

(process-args (cli-options))

(when (not *conf*)
    (pr-fatal "No '--conf path-to-conf' option provided"))

(loglevel-set (json-get "loglevel" *conf*))
(server *conf*)

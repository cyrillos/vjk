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
(defparameter *loglevels* '(("debug" . 3) ("info" . 2) ("warn" . 1) ("error" . 0)))

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
;;              @id                     entry id
;;              @catid ---------+       category id
;;              @name           |       activity name
;;              @comment        |       comment
;;              @tsstart        |       unix timestamp on start (utc time)
;;              @tzstart        |       timezone offset on start
;;              @tsstop         |       unix timestamp on stop
;;              @tzstop         |       timezone offset on stop
;;      category                |
;;              @id     <-------+
;;              @name                   category name
;;              @comment                comment

(defparameter *tb-activity* nil)
(defparameter *tb-category* nil)

(defun get-table-info (db table-name)
  (sqlite:execute-to-list db (format nil "pragma table_info(~s);" table-name)))

(defun get-table-column-names (table-info)
  (mapcar #'(lambda(col) (format nil "~s" (second col))) table-info))

(defun db-fmt-cond (x y z &key allow-nils)
  (if (and allow-nils (null z))
      (format nil "~a~anull" x y)
      (if (typep z 'string)
          (format nil "~a~a'~a'" x y (remove #\' z))
          (format nil "~a~a~a" x y z))))

(defun db-fmt-conds (cols conds vals &key allow-nils)
  (loop for x in cols for y in conds for z in vals
        when (if allow-nils (and x y) (and x y z))
        collect (values (db-fmt-cond x y z :allow-nils allow-nils))))

(defun db-fmt-vals (vals)
  (let ((res (mapcar #'(lambda (v)
                         (cond ((typep v 'string) (format nil "'~a'" (remove #\' v)))
                               ((null v) (string "null"))
                               (t (format nil "~d" v))))
                         vals)))
    res))

(defun db-op (db table op cols conds vals and-or)
  (ignore-errors
    (let* ((fmt (concatenate 'string op "from ~a "
                             (if (null conds) ""
                                 (concatenate 'string "where (~{~a~^ " and-or " ~})"))
                             ";"))
           (req (format nil fmt table (db-fmt-conds cols conds vals))))
      (pr-debug "db-op: ~a" req)
      (sqlite:execute-to-list db req))))

(defun db-lookup (db table cols conds vals and-or)
  (db-op db table "select * " cols conds vals and-or))

(defun db-delete (db table cols conds vals and-or)
  (db-op db table "delete " cols conds vals and-or))

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

(defun db-update (db table cols conds vals id &key allow-nils)
  (ignore-errors
    (let ((req (format nil "update ~a set ~{~a~^, ~} where id=~d;"
                       table (db-fmt-conds cols conds vals
                                           :allow-nils allow-nils) id)))
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

(defun activity-insert (db data)
  (pr-debug "activity-insert: ~a" data)
  (multiple-value-bind (ts-start tz-start ts-stop tz-stop activity category comment)
    (values-list (mapcar #'(lambda(v) (json-get v data))
                         '("tsstart" "tzstart" "tsstop" "tzstop" "activity" "category" "comment")))
    (when (not (and ts-start tz-start activity category))
      (return-from activity-insert
                   (ret-err "Missing timestamps, timezone, name or category")))
    (let ((catid (db-lookup-signle db "category" "name" "=" category)))
      (when (not catid)
        (return-from activity-insert
                     (ret-err "Unable to find category")))
      (let ((cols (list "catid" "name" "comment" "tsstart" "tzstart" "tsstop" "tzstop"))
            (vals (list (car catid) activity comment ts-start tz-start ts-stop tz-stop)))
        (let ((inserted (db-insert db "activity"
                                   (loop for x in cols for y in vals when y collect x)
                                   (loop for y in vals when y collect y))))
          (when (not inserted)
            (return-from activity-insert
                         (ret-err "Unable to insert activity")))
          (ret-ok))))))

(defun activity-stop (db data)
  (pr-debug "activity-stop: ~a" data)
  (let ((ts-stop (json-get "tsstop" data))
        (tz-stop (json-get "tzstop" data))
        (actid (db-last-id db "activity")))
    (when (not (and ts-stop tz-stop))
      (return-from activity-stop
                   (ret-err "Missing timestamp or timezone")))
    (when (not actid)
      (return-from activity-stop
                   (ret-err "No running activity found")))
    (let* ((rec (db-lookup-signle db "activity" "id" "=" actid))
           (id (car rec))
           (stopped (nth 7 rec)))
      (when stopped
        (return-from activity-stop
                     (ret-err "Activity already stopped")))
      (let ((updated (db-update db "activity"
                                '("tsstop" "tzstop")
                                '("=" "=")
                                (list ts-stop tz-stop) id)))
        (when (not updated)
           (return-from activity-stop
                       (ret-err "Failed to stop activity")))
        (ret-ok)))))

(defparameter *prev-catid* nil)
(defparameter *prev-rec* nil)

(defun get-category-name (db catid)
  (if (eq *prev-catid* catid)
      (if *prev-rec* (nth 1 *prev-rec*) "")
      (let ((rec (db-lookup-signle db "category" "id" "=" catid)))
        (setf *prev-catid* catid)
        (setf *prev-rec* rec)
        (if rec (nth 1 rec) ""))))

(defun gen-activity-recs (args)
  (let ((enames (list "id" "category" "name" "comment"
                      "tsstart" "tzstart" "tsstop" "tzstop")))
    (yason:with-array ()
      (loop for x in (second args)
          collect (yason:with-object ()
                    (loop for y in x
                          for z in enames
                          when y
                          collect (yason:encode-object-element
                                    z (cond ((eql z (nth 1 enames))
                                             (get-category-name (first args) y))
                                            (t y)))))))))

(defun activity-list (db data)
  (pr-debug "activity-list: ~a" data)
  (let ((ts-start (json-get "tsstart" data))
        (ts-stop (json-get "tsstop" data)))
    (when (not ts-start)
      (return-from activity-list
                   (ret-err "Missing time start/stop parameters")))
    (let ((recs
            (db-lookup db "activity"
                       '("tsstart" "tsstop")
                       '(">=" "<=")
                       (list ts-start ts-stop) nil)))
      (when (not recs)
        (return-from activity-list (ret-ok)))
      (values (json-encode-reply-ok
                :callback #'gen-activity-recs
                :data (list db recs)) nil))))

(defun activity-update (db data)
  (pr-debug "activity-update ~a" data)
  (multiple-value-bind (id ts-start tz-start ts-stop tz-stop activity category comment)
    (values-list (mapcar #'(lambda(v) (json-get v data))
                         '("id" "tsstart" "tzstart" "tsstop" "tzstop"
                           "activity" "category" "comment")))
    (when (not (and id activity category))
      (return-from activity-update
                   (ret-err "Missing id, activity or category")))
    (let ((catid (db-lookup-signle db "category" "name" "=" category)))
        (when (and category (not catid))
          (when (not catid)
            (return-from activity-update
                         (ret-err "Unable to find category"))))
        (let ((updated
                (db-update db "activity"
                           '("catid" "name" "comment" "tsstart" "tzstart" "tsstop" "tzstop")
                           '("=" "=" "=" "=" "=" "=" "=")
                           (list (car catid) activity comment
                                 ts-start tz-start ts-stop tz-stop)
                           id :allow-nils t)))
          (when (not updated)
            (return-from activity-update
                         (ret-err "Unable to update activity")))
          (ret-ok)))))

(defun category-add (db data)
  (pr-debug "category-add ~a" data)
  (let ((category (json-get "category" data)))
    (when (not category)
      (return-from category-add
                   (ret-err "No category provided")))
    (let ((catid (db-lookup-signle db "category" "name" "=" category)))
      (when catid
        (return-from category-add
                     (ret-err "Category already exists")))
      (let ((inserted (db-insert db "category"
                                 '("name")
                                 (list category))))
        (when (not inserted)
          (return-from category-add
                       (ret-err "Unable to write category")))
        (ret-ok)))))

(defun gen-category-recs (args)
  (let ((enames (list "id" "category")))
    (yason:with-array ()
      (loop for x in (second args)
          collect (yason:with-object ()
                    (loop for y in x
                          for z in enames
                          when y
                          collect (yason:encode-object-element z y)))))))

(defun category-list (db data)
  (pr-debug "category-list ~a" data)
  (let ((recs
          (db-lookup db "category"
                     '("id" "name")
                     nil nil nil)))
    (when (not recs)
      (return-from category-list (ret-ok)))
    (values (json-encode-reply-ok
              :callback #'gen-category-recs
              :data (list db recs)) nil)))

(defun category-update (db data)
  (pr-debug "category-update ~a" data)
  (let ((name (json-get "category" data))
        (id (json-get "id" data)))
    (when (not (and id name))
      (return-from category-update
                   (ret-err "No category or id provided")))
    (let ((updated
            (db-update db "category"
                       '("name") '("=")
                       (list name)
                       id)))
      (when (not updated)
        (return-from category-update
                     (ret-err "Unable to update category")))
    (ret-ok))))

(defun category-delete (db data)
  (pr-debug "category-delete ~a" data)
  (let ((id (json-get "id" data)))
    (when (null id)
      (return-from category-delete
                   (ret-err "No category id provided")))
    (let ((recs (db-lookup db "activity"
                           '("catid")
                           '("=")
                           (list id) nil)))
      (when recs
        (return-from category-delete
                     (ret-err "Clean activities first")))
      (db-delete db "category"
                 '("id")
                 '("=")
                 (list id) nil)
      (ret-ok))))

(defun activity-delete (db data)
  (pr-debug "activity-delete ~a" data)
  (let ((id (json-get "id" data)))
    (when (null id)
      (return-from activity-delete
                   (ret-err "No activity id provided")))
    (db-delete db "activity"
               '("id")
               '("=")
               (list id) nil)
    (ret-ok)))

(defun handle-request (db sk-ustream)
  (let* ((jdata (read-cmd sk-ustream))
         (cmd (json-get "cmd" jdata))
         (data (json-get "data" jdata)))
    (pr-debug "handle-request: cmd ~a data ~a" cmd data)
    (cond ((string= "activity-add" cmd)
           (activity-insert db data))
          ((string= "activity-start" cmd)
           (activity-insert db data))
          ((string= "activity-stop" cmd)
           (activity-stop db data))
          ((string= "activity-list" cmd)
           (activity-list db data))
          ((string= "activity-update" cmd)
           (activity-update db data))
          ((string= "activity-delete" cmd)
           (activity-delete db data))
          ((string= "category-add" cmd)
           (category-add db data))
          ((string= "category-list" cmd)
           (category-list db data))
          ((string= "category-update" cmd)
           (category-update db data))
          ((string= "category-delete" cmd)
           (category-delete db data))
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
      (setf *tb-activity* (get-table-info db "activity"))
      (setf *tb-category* (get-table-info db "category"))
      (when (not (and *tb-activity* *tb-category*))
        (pr-exit "Unexpected database structure"))
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

(loglevel-set (json-get "loglevel-server" *conf*))
(server *conf*)

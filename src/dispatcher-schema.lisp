;   Copyright 2020-2021 James Fleming <james@electronic-quill.net>
;
;   Licensed under the GNU General Public License
;   - for details, see LICENSE.txt in the top-level directory


(in-package #:restagraph)
(declaim (optimize (compilation-speed 0)
                   (speed 2)
                   (safety 3)
                   (debug 3)))


(defun schema-dispatcher-v1 ()
  "Hunchentoot dispatch function for managing Restagraph's schema."
  (handler-case
    (let ((uri-parts (get-uri-parts
                       (get-sub-uri (tbnl:request-uri*)
                                    (uri-base-schema tbnl:*acceptor*)))))
      (log-message :debug (format nil "Handling schema ~A request ~{/~A~}"
                                  (tbnl:request-method*) uri-parts))
      (cond
        ;; Get a list of schema versions
        ((and
           (equal (tbnl:request-method*) :GET)
           (equal "list" (tbnl:get-parameter "version")))
         (let* ((session (neo4cl:establish-bolt-session (datastore tbnl:*acceptor*)))
                (version-list (list-schema-versions session)))
           ;; Disconnect from the db before doing anything else
           (neo4cl:disconnect session)
           (setf (tbnl:content-type*) "application/json")
           (setf (tbnl:return-code*) tbnl:+http-ok+)
           (cl-json:encode-json-alist-to-string version-list)))
        ;; Get the description of a single resource-type
        ((and
           (equal (tbnl:request-method*) :GET)
           (equal 1 (length uri-parts)))
         (let ((rtype (gethash (car uri-parts) (schema tbnl:*acceptor*))))
           (if rtype
             (progn
               (setf (tbnl:content-type*) "application/json")
               (setf (tbnl:return-code*) tbnl:+http-ok+)
               (cl-json:encode-json-alist-to-string (a-listify rtype)))
             (progn
               (setf (tbnl:content-type*) "text/plain")
               (setf (tbnl:return-code*) tbnl:+http-not-found+)
               "Resourcetype not defined"))))
        ;; Get a description of the whole schema in HTML format
        ((and (equal (tbnl:request-method*) :GET)
              (equal (tbnl:get-parameter "format") "html"))
         (progn
           (log-message :info "Dumping schema in HTML format")
           (setf (tbnl:content-type*) "text/html")
           (setf (tbnl:return-code*) tbnl:+http-ok+)
           ;; Prevent html-template graunching everything to a halt
           (setf html-template:*warn-on-creation* nil)
           ;; Prevent the output turning into a sea of whitespace interspersed with markup
           (setf html-template:*ignore-empty-lines* t)
           ;; Render the schema and emit the result
           (let ((layout-template-path (make-pathname :defaults (template-path tbnl:*acceptor*)
                                                      :type "tmpl"
                                                      :name "schema")))
             (log-message :debug (format nil "Path to layout template: ~A" layout-template-path))
             (log-message :debug (format nil "State of layout template: ~A"
                                         (probe-file layout-template-path)))
             (with-output-to-string (outstr)
               (html-template:fill-and-print-template
                 layout-template-path
                 (let ((typenames
                         (sort
                           (loop for k being the hash-keys
                                 in (schema *restagraph-acceptor*)
                                 collecting k)
                           #'string<)))
                   (list :resourcetypes
                         (mapcar #'(lambda (rtype)
                                     (p-listify (gethash rtype (schema tbnl:*acceptor*))))
                                 typenames)))
                 :stream outstr)
               outstr))))
        ;; Get a description of the whole schema in JSON format
        ((equal (tbnl:request-method*) :GET)
         (progn
           (log-message :info "Dumping schema in native JSON format")
           (setf (tbnl:content-type*) "application/json")
           (setf (tbnl:return-code*) tbnl:+http-ok+)
           (cl-json:encode-json-to-string
             (let ((typenames
                     (sort
                       (loop for k being the hash-keys
                             in (schema *restagraph-acceptor*)
                             collecting k)
                       #'string<)))
               (mapcar #'(lambda (rtype)
                           (a-listify (gethash rtype (schema tbnl:*acceptor*))))
                       typenames)))))
        ;; Set the current schema-version
        ((and (equal (tbnl:request-method*) :PUT)
              (integerp (parse-integer (tbnl:parameter "version") :junk-allowed t)))
         (progn
           (log-message :info (format nil "Attempting to set schema version to ~A"
                                      (tbnl:parameter "version")))
           (setf (tbnl:content-type*) "text/plain")
           (let* ((session (neo4cl:establish-bolt-session (datastore tbnl:*acceptor*)))
                  (versions (list-schema-versions session))
                  (new-version (parse-integer (tbnl:parameter "version")
                                              :junk-allowed t)))
             ;; Do we have that version in the database?
             (if (member new-version (cdr (assoc :versions versions))
                         :test #'equal)
               ;; It's there; ensure it's current.
               (handler-case
                 (progn
                   (when (set-current-schema-version session new-version)
                     ;; If there was an update, reload the working schema
                     (setf (schema tbnl:*acceptor*) (fetch-current-schema session)))
                   ;; Disconnect from the db, because we won't need it any more in this branch
                   (neo4cl:disconnect session)
                   ;; Return a success message
                   (setf (tbnl:return-code*) tbnl:+http-ok+)
                   "OK")
                 (neo4cl:database-error
                   (e)
                   (setf (tbnl:return-code*) tbnl:+http-internal-server-error+)
                   (format nil "~A.~A: ~A"
                           (neo4cl:category e)
                           (neo4cl:title e)
                           (neo4cl:message e)))
                 (neo4cl:client-error
                   (e)
                   (setf (tbnl:return-code*) tbnl:+http-internal-server-error+)
                   (format nil "~A.~A: ~A"
                           (neo4cl:category e)
                           (neo4cl:title e)
                           (neo4cl:message e)))
                 (error (e)
                        (setf (tbnl:return-code*) tbnl:+http-internal-server-error+)
                        (format nil "~A" e)))
               ;; No such version
               (progn
                   ;; Disconnect from the db, because we don't need it here
                   (neo4cl:disconnect session)
                 (setf (tbnl:return-code*) tbnl:+http-not-found+)
                 "This version doesn't exist")))))
        ((equal (tbnl:request-method*) :POST)
         (handler-case
           (progn
             ;; Log diagnostic stuff first
             (log-message :debug (format nil "Content-type: ~A" (tbnl:header-in* "Content-type")))
             (log-message :debug (format nil "Received POST parameters ~A"
                                         (mapcar #'car (tbnl:post-parameters*))))
             (log-message :debug (format nil "Length of schema parameter: ~D"
                                         (length (tbnl:post-parameter "schema"))))
             (log-message :debug (format nil "Type of schema parameter: ~D"
                                         (type-of (tbnl:post-parameter "schema"))))
             ;; Create a new schema-version
             (when (equal "true" (tbnl:post-parameter "create"))
               (progn
                 (log-message :info "Received request to create new schema")
                 ;; Create the new version-root
                 (let* ((session (neo4cl:establish-bolt-session (datastore tbnl:*acceptor*)))
                        (version (create-new-schema-version session)))
                   ;; Install the core schema
                   (log-message
                     :info
                     (format nil "Created new schema version ~D. Installing core schema."version))
                   (install-subschema session *core-schema* version)
                   ;; Reload the in-memory schema
                   ;; Do this even if a new subschema has been uploaded, for robustness:
                   ;; if the upload fails, the server should still have a working schema.
                   (setf (schema tbnl:*acceptor*) (fetch-current-schema session)))))
             ;; Upload a schema to install in the db
             ;; Expects URL-encoded file upload, as in this example:
             ;; curl --data-urlencode schema@webcat.json -X POST http://localhost:4950/schema/v1/
             (when (tbnl:post-parameter "schema")
               (log-message :info "Received schema for upload.")
               (let ((schemasource (tbnl:post-parameter "schema"))
                     (session (neo4cl:establish-bolt-session (datastore tbnl:*acceptor*))))
                 (if
                   (install-uploaded-schema
                     ;; Adapt to either file or inline data (string)
                     (if (stringp schemasource) (cl-json:decode-json-from-string schemasource)
                       (cl-json:decode-json-from-source (first schemasource)))
                     session)
                   (progn
                     (log-message :info "Successfull installed uploaded schema; reloading.")
                     (setf (schema tbnl:*acceptor*) (fetch-current-schema session)))
                   (progn
                     (setf (tbnl:content-type*) "text/plain")
                     (setf (tbnl:return-code*) tbnl:+http-internal-server-error+)
                     "That... did not go as planned."))))
             ;; Return an appropriate response code
             (if (or (equal "true" (tbnl:post-parameter "create"))
                     (tbnl:post-parameter "schema"))
               ;; If there was something to do and we got this far, report success
               (progn
                 (setf (tbnl:content-type*) "text/plain")
                 (setf (tbnl:return-code*) tbnl:+http-created+)
                 "Created")
               ;; Nothing to do; we're good
               (progn
                 (setf (tbnl:content-type*) "text/plain")
                 (setf (tbnl:return-code*) tbnl:+http-ok+)
                 "OK")))
           (neo4cl:database-error
             (e)
             (let ((message (format nil "~A.~A: ~A"
                                    (neo4cl:category e)
                                    (neo4cl:title e)
                                    (neo4cl:message e))))
               (log-message :error message)
               (setf (tbnl:return-code*) tbnl:+http-internal-server-error+)
               message))
           (neo4cl:client-error
             (e)
             (let ((message (format nil "~A.~A: ~A"
                                    (neo4cl:category e)
                                    (neo4cl:title e)
                                    (neo4cl:message e))))
               (log-message :error message)
               (setf (tbnl:return-code*) tbnl:+http-internal-server-error+)
               message))
           (error (e)
                  (let ((message (format nil "~A" e)))
                    (log-message :error message)
                    (setf (tbnl:return-code*) tbnl:+http-internal-server-error+)
                    message))))
        ;; Delete a schema-version
        ((and (equal (tbnl:request-method*) :DELETE)
              (integerp (parse-integer (tbnl:parameter "version") :junk-allowed t)))
         (let ((version (parse-integer (tbnl:parameter "version") :junk-allowed t)))
           (log-message :info (format nil "Requested to delete schema version ~D" version))
           (setf (tbnl:content-type*) "text/plain")
           (handler-case
             ;; The happy path: the version was successfully deleted
             (progn
               (let ((session (neo4cl:establish-bolt-session (datastore tbnl:*acceptor*))))
                 (delete-schema-version session version)
                 (neo4cl:disconnect session))
               (setf (tbnl:return-code*) tbnl:+http-ok+)
               "OK")
             ;; Basic error-handling
             (error (e) (return-client-error (format nil "~A" e))))))
        ;;
        ;; Methods we don't support.
        ;; Take the allow-list approach
        ((not (member (tbnl:request-method*) '(:GET :POST :PUT :DELETE)))
         (method-not-allowed))
        ;; Handle all other cases
        (t
          (return-client-error "This wasn't a valid request"))))
    ;; Handle general errors
    ;;
    ;; Attempted violation of db integrity
    (integrity-error (e) (return-integrity-error (message e)))
    ;; Generic client errors
    (neo4cl:client-error (e) (return-client-error (neo4cl:message e)))
    ;; Transient error
    (neo4cl:transient-error (e) (return-transient-error e))
    ;; Database error
    (neo4cl:database-error (e) (return-database-error e))
    ;; Service errors, e.g. connection refused
    (neo4cl:service-error (e) (return-service-error (neo4cl:message e)))))

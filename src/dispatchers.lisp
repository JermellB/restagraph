;   Copyright 2020 James Fleming <james@electronic-quill.net>
;
;   Licensed under the GNU General Public License
;   - for details, see LICENSE.txt in the top-level directory


(in-package #:restagraph)
(declaim (optimize (compilation-speed 0)
                   (speed 2)
                   (safety 3)
                   (debug 3)))


;; Error response functions

(defun four-oh-four (&key file-p)
  "Fallthrough handler, for anything we haven't already defined.
   Also for requests for files we don't have."
  (setf (tbnl:content-type*) "text/plain")
  (setf (tbnl:return-code*) tbnl:+http-not-found+)
  (if file-p
      "File not found"
      "This is not a valid URI"))

(defun method-not-allowed ()
  "Default response for a client making a request we don't support"
  (setf (tbnl:content-type*) "text/plain")
  (setf (tbnl:return-code*) tbnl:+http-method-not-allowed+)
  "Method not allowed")

(defun uri-not-implemented ()
  "It's an API request, but not one we're configured for."
  (setf (tbnl:content-type*) "text/plain")
  (setf (tbnl:return-code*) tbnl:+http-not-implemented+)
  "Not implemented")

(defun return-integrity-error (logmessage &optional client-message)
  "Report to the client that their request would have violated an integrity constraint.
  The optional client-message "
  (declare (type (string) logmessage)
           (type (or null string) client-message))
  (log-message :warn (format nil "Client triggered integrity error: ~A" logmessage))
  (setf (tbnl:content-type*) "text/plain")
  (setf (tbnl:return-code*) tbnl:+http-conflict+)
  ;; If we were handed a specific message, use that.
  ;; Otherwise, just pass on the logmessage.
  (if client-message client-message logmessage))

(defun return-database-error (message)
  "There was a database problem. Log it and report something generic to the user, not to keep them in the dark but to reduce the potential for data leakage."
  (declare (type (string) message))
  (log-message :error (format nil "Database error: ~A" message))
  (setf (tbnl:content-type*) "text/plain")
  (setf (tbnl:return-code*) tbnl:+http-internal-server-error+)
  "An error occurred with the database. This has been logged, and will be fixed.")

(defun return-transient-error (message)
  "Transient problem, which may already have self-resolved.. Log it and report something generic to the user, not to keep them in the dark but to reduce the potential for data leakage."
  (declare (type (string) message))
  (log-message :error (format nil "Database error: ~A" message))
  (setf (tbnl:content-type*) "text/plain")
  (setf (tbnl:return-code*) tbnl:+http-service-unavailable+)
  "A transient error occurred, and has been logged for us to work on. Please try your request again.")

(defun return-client-error (logmessage &optional message)
  "The client made a bad request. Return this information to them, that they may learn from their mistakes."
  (declare (type (string) logmessage)
           (type (or null string) message))
  (log-message :info (format nil "Client error: ~A" logmessage))
  (setf (tbnl:content-type*) "text/plain")
  (setf (tbnl:return-code*) tbnl:+http-bad-request+)
  ;; If we were handed a specific message, use that.
  ;; Otherwise, just pass on the logmessage.
  (format nil "Client error: ~A"
          (or message logmessage)))

(defun return-service-error (logmessage &optional message)
  "There was a problem with connecting to the backend service."
  (declare (type (string) logmessage)
           (type (or null string) message))
  (log-message :crit (format nil "Service error: ~A" logmessage))
  (setf (tbnl:content-type*) "text/plain")
  (setf (tbnl:return-code*) tbnl:+http-internal-server-error+)
  (format nil "Service error: ~A"
          (or message logmessage)))


;;; API dispatchers

(defun format-schema-for-graphql (db)
  "Render the schema in a format suitable for GraphQL,
  in the format of Apollo Server and neo4j-graphql-js"
  (log-message :debug "Formatting schema for GraphQL")
  (let ((all-resourcetype-names
          (remove-if
            #'(lambda (name)
                (member name
                        '("any" "rgSchemas" "rgSchemaVersions")
                        :test #'equal))
            (mapcar
              #'(lambda (r)
                  ; Pull out just the name of the resource
                  (cdr (assoc :name r)))
              (get-resource-types db)))))
    (format nil "~{~A~^~%~%~}"
            (mapcar
              #'(lambda (r)
                  (describe-resource-type-for-graphql
                    db
                    ; resourcetype
                    r
                    ; all-resourcetype-names
                    all-resourcetype-names
                    ; rels-from-any
                    (describe-dependent-resources db "any" :resources-seen nil)))
              (remove-if #'(lambda (rtype)
                             (member rtype
                                     '("any" "rgSchemas" "rgSchemaVersions")
                                     :test #'equal))
                         all-resourcetype-names)))))

(defun schema-dispatcher-v1 ()
  "Hunchentoot dispatch function for managing Restagraph's schema."
  (handler-case
    (let ((uri-parts (get-uri-parts
                       (get-sub-uri (tbnl:request-uri*)
                                    (uri-base-schema tbnl:*acceptor*)))))
      (log-message :debug "Handling schema ~A request /~{/~A~}" (tbnl:request-method*) uri-parts)
      (cond
        ;; Get the description of a single resource-type
        ((and
           (equal (tbnl:request-method*) :GET)
           (first uri-parts))
         (progn
           (setf (tbnl:content-type*) "application/json")
           (setf (tbnl:return-code*) tbnl:+http-ok+)
           (cl-json:encode-json-alist-to-string
             (describe-resource-type
               (datastore tbnl:*acceptor*)
               (first uri-parts)))))
        ;; Get a description of the whole schema for the GraphQL engine
        ((and
           (equal (tbnl:request-method*) :GET)
           (tbnl:get-parameter "format")
           (equal (tbnl:get-parameter "format") "graphql"))
         (progn
           (log-message :info "Dumping schema in GraphQL format")
           (setf (tbnl:content-type*) "application/json")
           (setf (tbnl:return-code*) tbnl:+http-ok+)
           (format-schema-for-graphql (datastore tbnl:*acceptor*))))
        ;; Get a description of the whole schema in JSON format
        ((equal (tbnl:request-method*) :GET)
         (progn
           (log-message :info "Dumping schema in native JSON format")
           (setf (tbnl:content-type*) "application/json")
           (setf (tbnl:return-code*) tbnl:+http-ok+)
           (cl-json:encode-json-to-string
             (mapcar
               #'(lambda (r)
                   (describe-resource-type
                     (datastore tbnl:*acceptor*)
                     (cdr (assoc :name r))))
               (get-resource-types (datastore tbnl:*acceptor*))))))
        ;; Add a resource-type
        ((and
           (equal (tbnl:request-method*) :POST)
           (equal (first uri-parts) "resourcetype"))
         (let ((resourcetype (second uri-parts)))
           (cond
             ;; Sanity-check: was the resource-type supplied as a non-empty string?
             ((or
                (equal resourcetype "")
                (equal resourcetype "NIL"))
              (progn
                (setf (tbnl:content-type*) "text/plain")
                (setf (tbnl:return-code*) tbnl:+http-bad-request+)
                "At least give me the name of the resourcetype to create"))
             ;; Sanity-check: is the resource-type already present?
             ((resourcetype-exists-p (datastore tbnl:*acceptor*)
                                     resourcetype)
              (progn
                (setf (tbnl:content-type*) "text/plain")
                (setf (tbnl:return-code*) tbnl:+http-ok+)
                "Resourcetype already exists"))
             ;; Sanity tests passed; store it
             (t
               (let ((dependent (if (equal "true" (tbnl:post-parameter "dependent")) t nil)))
                 (log-message
                   :debug
                   (format nil "Adding resource type ~A with dependent status '~A' and notes '~A'."
                           resourcetype dependent (tbnl:post-parameter "notes")))
                 (add-resourcetype
                   (datastore tbnl:*acceptor*)
                   resourcetype
                   :dependent dependent
                   :notes (tbnl:post-parameter "notes"))
                 ;; Return something useful
                 (setf (tbnl:content-type*) "text/plain")
                 (setf (tbnl:return-code*) tbnl:+http-created+)
                 "Created")))))
        ;; Add an attribute to a resource-type
        ((and
           (equal (tbnl:request-method*) :POST)
           (equal (length uri-parts) 3)
           (equal (first uri-parts) "attribute"))
         (let ((resourcetype (second uri-parts))
               (attribute (third uri-parts)))
           (cond
             ;; Missing/nil resourcetype parameter.
             ;; If it's invalid, set-resourcetype-attribute will catch it.
             ((or
                (equal resourcetype "")
                (equal resourcetype "NIL"))
              (progn
                (setf (tbnl:content-type*) "text/plain")
                (setf (tbnl:return-code*) tbnl:+http-bad-request+)
                "At least give me the name of the resourcetype to modify"))
             ;; Missing/nil attribute parameter.
             ;; If it's invalid, set-resourcetype-attribute will catch it.
             ((or
                (equal attribute "")
                (equal attribute "NIL"))
              (progn
                (setf (tbnl:content-type*) "text/plain")
                (setf (tbnl:return-code*) tbnl:+http-bad-request+)
                "At least give me the name of the attribute to add"))
             ;; Sanity tests passed; set it.
             ;; Note that if the named attribute already exists, it will be forcibly replaced.
             (t
               (let ((description (tbnl:post-parameter "description"))
                     (vals (tbnl:post-parameter "vals")))
                 (log-message :debug (format nil "Adding attribute '~A' to resource type '~A'."
                                             attribute resourcetype))
                 (log-message :debug (format nil "Description: ~A" (if description
                                                                     description
                                                                     "Not supplied")))
                 (log-message :debug (format nil "Enum vals: ~A" (if vals
                                                                   vals
                                                                   "Not supplied")))
                 ;; Accumulate the parameters to make for a clean function-call
                 (set-resourcetype-attribute (datastore tbnl:*acceptor*)
                                             resourcetype
                                             :name attribute
                                             :description description
                                             :vals vals)
                 ;; Return something useful
                 (setf (tbnl:content-type*) "text/plain")
                 (setf (tbnl:return-code*) tbnl:+http-created+)
                 "Created")))))
        ;; Delete a resource-type
        ((and
           (equal (tbnl:request-method*) :DELETE)
           (equal (first uri-parts) "resourcetype"))
         (handler-case
           (let ((resourcetype (second uri-parts)))
             ;; Remove it
             (log-message :debug (format nil "Deleting resource type ~A" resourcetype))
             (delete-resourcetype (datastore tbnl:*acceptor*) resourcetype)
             ;; Return something useful
             (setf (tbnl:content-type*) "text/plain")
             (setf (tbnl:return-code*) tbnl:+http-no-content+)
             "")
           (client-error
             (e)
             (return-client-error (message e)))))
        ;; Delete an attribute from a resource-type
        ((and
           (equal (tbnl:request-method*) :DELETE)
           (equal (first uri-parts) "attribute"))
         (handler-case
           (let ((resourcetype (second uri-parts))
                 (attribute (third uri-parts)))
             ;; Remove it
             (log-message :debug (format nil "Deleting attribute '~A' from resource type '~A'"
                                         attribute resourcetype))
             (delete-resourcetype-attribute (datastore tbnl:*acceptor*) resourcetype attribute)
             ;; Return something useful
             (setf (tbnl:content-type*) "text/plain")
             (setf (tbnl:return-code*) tbnl:+http-no-content+)
             "")
           ;; If it breaks, pass the bad news back to the client.
           (client-error
             (e)
             (return-client-error (message e)))))
        ;; Add a relationship
        ((and
           (equal (tbnl:request-method*) :POST)
           (equal (first uri-parts) "relationships"))
         (let ((source-type (second uri-parts))
               (relationship (third uri-parts))
               (destination-type (fourth uri-parts)))
           ;; Sanity test
           (if (and
                 source-type
                 (not (equal source-type ""))
                 (not (equal source-type "NIL"))
                 relationship
                 (not (equal relationship ""))
                 (not (equal relationship "NIL"))
                 destination-type
                 (not (equal relationship ""))
                 (not (equal relationship "NIL")))
             ;; Store it
             (progn
               (log-message :debug
                            (format nil "Adding relationship ~A from ~A to ~A"
                                    relationship source-type destination-type))
               ;; Make it go
               (add-resource-relationship
                 (datastore tbnl:*acceptor*)
                 source-type
                 relationship
                 destination-type
                 :dependent (tbnl:post-parameter "dependent")
                 :cardinality (tbnl:post-parameter "cardinality"))
               ;; Return something useful
               (setf (tbnl:content-type*) "text/plain")
               (setf (tbnl:return-code*) tbnl:+http-created+)
               "Created")
             (progn
               (setf (tbnl:return-code*) tbnl:+http-bad-request+)
               (setf (tbnl:content-type*) "text/plain")
               "All parameters are required: /<source-type>/<relationship>/<destination-type>"))))
        ;; Delete a relationship
        ((and
           (equal (tbnl:request-method*) :DELETE)
           (equal (first uri-parts) "relationships"))
         (let ((source-type (second uri-parts))
               (relationship (third uri-parts))
               (destination-type (fourth uri-parts)))
           ;; Delete it
           (log-message :debug
                        (format nil "Deleting relationship ~A from ~A to ~A"
                                relationship source-type destination-type))
           (delete-resource-relationship (datastore tbnl:*acceptor*)
                                         source-type
                                         relationship
                                         destination-type)
           ;; Return something useful
           (setf (tbnl:content-type*) "text/plain")
           (setf (tbnl:return-code*) tbnl:+http-no-content+)
           ""))
        ;;
        ;; Methods we don't support.
        ;; Take the whitelist approach
        ((not (member (tbnl:request-method*) '(:POST :GET :PUT :DELETE)))
         (method-not-allowed))
        ;; Handle all other cases
        (t
          (return-client-error "This wasn't a valid request"))))
    ;; Handle general errors
    ;;
    ;; Attempted violation of db integrity
    (restagraph:integrity-error (e) (return-integrity-error (message e)))
    ;; Generic client errors
    (neo4cl:client-error (e) (return-client-error (neo4cl:message e)))
    ;; Transient error
    (neo4cl:transient-error (e) (return-transient-error e))
    ;; Database error
    (neo4cl:database-error (e) (return-database-error e))
    ;; Service errors, e.g. connection refused
    (neo4cl:service-error (e) (return-service-error (neo4cl:message e)))))

(defun api-dispatcher-v1 ()
  "Hunchentoot dispatch function for the Restagraph API, version 1."
  (handler-case
    (let* ((sub-uri (get-sub-uri (tbnl:request-uri*) (uri-base-api tbnl:*acceptor*)))
           (uri-parts (get-uri-parts sub-uri)))
      (log-message :debug "Handling request ~{~A~} " uri-parts)
      (cond
        ;;
        ;; Intercept and reject attempts to interact with the "any" resource-type
        ((equal (third uri-parts) "any")
         (progn
           (setf (tbnl:content-type*) "text/plain")
           (setf (tbnl:return-code*) tbnl:+http-not-found+)
           (format nil "No resources found for ~A" uri-parts)))
        ;;
        ;; GET -> Retrieve something
        ((equal (tbnl:request-method*) :GET)
         (log-message :debug
                      (format nil "Dispatching GET request for URI ~A"
                              (tbnl:request-uri*)))
         (let* (;; Do it separately because we use it again later in this function.
                ;; Get the search result
                (result (get-resources (datastore tbnl:*acceptor*)
                                       sub-uri
                                       :directional (when (tbnl:get-parameter "directional") t)
                                       :filters (remove-if
                                                  #'(lambda (par)
                                                      (equal (car par) "directional"))
                                                  (tbnl:get-parameters*)))))
           ;; Return what we found
           (progn
             (setf (tbnl:content-type*) "application/json")
             (setf (tbnl:return-code*) tbnl:+http-ok+)
             (cond
               ;; Single resource was requested, and nothing was found.
               ((and (= (mod (length uri-parts) 3) 2)
                     (or (null result)
                         (equal result "")))
                "[]")
               ;; Single resource was requested, and something was found.
               ((= (mod (length uri-parts) 3) 2)
                (cl-json:encode-json-alist-to-string result))
               ;; Class of resources was requested, and nothing was found.
               ((or (null result)
                    (equal result ""))
                "[]")
               ;; Class of resources was requested, and something was found.
               (t
                 (cl-json:encode-json-to-string result))))))
        ;; POST -> Store something
        ;;
        ;; Resource
        ((and
           (equal (tbnl:request-method*) :POST)
           (equal (length uri-parts) 1)
           (tbnl:post-parameter "uid"))
         (let ((resourcetype (first uri-parts)))
           (log-message :debug
                        (format nil
                                "Attempting to dispatch a POST request for resource type ~A"
                                resourcetype))
           ;; Do we already have one of these?
           (if (get-resources
                 (datastore tbnl:*acceptor*)
                 (format nil "/~A/~A" (car uri-parts) (tbnl:post-parameter "uid")))
             ;; It's already there; return 200/OK
             (progn
               (log-message
                 :debug
                 (format nil
                         "Doomed attempt to re-create resource /~A/~A. Reassuring the client that it's already there."
                         (car uri-parts)
                         (tbnl:post-parameter "uid")))
               (setf (tbnl:content-type*) "text/plain")
               (setf (tbnl:return-code*) tbnl:+http-ok+)
               (format nil "/~A/~A" resourcetype
                       (sanitise-uid (tbnl:post-parameter "uid"))))
             ;; We don't already have one of these; store it
             (handler-case
               (progn
                 (store-resource (datastore tbnl:*acceptor*)
                                 resourcetype
                                 (tbnl:post-parameters*))
                 ;; Return it from the database, for confirmation
                 (log-message :debug "Stored the new resource. Now retrieving it from the database, to return to the client.")
                 (setf (tbnl:content-type*) "text/plain")
                 (setf (tbnl:return-code*) tbnl:+http-created+)
                 ;; Return the URI to the newly-created resource
                 (format nil "/~A/~A" resourcetype
                         (sanitise-uid (tbnl:post-parameter "uid"))))
               ;; Handle integrity errors
               (restagraph:integrity-error (e) (return-integrity-error (message e)))
               ;; Handle general client errors
               (restagraph:client-error (e) (return-client-error (message e)))))))
        ;;
        ;; Store a relationship
        ((and
           (equal (tbnl:request-method*) :POST)
           (equal (mod (length uri-parts) 3) 0)
           (tbnl:post-parameter "target"))
         ;; Grab these once, as we'll be referring to them a few times
         (let ((dest-path (tbnl:post-parameter "target")))
           ;; Basic sanity check
           (log-message :debug (format nil "Creating a relationship from ~A to ~A" sub-uri dest-path))
           (handler-case
             (progn
               (create-relationship-by-path (datastore tbnl:*acceptor*) sub-uri dest-path)
               ;; Report success to the client
               (setf (tbnl:content-type*) "text/plain")
               (setf (tbnl:return-code*) tbnl:+http-created+)
               ;; Return the URI to the resource at the end of the newly-created relationship
               (format nil "~{/~A~}/~A/~A"
                       uri-parts    ; Path down to the relationship
                       (car (last (get-uri-parts dest-path) 2)) ; Target resourcetype
                       (car (last (get-uri-parts dest-path))))) ; Target UID
             ;; Attempted violation of db integrity
             (restagraph:integrity-error (e) (return-integrity-error (message e)))
             ;; Generic client errors
             (restagraph:client-error (e) (return-client-error (message e)))
             (neo4cl:client-error (e) (return-client-error (neo4cl:message e))))))
        ;;
        ;; Create a dependent resource
        ((and
           (equal (tbnl:request-method*) :POST)
           (equal (mod (length uri-parts) 3) 1)
           (tbnl:post-parameter "uid"))
         ;; Do we already have one of these?
         (if (get-resources
               (datastore tbnl:*acceptor*)
               (format nil "~{/~A~}/~A" uri-parts (tbnl:post-parameter "uid")))
           ;; It's already there; return 200/OK
           (progn
             (log-message
               :debug
               (format nil
                       "Doomed attempt to re-create resource /~A/~A. Reassuring the client that it's already there."
                       (car uri-parts)
                       (tbnl:post-parameter "uid")))
             (setf (tbnl:content-type*) "text/plain")
             (setf (tbnl:return-code*) tbnl:+http-ok+)
             (format nil "~{/~A~}/~A" uri-parts (sanitise-uid (tbnl:post-parameter "uid"))))
           ;; We don't already have one of these; store it
           (handler-case
             (let ((newtype (car (last uri-parts)))
                   (uid (tbnl:post-parameter "uid")))
               (log-message :debug (format nil "Attempting to create dependent resource ~A:~A on ~A"
                                           newtype uid sub-uri))
               (store-dependent-resource
                 (datastore tbnl:*acceptor*) sub-uri (tbnl:post-parameters*))
               (setf (tbnl:content-type*) "text/plain")
               (setf (tbnl:return-code*) tbnl:+http-created+)
               ;; Return the URI to the resource at the end of the newly-created resource
               (format nil "~{/~A~}/~A" uri-parts (sanitise-uid (tbnl:post-parameter "uid"))))
             ;; Attempted violation of db integrity
             (restagraph:integrity-error (e) (return-integrity-error (message e)))
             ;; Generic client errors
             (restagraph:client-error (e) (return-client-error (message e)))
             (neo4cl:client-error (e) (return-client-error (neo4cl:message e))))))
        ;;
        ;; Re-home a dependent resource
        ((and
           (equal (tbnl:request-method*) :POST)
           (> (length uri-parts) 0)
           (equal (mod (length uri-parts) 3) 2)
           (tbnl:post-parameter "target"))
         (log-message :debug (format nil "Attempting to move dependent resource ~A to ~A"
                                     sub-uri (tbnl:post-parameter "target")))
         (handler-case
           (progn
             (move-dependent-resource
               (datastore tbnl:*acceptor*)
               sub-uri
               (tbnl:post-parameter "target"))
             (setf (tbnl:content-type*) "text/plain")
             (setf (tbnl:return-code*) tbnl:+http-created+)
             ;; Return the URI to the new path for this resource
             (format nil "~{/~A~}/~A/~A" (get-uri-parts (tbnl:post-parameter "target"))
                     (car (last uri-parts 2))
                     (car (last uri-parts))))
           ;; Attempted violation of db integrity
           (restagraph:integrity-error (e) (return-integrity-error (message e)))
           ;; Generic client errors
           (restagraph:client-error (e) (return-client-error (message e)))
           (neo4cl:client-error (e) (return-client-error (neo4cl:message e)))))
        ;; PUT -> update something
        ;;
        ;; Resource attributes
        ((and
           (equal (tbnl:request-method*) :PUT)
           (equal (mod (length uri-parts) 3) 2))
         (handler-case
           (progn
             (log-message
               :debug
               (format nil "Attempting to update attributes of resource ~{/~A~}" uri-parts))
             (update-resource-attributes
               (datastore tbnl:*acceptor*)
               uri-parts
               (remove-if #'(lambda (param)
                              (or (null (cdr param))
                                  (equal (cdr param) "")))
                          (append (tbnl:post-parameters*)
                                  (tbnl:get-parameters*))))
             (setf (tbnl:content-type*) "application/json")
             (setf (tbnl:return-code*) tbnl:+http-no-content+)
             "")
           ;; Attempted violation of db integrity
           (restagraph:integrity-error (e) (return-integrity-error (message e)))
           ;; Generic client errors
           (restagraph:client-error (e) (return-client-error (message e)))
           (neo4cl:client-error (e) (return-client-error (neo4cl:message e)))))
        ;;
        ;; DELETE -> Delete something
        ;;
        ;; Attempting to delete a file
        ((and (equal (tbnl:request-method*) :DELETE)
              (equal (mod (length uri-parts) 3) 2)
              (equal "files" (first uri-parts)))
         (log-message :debug "Client is requesting deletion of a file")
         (setf (tbnl:content-type*) "text/plain")
         (setf (tbnl:return-code*) tbnl:+http-bad-request+)
         (format nil "Use the files API to delete files: /files/v1/~A" (second uri-parts)))
        ;; Resource
        ((and (equal (tbnl:request-method*) :DELETE)
              (equal (mod (length uri-parts) 3) 2))
         (log-message :debug "Attempting to delete a resource on an arbitrary path")
         (handler-case
           (progn
             (delete-resource-by-path
               (datastore tbnl:*acceptor*)
               sub-uri
               :recursive (tbnl:post-parameter "recursive"))
             (setf (tbnl:content-type*) "text/plain")
             (setf (tbnl:return-code*) tbnl:+http-no-content+)
             "")
           ;; Attempted violation of db integrity
           (restagraph:integrity-error (e) (return-integrity-error (message e)))
           ;; Generic client errors
           (restagraph:client-error (e) (return-client-error (message e)))))
        ;;
        ;; Delete a relationship on an arbitrary path
        ((and (equal (tbnl:request-method*) :DELETE)
              (or (tbnl:post-parameter "resource")
                  (tbnl:get-parameter "resource"))
              (equal (mod (length uri-parts) 3) 0))
         (handler-case
           (progn
             (log-message :debug "Attempting to delete a relationship on an arbitrary path: ~A" sub-uri)
             (delete-relationship-by-path (datastore tbnl:*acceptor*)
                                          sub-uri
                                          (or (tbnl:post-parameter "resource")
                                              (tbnl:get-parameter "resource")))
             (setf (tbnl:content-type*) "text/plain")
             (setf (tbnl:return-code*) tbnl:+http-no-content+)
             "")
           ;; Attempted violation of db integrity
           (restagraph:integrity-error (e) (return-integrity-error (message e)))
           ;; Generic client errors
           (restagraph:client-error (e) (return-client-error (message e)))))
        ;;
        ;; Methods we don't support.
        ;; Take the whitelist approach
        ((not (member (tbnl:request-method*) '(:POST :GET :PUT :DELETE)))
         (method-not-allowed))
        ;; Handle all other cases
        (t
          (log-message :warn "Bad request received with URI: ~A, reassembled as ~{/~A~}"
                       (tbnl:request-uri*) uri-parts)
          (return-client-error "This wasn't a valid request"))))
    ;; Handle general errors
    ;;
    ;; Generic client errors
    (neo4cl:client-error (e) (return-client-error (neo4cl:message e)))
    ;; Transient error
    (neo4cl:transient-error (e) (return-transient-error e))
    ;; Database error
    (neo4cl:database-error (e) (return-database-error e))
    ;; Service errors, e.g. connection refused
    (neo4cl:service-error (e) (return-service-error (neo4cl:message e)))))

(defun files-dispatcher-v1 ()
  "Files API handler, API version 1."
  (log-message :debug "Handling files request")
  (log-message :debug (format nil "Handling files '~A' request with content-type '~A'"
                              (tbnl:request-method*) (tbnl:content-type*)))
  (cond
    ;; Client fails to upload a file
    ((and (equal (tbnl:request-method*) :POST)
          (or (null (tbnl:post-parameter "name"))
              (equal "" (tbnl:post-parameter "name")))
          (or (null (tbnl:post-parameter "file"))
              (equal "" (tbnl:post-parameter "file"))))
     (return-client-error "The 'name' and 'file' parameters must be supplied"))
    ((and (equal (tbnl:request-method*) :POST)
          (or (null (tbnl:post-parameter "name"))
              (equal "" (tbnl:post-parameter "name"))))
     (return-client-error "The 'name' parameter must be supplied"))
    ((and (equal (tbnl:request-method*) :POST)
          (or (null (tbnl:post-parameter "file"))
              (equal "" (tbnl:post-parameter "file"))))
     (return-client-error "The 'file' parameter must be supplied"))
    ;; Client uploads a file
    ((equal (tbnl:request-method*) :POST)
     (log-message :debug (format nil "Requested filename: ~A" (or (tbnl:post-parameter "name") "<not specified>")))
     (log-message :debug (format nil "Temporary filepath: ~A" (first (tbnl:post-parameter "file"))))
     (log-message :debug (format nil "Original filename: ~A" (second (tbnl:post-parameter "file"))))
     (log-message :debug (format nil "Reported content-type: ~A" (third (tbnl:post-parameter "file"))))
     (let* ((requested-filename (tbnl:post-parameter "name"))
            ;; File-details should be a three-element list: (path file-name content-type)
            ;; Where do we store these, anyway?
            ;; "path is a pathname denoting the place were the uploaded file was stored, file-name
            ;; (a string) is the file name sent by the browser, and content-type (also a string) is
            ;; the content type sent by the browser. The file denoted by path will be deleted after
            ;; the request has been handled - you have to move or copy it somewhere else if you want
            ;; to keep it."
            ;; Get the filepath of the uploaded file
            (filepath-temp (first (tbnl:post-parameter "file")))
            ;; Get the checksum of the file
            (checksum (hash-file filepath-temp))
            (mimetype (get-file-mime-type (namestring filepath-temp)))
            (filepath-target (digest-to-filepath (make-pathname :defaults (files-location tbnl:*acceptor*))
                                                                checksum)))
       ;; Does a file of this name already exist?
       (log-message :debug (format nil "Checking for an existing file by name '~A'"
                                   (sanitise-uid requested-filename)))
       (if (get-resources (datastore tbnl:*acceptor*)
                          (format nil "/files/~A" (sanitise-uid requested-filename)))
         ;; If it already exists, bail out now.
         (progn
           (log-message :error "File ~A already exists; bailing out."
                        (sanitise-uid requested-filename))
           ;; Return client-error message indicating name collision
           (setf (tbnl:content-type*) "text/plain")
           (setf (tbnl:return-code*) tbnl:+http-conflict+)
           "Filename already exists")
         ;; Name isn't already taken; rock on.
         (progn
           (log-message :debug "File ~A does not already exist; proceeding."
                        (sanitise-uid requested-filename))
           ;; Now we need to store the file's metadata,
           (log-message :debug "Storing file metadata: name = '~A', checksum = '~A', original name '~A', mimetype '~A'"
                        requested-filename
                        checksum
                        (second (tbnl:post-parameter "file"))
                        mimetype)
           (handler-case
             (progn
               (store-resource (datastore tbnl:*acceptor*)
                               "files"
                               `(("uid" . ,(sanitise-uid requested-filename))
                                 ("title" . ,requested-filename)
                                 ("sha3256sum" . ,checksum)
                                 ("originalname" . ,(second (tbnl:post-parameter "file")))
                                 ("mimetype" . ,mimetype)))
               ;; then if that succeeds move it to its new location.
               ;; Check whether this file already exists by another name
               (log-message :debug "Moving the file to its new home.")
               (log-message :debug (format nil "New home: '~A'" filepath-target))
               (if (probe-file filepath-target)
                 ;; If it does, don't bother overwriting it.
                 (log-message :debug
                              (format nil "File ~A already exists. Not overwriting it with a duplicate."
                                      filepath-target))
                 ;; If it doesn't, move it now.
                 (progn
                   (log-message :debug
                                (format nil "Moving received file '~A' to storage location '~A'"
                                        filepath-temp filepath-target))
                   ;; Ensure the destination path actually exists
                   (log-message :debug (format nil "Ensuring existence of target directory '~A'"
                                               (directory-namestring filepath-target)))
                   (ensure-directories-exist (directory-namestring filepath-target))
                   ;; Now move the file.
                   (log-message :debug
                                (format nil "Actually moving received file '~A' to storage location '~A'"
                                        filepath-temp filepath-target))
                   (rename-file filepath-temp filepath-target)))
               ;; If the location-move fails, we should probably remove the metadata and tell the client.
               ;;
               ;; Now return success to the client
               (setf (tbnl:content-type*) "text/plain")
               (setf (tbnl:return-code*) tbnl:+http-created+)
               (concatenate 'string "/files/" (sanitise-uid requested-filename)))
             ;; Catch errors
             (integrity-error
               (e)
               (let ((error-message (format nil "Integrity error: ~A"
                                            (message e))))
                 (log-message :error error-message)
                 (setf (tbnl:content-type*) "text/plain")
                 (setf (tbnl:return-code*) tbnl:+http-internal-server-error+)
                 error-message))
             (neo4cl::client-error
               (e)
               (let ((error-message (format nil "~A.~A: ~A"
                                            (neo4cl:category e)
                                            (neo4cl:title e)
                                            (neo4cl:message e))))
                 (log-message :error error-message)
                 (setf (tbnl:content-type*) "text/plain")
                 (setf (tbnl:return-code*) tbnl:+http-internal-server-error+)
                 error-message))
             (error
               (e)
               (let ((error-message (format nil "Error: ~A" (neo4cl:message e))))
                 (log-message :error error-message)
                 (setf (tbnl:content-type*) "text/plain")
                 (setf (tbnl:return-code*) tbnl:+http-internal-server-error+)
                 error-message)))))))
    ;; Client requests a file
    ((equal (tbnl:request-method*) :GET)
     (log-message :debug
                  (format nil "Dispatching GET request for URI ~A"
                          (tbnl:request-uri*)))
     (let* ((filename (first (get-uri-parts
                               (get-sub-uri (tbnl:request-uri*)
                                            (uri-base-files tbnl:*acceptor*)))))
            ;; Do it separately because we use it again later in this function.
            ;; Get the search result
            (result (get-resources (datastore tbnl:*acceptor*)
                                   (format nil "/files/~A" filename)
                                   :directional nil
                                   :filters nil)))
       (log-message :debug (format nil "Retrieved resource details ~A" result))
       ;; Return the file to the client if it's present
       (cond
         ((not (null result))
          (let* ((source-path (digest-to-filepath
                                      (make-pathname :defaults (files-location tbnl:*acceptor*))
                                      (cdr (assoc :sha3256sum result)))))
            (log-message :debug (format nil "Returning the file from source-path '~A'"
                                        source-path))
            (tbnl:handle-static-file
              source-path
              ;; Specify the mime-type
              (cdr (assoc :mime-type result)))))
         ;; Fallthrough for not finding the file
         (t
           (log-message :debug "Null result returned")
           (setf (tbnl:content-type*) "text/plain")
           (setf (tbnl:return-code*) tbnl:+http-not-found+)
           "File not found"))))
    ;; Client requests deletion of a file
    ((equal (tbnl:request-method*) :DELETE)
     (log-message :debug "File deletion requested")
     (let* ((filename (first (get-uri-parts
                               (get-sub-uri (tbnl:request-uri*)
                                            (uri-base-files tbnl:*acceptor*))))))
       (log-message :debug (format nil "Client requested deletion of file '~A'" filename))
       ;; Check whether the file is present.
       (let ((result (get-resources (datastore tbnl:*acceptor*)
                                    (format nil "/files/~A" filename)
                                    :directional nil
                                    :filters nil)))
         (log-message :debug (format nil "Got result '~A'" result))
         (if result
           ;; If it is, delete its metadata and then (conditionally) the file itself.
           (progn
             (log-message :debug (format nil "Found file details '~A'" result))
             ;; Delete the metadata
             (delete-resource-by-path
               (datastore tbnl:*acceptor*)
               (concatenate 'string "/files/" filename)
               :recursive nil)
             ;; Now delete the file itself
             (when (null (get-resources (datastore *restagraph-acceptor*)
                                        "/files"
                                        :filters '(("sha3256sum" .
                                                    (cdr (assoc :SHA3256SUM (cdr result)))))))
               (let* ((source-path (digest-to-filepath
                                           (make-pathname :defaults (files-location tbnl:*acceptor*))
                                           (cdr (assoc :sha3256sum result)))))
                 (if (probe-file source-path)
                   (progn
                     (log-message :debug (format nil "Deleting the file at source-path '~A'"
                                                 source-path))
                     (delete-file source-path))
                   (log-message :warn (format nil "No file found at source-path '~A'"
                                              source-path)))))
             ;; Now return a suitable message to the client
             (setf (tbnl:content-type*) "text/plain")
             (setf (tbnl:return-code*) tbnl:+http-no-content+)
             "")
           ;; If not, return 404
           (progn
             (log-message :debug (format nil "Requested file '~A' not found. Returning 404" filename))
             (four-oh-four :file-p t))))))
    ;;
    ;; Methods we don't support.
    ;; Take the whitelist approach
    ((not (member (tbnl:request-method*) '(:POST :GET :DELETE)))
     (method-not-allowed))
    ;; Handle all other cases
    (t
      (log-message :warn "Bad request received with URI: ~A" (tbnl:request-uri*))
      (return-client-error "This wasn't a valid request"))))

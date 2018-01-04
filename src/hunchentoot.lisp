;   Copyright 2017 James Fleming <james@electronic-quill.net>
;
;   Licensed under the Apache License, Version 2.0 (the "License");
;   you may not use this file except in compliance with the License.
;   You may obtain a copy of the License at
;
;       http://www.apache.org/licenses/LICENSE-2.0
;
;   Unless required by applicable law or agreed to in writing, software
;   distributed under the License is distributed on an "AS IS" BASIS,
;   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;   See the License for the specific language governing permissions and
;   limitations under the License.


;;;; The REST API server application

(in-package #:restagraph)

;;; Customised Hunchentoot acceptor.
;;; Carries information about the datastore being used.
(defclass restagraph-acceptor (tbnl:easy-acceptor)
  ;; Class attributes
  ((datastore :initarg :datastore
              :reader datastore
              :initform (error "Datastore object must be supplied.")
              :documentation "An object representing the datastore, on which the generic functions will be dispatched.")
   (url-base :initarg :url-base
             :reader url-base
             :initform "localhost"))
  ;; Class defaults
  (:default-initargs :address "127.0.0.1")
  (:documentation "vhost object, subclassed from tbnl:easy-acceptor"))

;;; We can't directly check whether this acceptor is running,
;;; so we're using the existence of its special variable as a proxy.
(defparameter *restagraph-acceptor*
  (make-instance 'restagraph-acceptor
                 :address (getf *config-vars* :listen-address)
                 :port (getf *config-vars* :listen-port)
                 :url-base (getf *config-vars* ::url-base)
                 ;; Send all logs to STDOUT, and let Docker sort 'em out
                 :access-log-destination (make-synonym-stream 'cl:*standard-output*)
                 :message-log-destination (make-synonym-stream 'cl:*standard-output*)
                 ;; Datastore object - for specialising all the db methods on
                 :datastore (make-instance 'neo4cl:neo4j-rest-server
                                           :hostname (getf *config-vars* :dbhostname)
                                           :dbpasswd (getf *config-vars* :dbpasswd)
                                           :dbuser (getf *config-vars* :dbusername))))

;;; Define a logging method
(defmethod tbnl:acceptor-log-message ((acceptor restagraph-acceptor)
                                      log-level
                                      format-string
                                      &rest format-arguments)
  (log-message log-level (append (list format-string) format-arguments)))

;;; Configure Hunchentoot to extract POST-style parameters
;;; when processing PUT and DELETE requests
(pushnew :PUT tbnl:*methods-for-post-parameters*)
(pushnew :DELETE tbnl:*methods-for-post-parameters*)


;;; Utility functions

(defun get-uri-parts (uri)
  "Break the URI into parts for processing by uri-node-helper"
  (cdr
    (ppcre:split "/"
                 (cl-ppcre:regex-replace (getf *config-vars* :api-uri-base) uri ""))))

(defun uri-node-helper (uri-parts &optional (path "") (marker "n"))
  "Build a Cypher path ending in a node variable, which defaults to 'n'.
  Accepts a list of strings and returns a single string."
  (cond
    ((null uri-parts)
     (format nil "~A(~A)" path marker))
    ((equal (length uri-parts) 1)
     (format nil "~A(~A:~A)" path marker (first uri-parts)))
    ((equal (length uri-parts) 2)
     (format nil "~A(~A:~A { uid: '~A' })"
             path marker (first uri-parts) (second uri-parts)))
    ((equal (length uri-parts) 3)
     (format nil "~A(:~A { uid: '~A' })-[:~A]->(~A)"
             path (first uri-parts) (second uri-parts) (third uri-parts) marker))
    (t
     (uri-node-helper
       (cdddr uri-parts)
       (format nil "~A(:~A { uid: '~A' })-[:~A]->"
               path (first uri-parts) (second uri-parts) (third uri-parts))
       marker))))

(defun uri-rel-helper (uri-parts &optional (path "") (marker "n"))
  "Build a Cypher path ending in a relationship variable, which defaults to 'n'.
  Accepts a list of strings and returns a single string."
  ;; Path-length must be a multiple of 3
  (if (= (mod (length uri-parts) 3) 0)
    ;; Path length is OK.
    ;; Is this the end of the path?
    (if (> (length uri-parts) 3)
      ;; More path to come
      (uri-rel-helper
        (cdddr uri-parts)
        (format nil "~A(:~A {uid: '~A'})-[:~A]->"
                path
                (first uri-parts)
                (second uri-parts)
                (third uri-parts)))
      ;; End of the path.
      ;; Return this along with whatever came before.
      (format nil "~A(:~A {uid: '~A'})-[~A:~A]"
              path
              (first uri-parts)
              (second uri-parts)
              marker
              (third uri-parts)))
    ;; This isn't a path to a relationship
    (error 'client-error :message "Path length must be a multiple of 3.")))

(defun build-cypher-path (uri-parts &optional (path "") (marker "m"))
  "Build a Cypher path from the list of strings supplied.
  Attach a marker variable to the last node in the list, defaulting to 'm'."
  ;; sep == separator
  (let ((sep (if (equal path "") "" "->")))
    (cond
      ((null uri-parts)
       path)
      ((equal (length uri-parts) 1)
       (format nil "~A~A(~A:~A)" path sep marker (first uri-parts)))
      ((equal (length uri-parts) 2)
       (format nil "~A~A(~A:~A { uid: '~A' })"
               path sep marker (first uri-parts) (second uri-parts)))
      ((equal (length uri-parts) 3)
       (format nil "~A~A(~A:~A { uid: '~A' })-[:~A]"
               path sep marker (first uri-parts) (second uri-parts) (third uri-parts)))
      (t
       (build-cypher-path
         (cdddr uri-parts)
         (format nil "~A~A(:~A { uid: '~A' })-[:~A]"
                 path sep (first uri-parts) (second uri-parts) (third uri-parts)))))))


;; Error response functions

(defun four-oh-four ()
  "Fallthrough handler, for anything we haven't already defined."
  (setf (tbnl:content-type*) "text/plain")
  (setf (tbnl:return-code*) tbnl:+http-not-found+)
  "This is not a valid URI")

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
  (log-message :warn (format nil "Client triggered integrity error: ~A" logmessage))
  (setf (tbnl:content-type*) "text/plain")
  (setf (tbnl:return-code*) tbnl:+http-conflict+)
  ;; If we were handed a specific message, use that.
  ;; Otherwise, just pass on the logmessage.
  (if client-message client-message logmessage))

(defun return-database-error (message)
  "There was a database problem. Log it and report something generic to the user, not to keep them in the dark but to reduce the potential for data leakage."
  (log-message :error (format nil "Database error: ~A" message))
  (setf (tbnl:content-type*) "text/plain")
  (setf (tbnl:return-code*) tbnl:+http-internal-server-error+)
  "An error occurred with the database. This has been logged, and will be fixed.")

(defun return-transient-error (message)
  "Transient problem, which may already have self-resolved.. Log it and report something generic to the user, not to keep them in the dark but to reduce the potential for data leakage."
  (log-message :error (format nil "Database error: ~A" message))
  (setf (tbnl:content-type*) "text/plain")
  (setf (tbnl:return-code*) tbnl:+http-service-unavailable+)
  "A transient error occurred, and has been logged for us to work on. Please try your request again.")

(defun return-client-error (logmessage &optional message)
  "The client made a bad request. Return this information to them, that they may learn from their mistakes."
  (log-message :info (format nil "Client error: ~A" logmessage))
  (setf (tbnl:content-type*) "text/plain")
  (setf (tbnl:return-code*) tbnl:+http-bad-request+)
  ;; If we were handed a specific message, use that.
  ;; Otherwise, just pass on the logmessage.
  (if message message logmessage))


;; Functions for dispatching requests

(defun sanitise-uid (uid)
  "Replace UID-unfriendly characters in UIDs with something safe"
  (cl-ppcre:regex-replace-all "[/ ]" uid "_"))

(defun get-sub-uri (uri base-uri)
  "Extract the URI from the full request string,
   excluding the base URL and any GET parameters."
  (first (cl-ppcre:split "\\?" (cl-ppcre:regex-replace base-uri uri ""))))

(defun schema-dispatcher-v1 ()
  "Hunchentoot dispatch function for managing Restagraph's schema."
  (handler-case
    (let* ((uri-parts (get-uri-parts (tbnl:request-uri*))))
      (cond
        ;; Get the description of a single resource-type
        ((and
           (equal (tbnl:request-method*) :GET)
           (tbnl:get-parameter "name"))
         (progn
           (setf (tbnl:content-type*) "application/json")
           (setf (tbnl:return-code*) tbnl:+http-ok+)
           (cl-json:encode-json-alist-to-string
             (describe-resource-type (datastore tbnl:*acceptor*) (tbnl:get-parameter "name")))))
        ;; Get a description of the whole schema
        ((equal (tbnl:request-method*) :GET)
         (progn
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
           (equal (third uri-parts) "resourcetype"))
         (let ((resourcetype (fourth uri-parts)))
           (if (and
                 (not (equal resourcetype ""))
                 (not (equal resourcetype "NIL")))
               ;; Sanity test passed; store it
               (let ((attrs
                       (when (tbnl:post-parameter "attributes")
                         (cl-ppcre:split "," (tbnl:post-parameter "attributes")))))
                 (log-message
                   :debug
                   (format nil "Adding resource type ~A with notes '~A', dependent status '~A' and attributes ~{~A~^ ~}."
                           resourcetype
                           (tbnl:post-parameter "notes")
                           (tbnl:post-parameter "dependent")
                           attrs))
                 (add-resourcetype
                   (datastore tbnl:*acceptor*)
                   resourcetype
                   :attrs attrs
                   :dependent (tbnl:post-parameter "dependent")
                   :notes (tbnl:post-parameter "notes"))
                 ;; Return something useful
                 (setf (tbnl:content-type*) "application/text")
                 (setf (tbnl:return-code*) tbnl:+http-created+)
                 "Created")
               ;; Sanity test failed; report the problem
               (progn
                 (setf (tbnl:content-type*) "application/text")
                 (setf (tbnl:return-code*) tbnl:+http-bad-request+)
                 "At least give me the name of the resourcetype to create"))))
        ;; Delete a resource-type
        ((and
           (equal (tbnl:request-method*) :DELETE)
           (equal (third uri-parts) "resourcetype"))
         (let ((resourcetype (fourth uri-parts)))
           ;; Remove it
           (log-message :debug (format nil "Deleting resource type ~A" resourcetype))
           (delete-resourcetype (datastore tbnl:*acceptor*)
                                resourcetype)
           ;; Return something useful
           (setf (tbnl:content-type*) "application/text")
           (setf (tbnl:return-code*) tbnl:+http-no-content+)
           ""))
        ;; Add a relationship
        ((and
           (equal (tbnl:request-method*) :POST)
           (equal (third uri-parts) "relationship"))
         (let ((source-type (fourth uri-parts))
               (relationship (fifth uri-parts))
               (destination-type (sixth uri-parts)))
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
               ;; Handle cardinality and dependent attributes
               ;; Make it go
               (add-resource-relationship
                 (datastore tbnl:*acceptor*)
                 source-type
                 relationship
                 destination-type
                 :dependent (tbnl:post-parameter "dependent")
                 :cardinality (tbnl:post-parameter "cardinality"))
               ;; Return something useful
               (setf (tbnl:content-type*) "application/text")
               (setf (tbnl:return-code*) tbnl:+http-created+)
               "Created")
             (progn
               (setf (tbnl:return-code*) tbnl:+http-bad-request+)
               (setf (tbnl:content-type*) "application/text")
               "All parameters are required: /<source-type>/<relationship>/<destination-type>"))))
        ;; Delete a relationship
        ((and
           (equal (tbnl:request-method*) :DELETE)
           (equal (third uri-parts) "relationship"))
         (let ((source-type (fourth uri-parts))
               (relationship (fifth uri-parts))
               (destination-type (sixth uri-parts)))
           ;; Store it
           (log-message :debug
                        (format nil "Adding relationship ~A from ~A to ~A"
                                relationship source-type destination-type))
           (add-resource-relationship (datastore tbnl:*acceptor*)
                                      source-type
                                      relationship
                                      destination-type)
           ;; Return something useful
           (setf (tbnl:content-type*) "application/text")
           (setf (tbnl:return-code*) tbnl:+http-no-content+)
           ""))
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
    (neo4cl:database-error (e) (return-database-error e))))

(defun api-dispatcher-v1 ()
  "Hunchentoot dispatch function for the Restagraph API, version 1."
  (handler-case
    (let* ((uri-parts (get-uri-parts (tbnl:request-uri*)))
           (resourcetype (first uri-parts)))
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
         (let* (;; Extract the URI by dropping the base URL.
                ;; Do it separately because we use it again later in this function.
                (sub-uri (get-sub-uri (tbnl:request-uri*) (getf *config-vars* :api-uri-base)))
                ;; Get the search result
                (result (get-resources (datastore tbnl:*acceptor*)
                                       sub-uri
                                       (tbnl:get-parameters*))))
           ;; Handle the null result
           (if (or (null result)
                   (equal result ""))
             (progn
               (setf (tbnl:content-type*) "text/plain")
               (setf (tbnl:return-code*) tbnl:+http-not-found+)
               (format nil "No resources found for ~A" sub-uri))
             ;; It worked; return what we found
             (progn
               (setf (tbnl:content-type*) "application/json")
               (setf (tbnl:return-code*) tbnl:+http-ok+)
               (if (= (mod (length uri-parts) 3) 2)
                 (cl-json:encode-json-alist-to-string result)
                 (cl-json:encode-json-to-string result))))))
        ;; PUT -> Update something already present
        ;;
        ;; Resource
        ((and
           (equal (tbnl:request-method*) :PUT)
           (> (length uri-parts) 0)
           (equal (mod (length uri-parts) 3) 2))
         (handler-case
           (progn
             (log-message
               :debug
               (format nil "Attempting to update attributes of resource ~{/~A~}" uri-parts))
             (update-resource-attributes
               (datastore tbnl:*acceptor*)
               uri-parts
               (tbnl:post-parameters*))
             (setf (tbnl:content-type*) "text/plain")
             (setf (tbnl:return-code*) tbnl:+http-created+)
             ;; Return JSON representation of the newly-updated resource
             (cl-json:encode-json-alist-to-string
               (get-resources (datastore tbnl:*acceptor*) (tbnl:request-uri*))))
           ;; Attempted violation of db integrity
           (restagraph:integrity-error (e) (return-integrity-error (message e)))
           ;; Generic client errors
           (restagraph:client-error (e) (return-client-error (message e)))
           (neo4cl:client-error (e) (return-client-error (neo4cl:message e)))))
        ;; POST -> Store something
        ;;
        ;; Resource
        ((and
           (equal (tbnl:request-method*) :POST)
           (equal (length uri-parts) 1)
           (tbnl:post-parameter "uid"))
         (log-message :debug (format nil "Attempting to dispatch a POST request for resource type ~A" resourcetype))
         ;; Store it
         (handler-case
           (progn
             (store-resource (datastore tbnl:*acceptor*)
                             resourcetype
                             (tbnl:post-parameters*))
             ;; Return it from the database, for confirmation
             (log-message :debug "Stored the new resource. Now retrieving it from the database, to return to the client.")
             (setf (tbnl:content-type*) "application/json")
             (setf (tbnl:return-code*) tbnl:+http-created+)
             (cl-json:encode-json-alist-to-string
               (get-resources (datastore tbnl:*acceptor*)
                              (format nil "/~A/~A"
                                      resourcetype
                                      (tbnl:post-parameter "uid")))))
           ;; Handle integrity errors
           (restagraph:integrity-error (e) (return-integrity-error (message e)))
           ;; Handle general client errors
           (restagraph:client-error (e) (return-client-error (message e)))))
        ;;
        ;; Store a relationship
        ((and
           (equal (tbnl:request-method*) :POST)
           (equal (mod (length uri-parts) 3) 0)
           (tbnl:post-parameter "target"))
         ;; Grab these once, as we'll be referring to them a few times
         (let ((dest-path (tbnl:post-parameter "target")))
           ;; Basic sanity check
           (log-message :debug (format nil
                                       "Creating a relationship from ~A to ~A"
                                       (tbnl:request-uri*) dest-path))
           (handler-case
             (progn
               (create-relationship-by-path
                 (datastore tbnl:*acceptor*) (tbnl:request-uri*) dest-path)
               ;; Report success to the client
               (setf (tbnl:content-type*) "text/plain")
               (setf (tbnl:return-code*) tbnl:+http-created+)
               ;; FIXME: find a good JSON representation of what was just created
               "CREATED")
             ;; Attempted violation of db integrity
             (restagraph:integrity-error (e) (return-integrity-error (message e)))
             ;; Generic client errors
             (restagraph:client-error (e) (return-client-error (message e)))
             (neo4cl:client-error (e) (return-client-error (neo4cl:message e))))))
        ;;
        ;; Create a dependent resource
        ((and
           (equal (tbnl:request-method*) :POST)
           (> (length uri-parts) 0)
           (equal (mod (length uri-parts) 3) 1)
           (tbnl:post-parameter "uid"))
         (handler-case
           (let ((sub-uri (cl-ppcre:regex-replace
                            (getf *config-vars* :api-uri-base) (tbnl:request-uri*) ""))
                 (newtype (car (last uri-parts)))
                 (uid (tbnl:post-parameter "uid")))
             (log-message :debug (format nil "Attempting to create dependent resource ~A:~A on ~A"
                                         newtype uid sub-uri))
             (store-dependent-resource
               (datastore tbnl:*acceptor*) sub-uri (tbnl:post-parameters*))
             (setf (tbnl:content-type*) "text/plain")
             (setf (tbnl:return-code*) tbnl:+http-created+)
             ;; FIXME: find a good JSON representation of what was just created
             "CREATED")
           ;; Attempted violation of db integrity
           (restagraph:integrity-error (e) (return-integrity-error (message e)))
           ;; Generic client errors
           (restagraph:client-error (e) (return-client-error (message e)))
           (neo4cl:client-error (e) (return-client-error (neo4cl:message e)))))
        ;;
        ;; Re-home a dependent resource
        ((and
           (equal (tbnl:request-method*) :POST)
           (> (length uri-parts) 0)
           (equal (mod (length uri-parts) 3) 2)
           (tbnl:post-parameter "target"))
         (handler-case
           (let ((sub-uri (cl-ppcre:regex-replace
                            (getf *config-vars* :api-uri-base) (tbnl:request-uri*) "")))
             (log-message :debug (format nil "Attempting to move dependent resource ~A to ~A"
                                         sub-uri (tbnl:post-parameter "target")))
             (move-dependent-resource
               (datastore tbnl:*acceptor*)
               sub-uri
               (tbnl:post-parameter "target"))
             (setf (tbnl:content-type*) "text/plain")
             (setf (tbnl:return-code*) tbnl:+http-created+)
             ;; FIXME: find a good JSON representation of what was just created
             "CREATED")
           ;; Attempted violation of db integrity
           (restagraph:integrity-error (e) (return-integrity-error (message e)))
           ;; Generic client errors
           (restagraph:client-error (e) (return-client-error (message e)))
           (neo4cl:client-error (e) (return-client-error (neo4cl:message e)))))
        ;;
        ;; DELETE -> Delete something
        ;;
        ;; Resource
        ((and (equal (tbnl:request-method*) :DELETE)
              (equal (mod (length uri-parts) 3) 2))
         (handler-case
           (let ((sub-uri (cl-ppcre:regex-replace
                            (getf *config-vars* :api-uri-base) (tbnl:request-uri*) "")))
             (log-message :debug "Attempting to delete a resource on an arbitrary path")
             (delete-resource-by-path
               (datastore tbnl:*acceptor*)
               sub-uri
               :delete-dependent (tbnl:post-parameter "delete-dependent")
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
              (tbnl:post-parameter "resource")
              (> (length uri-parts) 3)
              (equal (mod (length uri-parts) 3) 0))
         (handler-case
           (progn
             (log-message :debug "Attempting to delete a relationship on an arbitrary path")
             (delete-relationship-by-path (datastore tbnl:*acceptor*)
                                          (tbnl:request-uri*)
                                          (tbnl:post-parameter "resource"))
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
          (return-client-error "This wasn't a valid request"))))
    ;; Handle general errors
    ;;
    ;; Generic client errors
    (neo4cl:client-error (e) (return-client-error (neo4cl:message e)))
    ;; Transient error
    (neo4cl:transient-error (e) (return-transient-error e))
    ;; Database error
    (neo4cl:database-error (e) (return-database-error e))))

(defun startup (&key docker)
  (log-message :info "Starting up the restagraph application server")
  ;; Ensure we have a uniqueness constraint on resource-types
  (handler-case
    (neo4cl:neo4j-transaction
      (datastore *restagraph-acceptor*)
      `((:STATEMENTS
          ((:STATEMENT . "CREATE CONSTRAINT ON (r:rgResource) ASSERT r.name IS UNIQUE")))))
    (neo4cl:database-error (e)
                           (if (equal (neo4cl:title e) "ConstraintCreateFailed")
                             nil   ; This is OK - do nothing
                             (return-database-error
                               (format nil "~A.~A: ~A"
                                       (neo4cl:category e)
                                       (neo4cl:title e)
                                       (neo4cl:message e))))))
  ;; Set the dispatch table
  (setf tbnl:*dispatch-table*
        (list
          (tbnl:create-prefix-dispatcher (getf *config-vars* :api-uri-base) 'api-dispatcher-v1)
          (tbnl:create-prefix-dispatcher (getf *config-vars* :schema-uri-base) 'schema-dispatcher-v1)
          (tbnl:create-prefix-dispatcher "/" 'four-oh-four)))
  ;; Start up the server
  (log-message :info "Starting up Hunchentoot to serve HTTP requests")
  (handler-case
    (tbnl:start *restagraph-acceptor*)
    (usocket:address-in-use-error
      () (log-message :error
                      (format nil "Attempted to start an already-running instance!"))))
  (when docker
    (sb-thread:join-thread
      (find-if
        (lambda (th)
          (string= (sb-thread:thread-name th)
                   (format nil "hunchentoot-listener-localhost:~A"
                           (getf *config-vars* :listen-port))))
        (sb-thread:list-all-threads)))))

(defun dockerstart ()
  (startup :docker t))

(defun save-image (&optional (path "/tmp/restagraph"))
  (sb-ext:save-lisp-and-die path :executable t :toplevel 'restagraph::dockerstart))

(defun shutdown ()
  (log-message :info
               (format nil "Shutting down the restagraph application server"))
  (handler-case
    (tbnl:stop *restagraph-acceptor*)
    ;; Catch the case where it's already shut down
    (tbnl::unbound-slot () (log-message :info "Attempting to shut down Hunchentoot, but it's not running."))))

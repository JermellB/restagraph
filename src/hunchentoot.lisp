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
(defvar *restagraph-acceptor*
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
  (log-message :warn (format nil "A client triggered an integrity error: ~A" logmessage))
  (setf (tbnl:return-code*) tbnl:+http-conflict+)
  (setf (tbnl:content-type*) "text/plain")
  ;; If we were handed a specific message, use that.
  ;; Otherwise, just pass on the logmessage.
  (if client-message client-message logmessage))

(defun return-database-error (message)
  "There was a database problem. Log it and report something generic to the user, not to keep them in the dark but to reduce the potential for data leakage."
  (log-message :error (format nil "Database error: ~A" message))
  (setf (tbnl:return-code*) tbnl:+http-internal-server-error+)
  (setf (tbnl:content-type*) "text/plain")
  "An error occurred with the database. This has been logged, and will be fixed.")

(defun return-transient-error (message)
  "Transient problem, which may already have self-resolved.. Log it and report something generic to the user, not to keep them in the dark but to reduce the potential for data leakage."
  (log-message :error (format nil "Database error: ~A" message))
  (setf (tbnl:return-code*) tbnl:+http-service-unavailable+)
  (setf (tbnl:content-type*) "text/plain")
  "A transient error occurred, and has been logged for us to work on. Please try your request again.")

(defun return-client-error (logmessage &optional message)
  "The client made a bad request. Return this information to them, that they may learn from their mistakes."
  (log-message :info (format nil "Client error: ~A" logmessage))
  (setf (tbnl:return-code*) tbnl:+http-bad-request+)
  (setf (tbnl:content-type*) "text/plain")
  ;; If we were handed a specific message, use that.
  ;; Otherwise, just pass on the logmessage.
  (if message message logmessage))


;; Functions for dispatching requests

(defun api-dispatcher-v1 ()
  "Hunchentoot dispatch function for the Restagraph API, version 1."
  (handler-case
    (let* ((uri-parts (cdr (ppcre:split "/"
                                        (cl-ppcre:regex-replace
                                          (getf *config-vars* :uri-base)
                                          (tbnl:request-uri*) ""))))
           (resourcetype (first uri-parts)))
      (cond
        ;; Methods we don't support.
        ;; Take the whitelist approach
        ((not (member (tbnl:request-method*) '(:POST :GET :PUT :DELETE)))
         (method-not-allowed))
        ;;
        ;; GET -> Retrieve something
        ;;
        ;; Resource
        ((and
           (equal (tbnl:request-method*) :GET)
           (equal (length uri-parts) 2))
         (log-message :debug
                      (format nil "Dispatching GET request for ~A with UID ~A"
                              resourcetype
                              (second uri-parts)))
         (let ((result
                 (get-resource-by-uid (datastore tbnl:*acceptor*)
                                      resourcetype
                                      (second uri-parts))))
           ;; Handle the null result
           (if (equal result "{}")
             (progn
               (setf (tbnl:content-type*) "application/json")
               (setf (tbnl:return-code*) tbnl:+http-not-found+)
               (format nil "No ~A found with UID ~A" resourcetype (second uri-parts)))
             ;; It worked; return what we found
             (progn
               (setf (tbnl:content-type*) "application/json")
               (setf (tbnl:return-code*) tbnl:+http-ok+)
               result))))
        ;; Resources with a given relationship to this one
        ((and (equal (tbnl:request-method*) :GET)
              (equal (length uri-parts) 3))
         (let ((uid (second uri-parts))
               (relationship (third uri-parts)))
           (log-message :debug "Attempting to retrieve resources with relationship ~A to the ~A resource ~A"
                        relationship resourcetype uid)
           ;; Set the headers
           (setf (tbnl:content-type*) "application/json")
           (setf (tbnl:return-code*) tbnl:+http-ok+)
           ;; Make the request and return the result
           (let ((result
                   (get-resources-with-relationship (datastore tbnl:*acceptor*)
                                                    resourcetype
                                                    uid
                                                    relationship)))
             (if result
               ;; If it was actual content, return that
               (progn
                 (setf (tbnl:return-code*) tbnl:+http-ok+)
                 (setf (tbnl:content-type*) "application/json")
                 (log-message :debug (format nil "Returning result '~A'" result))
                 (cl-json:encode-json-to-string result))
               ;; If it was empty, report that instead
               (progn
                 (setf (tbnl:return-code*) tbnl:+http-not-found+)
                 (setf (tbnl:content-type*) "text/plain")
                 (format nil "No ~A found for ~A ~A" relationship resourcetype uid))))))
        ;;
        ;; POST -> Store something
        ;;
        ;; Resource
        ((and
           (equal (tbnl:request-method*) :POST)
           (equal (length uri-parts) 1))
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
             (get-resource-by-uid (datastore tbnl:*acceptor*)
                                  resourcetype
                                  (tbnl:post-parameter "uid")))
           ;; Handle integrity errors
           (restagraph:integrity-error (e) (return-integrity-error (message e)))
           ;; Handle general client errors
           (restagraph:client-error (e) (return-client-error (message e)))))
        ;; Relationship
        ((and
           (equal (tbnl:request-method*) :POST)
           (equal (length uri-parts) 3))
         ;; Grab these once, as we'll be referring to them a few times
         (let ((uid (second uri-parts))
               (relationship (third uri-parts)))
           (log-message :debug (format nil "Attempting to dispatch a POST request for resource type ~A and relationship type ~A"
                                       resourcetype relationship))
           ;; Basic sanity check
           (if (and (tbnl:post-parameter "type")
                    (tbnl:post-parameter "uid"))
             (handler-case
               (progn
                 (create-relationship (datastore tbnl:*acceptor*)
                                      resourcetype
                                      uid
                                      relationship
                                      (tbnl:post-parameter "type")
                                      (tbnl:post-parameter "uid"))
                 ;; Report success to the client
                 (setf (tbnl:return-code*) tbnl:+http-created+)
                 (setf (tbnl:content-type*) "text/plain")
                 ;; FIXME: find a good JSON representation of what was just created
                 "CREATED")
               ;; Attempted violation of db integrity
               (restagraph:integrity-error (e) (return-integrity-error (message e)))
               ;; Generic client errors
               (neo4cl:client-error (e) (return-client-error (neo4cl:message e))))
             ;; Sanity check failed
             (progn
               (log-message :debug "Client failed to supply both the 'type' and 'uid' parameters when creating a relationship")
               (return-client-error "Both the 'type' and 'uid' parameters are required")))))
        ;;
        ;; DELETE -> Delete something
        ;;
        ;; Resource
        ((and
           (equal (tbnl:request-method*) :DELETE)
           (equal (length uri-parts) 1))
         (log-message :debug (format nil "Attempting to dispatch a DELETE request for resource type ~A" resourcetype))
         ;; Basic sanity check
         (if (tbnl:post-parameter "uid")
           ;; Delete the resource, and inform the client we have nothing
           ;; more to say on the subject.
           (progn
             (delete-resource-by-uid (datastore tbnl:*acceptor*)
                                     resourcetype
                                     (tbnl:post-parameter "uid"))
             (setf (tbnl:content-type*) "text/plain")
             (setf (tbnl:return-code*) tbnl:+http-no-content+)
             "")
           ;; Sanity check failed. Inform the client they're insane.
           (return-client-error "UID is a required parameter")))
        ;; Relationship
        ((and (equal (tbnl:request-method*) :DELETE)
              (equal (length uri-parts) 3))
         (log-message :debug "Attempting to delete a relationship")
         ;; Basic sanity-check
         (if (and
               (tbnl:post-parameter "type")
               (tbnl:post-parameter "uid"))
           ;; Attempt to delete it
           (progn
             (delete-relationship (datastore tbnl:*acceptor*)
                                  (first uri-parts)
                                  (second uri-parts)
                                  (third uri-parts)
                                  (tbnl:post-parameter "type")
                                  (tbnl:post-parameter "uid"))
             (setf (tbnl:return-code*) tbnl:+http-no-content+)
             (setf (tbnl:content-type*) "text/plain")
             "")
           ;; Sanity-check failed; relay the sad news to the client
           (return-client-error "Both 'type' and 'uid' parameters are required")))
        ;; Fallback: anything we don't already know how to handle
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

(defun startup ()
  (log-message :info "Starting up the restagraph application server")
  (setf tbnl:*dispatch-table*
        (list
          (tbnl:create-prefix-dispatcher (getf *config-vars* :uri-base) 'api-dispatcher-v1)
          (tbnl:create-prefix-dispatcher "/" 'four-oh-four)))
  ;; Start up the server
  (log-message :info "Starting up Hunchentoot to serve HTTP requests")
  (handler-case
    (tbnl:start *restagraph-acceptor*)
    (usocket:address-in-use-error
      () (log-message :error
                      (format nil "Attempted to start an already-running instance!")))))

(defun shutdown ()
  (log-message :info
               (format nil "Shutting down the restagraph application server"))
  (handler-case
    (tbnl:stop *restagraph-acceptor*)
    ;; Catch the case where it's already shut down
    (tbnl::unbound-slot () (log-message :info "Attempting to shut down Hunchentoot, but it's not running."))))

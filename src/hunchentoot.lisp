;;;; The REST API server application

(in-package #:restagraph)

(defparameter *uri-base* "/api/v1/")

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
(push :PUT tbnl:*methods-for-post-parameters*)
(push :DELETE tbnl:*methods-for-post-parameters*)

;; Functions for dispatching requests

(defun four-oh-four ()
  "Fallthrough handler, for anything we haven't already defined."
  (setf (tbnl:content-type*) "text/plain")
  (setf (tbnl:return-code*) tbnl:+http-bad-request+)
  "This is not a valid URI")

(defun method-not-implemented ()
  "Default response for a client making a request we don't support"
  (setf (tbnl:content-type*) "text/plain")
  (setf (tbnl:return-code*) tbnl:+http-method-not-allowed+)
  "Method not supported")

(defun resource-dispatcher ()
  "Hunchentoot dispatch function to handle an API request relating directly to a resource."
  (log-message :debug "Attempting to dispatch a resource request")
  (let* ((uri-parts (ppcre:split "/" (cl-ppcre:regex-replace *uri-base* (tbnl:request-uri*) "")))
         (resource-type (first uri-parts)))
    (unless resource-type
      (error (format nil "No resource-type identified from URI '~A'" (tbnl:request-uri*))))
    (log-message :debug (format nil "Dispatching a ~A request for resource-type '~A'"
                                (tbnl:request-method*) resource-type))
    (cond
      ;; POST -> Store a resource
      ((equal (tbnl:request-method*) :POST)
       (setf (tbnl:content-type*) "text/plain")
       (handler-case
         (multiple-value-bind (results code message)
           (store-resource (datastore tbnl:*acceptor*) resource-type (tbnl:post-parameters*))
           (declare (ignore results)
                    (ignore message))
           (if (equal code 200)
             (progn
               (setf (tbnl:return-code*) tbnl:+http-created+)
               "201 CREATED")
             (progn
               (setf (tbnl:return-code*) tbnl:+http-internal-server-error+)
               (format nil "~A Well, that didn't work." code))))
         ;; Client error
         (restagraph:integrity-error (e)
                                     (return-integrity-error (message e)))))
      ;; GET -> Retrieve the resource's details
      ((equal (tbnl:request-method*) :GET)
       (let ((uid (second uri-parts)))
         ;; We have a UID; carry on
         (if uid
           (let ((result (get-resource-by-uid (datastore tbnl:*acceptor*) resource-type uid)))
             ;; If nothing was returned, that's a 404
             (if (equal result "{}")
               (progn
                 (setf (tbnl:content-type*) "text/plain")
                 (setf (tbnl:return-code*) tbnl:+http-not-found+)
                 (format nil "No ~A found with a UID of ~A." resource-type uid))
               ;; If we got this far, assume it worked and return whatever we received
               (progn
                 (setf (tbnl:return-code*) tbnl:+http-ok+)
                 (setf (tbnl:content-type*) "application/json")
                 result)))
           ;; No UID, no service
           (progn
             (setf (tbnl:content-type*) "text/plain")
             (setf (tbnl:return-code*) tbnl:+http-bad-request+)
             "The UID portion of the URI is required"))))
      ;; DELETE -> Delete the resource from the db
      ((equal (tbnl:request-method*) :DELETE)
       ;; It'll be this type either way; just set it once.
       (setf (tbnl:content-type*) "text/plain")
       ;; If we don't have the UID, this won't work so well
       (let ((uid (tbnl:post-parameter "uid")))
         (if uid
           (multiple-value-bind (results code message)
             (delete-resource-by-uid (datastore tbnl:*acceptor*) resource-type uid)
             (declare (ignore results))
             (setf (tbnl:return-code*) code)
             message)
           (progn
             (setf (tbnl:return-code*) tbnl:+http-bad-request+)
             (setf (tbnl:content-type*) "text/plain")
             "UID is required"))))
      ;; Fallback: anything we don't already know how to handle
      (t
       (method-not-implemented)))))

(defun return-integrity-error (message)
  "Report to the client that their request would have violated an integrity constraint"
  (setf (tbnl:return-code*) tbnl:+http-conflict+)
  (setf (tbnl:content-type*) "text/plain")
  message)

(defun relationship-dispatcher ()
  "Hunchentoot dispatch function to handle an API request to create, delete or inspect relationships between resources."
  (log-message :debug "Attempting to dispatch a relationship request")
  (let* ((uri-parts (ppcre:split "/" (cl-ppcre:regex-replace *uri-base* (tbnl:request-uri*) "")))
         (resource-type (first uri-parts))
         (uid (second uri-parts))
         (relationship (third uri-parts)))
    (log-message :debug (format nil "Dispatching a request for a ~A relationship on resource type ~A."
                                relationship resource-type))
    (cond
      ;;; POST -> Create a relationship
      ((equal (tbnl:request-method*) :POST)
       ;; Sanity-check of parameters
       (progn
         (unless (and
                   (tbnl:post-parameter "to-type")
                   (tbnl:post-parameter "to-uid"))
           (progn
             (setf (tbnl:return-code*) tbnl:+http-bad-request+)
             (setf (tbnl:content-type*) "text/plain")
             "All parameters are required: to-type and to-uid"))
         ;; Attempt to create it
         (log-message :debug "Attempting to create a ~A relationship from ~A ~A to ~A ~A"
                      relationship
                      resource-type
                      uid
                      (tbnl:post-parameter "to-type")
                      (tbnl:post-parameter "to-uid"))
         (handler-case
           (multiple-value-bind (result code message)
             (create-relationship (datastore tbnl:*acceptor*)
                                  resource-type
                                  uid
                                  relationship
                                  (tbnl:post-parameter "to-type")
                                  (tbnl:post-parameter "to-uid"))
             (declare (ignore result)
                      (ignore message))
             ;; Handle the various outcomes
             (if (equal code 200)
               ;; It worked!
               (progn
                 (setf (tbnl:return-code*) tbnl:+http-created+)
                 (setf (tbnl:content-type*) "text/plain")
                 "201 CREATED")
               ;; It didn't work
               (progn
                 (setf (tbnl:return-code*) tbnl:+http-bad-request+)
                 (setf (tbnl:content-type*) "text/plain")
                 "Nope. Try again.")))
           (restagraph:integrity-error (e)
           (return-integrity-error (message e))))))
      ;;; GET -> Retrieve a summary of resources with a given relationship to this one
      ((equal (tbnl:request-method*) :GET)
       (log-message :debug "Attempting to dispatch a GET request")
       (log-message :debug (format nil "Retrieving ~A relationships from ~A ~A"
                                   relationship resource-type uid))
       (let ((result
               (get-resources-with-relationship
                 (datastore tbnl:*acceptor*)
                 resource-type
                 uid
                 relationship)))
         ;; Check what came back
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
             (format nil "No ~A found for ~A ~A" relationship resource-type uid)))))
      ;;; DELETE -> Remove a relationship
      ((equal (tbnl:request-method*) :DELETE)
       ;; Sanity-check of parameters
       (progn
         (unless (and
                   (tbnl:post-parameter "to-type")
                   (tbnl:post-parameter "to-uid"))
           (error "All parameters are required: to-type and to-uid"))
         ;; Attempt to create it
         (multiple-value-bind (result code message)
           (delete-relationship (datastore tbnl:*acceptor*)
                                resource-type
                                uid
                                relationship
                                (tbnl:post-parameter "to-type")
                                (tbnl:post-parameter "to-uid"))
           (declare (ignore result)
                    (ignore message))
           ;; Handle the various outcomes
           (if (equal code 200)
             ;; It worked!
             (progn
               (setf (tbnl:return-code*) tbnl:+http-ok+)
               (setf (tbnl:content-type*) "text/plain")
               "201 CREATED")
             ;; It didn't work
             (progn
               (setf (tbnl:return-code*) tbnl:+http-bad-request+)
               (setf (tbnl:content-type*) "text/plain")
               "Nope. Try again.")))))
      (t
        (method-not-implemented)))))

(defmethod generate-dispatch-table ((schema hash-table))
  (let ((dispatch-table (list
                          ;; Fallback.
                          ;; This must be last, because they're inspected in order,
                          ;; and the first match wins.
                          (tbnl:create-prefix-dispatcher "/" 'four-oh-four))))
    (maphash
      ;; Outer loop of resources
      #'(lambda (resource details)
          ;; Add the less-specific handler for the resource itself.
          ;; Predefine the URI for consistency when we use it more than once.
          (let ((uri (format nil "~A~A" *uri-base* resource)))
            (log-message :debug (format nil "Installing a resource handler for '~A' at '~A'."
                                        resource uri))
            (pushnew (tbnl:create-regex-dispatcher uri 'resource-dispatcher)
                     dispatch-table))
          ;; Add a catch-all for attempts to create an invalid relationship
          (let ((uri (format nil "~A~A/.+/.+" *uri-base* resource)))
            (pushnew (tbnl:create-regex-dispatcher
                       uri #'(lambda ()
                               (return-integrity-error
                                 (format nil "This relationship is not valid for ~A" resource))))
                     dispatch-table))
          ;; Inner loop of relationships on resources.
          ;; Add these last, to make them the more-specific match.
          (maphash #'(lambda (relationship targets)
                       (declare (ignore targets))
                       ;; Predefine the URI once for consistency
                       (let ((uri (format nil "~A~A/.+/~A" *uri-base* resource relationship)))
                         (log-message :debug (format nil "Installing a relationship handler for '~A/~A' at '~A'"
                                                     resource relationship uri))
                         ;; Actually add it to the dispatch table
                         (pushnew (tbnl:create-regex-dispatcher uri 'relationship-dispatcher)
                                  dispatch-table)))
                   (gethash "relationships" details)))
      schema)
    ;; Now return the dispatch-table we created
    dispatch-table))

(defun update-dispatch-table ()
  "Update the dispatch table while Hunchentoot is running. Convenience function for schema development."
  (setf tbnl:*dispatch-table* (generate-dispatch-table (getf *config-vars* :schema))))

(defun startup ()
  (log-message :info "Starting up the restagraph application server")
  ;; Populate the schema from the database
  (log-message :info "Generating the schema from the database contents")
  (setf (getf *config-vars* :schema)
        (populate-schema (datastore *restagraph-acceptor*)))
  ;; Create the schema within the database
  (create-db-schema (datastore *restagraph-acceptor*) (getf *config-vars* :schema))
  ;; Configure Hunchentoot's dispatch table
  (log-message :info "Generating the REST API from the schema")
  (update-dispatch-table)
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

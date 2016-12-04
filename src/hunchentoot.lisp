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
                 :datastore (getf *config-vars* :datastore)))

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
  (setf (tbnl:return-code*) tbnl:+http-not-found+)
  "This is not a valid URI")

(defun method-not-implemented ()
  "Default response for a client making a request we don't support"
  (setf (tbnl:return-code*) tbnl:+http-method-not-allowed+)
  "Method not supported")

(defun resource-dispatcher ()
  "Hunchentoot dispatch function to handle an API request relating directly to a resource."
  (log-message :debug "Attempting to dispatch a resource request")
  (let* ((uri-parts (ppcre:split "/" (tbnl:request-uri*)))
         (resource-type (fourth uri-parts)))
    (unless resource-type
      (error (format nil "No resource-type identified from URI '~A'" (tbnl:request-uri*))))
    (log-message :debug (format nil "Dispatching a ~A request for resource-type '~A'"
                                (tbnl:request-method*) resource-type))
    (cond
      ;; POST -> Store a resource
      ((equal (tbnl:request-method*) :POST)
       (setf (tbnl:content-type*) "text/plain")
       (handler-bind
         ((error
            #'(lambda (e)
                (declare (ignore e))
                (setf (tbnl:return-code*) tbnl:+http-bad-request+)
                "Well, that didn't work.")))
         (multiple-value-bind (results code message)
           (store-class-instance (datastore tbnl:*acceptor*) resource-type (tbnl:post-parameters*))
           (declare (ignore results)
                    (ignore message))
           (if (equal code 200)
             (progn
               (setf (tbnl:return-code*) tbnl:+http-created+)
               "201 CREATED")
             (progn
               (setf (tbnl:return-code*) tbnl:+http-internal-server-error+)
               (format nil "~A Well, that didn't work." code))))))
      ;; Fallback: anything we don't already know how to handle
      (t
       (method-not-implemented)))))

(defun startup ()
  (log-message :info "Starting up the restagraph application server")
  ;; Populate the schema from the database
  (log-message :info "Generating the schema from the database contents")
  (setf (getf *config-vars* :schema)
        (populate-schema (datastore *restagraph-acceptor*)))
  ;; Configure Hunchentoot's dispatch table
  (log-message :info "Generating the REST API from the schema")
  (setf tbnl:*dispatch-table*
        (list
          ;; Fallback.
          ;; This must be last, because they're inspected in order,
          ;; and the first match wins.
          (tbnl:create-prefix-dispatcher "/" 'four-oh-four)))
  (maphash #'(lambda (resource details)
               (declare (ignore details))
               (let ((uri (format nil "~A~A" *uri-base* resource)))
                 (log-message :debug (format nil "Installing a resource handler for '~A' at '~A'."
                                             resource uri))
                 (pushnew (tbnl:create-regex-dispatcher uri 'resource-dispatcher)
                          tbnl:*dispatch-table*)))
           (getf *config-vars* :schema))
  (log-message :info "Starting up Hunchentoot to serve HTTP requests")
  (handler-case
    (tbnl:start *restagraph-acceptor*)
    (usocket:address-in-use-error
      () (log-message :error
                      (format nil "Attempted to start an already-running instance!")))))

(defun shutdown ()
  (log-message :info
               (format nil "Shutting down the restagraph application server"))
  (tbnl:stop *restagraph-acceptor*))

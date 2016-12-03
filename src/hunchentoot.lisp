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

;;; Define a default dispatch-table
(setf tbnl:*dispatch-table*
      (list
        ;(tbnl:create-prefix-dispatcher "/v1/ipv4-addresses/" 'ipv4-address)
        ;;
        ;; Fallback.
        ;; This must be last, because they're inspected in order,
        ;; and the first match wins.
        (tbnl:create-prefix-dispatcher "/" 'four-oh-four)))

(defun startup ()
  (log-message :info "Starting up the restagraph application server")
  (log-message :info "Generating the schema from the database contents")
  (setf (getf *config-vars* :schema)
        (populate-schema (datastore *restagraph-acceptor*)))
  (log-message :info "Generating the REST API from the schema")
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

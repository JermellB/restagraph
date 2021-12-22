;   Copyright 2020 James Fleming <james@electronic-quill.net>
;
;   Licensed under the GNU General Public License
;   - for details, see LICENSE.txt in the top-level directory


;;; Classes that need to be predefined because of the way SBCL's compiler works

(in-package #:restagraph)

(declaim (optimize (compilation-speed 0)
                   (speed 2)
                   (safety 3)
                   (debug 3)))


(defclass restagraph-acceptor (tbnl:easy-acceptor)
  ;; Class attributes
  ((datastore :initarg :datastore
              :reader datastore
              :initform (error "Datastore object must be supplied.")
              :documentation "An object representing the datastore, on which the generic functions will be dispatched.")
   (uri-base-api :initarg :uri-base-api
                 :reader uri-base-api
                 :initform "/raw/v1"
                 :documentation "Base URI on which the raw API is to be presented.")
   (uri-base-schema :initarg :uri-base-schema
                    :reader uri-base-schema
                    :initform "/schema/v1"
                    :documentation "Base URI on which the schema API is to be presented.")
   (uri-base-files :initarg :uri-base-files
                   :reader uri-base-files
                   :initform "/files/v1"
                   :documentation "Base URI on which the files API is to be presented.")
   (uri-base-subnets :initarg :uri-base-subnets
                     :reader uri-base-subnets
                     :initform "/ipam/v1/subnets")
   (uri-base-addresses :initarg :uri-base-addresses
                       :reader uri-base-addresses
                       :initform "/ipam/v1/addresses")
   (files-location :initarg :files-location
                   :reader files-location
                   :initform (error "files-location is required")
                   :documentation "Parent directory under which file objects are to be stored.")
   (template-path :initarg :template-path
                  :reader template-path
                  :initform "/tmp")
   (schema :initarg :schema
           :accessor schema
           :initform (make-schema-hash-table)
           :documentation "What to use for schema actions. Valid options are a hash-table or the datastore object, as methods are specialised on both.")
   (access-policy :initarg :access-policy
                  :reader access-policy
                  :initform (error "Required argument")
                  :documentation "The access policy to be applied to each incoming request."))
  ;; Class defaults for initalising the superclass
  (:default-initargs :address "127.0.0.1")
  ;; Class documentation
  (:documentation "Customised Hunchentoot acceptor, subclassed from tbnl:easy-acceptor. Carries additional configuration data for the site."))


;; Helper function
(defun make-default-acceptor ()
  (make-instance 'restagraph-acceptor
                 :address (or (sb-ext:posix-getenv "LISTEN_ADDR")
                              (getf *config-vars* :listen-address))
                 :port (or (when (sb-ext:posix-getenv "LISTEN_PORT")
                             (parse-integer (sb-ext:posix-getenv "LISTEN_PORT")))
                           (getf *config-vars* :listen-port))
                 :uri-base-api (or (sb-ext:posix-getenv "API_URI_BASE")
                                   (getf *config-vars* :api-uri-base))
                 :uri-base-schema (or (sb-ext:posix-getenv "SCHEMA_URI_BASE")
                                      (getf *config-vars* :schema-uri-base))
                 :uri-base-files (or (sb-ext:posix-getenv "FILES_URI_BASE")
                                     (getf *config-vars* :files-uri-base))
                 :files-location (or (sb-ext:posix-getenv "FILES_LOCATION")
                                     (getf *config-vars* :files-location))
                 :template-path (make-pathname :defaults
                                               (or (sb-ext:posix-getenv "TEMPLATE_PATH")
                                                   (getf *config-vars* :template-path)))
                 ;; Send all logs to STDOUT, and let Docker sort 'em out
                 :access-log-destination (make-synonym-stream 'cl:*standard-output*)
                 :message-log-destination (make-synonym-stream 'cl:*standard-output*)
                 ;; Datastore object - for specialising all the db methods on
                 :datastore (make-instance
                              'neo4cl:neo4j-rest-server
                              :hostname (or (sb-ext:posix-getenv "NEO4J_HOSTNAME")
                                            (getf *config-vars* :dbhostname))
                              :port (or (when (sb-ext:posix-getenv "NEO4J_PORT")
                                          (parse-integer (sb-ext:posix-getenv "NEO4J_PORT")))
                                        (getf *config-vars* :dbport))
                              :dbname (or (sb-ext:posix-getenv "NEO4J_DBNAME")
                                          (getf *config-vars* :dbname))
                              :dbuser (or (sb-ext:posix-getenv "NEO4J_USER")
                                          (getf *config-vars* :dbusername))
                              :dbpasswd (or (sb-ext:posix-getenv "NEO4J_PASSWORD")
                                            (getf *config-vars* :dbpasswd)))
                 :access-policy (define-policy (or (sb-ext:posix-getenv "ACCESS_POLICY")
                                                   "open"))))

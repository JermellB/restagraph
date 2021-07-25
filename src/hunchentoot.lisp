;   Copyright 2017-21 James Fleming <james@electronic-quill.net>
;
;   Licensed under the GNU General Public License
;   - for details, see LICENSE.txt in the top-level directory


;;;; The HTTP API server application

(in-package #:restagraph)

(declaim (optimize (compilation-speed 0)
                   (speed 2)
                   (safety 3)
                   (debug 3)))


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


;;; Appserver startup/shutdown

(defun startup (&key acceptor dispatchers docker)
  "Start up the appserver.
  Ensures the uniqueness constraint on resource-types is present in Neo4j.
  Keyword arguments:
  - acceptor = prebuilt acceptor, to use instead of the default.
  - dispatchers = extra dispatchers to add to tbnl:*dispatch-table* in addition to the defaults.
  - docker = whether to start up in a manner suitable to running under docker,
  i.e. return only after Hunchentoot shuts down, instead of immediately after it starts up."
  (declare (type (boolean) docker))
  ;; Set debug logging, if requested
  (when (sb-ext:posix-getenv "DEBUG")
    (setf *loglevel* :debug))
  ;; Diagnostics
  (log-message :debug (format nil "Shell search path: ~A" (sb-ext:posix-getenv "PATH")))
  (log-message :info "Attempting to start up the restagraph application server")
  ;; Control the decoding of JSON identifiers
  (setf JSON:*JSON-IDENTIFIER-NAME-TO-LISP* 'common-lisp:string-upcase)
  ;; Sanity-check: do we have a storage directory?
  (let ((files-location (or (sb-ext:posix-getenv "FILES_LOCATION")
                            (getf *config-vars* :files-location))))
    (log-message :info (format nil "Ensuring file-storage location '~A' is present" files-location))
    (ensure-directories-exist files-location)
    (unless (probe-file files-location)
      (error (format nil "File storage location ~A does not exist!" files-location))))
  ;; Sanity-check: is an acceptor already running?
  ;;; We can't directly check whether this acceptor is running,
  ;;; so we're using the existence of its special variable as a proxy.
  (if *restagraph-acceptor*
    ;; There's an acceptor already in play; bail out.
    (log-message :warn "Acceptor already exists; refusing to create a new one.")
    ;; No existing acceptor; we're good to go.
    ;;
    ;; Figure out whether we have a schema directory to work with
    (progn
      ;; Ensure we have an acceptor to work with
      (when (null acceptor) (setf acceptor (make-default-acceptor)))
      ;; Make it available as a dynamic variable, for shutdown to work on
      (defparameter *restagraph-acceptor* acceptor)
      ;; Sanity-check whether the database is available
      (unless (confirm-db-is-running (datastore acceptor) :max-count 25)
        (error "Database is not available"))
      ;; Update the schema, if one has been specified
      (ensure-current-schema (datastore acceptor) *core-schema*)
      (setf (schema acceptor) (fetch-current-schema (datastore acceptor)))
      ;; Methods
      ;; Set the dispatch table
      (log-message :info "Configuring the dispatch table")
      (setf tbnl:*dispatch-table*
            (append
              ;; Restagraph defaults
              (list (tbnl:create-prefix-dispatcher (uri-base-api acceptor) 'api-dispatcher-v1)
                    (tbnl:create-prefix-dispatcher (uri-base-schema acceptor) 'schema-dispatcher-v1)
                    (tbnl:create-prefix-dispatcher (uri-base-files acceptor) 'files-dispatcher-v1)
                    (tbnl:create-prefix-dispatcher (uri-base-subnets acceptor) 'subnet-dispatcher-v1)
                    (tbnl:create-prefix-dispatcher (uri-base-addresses acceptor) 'address-dispatcher-v1))
              ;; Include the additional dispatchers here
              dispatchers
              ;; Default fallback
              (list (tbnl:create-prefix-dispatcher "/" 'four-oh-four))))
      ;; Prepare for file upload
      (log-message :info "Ensuring the file-upload temp directory is present")
      (ensure-directories-exist
        (if (sb-ext:posix-getenv "FILES_TEMP_LOCATION")
          (sb-ext:posix-getenv "FILES_TEMP_LOCATION")
          (getf *config-vars* :files-temp-location)))
      (setf tbnl:*tmp-directory* (getf *config-vars* :files-temp-location))
      ;; Start up the server
      (log-message :info "Starting up Hunchentoot to serve HTTP requests")
      (handler-case
        (tbnl:start acceptor)
        (usocket:address-in-use-error
          () (log-message :error
                          (format nil "Attempted to start an already-running instance!"))))
      (when docker
        (sb-thread:join-thread
          (find-if
            (lambda (th)
              (string= (sb-thread:thread-name th)
                       (format nil "hunchentoot-listener-~A:~A"
                               (tbnl:acceptor-address acceptor)
                               (tbnl:acceptor-port acceptor))))
            (sb-thread:list-all-threads)))))))

(defun dockerstart ()
  (startup :docker t))

(defun shutdown ()
  ;; Check whether there's something to shut down
  (if *restagraph-acceptor*
      ;; There is; go ahead
      (progn
      ;; Check whether it's still present but shutdown
      (if (tbnl::acceptor-shutdown-p *restagraph-acceptor*)
          (log-message :info "Acceptor was present but already shut down.")
          (progn
            (log-message
              :info
              (format nil "Shutting down the restagraph application server"))
            (handler-case
              ;; Perform a soft shutdown: finish serving any requests in flight
              (tbnl:stop *restagraph-acceptor* :soft t)
              ;; Catch the case where it's already shut down
              (tbnl::unbound-slot
                ()
                (log-message
                  :info
                  "Attempting to shut down Hunchentoot, but it's not running."))
              (sb-pcl::no-applicable-method-error
                ()
                (log-message
                  :info
                  "Attempted to shut down Hunchentoot, but received an error. Assuming it wasn't running.")))))
        ;; Nuke the acceptor
        (setf *restagraph-acceptor* nil))
      ;; No acceptor. Note the fact and do nothing.
      (log-message :warn "No acceptor present; nothing to shut down.")))

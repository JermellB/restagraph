;   Copyright 2020-2021 James Fleming <james@electronic-quill.net>
;
;   Licensed under the GNU General Public License
;   - for details, see LICENSE.txt in the top-level directory


(in-package #:restagraph)
(declaim (optimize (compilation-speed 0)
                   (speed 2)
                   (safety 3)
                   (debug 3)))

(defun any-readonly-attrs (resourcetype params)
  "Detect whether the supplied parameters include any read-only ones for this resourcetype."
  (declare (type (or null schema-rtypes) resourcetype))
  (when resourcetype
    (progn
      (log-message :debug
                   (format nil "Checking for read-only attributes in resourcetype '~A'"
                           (name resourcetype)))
      (intersection (map 'list
                         'restagraph::name
                         (remove-if-not 'restagraph::readonly (attributes resourcetype)))
                    (mapcar 'car params)
                    :test 'equal))))

(defun api-dispatcher-v1 ()
  "Hunchentoot dispatch function for the Restagraph API, version 1."
  (handler-case
    (let* ((sub-uri (get-sub-uri (tbnl:request-uri*) (uri-base-api tbnl:*acceptor*)))
           (uri-parts (get-uri-parts sub-uri)))
      (log-message :debug (format nil "Handling API ~A request ~{/~A~} " (tbnl:request-method*) uri-parts))
      (cond
        ;; Access policies: dispatch "deny" as early as possible
        ((and (equal :GET (tbnl:request-method*))
              (equal :DENY (get-policy (access-policy *restagraph-acceptor*))))
         (forbidden))
        ((and (equal :POST (tbnl:request-method*))
              (equal :DENY (post-policy (access-policy *restagraph-acceptor*))))
         (forbidden))
        ((and (equal :PUT (tbnl:request-method*))
              (equal :DENY (put-policy (access-policy *restagraph-acceptor*))))
         (forbidden))
        ((and (equal :DELETE (tbnl:request-method*))
              (equal :DENY (delete-policy (access-policy *restagraph-acceptor*))))
         (forbidden))
        ;;
        ;; Intercept and reject attempts to interact with the "any" resource-type
        ((equal (third uri-parts) "any")
         (progn
           (setf (tbnl:content-type*) "text/plain")
           (setf (tbnl:return-code*) tbnl:+http-not-found+)
           (format nil "No resources found for ~A" uri-parts)))
        ;;
        ;; Mistaken attempt to create a relationship from a primary resource,
        ;; forgetting to include the relationship in the URL
        ((and (equal :POST (tbnl:request-method*))
              (equal 2 (length uri-parts)))
         (log-message :debug "Client attempted to create a relationship from a primary resource, without specifying the relationship.")
         (setf (tbnl:content-type*) "text/plain")
         (setf (tbnl:return-code*) tbnl:+http-bad-request+)
         "Request not valid. Did you forget to specify the relationship?")
        ;;
        ;; GET -> Retrieve something
        ;;
        ;; Single resource was requested
        ;; ...or a dangling relationship, which may or may not go the way we expect
        ((and (equal (tbnl:request-method*) :GET)
              (member (mod (length uri-parts) 3)
                      '(2 0)))
         (log-message :debug (format nil "Dispatching GET request for URI ~A" (tbnl:request-uri*)))
         (let ((top-rtype-def (gethash (first uri-parts) (schema tbnl:*acceptor*))))
           ;; Prevent clients directly requesting dependent resources
           (if (and top-rtype-def
                    (< (length uri-parts) 4)
                    (dependent top-rtype-def))
             ;; They're trying to directly fetch a dependent resource
             ;; as though it were a primary one.
             (progn
               (log-message :debug "Refusing to fetch a dependent resource as though it were a primary one.")
               (setf (tbnl:content-type*) "text/plain")
               (setf (tbnl:return-code*) tbnl:+http-bad-request+)
               "Error: dependent resources must be fetched in context.")
             ;; It passed that test.
             ;; Try to fetch the resource.
             (let* ((session (neo4cl:establish-bolt-session (datastore tbnl:*acceptor*)))
                    (result (get-resources session sub-uri)))
               (neo4cl:disconnect session)
               (log-message :debug (format nil "Fetched content ~A" result))
               (if result
                 (progn
                   (setf (tbnl:content-type*) "application/json")
                   (setf (tbnl:return-code*) tbnl:+http-ok+)
                   (cl-json:encode-json-to-string result))
                 (progn
                   (setf (tbnl:content-type*) "text/plain")
                   (setf (tbnl:return-code*) tbnl:+http-not-found+)
                   "Resource not found."))))))
        ;;
        ;; Class of resources was requested
        ((and (equal (tbnl:request-method*) :GET)
              ;; There's only one remaining option after the previous clause.
              ;; I'm leaving this in place but commented-out, in case I find some reason
              ;; to re-order them.
              ;(= (mod (length uri-parts) 3) 1)
              )
         (log-message :debug (format nil "Dispatching GET request for URI ~A" (tbnl:request-uri*)))
         (let ((top-rtype-def (gethash (first uri-parts) (schema tbnl:*acceptor*))))
           ;; Prevent clients directly requesting dependent resources
           (if (and top-rtype-def
                    (dependent top-rtype-def)
                    (= (length uri-parts) 1))
             ;; They're trying to directly fetch resources of a dependent type,
             ;; as though it were a primary one.
             (progn
               (log-message :debug "Refusing to fetch a dependent resource as though it were a primary one.")
               (setf (tbnl:content-type*) "text/plain")
               (setf (tbnl:return-code*) tbnl:+http-bad-request+)
               "Error: dependent resources must be fetched in context.")
             ;; It passed that test; try to fetch the requested resources.
             (let* ((session (neo4cl:establish-bolt-session (datastore tbnl:*acceptor*)))
                    (result (get-resources session
                                           sub-uri
                                           :filters (process-filters
                                                      (tbnl:get-parameters*)
                                                      (schema tbnl:*acceptor*)
                                                      (car (last uri-parts))))))
               (neo4cl:disconnect session)
               (log-message :debug (format nil "Fetched content ~A" result))
               (setf (tbnl:content-type*) "application/json")
               (setf (tbnl:return-code*) tbnl:+http-ok+)
               (if result
                 (cl-json:encode-json-to-string result)
                 "[]")))))
        ;; POST -> Store something
        ;;
        ;; Primary resource
        ((and
           (equal (tbnl:request-method*) :POST)
           (equal (length uri-parts) 1)
           (tbnl:post-parameter "uid"))
         (let ((resourcetype (first uri-parts))
               (uid (tbnl:post-parameter "uid"))
               ;; It's just simpler to establish this now, so we can use it in cond test-clauses.
               (session (neo4cl:establish-bolt-session (datastore tbnl:*acceptor*))))
           (log-message
             :debug
             (format nil "Attempting to dispatch a POST request for resource type ~A" resourcetype))
           ;; Perform a series of sanity-tests.
           ;; If all of them pass, try to store it.
           (cond
             ;; Is the client trying to store a dependent resource at the top level?
             ;; NB: this looks a little odd, but it's (somewhat) simpler this way.
             ((let ((top-rtype-def (gethash (first uri-parts) (schema tbnl:*acceptor*))))
                (and top-rtype-def
                     (dependent top-rtype-def)))
              (progn
                (log-message :debug "Refusing to store a dependent resource as though it were a primary one.")
                (setf (tbnl:content-type*) "text/plain")
                (setf (tbnl:return-code*) tbnl:+http-bad-request+)
                "Error: dependent resources must be stored in context."))
             ;; Do we already have one of these?
             ((get-resources
                session
                (format nil "/~A/~A" resourcetype (sanitise-uid uid)))
              ;; It's already there; return 304 "Not modified"
              (progn
                (neo4cl:disconnect session)
                (log-message :debug (format nil "Doomed attempt to re-create resource /~A/~A."
                                            resourcetype (sanitise-uid uid)))
                (setf (tbnl:content-type*) "text/plain")
                (setf (tbnl:return-code*) tbnl:+http-not-modified+)
                (format nil "/~A/~A" resourcetype (sanitise-uid uid))))
             ;; Did the client try to set any read-only attributes?
             ((any-readonly-attrs (gethash resourcetype (schema tbnl:*acceptor*))
                                  (tbnl:post-parameters*))
              (progn
                (neo4cl:disconnect session)
                (log-message :debug "Client tried to set 1+ read-only attributes.")
                (setf (tbnl:content-type*) "text/plain")
                ;; Using 403 here instead of 400, because the request is understood and
                ;; _semantically_ valid, but the client is trying to do something it's not
                ;; allowed/authorised to do.
                (setf (tbnl:return-code*) tbnl:+http-forbidden+)
                "Invalid attempt to set read-only attributes."))
             ;; Do we have a valid user to store it under?
             ;; - Currently a no-op, but we'll need this when adding users and AAA.
             ;;
             ;; We don't already have one of these; store it
             (t
               (handler-case
                 (let ((uri (format
                              nil
                              "/~A/~A"
                              resourcetype
                              (store-resource session
                                              (schema tbnl:*acceptor*)
                                              resourcetype
                                              (tbnl:post-parameters*)
                                              (get-creator
                                                (post-policy
                                                  (access-policy *restagraph-acceptor*)))))))
                   ;; Close the Bolt session
                   (neo4cl:disconnect session)
                   ;; Return 201 and the URI
                   (log-message :debug (format nil "Stored the new resource with URI '~A'" uri))
                   (setf (tbnl:content-type*) "text/plain")
                   (setf (tbnl:return-code*) tbnl:+http-created+)
                   (setf (tbnl:header-out "Location") uri)
                   ;; Return the URI to the newly-created resource
                   uri)
                 ;; Handle integrity errors
                 (integrity-error (e) (return-integrity-error (message e)))
                 ;; Handle general client errors
                 (client-error (e) (return-client-error (message e))))))))
        ;;
        ;; Store a relationship
        ((and (equal (tbnl:request-method*) :POST)
              (equal (mod (length uri-parts) 3) 0)
              (tbnl:post-parameter "target"))
         ;; Grab these once, as we'll be referring to them a few times
         (let ((top-rtype-def (gethash (first uri-parts) (schema tbnl:*acceptor*)))
               (dest-path (tbnl:post-parameter "target")))
           (log-message :debug (format nil "Attempting to create a relationship from ~A to ~A"
                                       sub-uri dest-path))
           ;; Basic sanity checks
           (cond
             ;; Reject any attempt to create a :RG_CREATOR relationship.
             ((equal "RG_CREATOR" (car (last uri-parts)))
              (forbidden))
             ;; Reject any attempt to link from a dependent resource as though it's a top-level one.
             ((and top-rtype-def
                   (dependent top-rtype-def))
              (progn
                (log-message :debug "Refusing to store a dependent resource as though it were a primary one.")
                (setf (tbnl:content-type*) "text/plain")
                (setf (tbnl:return-code*) tbnl:+http-bad-request+)
                "Error: dependent resources must be stored in context."))
              ;; Passed the sanity-checks; proceed.
              (t
                (handler-case
                  (let ((new-uri (format nil "~{/~A~}/~A/~A"
                                         uri-parts    ; Path down to the relationship
                                         (car (last (get-uri-parts dest-path) 2))  ; Target resourcetype
                                         (car (last (get-uri-parts dest-path)))))  ; Target UID
                        (session (neo4cl:establish-bolt-session (datastore tbnl:*acceptor*))))
                    (create-relationship-by-path session
                                                 sub-uri
                                                 dest-path
                                                 (schema tbnl:*acceptor*))
                    (neo4cl:disconnect session)
                    ;; Report success to the client, plus the URI
                    (setf (tbnl:content-type*) "text/plain")
                    (setf (tbnl:return-code*) tbnl:+http-created+)
                    (setf (tbnl:header-out "Location") new-uri)
                    ;; Return the URI to the resource at the end of the newly-created relationship
                    new-uri) ; Target UID
                  ;; Attempted violation of db integrity
                  (integrity-error (e) (return-integrity-error (message e)))
                  ;; Generic client errors
                  (client-error (e) (return-client-error (message e)))
                  (neo4cl:client-error (e) (return-client-error (neo4cl:message e))))))))
        ;;
        ;; Create a dependent resource
        ((and
           (equal (tbnl:request-method*) :POST)
           (equal (mod (length uri-parts) 3) 1)
           (tbnl:post-parameter "uid"))
         ;; Is the top-level resource a dependent one?
         (let ((top-rtype-def (gethash (first uri-parts) (schema tbnl:*acceptor*))))
           (if (and top-rtype-def
                    (dependent top-rtype-def))
             (progn
               (log-message :debug "Refusing to store a dependent resource as though it were a primary one.")
               (setf (tbnl:content-type*) "text/plain")
               (setf (tbnl:return-code*) tbnl:+http-bad-request+)
               "Error: dependent resources must be stored in context.")
             ;; Do we already have one of these?
             (let ((session (neo4cl:establish-bolt-session (datastore tbnl:*acceptor*))))
               (if (get-resources session (format nil
                                                  "~{/~A~}/~A"
                                                  uri-parts
                                                  (sanitise-uid (tbnl:post-parameter "uid"))))
                 ;; It's already there; return 200/OK
                 (progn
                   (neo4cl:disconnect session)
                   (log-message :debug (format nil "Doomed attempt to re-create resource /~A/~A."
                                               (car uri-parts) (sanitise-uid (tbnl:post-parameter "uid"))))
                   (setf (tbnl:content-type*) "text/plain")
                   (setf (tbnl:return-code*) tbnl:+http-ok+)
                   (format nil "~{/~A~}/~A" uri-parts (sanitise-uid (tbnl:post-parameter "uid"))))
                 ;; We don't already have one of these; store it
                 (handler-case
                   (let ((newtype (car (last uri-parts)))
                         (uid (tbnl:post-parameter "uid"))
                         (new-uri (format nil "~{/~A~}/~A"
                                          uri-parts
                                          (sanitise-uid (tbnl:post-parameter "uid")))))
                     (log-message :debug (format nil "Attempting to create dependent resource '~A:~A' on '~A'"
                                                 newtype uid sub-uri))
                     (store-dependent-resource
                       session
                       (schema tbnl:*acceptor*)
                       sub-uri
                       (tbnl:post-parameters*)
                       (get-creator (post-policy (access-policy tbnl:*acceptor*))))
                     (neo4cl:disconnect session)
                     (setf (tbnl:content-type*) "text/plain")
                     (setf (tbnl:return-code*) tbnl:+http-created+)
                     (setf (tbnl:header-out "Location") new-uri)
                     ;; Return the URI to the resource at the end of the newly-created resource
                     new-uri)
                   ;; Attempted violation of db integrity
                   (integrity-error (e) (return-integrity-error (message e)))
                   ;; Generic client errors
                   (client-error (e) (return-client-error (message e)))
                   (neo4cl:client-error (e) (return-client-error (neo4cl:message e)))))))))
        ;;
        ;; Re-home a dependent resource
        ((and
           (equal (tbnl:request-method*) :POST)
           (> (length uri-parts) 0)
           (equal (mod (length uri-parts) 3) 2)
           (tbnl:post-parameter "target"))
         (log-message :debug (format nil "Attempting to move dependent resource ~A to ~A"
                                     sub-uri (tbnl:post-parameter "target")))
         ;; Is the top-level resource a dependent one?
         (let ((top-rtype-def (gethash (first uri-parts) (schema tbnl:*acceptor*))))
           (if (and top-rtype-def
                    (dependent top-rtype-def))
             (progn
               (log-message :debug "Refusing to store a dependent resource as though it were a primary one.")
               (setf (tbnl:content-type*) "text/plain")
               (setf (tbnl:return-code*) tbnl:+http-bad-request+)
               "Error: dependent resources must be stored in context.")
             (handler-case
               (let ((new-uri (format nil "~{/~A~}/~A/~A"
                                      (get-uri-parts (tbnl:post-parameter "target"))
                                      (car (last uri-parts 2))
                                      (car (last uri-parts))))
                     (session (neo4cl:establish-bolt-session (datastore tbnl:*session*))))
                 (move-dependent-resource
                   session
                   sub-uri
                   (tbnl:post-parameter "target")
                   (schema tbnl:*acceptor*))
                 (neo4cl:disconnect session)
                 (setf (tbnl:content-type*) "text/plain")
                 (setf (tbnl:return-code*) tbnl:+http-created+)
                 (setf (tbnl:header-out "Location") new-uri)
                 ;; Return the URI to the new path for this resource
                 new-uri)
               ;; Attempted violation of db integrity
               (integrity-error (e) (return-integrity-error (message e)))
               ;; Generic client errors
               (client-error (e) (return-client-error (message e)))
               (neo4cl:client-error (e) (return-client-error (neo4cl:message e)))))))
        ;; PUT -> update something
        ;;
        ;; Resource attributes
        ((and
           (equal (tbnl:request-method*) :PUT)
           (equal (mod (length uri-parts) 3) 2))
         (handler-case
           (let ((resourcetype (first uri-parts))
                 (top-rtype-def (gethash (first uri-parts) (schema tbnl:*acceptor*))))
             (cond
               ;; Attempt to update a dependent resource from the top level.
               ((and top-rtype-def
                     (dependent top-rtype-def))
                (progn
                  (log-message :debug "Refusing to store a dependent resource as though it were a primary one.")
                  (setf (tbnl:content-type*) "text/plain")
                  (setf (tbnl:return-code*) tbnl:+http-bad-request+)
                  "Error: dependent resources must be stored in context."))
               ;; Attempt to update one or more read-only attributes.
               ((any-readonly-attrs (gethash resourcetype (schema tbnl:*acceptor*))
                                    (append (tbnl:post-parameters*)
                                            (tbnl:get-parameters*)))
                (progn
                  (log-message :debug "Client tried to set 1+ read-only attributes.")
                  (setf (tbnl:content-type*) "text/plain")
                  ;; Using 403 here instead of 400, because the request is understood and
                  ;; _semantically_ valid, but the client is trying to do something it's not
                  ;; allowed/authorised to do.
                  (setf (tbnl:return-code*) tbnl:+http-forbidden+)
                  "Invalid attempt to set read-only attributes."))
               ;; Default case: we're good, carry on.
               (t
                 (let ((session (neo4cl:establish-bolt-session (datastore tbnl:*acceptor*))))
                   (log-message
                     :debug
                     (format nil "Attempting to update attributes of resource ~{/~A~}" uri-parts))
                   (update-resource-attributes
                     session
                     (schema tbnl:*acceptor*)
                     uri-parts
                     (remove-if #'(lambda (param)
                                    (or (null (cdr param))
                                        (equal (cdr param) "")))
                                (append (tbnl:post-parameters*)
                                        (tbnl:get-parameters*))))
                   (neo4cl:disconnect session)
                   ;; Return 200/Updated in accordance with the Working Group spec:
                   ;; https://httpwg.org/specs/rfc7231.html#PUT
                   ;; Technically, it'd be more correct to return 201/Created if a new attribute is set,
                   ;; but there are two issues with this:
                   ;; - all attributes are null by default in this system
                   ;; - multiple attributes can be updated in a single PUT request, leading to a conflict
                   ;;   where one or more is updated, and one or more is not.
                   ;;   The simplest solution to this conflict is to use the one return-code shared by
                   ;;   these cases, and interpret the spec as "ensure that these attributes have these
                   ;;   values" rather than "conditionally update whichever of these attributes doesn't
                   ;;   already have the value specified in this request."
                   (setf (tbnl:content-type*) "text/plain")
                   (setf (tbnl:return-code*) tbnl:+http-ok+)
                   "Updated"))))
           ;; Attempted violation of db integrity
           (integrity-error (e) (return-integrity-error (message e)))
           ;; Generic client errors
           (client-error (e) (return-client-error (message e)))
           (neo4cl:client-error (e) (return-client-error (neo4cl:message e)))))
        ;;
        ;; DELETE -> Delete something
        ;;
        ;; Attempting to delete a file
        ((and (equal (tbnl:request-method*) :DELETE)
              (equal (mod (length uri-parts) 3) 2)
              (equal "Files" (first uri-parts)))
         (log-message :debug "Client is requesting deletion of a file")
         (setf (tbnl:content-type*) "text/plain")
         (setf (tbnl:return-code*) tbnl:+http-bad-request+)
         (format nil "Use the files API to delete files: /files/v1/~A" (second uri-parts)))
        ;; Attempting to delete a resource
        ((and (equal (tbnl:request-method*) :DELETE)
              (equal (mod (length uri-parts) 3) 2))
         (log-message :debug "Attempting to delete a resource on an arbitrary path")
         ;; Try to get a copy of the resource
         (let ((top-rtype-def (gethash (first uri-parts) (schema tbnl:*acceptor*))))
           ;; Prevent clients directly requesting dependent resources
           (if (and top-rtype-def
                    (dependent top-rtype-def))
             ;; They're trying to directly delete a dependent resource
             ;; as though it were a primary one.
             (progn
               (log-message :debug "Refusing to delete a dependent resource as though it were a primary one.")
               (setf (tbnl:content-type*) "text/plain")
               (setf (tbnl:return-code*) tbnl:+http-bad-request+)
               "Error: dependent resources must be deleted in context.")
             ;; It passed that test.
             ;; Try to delete the resource.
             (let* ((session (neo4cl:establish-bolt-session (datastore tbnl:*acceptor*)))
                   (resource (get-resources session sub-uri)))
               (handler-case
                 (if resource
                   ;; If the resource is there, delete it
                   (progn
                     (delete-resource-by-path
                       session
                       sub-uri
                       (schema tbnl:*acceptor*)
                       :recursive (and (tbnl:parameter "recursive")
                                       (not (equal "" (tbnl:parameter "recursive")))))
                     (neo4cl:disconnect session)
                     (setf (tbnl:content-type*) "text/plain")
                     ;; If the client requested "yoink" then return a representation of what we just deleted.
                     (if (tbnl:get-parameter "yoink")
                       (progn
                         (setf (tbnl:return-code*) tbnl:+http-ok+)
                         (cl-json:encode-json-to-string resource))
                       ;; If they didn't, just acknowledge
                       (progn
                         (setf (tbnl:return-code*) tbnl:+http-no-content+)
                         "")))
                   ;; Resource is not there
                   (progn
                     (neo4cl:disconnect session)
                     (setf (tbnl:content-type*) "text/plain")
                     (setf (tbnl:return-code*) tbnl:+http-no-content+)
                     ""))
                 ;; Attempted violation of db integrity
                 (integrity-error (e) (return-integrity-error (message e)))
                 ;; Generic client errors
                 (client-error (e) (return-client-error (message e))))))))
        ;;
        ;; Delete a relationship on an arbitrary path
        ((and (equal (tbnl:request-method*) :DELETE)
              (or (tbnl:post-parameter "target")
                  (tbnl:get-parameter "target"))
              (equal (mod (length uri-parts) 3) 0))
         ;; Reject any attempt to create a :RG_CREATOR relationship
         (if (equal "RG_CREATOR" (car (last uri-parts)))
           (forbidden)
           (handler-case
             (let ((session (neo4cl:establish-bolt-session (datastore tbnl:*acceptor*))))
               (log-message :debug (format nil "Attempting to delete a relationship on an arbitrary path: ~A" sub-uri))
               (delete-relationship-by-path session
                                            (schema tbnl:*acceptor*)
                                            sub-uri
                                            (tbnl:parameter "target"))
               (neo4cl:disconnect session)
               (setf (tbnl:content-type*) "text/plain")
               (setf (tbnl:return-code*) tbnl:+http-no-content+)
               "")
             ;; Attempted violation of db integrity
             (integrity-error (e) (return-integrity-error (message e)))
             ;; Generic client errors
             (client-error (e) (return-client-error (message e))))))
        ;;
        ;; Methods we don't support.
        ;; Take the whitelist approach
        ((not (member (tbnl:request-method*) '(:POST :GET :PUT :DELETE)))
         (method-not-allowed))
        ;; Handle all other cases
        (t
          (log-message
            :warn
            (format nil "Bad request received with URI: ~A, reassembled as ~{/~A~}" (tbnl:request-uri*) uri-parts))
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

;   Copyright 2020-2021 James Fleming <james@electronic-quill.net>
;
;   Licensed under the GNU General Public License
;   - for details, see LICENSE.txt in the top-level directory


(in-package #:restagraph)
(declaim (optimize (compilation-speed 0)
                   (speed 2)
                   (safety 3)
                   (debug 3)))

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
        ;; GET -> Retrieve something
        ((equal (tbnl:request-method*) :GET)
         (log-message :debug (format nil "Dispatching GET request for URI ~A" (tbnl:request-uri*)))
         (let* (;; Do it separately because we use it again later in this function.
                ;; Get the search result
                (result (get-resources (datastore tbnl:*acceptor*)
                                       sub-uri
                                       :directional (when (tbnl:get-parameter "directional") t)
                                       :filters (remove-if
                                                          #'(lambda (par)
                                                              (equal (car par) "directional"))
                                                          (tbnl:get-parameters*)))))
           (log-message :debug (format nil "Fetched content ~A" result))
           ;; Return what we found
           (setf (tbnl:content-type*) "application/json")
           (setf (tbnl:return-code*) tbnl:+http-ok+)
           (cond
             ;; Single resource was requested, and nothing was found.
             ((and (= (mod (length uri-parts) 3) 2)
                   (or (null result)
                       (equal result "")))
              (progn
                (setf (tbnl:content-type*) "text/plain")
                (setf (tbnl:return-code*) tbnl:+http-not-found+)
                "Resource not found."))
             ;; Single resource was requested, and something was found.
             ((= (mod (length uri-parts) 3) 2)
              (progn
                (setf (tbnl:content-type*) "application/json")
                (setf (tbnl:return-code*) tbnl:+http-ok+)
                (cl-json:encode-json-alist-to-string result)))
             ;; Class of resources was requested, and nothing was found.
             ((or (null result)
                  (equal result ""))
              (progn
                (setf (tbnl:content-type*) "application/json")
                (setf (tbnl:return-code*) tbnl:+http-ok+)
                "[]"))
             ;; Class of resources was requested, and something was found.
             (t
              (cl-json:encode-json-to-string result)))))
        ;; POST -> Store something
        ;;
        ;; Resource
        ((and
           (equal (tbnl:request-method*) :POST)
           (equal (length uri-parts) 1)
           (tbnl:post-parameter "uid"))
         (let ((resourcetype (first uri-parts))
               (uid (tbnl:post-parameter "uid")))
           (log-message
             :debug
             (format nil "Attempting to dispatch a POST request for resource type ~A" resourcetype))
           ;; Do we already have one of these?
           (if (get-resources
                 (datastore tbnl:*acceptor*)
                 (format nil "/~A/~A" resourcetype (sanitise-uid uid)))
               ;; It's already there; return 304 "Not modified"
               (progn
                 (log-message :debug (format nil "Doomed attempt to re-create resource /~A/~A."
                                             resourcetype (sanitise-uid uid)))
                 (setf (tbnl:content-type*) "text/plain")
                 (setf (tbnl:return-code*) tbnl:+http-not-modified+)
                 (format nil "/~A/~A" resourcetype (sanitise-uid uid)))
               ;; We don't already have one of these; store it
               ;; But first, do we have a valid user to store it under?
               (handler-case
                 (let ((uri (format
                              nil
                              "/~A/~A"
                              resourcetype
                              (store-resource (datastore tbnl:*acceptor*)
                                              (schema tbnl:*acceptor*)
                                              resourcetype
                                              (tbnl:post-parameters*)
                                              (get-creator
                                                (post-policy
                                                  (access-policy *restagraph-acceptor*)))))))
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
                 (client-error (e) (return-client-error (message e)))))))
        ;;
        ;; Store a relationship
        ((and (equal (tbnl:request-method*) :POST)
              (equal (mod (length uri-parts) 3) 0)
              (tbnl:post-parameter "target"))
         ;; Grab these once, as we'll be referring to them a few times
         (let ((dest-path (tbnl:post-parameter "target")))
           ;; Basic sanity check
           (log-message :debug (format nil "Creating a relationship from ~A to ~A" sub-uri dest-path))
           ;; Reject any attempt to create a :CREATOR relationship
           (if (equal "CREATOR" (car (last uri-parts)))
               (forbidden)
               ;; Passed the sanity-checks; proceed.
               (handler-case
                 (let ((new-uri (format nil "~{/~A~}/~A/~A"
                                        uri-parts    ; Path down to the relationship
                                        (car (last (get-uri-parts dest-path) 2))  ; Target resourcetype
                                        (car (last (get-uri-parts dest-path)))))) ; Target UID
                   (create-relationship-by-path (datastore tbnl:*acceptor*)
                                                sub-uri
                                                dest-path
                                                (schema tbnl:*acceptor*))
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
                 (neo4cl:client-error (e) (return-client-error (neo4cl:message e)))))))
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
               (log-message :debug (format nil "Doomed attempt to re-create resource /~A/~A."
                                           (car uri-parts) (tbnl:post-parameter "uid")))
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
                   (datastore tbnl:*acceptor*)
                   (schema tbnl:*acceptor*)
                   sub-uri
                   (tbnl:post-parameters*)
                   (get-creator (post-policy (access-policy *restagraph-acceptor*))))
                 (setf (tbnl:content-type*) "text/plain")
                 (setf (tbnl:return-code*) tbnl:+http-created+)
                 (setf (tbnl:header-out "Location") new-uri)
                 ;; Return the URI to the resource at the end of the newly-created resource
                 new-uri)
               ;; Attempted violation of db integrity
               (integrity-error (e) (return-integrity-error (message e)))
               ;; Generic client errors
               (client-error (e) (return-client-error (message e)))
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
           (let ((new-uri (format nil "~{/~A~}/~A/~A"
                                  (get-uri-parts (tbnl:post-parameter "target"))
                                  (car (last uri-parts 2))
                                  (car (last uri-parts)))))
             (move-dependent-resource
               (datastore tbnl:*acceptor*)
               sub-uri
               (tbnl:post-parameter "target")
               (schema tbnl:*acceptor*))
             (setf (tbnl:content-type*) "text/plain")
             (setf (tbnl:return-code*) tbnl:+http-created+)
             (setf (tbnl:header-out "Location") new-uri)
             ;; Return the URI to the new path for this resource
             new-uri)
           ;; Attempted violation of db integrity
           (integrity-error (e) (return-integrity-error (message e)))
           ;; Generic client errors
           (client-error (e) (return-client-error (message e)))
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
               (schema tbnl:*acceptor*)
               uri-parts
               (remove-if #'(lambda (param)
                              (or (null (cdr param))
                                  (equal (cdr param) "")))
                          (append (tbnl:post-parameters*)
                                  (tbnl:get-parameters*))))
             (setf (tbnl:content-type*) "text/plain")
             (setf (tbnl:return-code*) tbnl:+http-no-content+)
             "")
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
        ;; Resource
        ((and (equal (tbnl:request-method*) :DELETE)
              (equal (mod (length uri-parts) 3) 2))
         (log-message :debug "Attempting to delete a resource on an arbitrary path")
         (handler-case
           (progn
             (delete-resource-by-path
               (datastore tbnl:*acceptor*)
               sub-uri
               (schema tbnl:*acceptor*)
               :recursive (and (tbnl:parameter "recursive")
                               (not (equal "" (tbnl:parameter "recursive")))))
             (setf (tbnl:content-type*) "text/plain")
             (setf (tbnl:return-code*) tbnl:+http-no-content+)
             "")
           ;; Attempted violation of db integrity
           (integrity-error (e) (return-integrity-error (message e)))
           ;; Generic client errors
           (client-error (e) (return-client-error (message e)))))
        ;;
        ;; Delete a relationship on an arbitrary path
        ((and (equal (tbnl:request-method*) :DELETE)
              (or (tbnl:post-parameter "resource")
                  (tbnl:get-parameter "resource"))
              (equal (mod (length uri-parts) 3) 0))
         (handler-case
           (progn
             (log-message :debug (format nil "Attempting to delete a relationship on an arbitrary path: ~A" sub-uri))
             (delete-relationship-by-path (datastore tbnl:*acceptor*)
                                          sub-uri
                                          (or (tbnl:post-parameter "resource")
                                              (tbnl:get-parameter "resource"))
                                          (schema tbnl:*acceptor*))
             (setf (tbnl:content-type*) "text/plain")
             (setf (tbnl:return-code*) tbnl:+http-no-content+)
             "")
           ;; Attempted violation of db integrity
           (integrity-error (e) (return-integrity-error (message e)))
           ;; Generic client errors
           (client-error (e) (return-client-error (message e)))))
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

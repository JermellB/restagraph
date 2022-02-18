;   Copyright 2020-2021 James Fleming <james@electronic-quill.net>
;
;   Licensed under the GNU General Public License
;   - for details, see LICENSE.txt in the top-level directory


(in-package #:restagraph)
(declaim (optimize (compilation-speed 0)
                   (speed 2)
                   (safety 3)
                   (debug 3)))


(defun files-dispatcher-v1 ()
  "Files API handler, API version 1."
  (log-message :debug "Handling files request")
  (log-message :debug (format nil "Handling files '~A' request with content-type '~A'"
                              (tbnl:request-method*) (tbnl:content-type*)))
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
                                                 checksum))
            (session (neo4cl:establish-bolt-session (datastore tbnl:*acceptor*))))
       ;; Does a file of this name already exist?
       (log-message :debug (format nil "Checking for an existing file by name '~A'"
                                   (sanitise-uid requested-filename)))
       (if (get-resources session
                          (format nil "/Files/~A" (sanitise-uid requested-filename)))
         ;; If it already exists, bail out now.
         (progn
           (log-message :error (format nil "File ~A already exists; bailing out."
                                       (sanitise-uid requested-filename)))
           ;; Return client-error message indicating name collision
           (setf (tbnl:content-type*) "text/plain")
           (setf (tbnl:return-code*) tbnl:+http-conflict+)
           "Filename already exists")
         ;; Name isn't already taken; rock on.
         (progn
           (log-message
             :debug
             (format nil "File ~A does not already exist; proceeding." (sanitise-uid requested-filename)))
           ;; Now we need to store the file's metadata,
           (log-message
             :debug
             (format nil "Storing file metadata: name = '~A', checksum = '~A', mimetype '~A'"
                     requested-filename checksum mimetype))
           (handler-case
             (let ((uri (concatenate
                          'string
                          "/Files/"
                          (store-resource session
                                          (schema tbnl:*acceptor*)
                                          "Files"
                                          `(("uid" . ,(sanitise-uid requested-filename))
                                            ("title" . ,requested-filename)
                                            ("sha3256sum" . ,checksum)
                                            ("mimetype" . ,mimetype))
                                          (get-creator
                                            (post-policy
                                              (access-policy *restagraph-acceptor*)))))))
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
                   (move-file filepath-temp (namestring filepath-target))))
               ;; If the location-move fails, we should probably remove the metadata and tell the client.
               ;;
               ;; Now return success to the client
               (setf (tbnl:content-type*) "text/plain")
               (setf (tbnl:return-code*) tbnl:+http-created+)
               (setf (tbnl:header-out "Location") uri)
               uri)
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
             (file-error
               (e)
               (let ((error-message (format nil "File error: ~A" (file-error-pathname e))))
                 (log-message :error error-message)
                 (setf (tbnl:content-type*) "text/plain")
                 (setf (tbnl:return-code*) tbnl:+http-internal-server-error+)
                 error-message))
             (error
               (e)
               (let ((error-message (format nil "Error: ~A" (type-of e))))
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
            (session (neo4cl:establish-bolt-session (datastore tbnl:*acceptor*)))
            ;; Do it separately because we use it again later in this function.
            ;; Get the search result
            (result (get-resources session (format nil "/Files/~A" filename))))
       (neo4cl:disconnect session)
       (log-message :debug (format nil "Retrieved resource details ~A" result))
       ;; Return the file to the client if it's present
       (cond
         ((not (null result))
          (let* ((source-path (digest-to-filepath
                                (make-pathname :defaults (files-location tbnl:*acceptor*))
                                (gethash "sha3256sum" result))))
            (log-message :debug (format nil "Returning the file from source-path '~A'"
                                        source-path))
            (tbnl:handle-static-file
              source-path
              ;; Specify the mime-type
              (gethash "mimetype" result))))
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
       (let* ((session (neo4cl:establish-bolt-session (datastore tbnl:*acceptor*)))
              (result (get-resources session
                                    (format nil "/Files/~A" filename))))
         (log-message :debug (format nil "Got result '~A'" result))
         (if result
           ;; If it is, delete its metadata and then (conditionally) the file itself.
           (progn
             (log-message :debug (format nil "Found file details '~A'" result))
             ;; Delete the metadata
             (delete-resource-by-path
               session
               (concatenate 'string "/Files/" filename)
               (schema tbnl:*acceptor*)
               :recursive nil)
             ;; Now delete the file itself
             (log-message :debug "File deleted from db. Now removing from filesystem.")
             (when (null (get-resources
                           session
                           "/Files"
                           :filters (process-filters
                                      `(("sha3256sum" .
                                         ,(gethash "sha3256sum" result)))
                                      (schema tbnl:*acceptor*)
                                      "Files")))
               (let* ((source-path (digest-to-filepath
                                     (make-pathname :defaults (files-location tbnl:*acceptor*))
                                     (gethash "sha3256sum" result))))
                 (if (probe-file source-path)
                   (progn
                     (log-message :debug (format nil "Deleting the file at source-path '~A'"
                                                 source-path))
                     (delete-file source-path))
                   (log-message :warn (format nil "No file found at source-path '~A'"
                                              source-path)))))
             ;; Don't need that session any more
             (neo4cl:disconnect session)
             ;; Now return a suitable message to the client
             (setf (tbnl:content-type*) "text/plain")
             (setf (tbnl:return-code*) tbnl:+http-no-content+)
             "")
           ;; If not, return 404
           (progn
             ;; Don't need the session here; clean it up.
             (neo4cl:disconnect session)
             (log-message :debug (format nil "Requested file '~A' not found. Returning 404" filename))
             (four-oh-four :file-p t))))))
    ;;
    ;; Methods we don't support.
    ;; Take the whitelist approach
    ((not (member (tbnl:request-method*) '(:POST :GET :DELETE)))
     (method-not-allowed))
    ;; Handle all other cases
    (t
      (log-message :warn (format nil "Bad request received with URI: ~A" (tbnl:request-uri*)))
      (return-client-error "This wasn't a valid request"))))

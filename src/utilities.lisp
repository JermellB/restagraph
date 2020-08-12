;   Copyright 2020 James Fleming <james@electronic-quill.net>
;
;   Licensed under the GNU General Public License
;   - for details, see LICENSE.txt in the top-level directory

(in-package #:restagraph)


;;; Utility functions

(defun sanitise-uid (uid)
  "Replace UID-unfriendly characters in UIDs with something safe"
  (declare (type (string) uid))
  (escape-neo4j
    (cl-ppcre:regex-replace-all "[/ ]" uid "_")))

(defun get-sub-uri (uri base-uri)
  (declare (type (string) uri base-uri))
  "Extract the URI from the full request string,
  excluding the base URL and any GET parameters."
  ;; The (or) is to prevent breakage when the URI matches the base-uri,
  ;; which would return NIL instead of a string.
  (or (first (cl-ppcre:split
               "\\?"
               (cl-ppcre:regex-replace base-uri uri "")))
      ""))

(defun get-uri-parts (uri)
  "Break the URI into parts for processing by uri-node-helper.
  Assumes the base-uri and trailing parameters have already been removed."
  (declare (type (string) uri))
  (mapcar #'sanitise-uid
          (cdr (ppcre:split "/" uri))))

(defun uri-node-helper (uri-parts &key (path "") (marker "n") (directional t))
  "Build a Cypher path ending in a node variable, which defaults to 'n'.
  Accepts a list of strings and returns a single string."
  (declare (type (or null cons) uri-parts)
           (type (string) path marker)
           (type (boolean) directional))
  (cond
    ((null uri-parts)
     (format nil "~A(~A)" path (escape-neo4j marker)))
    ((equal (length uri-parts) 1)
     (format nil "~A(~A:~A)" path (escape-neo4j marker) (first uri-parts)))
    ((equal (length uri-parts) 2)
     (format nil "~A(~A:~A { uid: '~A' })"
             path (escape-neo4j marker) (first uri-parts) (second uri-parts)))
    ((equal (length uri-parts) 3)
     (format nil "~A(:~A { uid: '~A' })-[:~A]~A(~A)"
             path
             (sanitise-uid (first uri-parts))
             (sanitise-uid (second uri-parts))
             (sanitise-uid (third uri-parts))
             (if directional "->" "-")
             (escape-neo4j marker)))
    (t
      (uri-node-helper
        (cdddr uri-parts)
        :path (format nil "~A(:~A { uid: '~A' })-[:~A]~A"
                      path
                      (sanitise-uid (first uri-parts))
                      (sanitise-uid (second uri-parts))
                      (sanitise-uid (third uri-parts))
                      (if directional "->" "-"))
        :marker marker
        :directional directional))))

(defun uri-rel-helper (uri-parts &key (path "") (marker "n") (directional t))
  "Build a Cypher path ending in a relationship variable, which defaults to 'n'.
  Accepts a list of strings and returns a single string."
  (declare (type (cons) uri-parts)
           (type (string) path marker)
           (type (boolean) directional))
  ;; Path-length must be a multiple of 3
  (if (= (mod (length uri-parts) 3) 0)
    ;; Path length is OK.
    ;; Is this the end of the path?
    (if (> (length uri-parts) 3)
      ;; More path to come
      (uri-rel-helper
        (cdddr uri-parts)
        :path (format nil "~A(:~A {uid: '~A'})-[:~A]~A"
                      path
                      (sanitise-uid (first uri-parts))
                      (sanitise-uid (second uri-parts))
                      (sanitise-uid (third uri-parts))
                      (if directional "->" "-"))
        :marker (escape-neo4j marker)
        :directional directional)
      ;; End of the path.
      ;; Return this along with whatever came before.
      (format nil "~A(:~A {uid: '~A'})-[~A:~A]"
              path
              (sanitise-uid (first uri-parts))
              (sanitise-uid (second uri-parts))
              (escape-neo4j marker)
              (sanitise-uid (third uri-parts))))
    ;; This isn't a path to a relationship
    (error 'client-error :message "Path length must be a multiple of 3.")))

(defun build-cypher-path (uri-parts &optional (path "") (marker "m"))
  "Build a Cypher path from the list of strings supplied.
  Attach a marker variable to the last node in the list, defaulting to 'm'."
  (declare (type (or null cons) uri-parts)
           (type (string) path)
           (type (string) marker))
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

(defun hash-file (filepath &optional (digest 'ironclad:sha3/256))
  "Return the hash-digest of a file, as a string."
  (declare (type pathname filepath))
  (log-message :debug (format nil "Producing a hash digest of file '~A'" filepath))
  ;; Convert the digest back into a string
  (format nil "~{~2,'0X~}"
          (loop for b across
                ;; Digest the file
                (with-open-file (filestream filepath :element-type '(unsigned-byte 8))
                  (ironclad:digest-stream digest filestream))
                ;; Accumulator for the string to return
                collecting b)))

(defun digest-to-filepath (basedir digest)
  "Take a 64-bit digest string and the basedir, and return the target path to the file
   as a cons of two strings: the directory path and the filename"
  (declare (string basedir)
           (type string digest))
  (log-message :debug (format nil "Generating a filepath from base-dir '~A' and digest '~A'" basedir digest))
  (cons (format nil "~A/~A/~A/~A/"
                basedir
                (subseq digest 0 2)
                (subseq digest 2 4)
                (subseq digest 4 6))
        (subseq digest 6)))

(defun get-file-mime-type (filepath)
  (string-right-trim
    '(#\NewLine)
    (with-output-to-string (str)
      (sb-ext:run-program "/bin/file"
                          (list "-b" "--mime-type" (namestring filepath))
                          :output str)
      str)))

(defun ensure-db-passwd (server)
  "Check the credentials for the database, and return a boolean."
  (log-message :info "Checking database credentials.")
  (let ((err (cdr (assoc :errors
                    (neo4cl:neo4j-transaction
                      server
                      '((:statements ((:statement . "RETURN \"test\"")))))))))
    ;; Crude error handling
    (if (and (listp err)
             (null err))
      ;; Credentials check out; we're good
      (progn
        (log-message :debug "Password is good.")
        t)
      ;; Credentials failed; we have a problem
      (progn
        (log-message :debug (format nil "Credentials failed: '~A'" err))
        nil))))

(defun confirm-db-is-running (server &key (counter 1) (max-count 5) (sleep-time 5))
  "Check whether the database server is running by polling the discovery endpoint."
  (declare (type (integer) counter max-count sleep-time))
  (log-message :debug "Checking whether the database is running on ~A:~A"
               (neo4cl:hostname server) (neo4cl:port server))
  (handler-case
    (when (drakma:http-request
            (format nil "http://~A:~A" (neo4cl:hostname server) (neo4cl:port server)))
      (ensure-db-passwd server))
    (USOCKET:CONNECTION-REFUSED-ERROR
      (e)
      (declare (ignore e))
      (if (>= counter max-count)
        ;; Timed out.
        ;; Leave a message, then exit this whole thing.
        (progn
          (log-message :crit "Timed out trying to connect to the database. Exiting.")
          (sb-ext:exit))
        ;; Still isn't responding, but we haven't yet hit timeout.
        ;; Leave a message, pause, then try again.
        (progn
          (log-message :warn "Connection refused. Pausing for ~A seconds before retrying" sleep-time)
          (sleep sleep-time)
          (confirm-db-is-running server
                                 :counter (+ counter 1)
                                 :max-count max-count
                                 :sleep-time sleep-time))))))

(defun save-image (&optional (path "/tmp/restagraph"))
  (declare (type (string) path))
  (sb-ext:save-lisp-and-die path :executable t :toplevel 'restagraph::dockerstart))

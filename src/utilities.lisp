;   Copyright 2020 James Fleming <james@electronic-quill.net>
;
;   Licensed under the GNU General Public License
;   - for details, see LICENSE.txt in the top-level directory

(in-package #:restagraph)

(declaim (optimize (compilation-speed 0)
                   (speed 2)
                   (safety 3)
                   (debug 3)))


;;; Utility functions

;; Handy reference of definitions from RFC 3986
;(defvar unreserved (list #\- #\. #\_ #\~))
;(defvar delimiters (list #\: #\/ #\? #\# #\[ #\] #\@))
;(defvar sub-delimiters (list #\! #\$ #\& #\' #\( #\) #\* #\+ #\, #\; #\=))

(defun unreserved-char-p (c)
  "Test whether a character is unreserved, per section 2.3 of RFC 3986."
  (or (alphanumericp c)
      (char= #\- c)
      (char= #\. c)
      (char= #\_ c)
      (char= #\~ c)))

;; Design-decision note
;;
;; Two options here:
;; - Replace unfriendly characters with something neutral, like an underscore.
;; - Remove unfriendly characters altogether.
;;
;; For almost all cases, replacing them will make for a crappy-looking URL with very little gain.
;; However, preserving some kind of separation between words can improve URL readability at low cost.
;;
;; Even though the API itself is unlikely to actually be used directly by humans,
;; URIs do get reflected in user interfaces in various ways, such as in the URL of a web GUI,
;; which people sometimes need to transcribe or communicate verbally.
;; Because of that, human-friendliness of identifiers is important in a way that
;; compactness in _paths_ is not.
(defun sanitise-uid (uid)
  "Strip out UID-unfriendly characters, after replacing whitespace with underscores.
   UID-friendly means being an unreserved character as defined in RFC 3986."
  (declare (type (string) uid))
  (remove-if-not #'unreserved-char-p
                 (substitute #\_ #\Space uid)))

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
  Assumes the base-uri and trailing parameters have already been removed.
  Expects a leading forward-slash before the first element;
  anything *before and including* the first forward slash will be discarded."
  (declare (type (string) uri))
  (mapcar #'sanitise-uid
          (cdr (ppcre:split "/" uri))))

(defun regex-p (str)
  "Test whether a string contains a Java-style regex."
  (cl-ppcre:all-matches "[\\.\\*\\+[]" str))

(defun uri-node-helper (uri-parts &key (path "") (marker "n"))
  "Build a Cypher path ending in a node variable, which defaults to 'n'.
   Accepts a list of strings and returns a single string."
  (declare (type (or null cons) uri-parts)
           (type (string) path marker))
  (cond
    ;; End of the list; terminate the path with the marker.
    ((null uri-parts)
     (format nil "~A(~A)" path marker))
    ;; 1-element URI; terminate the path with the marker identifying a resourcetype.
    ((equal (length uri-parts) 1)
     (format nil "~A(~A:~A)" path marker (first uri-parts)))
    ;; 2-element URI; terminate the path with the marker identifying a specific resource.
    ((equal (length uri-parts) 2)
     (format nil "~A(~A:~A { uid: '~A' })"
             path marker (first uri-parts) (second uri-parts)))
    ;; 3-element URI; terminate the path with a relationship from a resource to the marker.
    ((equal (length uri-parts) 3)
     (format nil "~A(:~A~A)-[:~A]->(~A)"
             path
             ;; Resourcetype
             (sanitise-uid (first uri-parts))
             ;; UID
             ;; Allow for a wildcard.
             (if (equal "*" (second uri-parts))
                 ""
                 (format nil " { uid: '~A' }" (sanitise-uid (second uri-parts))))
             ;; Relationship to target
             (sanitise-uid (third uri-parts))
             ;; Target
             marker))
    ;; The URI is longer than 3 elements.
    ;; Extend the path with its first 3 elements, then recurse through this function
    ;; with whatever is left over.
    (t
     (uri-node-helper
       (cdddr uri-parts)
       :path (format nil "~A(:~A { uid: '~A' })-[:~A]->"
                     path
                     (sanitise-uid (first uri-parts))
                     (sanitise-uid (second uri-parts))
                     (sanitise-uid (third uri-parts)))
       :marker marker))))

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
  (declare (type pathname basedir)
           (type string digest))
  (log-message :debug (format nil "Generating a filepath from base-dir '~A' and digest '~A'" basedir digest))
  (merge-pathnames (make-pathname :directory `(:relative
                                                ,(format nil "~A/~A/~A" (subseq digest 0 2)
                                                        (subseq digest 2 4)
                                                        (subseq digest 4 6)))
                                  :name (subseq digest 6))
                   (make-pathname :defaults basedir)))

(defun get-file-mime-type (filepath)
  "Return the MIME-type of a file, as a string.
  The path argument must be a string, as it's passed verbatim to the Unix shell."
  (declare (type string filepath))
  (log-message :debug (format nil "Identifying MIME-type for file '~A'" filepath))
  (log-message :debug (format nil "PATH value: ~A" (sb-ext:posix-getenv "PATH")))
  (string-right-trim
    '(#\NewLine)
    (with-output-to-string (str)
      (sb-ext:run-program "file" (list "-b" "--mime-type" filepath) :search t :output str)
      str)))

(defun move-file (old-path new-path)
  "Move a file by calling out to the Unix 'mv' utility.
  Do this because rename-file doesn't work across filesystem boundaries."
  (let ((oldpath (format nil "~A" old-path))
        (newpath (format nil "~A" new-path)))
    (log-message :debug (format nil "Moving file '~A' to new location '~A'" oldpath newpath))
    (log-message :debug (format nil "Result of file move: ~A"
                                (with-output-to-string (outstr)
                                  (sb-ext:run-program "mv"
                                                      (list oldpath newpath)
                                                      :search t
                                                      :output outstr)
                                  outstr)))))


(defun confirm-db-is-running (server &key (counter 1) (max-count 5) (sleep-time 5))
  "Check whether the database server is running by polling the discovery endpoint."
  (declare (type neo4cl:bolt-server server)
           (type integer counter max-count sleep-time))
  (log-message :debug (format nil "Checking whether the database is running on ~A:~A"
                              (neo4cl:hostname server) (neo4cl:port server)))
  (handler-case
    ;; Try to run a non-destructive transaction
    (let ((session (neo4cl:establish-bolt-session server)))
      (neo4cl:bolt-transaction-autocommit session "RETURN 'hello'")
      (neo4cl:disconnect session))
    ;; If the port isn't open, pause for a few seconds before trying again.
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
          (log-message :warn (format nil "Connection refused. Pausing for ~A seconds before retrying" sleep-time))
          (sleep sleep-time)
          (confirm-db-is-running server
                                 :counter (+ counter 1)
                                 :max-count max-count
                                 :sleep-time sleep-time))))))

(defun replace-all (string part replacement &key (test #'char=))
  "Returns a new string in which all the occurences of the part is replaced with replacement.
   Swiped from the Common Lisp cookbook."
  (with-output-to-string (out)
    (loop with part-length = (length part)
          for old-pos = 0 then (+ pos part-length)
          for pos = (search part string
                            :start2 old-pos
                            :test test)
          do (write-string string out
                           :start old-pos
                           :end (or pos (length string)))
          when pos do (write-string replacement out)
          while pos)))

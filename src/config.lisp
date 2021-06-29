;   Copyright 2017 James Fleming <james@electronic-quill.net>
;
;   Licensed under the GNU General Public License
;   - for details, see LICENSE.txt in the top-level directory

;;;; Configs for the server to use

(in-package #:restagraph)

;; Control the decoding of JSON identifiers received from Neo4j.
;; Convert all symbols (including keywords, but excluding strings) to upper-case
;; for least-effort comparison after decoding.
;; Do it this way to avoid issues caused by CamelCase<-->snake-case conversion.
(setf json:*json-identifier-name-to-lisp* 'common-lisp:string-upcase)

;; Control the encoding of JSON identifiers sent to Neo4j.
;; Downcase them on the way out, despite this appearing to conflict with the
;; decision to upcase them on the way back in.
;; Upcasing causes the _whole_ request to be upcased, including the bits that Neo4j
;; requires in lowercase, causing a Neo.ClientError.Request.InvalidFormat error.
;; Thus, this is the simplest robust approach.
(setf json:*lisp-identifier-name-to-json* 'common-lisp:string-downcase)


(defparameter *config-vars*
  `(:listen-address "localhost"
    :listen-port 4950
    :dbhostname "10.255.0.1"
    :dbport 7474
    :dbname "neo4j"
    :dbusername "neo4j"
    :dbpasswd "wallaby"
    :api-uri-base "/raw/v1"
    :schema-uri-base "/schema/v1"
    :files-uri-base "/files/v1"
    :files-temp-location "/tmp/restagraph-files-tmp/"
    :files-location "/tmp/restagraph-files/"))

(setf *loglevel* :info)

;; Global var holding the acceptor.
;; Define it here so that it's already present when the compiler processes
;; hunchentoot.lisp. That way it doesn't complain about references to an
;; undefined variable.
(defvar *restagraph-acceptor* nil)

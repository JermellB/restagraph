;   Copyright 2017 James Fleming <james@electronic-quill.net>
;
;   Licensed under the GNU General Public License
;   - for details, see LICENSE.txt in the top-level directory

(asdf:defsystem #:restagraph
  :serial t
  :license "MIT license"
  :author "James Fleming <james@electronic-quill.net>"
  :description "Generates a REST API from a shema defined in Neo4J"
  :depends-on (#:neo4cl
               #:hunchentoot
               #:drakma
               #:ironclad
               #:cl-json
               #:ipaddress)
  :components ((:file "package")
               (:file "conditions")
               (:file "logging")
               (:file "utilities")
               (:file "schema-structures")
               (:file "access-control")
               (:file "schema-db")
               (:file "schema-queries")
               (:file "resources")
               (:file "relationships")
               (:file "ipam")
               (:file "core-schema")
               (:file "config")
               (:file "dispatcher-responses")
               (:file "dispatcher-schema")
               (:file "dispatcher-raw-api")
               (:file "dispatcher-files")
               (:file "dispatcher-ipam")
               (:file "hunchentoot-classes")
               (:file "hunchentoot")))

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
               #:cl-yaml
               #:ironclad)
  :components ((:file "package")
               (:file "structures")
               (:file "config")
               (:file "conditions")
               (:file "logging")
               (:file "generics")
               (:file "neo4j")
               (:file "utilities")
               (:file "schema")
               (:file "dispatchers")
               (:file "hunchentoot")))

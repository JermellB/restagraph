;   Copyright 2017 James Fleming <james@electronic-quill.net>
;
;   Licensed under the GNU General Public License
;   - for details, see LICENSE.txt in the top-level directory

(asdf:defsystem #:restagraph-test
                :serial t
                :license "MIT license"
                :author "James Fleming <james@electronic-quill.net>"
                :description "Test suite for Restagraph"
                :depends-on (#:restagraph
                             #:fiveam)
                :components ((:file "package")
                             (:file "restagraph-test")
                             (:file "utilities-pure")
                             (:file "utilities-side-effecting")
                             (:file "pure-tests")
                             (:file "bolt-schema")
                             (:file "bolt-side-effecting-resources")
                             (:file "bolt-side-effecting-relationships")
                             (:file "ipam")))

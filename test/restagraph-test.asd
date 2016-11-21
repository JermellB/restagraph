(asdf:defsystem #:restagraph-test
                :serial t
                :license "MIT license"
                :author "James Fleming <james@electronic-quill.net>"
                :description "Test suite for Restagraph"
                :depends-on (#:restagraph
                             #:fiveam)
                :components ((:file "package")
                             (:file "restagraph-test")))

;   Copyright 2021 James Fleming <james@electronic-quill.net>
;
;   Licensed under the GNU General Public License
;   - for details, see LICENSE.txt in the top-level directory


;;;; Test suite for pure code.
;;;; I.e, functions and methods without side-effects.

(in-package #:restagraph-test)

(declaim (optimize (compilation-speed 0)
                   (speed 2)
                   (safety 3)
                   (debug 3)))

(fiveam:def-suite
  utilities-pure
  :description "Tests for *non* side-effecting functions in utilities.lisp. Tests are named for the functions they're testing."
  :in main)

(fiveam:in-suite utilities-pure)

(fiveam:test
  restagraph::sanitise-uid
  (fiveam:is (equal "foo_bar"
                    (restagraph::sanitise-uid "foo bar"))))

(fiveam:test
  get-sub-uri
  (fiveam:is (equal "/People/TestPerson"
                    (restagraph::get-sub-uri
                      "/raw/v1/People/TestPerson"
                      "/raw/v1"))))

(fiveam:test
  get-uri-parts
  (fiveam:is (equal '("People" "TestPerson")
                    (restagraph::get-uri-parts "/People/TestPerson")))
  (fiveam:is (equal '("TestPerson")
                    (restagraph::get-uri-parts "People/TestPerson"))))

(fiveam:test
  uri-node-helper
  ;; End-of-list
  (fiveam:is (equal "(n)" (restagraph::uri-node-helper '() :path "" :marker "n")))
  (fiveam:is (equal "()" (restagraph::uri-node-helper '() :path "" :marker "")))
  (fiveam:is (equal "path(n)" (restagraph::uri-node-helper '() :path "path" :marker "n")))
  (fiveam:is (equal "path()" (restagraph::uri-node-helper '() :path "path" :marker "")))
  ;; 1-element URI
  (fiveam:is (equal "(n:foo)" (restagraph::uri-node-helper '("foo") :path "" :marker "n")))
  (fiveam:is (equal "(:foo)" (restagraph::uri-node-helper '("foo") :path "" :marker "")))
  (fiveam:is (equal "path(n:foo)" (restagraph::uri-node-helper '("foo") :path "path" :marker "n")))
  (fiveam:is (equal "path(:foo)" (restagraph::uri-node-helper '("foo") :path "path" :marker "")))
  ;; 2-element URI
  (fiveam:is (equal "(n:Foo { uid: 'bar' })"
                    (restagraph::uri-node-helper '("Foo" "bar") :path "" :marker "n")))
  (fiveam:is (equal "(:Foo { uid: 'bar' })"
                    (restagraph::uri-node-helper '("Foo" "bar") :path "" :marker "")))
  (fiveam:is (equal "path(n:Foo { uid: 'bar' })"
                    (restagraph::uri-node-helper '("Foo" "bar") :path "path" :marker "n")))
  (fiveam:is (equal "path(:Foo { uid: 'bar' })"
                    (restagraph::uri-node-helper '("Foo" "bar") :path "path" :marker "")))
  ;; 3-element URI
  (fiveam:is (equal "(:Foo { uid: 'bar' })-[:BAZ]->(n)"
                    (restagraph::uri-node-helper '("Foo" "bar" "BAZ")
                                                 :path ""
                                                 :marker "n")))
  (fiveam:is (equal "(:Foo { uid: 'bar' })-[:BAZ]->()"
                    (restagraph::uri-node-helper '("Foo" "bar" "BAZ")
                                                 :path ""
                                                 :marker "")))
  (fiveam:is (equal "path(:Foo { uid: 'bar' })-[:BAZ]->(n)"
                    (restagraph::uri-node-helper '("Foo" "bar" "BAZ")
                                                 :path "path"
                                                 :marker "n")))
  (fiveam:is (equal "path(:Foo { uid: 'bar' })-[:BAZ]->()"
                    (restagraph::uri-node-helper '("Foo" "bar" "BAZ")
                                                 :path "path"
                                                 :marker "")))
  ;; 3-element URI with wildcard
  (fiveam:is (equal "(:Foo)-[:BAZ]->(n)"
                    (restagraph::uri-node-helper '("Foo" "*" "BAZ")
                                                 :path ""
                                                 :marker "n")))
  (fiveam:is (equal "(:Foo)-[:BAZ]->()"
                    (restagraph::uri-node-helper '("Foo" "*" "BAZ")
                                                 :path ""
                                                 :marker "")))
  (fiveam:is (equal "path(:Foo)-[:BAZ]->(n)"
                    (restagraph::uri-node-helper '("Foo" "*" "BAZ")
                                                 :path "path"
                                                 :marker "n")))
  (fiveam:is (equal "path(:Foo)-[:BAZ]->()"
                    (restagraph::uri-node-helper '("Foo" "*" "BAZ")
                                                 :path "path"
                                                 :marker "")))
  )


;; FIXME: write these tests
#+(or)
(fiveam:test
  build-cypher-path)

#+(or)
(fiveam:test
  digest-to-filepath)

#+(or)
(fiveam:test
  replace-all)

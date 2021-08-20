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

;; FIXME: write these tests
#+(or)
(fiveam:test
  uri-node-helper)

#+(or)
(fiveam:test
  uri-rel-helper)

#+(or)
(fiveam:test
  build-cypher-path)

#+(or)
(fiveam:test
  digest-to-filepath)

#+(or)
(fiveam:test
  replace-all)

#+(or)
(fiveam:test
  escape-neo4j)

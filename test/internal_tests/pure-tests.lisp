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
  pure
  :description "Tests for pure code, i.e. code with no side-effects."
  :in main)

(fiveam:in-suite pure)


(fiveam:test
  schema-hash-basic
  "Basic tests of functions and methods on a schema implemented as a hash-table."
  (let ((schema (restagraph::make-schema-hash-table)))
    (setf (gethash "foo" schema)
          (restagraph::make-schema-rtypes :name "foo"
                                          :dependent nil
                                          :description nil
                                          :attributes nil
                                          :relationships nil))
    (setf (gethash "bar" schema)
          (restagraph::make-schema-rtypes
            :name "bar"
            :dependent nil
            :description "For testing attribute validation"
            :attributes (list (restagraph::make-schema-rtype-attrs
                                :name "baz"
                                :description "Valid attribute."))))
    ;; Confirm it isn't dependent
    (fiveam:is (null (restagraph::dependent (gethash "foo" schema))))))

(fiveam:test
  validate-attributes
  "Check the validation of attributes"
  (let ((attrs (list (restagraph::make-schema-rtype-attrs
                       :NAME "status"
                       :DESCRIPTION "Task status."
                       :attr-values '("idea"
                                 "active"
                                 "waiting"
                                 "scheduled"
                                 "done"
                                 "cancelled"))
                     (restagraph::make-schema-rtype-attrs
                       :NAME "urgency"
                       :DESCRIPTION "How soon it needs to be done.")
                     (restagraph::make-schema-rtype-attrs
                       :NAME "importance"
                       :DESCRIPTION "How important it is that it's done.")
                     (restagraph::make-schema-rtype-attrs
                       :NAME "scale"
                       :DESCRIPTION "How big the job appears to be.")
                     (restagraph::make-schema-rtype-attrs
                       :NAME "deadline"
                       :DESCRIPTION "When the task should be done by.")
                     (restagraph::make-schema-rtype-attrs
                       :NAME "description"
                       :DESCRIPTION "More details about the task."
                       :attr-values '())
                     (restagraph::make-schema-rtype-attrs
                       :NAME "scheduled"
                       :DESCRIPTION "A date/time."))))
    ;; Simple check for no attributes at all
    (fiveam:is (equal '(nil nil)
                      (restagraph::validate-attributes '() attrs)))
    ;; Simple check for valid attribute
    (fiveam:is (equalp '(nil nil)
                       (restagraph::validate-attributes '(("status" . "active")) attrs)))
    ;; Simple check for invalid attribute
    (fiveam:is (equalp '((("foo" . "active")) nil)
                       (restagraph::validate-attributes '(("foo" . "active")) attrs)))
    ;; Simple check for invalid value
    (fiveam:is (equalp '(nil (("status" . "inactive")))
                       (restagraph::validate-attributes '(("status" . "inactive")) attrs)))
    ;; Value with a null enum set
    (fiveam:is (equalp '(nil nil)
                       (restagraph::validate-attributes '(("description" . "I love kung foooooo!")) attrs)))
    ;; Obvious combo-check
    (fiveam:is (equalp '((("foo" . "active")) (("status" . "inactive")))
                       (restagraph::validate-attributes '(("status" . "active")
                                                          ("foo" . "active")
                                                          ("description" . "The legends were true.")
                                                          ("status" . "inactive"))
                                                        attrs)))))

(fiveam:test
  process-filter
  "Check the filtering of GET query parameters."
  (let ((schema (restagraph::fetch-current-schema *server*)))
    ;; Add test-specific resourcetypes
    (setf (gethash "EnumTest" schema)
          (restagraph::make-schema-rtypes
            :name "EnumTest"
            :description "For testing enum values"
            :attributes (list (make-instance
                                'restagraph::schema-rtype-attrs
                                :name "carl"
                                :description "It needed a name, alright?"
                                :attr-values '("one" "two" "three")))))
    ;; On with the tests
    (restagraph::log-message :DEBUG "TEST: null filter")
    (fiveam:is (null (restagraph::process-filter '() schema "any")))
    (restagraph::log-message :DEBUG "TEST: regex filter")
    (fiveam:is (equal "n.foo =~ '.*foo.*'"
                      (restagraph::process-filter '("foo" . ".*foo.*") schema "People")))
    (restagraph::log-message :DEBUG "TEST: negated regex filter")
    (fiveam:is (equal "NOT n.foo =~ '.*foo.*'"
                      (restagraph::process-filter '("foo" . "!.*foo.*") schema "People")))
    (restagraph::log-message :DEBUG "TEST: existence filter")
    (fiveam:is (equal "exists(n.foo)"
                      (restagraph::process-filter '("foo" . "exists") schema "People")))
    (restagraph::log-message :DEBUG "TEST: negated existence filter")
    (fiveam:is (equal "NOT exists(n.foo)"
                      (restagraph::process-filter '("foo" . "!exists") schema "People")))
    (restagraph::log-message :DEBUG "TEST: enum filter")
    (fiveam:is (equal "n.carl IN [\"one\"]"
                      (restagraph::process-filter '("carl" . "one") schema "EnumTest")))
    (fiveam:is (equal "n.carl IN [\"one\", \"three\"]"
                      (restagraph::process-filter '("carl" . "one,three") schema "EnumTest")))
    ;; This next one _should_ throw some kind of error, because it's not a valid value.
    ;; However, that will have to wait until I figure out how to usefull report on invalid filters.
    (fiveam:is (equal "n.carl IN [\"four\"]"
                      (restagraph::process-filter '("carl" . "four") schema "EnumTest")))
    (restagraph::log-message :DEBUG "TEST: text filter")
    (fiveam:is (equal "n.displayname = 'blah'"
                      (restagraph::process-filter '("displayname" . "blah") schema "People")))
    (restagraph::log-message :DEBUG "TEST: negated text filter")
    (fiveam:is (equal "NOT n.foo = 'blah'"
                      (restagraph::process-filter '("foo" . "!blah") schema "People")))
    (restagraph::log-message :DEBUG "TEST: outbound filters")
    (fiveam:is (equal "(n)-[:TAGS]->(:Tags { uid: 'thisTag' })"
                      (restagraph::process-filter '("RGoutbound" . "/TAGS/Tags/thisTag")
                                                  schema
                                                  "People")))
    (fiveam:is (equal "(n)-[:THINGS]->(:Things { uid: 'this' })-[:RELATES_TO]->(:Things { uid: 'that' })"
                      (restagraph::process-filter
                        '("RGoutbound" . "/THINGS/Things/this/RELATES_TO/Things/that")
                        schema
                        "People")))
    (restagraph::log-message :DEBUG "TEST: negated outbound filters")
    (fiveam:is (equal "NOT (n)-[:TAGS]->(:Tags { uid: 'thisTag' })"
                      (restagraph::process-filter '("RGoutbound" . "!/TAGS/Tags/thisTag")
                                                  schema
                                                  "People")))
    (fiveam:is (equal "NOT (n)-[:THINGS]->(:Things { uid: 'this' })-[:RELATES_TO]->(:Things { uid: 'that' })"
                      (restagraph::process-filter
                        '("RGoutbound" . "!/THINGS/Things/this/RELATES_TO/Things/that")
                        schema
                        "People")))
    (restagraph::log-message :DEBUG "TEST: inbound filters")
    (fiveam:is (equal "(:Things { uid: 'this' })-[:RELATES_TO]->(n)"
                      (restagraph::process-filter '("RGinbound" . "/Things/this/RELATES_TO")
                                                  schema
                                                  "People")))
    (restagraph::log-message :DEBUG "TEST: inbound filters with wildcards")
    (fiveam:is (equal "(:Things)-[:RELATES_TO]->(n)"
                      (restagraph::process-filter '("RGinbound" . "/Things/*/RELATES_TO")
                                                  schema
                                                  "People")))
    (fiveam:is (equal "(:Things { uid: 'this' })-[:SUBTHINGS]->(:Things { uid: 'that' })-[:RELATES_TO]->(n)"
                      (restagraph::process-filter
                        '("RGinbound" . "/Things/this/SUBTHINGS/Things/that/RELATES_TO")
                        schema
                        "People")))
    (restagraph::log-message :DEBUG "TEST: negated inbound filters")
    (fiveam:is (equal "NOT (:Things { uid: 'this' })-[:RELATES_TO]->(n)"
                      (restagraph::process-filter '("RGinbound" . "!/Things/this/RELATES_TO")
                                                  schema
                                                  "People")))
    (fiveam:is (equal "NOT (:Things { uid: 'this' })-[:SUBTHINGS]->(:Things { uid: 'that' })-[:RELATES_TO]->(n)"
                      (restagraph::process-filter
                        '("RGinbound" . "!/Things/this/SUBTHINGS/Things/that/RELATES_TO")
                        schema
                        "People")))))

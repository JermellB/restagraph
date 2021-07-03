;   Copyright 2021 James Fleming <james@electronic-quill.net>
;
;   Licensed under the GNU General Public License
;   - for details, see LICENSE.txt in the top-level directory


;;;; Test suite for pure code.
;;;; I.e, functions and methods without side-effects.

(in-package #:restagraph-test)

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
                                          :notes nil
                                          :attributes nil
                                          :relationships nil))
    (setf (gethash "bar" schema)
          (restagraph::make-schema-rtypes
            :name "bar"
            :dependent nil
            :notes "For testing attribute validation"
            :attributes (list (restagraph::make-incoming-rtype-attrs
                                :name "baz"
                                :description "Valid attribute."))))
    ;; Confirm it isn't dependent
    (fiveam:is (null (restagraph::dependent-resource-p schema "foo")))))

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

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
            :attributes (list (make-instance 'restagraph::schema-rtype-attr-varchar
                                :name "baz"
                                :description "Valid attribute."))))
    ;; Confirm it isn't dependent
    (fiveam:is (null (restagraph::dependent (gethash "foo" schema))))))

(fiveam:test
  validate-attributes
  "Check the validation of attribute values, as supplied by a client for updating a resource instance."
  (let ((defined (list (make-instance 'restagraph::schema-rtype-attr-varchar
                                      :name "status"
                                      :description "Task status."
                                      :attrvalues '("idea"
                                                    "active"
                                                    "waiting"
                                                    "scheduled"
                                                    "done"
                                                    "cancelled"))
                       (make-instance 'restagraph::schema-rtype-attr-text
                                      :name "description"
                                      :description "More details about the task.")
                       (make-instance 'restagraph::schema-rtype-attr-integer
                                      :name "numeric"
                                      :description "Something something numbers"
                                      :minimum -5
                                      :maximum 20)
                       (make-instance 'restagraph::schema-rtype-attr-boolean
                                      :name "truefalse"
                                      :description "Well, it's one of those."))))
    ;; Simple check for no attributes at all
    (fiveam:is (equal '(nil nil nil)
                      (restagraph::validate-attributes '() defined)))
    ;; Varchar
    ;; Simple check for valid attribute
    (fiveam:is (equalp '((("status" . "active")) nil nil)
                       (restagraph::validate-attributes '(("status" . "active")) defined)))
    ;; Simple check for invalid attribute
    (fiveam:is (equalp '(nil (("foo" . "active")) nil)
                       (restagraph::validate-attributes '(("foo" . "active")) defined)))
    ;; Simple check for invalid value
    (fiveam:is (equalp '(nil nil (("status" . "inactive")))
                       (restagraph::validate-attributes '(("status" . "inactive")) defined)))
    ;; Combo-check for enums
    (fiveam:is (equalp '((("status" . "active") ("description" . "The legends were true."))
                         (("foo" . "active"))
                         (("status" . "inactive")))
                       (restagraph::validate-attributes '(("status" . "active")
                                                          ("foo" . "active")
                                                          ("description" . "The legends were true.")
                                                          ("status" . "inactive"))
                                                        defined)))
    ;; Text
    ;; Basic validation
    (fiveam:is (equalp '((("description" . "I love kung foooooo!")) nil nil)
                       (restagraph::validate-attributes '(("description" . "I love kung foooooo!")) defined)))
    ;; Integer
    ;; Basic validation
    (fiveam:is (equalp '((("numeric" . 0)) nil nil)
                       (restagraph::validate-attributes '(("numeric" . 0)) defined)))
    ;; Basic validation with string-parsing
    (fiveam:is (equalp '((("numeric" . 0)) nil nil)
                       (restagraph::validate-attributes '(("numeric" . "0")) defined)))
    ;; Enforce the minimum
    (fiveam:is (equalp '(nil nil (("numeric" . -10)))
                       (restagraph::validate-attributes '(("numeric" . -10)) defined)))
    (fiveam:is (equalp '(nil nil (("numeric" . "-10")))
                       (restagraph::validate-attributes '(("numeric" . "-10")) defined)))
    ;; Enforce the maximum
    (fiveam:is (equalp '(nil nil (("numeric" . 30)))
                       (restagraph::validate-attributes '(("numeric" . 30)) defined)))
    ;; Boolean
    ;; Basic validation
    (fiveam:is (equalp '((("truefalse" . :false)) nil nil)
                       (restagraph::validate-attributes '(("truefalse" . nil)) defined)))
    (fiveam:is (equalp '((("truefalse" . :true)) nil nil)
                       (restagraph::validate-attributes '(("truefalse" . t)) defined)))
    ;; Reject invalid values
    (fiveam:is (equalp '(nil nil (("truefalse" . "true")))
                       (restagraph::validate-attributes '(("truefalse" . "true")) defined)))))


(fiveam:test
  any-readonly-attrs
  "Test the process of checking for read-only attributes."
  (restagraph::log-message :debug ";TEST: Simplest case: null vs null.")
  (fiveam:is (null (restagraph::any-readonly-attrs
                     (restagraph::make-schema-rtypes :name "EmptyType")
                     '())))
  (restagraph::log-message :debug ";TEST: Non-empty parameters, only non-read-only attrs.")
  (fiveam:is (null (restagraph::any-readonly-attrs
                     (restagraph::make-schema-rtypes
                       :name "NonReadOnly"
                       :description "Has attributes, but none are read-only."
                       :attributes (list
                                     (make-instance 'restagraph::schema-rtype-attr-varchar
                                                    :name "foo")))
                     '(("foo" . "bar")
                       ("baz" . "quux")))))
  (restagraph::log-message :debug ";TEST: Null resourcetype")
  (fiveam:is (null (restagraph::any-readonly-attrs nil '())))
  (restagraph::log-message :debug ";TEST: One read-only attribute in both.")
  (fiveam:is (equal (restagraph::any-readonly-attrs
                      (restagraph::make-schema-rtypes
                        :name "OneReadOnly"
                        :description "Has a single read-only attribute."
                        :attributes (list
                                      (make-instance 'restagraph::schema-rtype-attr-varchar
                                                     :name "sha3256sum"
                                                     :readonly t)))
                      '(("sha3256sum" . "asdh2676trhfgh")))
                    '("sha3256sum")))
  (restagraph::log-message :debug ";TEST: One read-only attribute in schema, none in input.")
  (fiveam:is (equal (restagraph::any-readonly-attrs
                      (restagraph::make-schema-rtypes
                        :name "OneReadOnly"
                        :description "Has a single read-only attribute."
                        :attributes (list
                                      (make-instance 'restagraph::schema-rtype-attr-varchar
                                                     :name "sha3256sum"
                                                     :readonly t)))
                      '(("foo" . "bar")))
                    nil)))

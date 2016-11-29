;;;; Test suite for all of syscat
;;;;
;;;; Beware: it currently only tests _expected_ cases,
;;;; and does not test edge-cases or wrong input.

(in-package #:restagraph-test)

(defparameter *server*
  (restagraph::datastore restagraph::*restagraph-acceptor*))

(fiveam:def-suite main)
(fiveam:in-suite main)

(fiveam:test
  rg-schema
  "Check the core schema operations"
  (let ((schema (make-hash-table :test 'equal)))
  ;; Add a class to the schema
  (fiveam:is (hash-table-p (restagraph::add-class-to-schema schema "foo")))
  ;; Confirm the class' presence in the schema
  (fiveam:is (hash-table-p (restagraph::get-class-from-schema-by-name schema "foo")))
  ;; Add another class
  (fiveam:is (hash-table-p (restagraph::add-class-to-schema schema "bar")))
  ;; Confirm that we now have exactly the two classes in the schema that we expected
  (fiveam:is (hash-table-p (restagraph::get-class-from-schema-by-name schema "foo")))
  (fiveam:is (hash-table-p (restagraph::get-class-from-schema-by-name schema "bar")))
  ;; Add a relationship between the classes
  (fiveam:is (listp (restagraph::add-class-relationship-to-schema schema "foo" "is-a" "bar")))
  ;; Confirm that the relationship is present
  ))

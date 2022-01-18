;   Copyright 2022 James Fleming <james@electronic-quill.net>
;
;   Licensed under the GNU General Public License
;   - for details, see LICENSE.txt in the top-level directory


;;;; Test suite for side-effecting code pertaining to relationships.

(in-package #:restagraph-test)

(declaim (optimize (compilation-speed 0)
                   (speed 2)
                   (safety 3)
                   (debug 3)))


(fiveam:def-suite
  side-effecting-relationships
  :description "Tests for side-effecting code pertaining to relationships."
  :in main)

(fiveam:in-suite side-effecting-relationships)

(fiveam:test
  relationships
  :depends-on 'resources-basic
  "Basic operations on relationships between resources"
  (let* ((schema-version (restagraph::create-new-schema-version *server*))
         (to-type (restagraph::make-incoming-rtypes :name "Asn"))
         (from-type (restagraph::make-incoming-rtypes :name "Routers"))
         (relationship (restagraph::make-incoming-rels :NAME "ASN"
                                                     :SOURCE-TYPE (restagraph::name from-type)
                                                     :CARDINALITY "many:many"
                                                     :TARGET-TYPE (restagraph::name to-type)))
         (from-uid "bikini")
         (to-uid "64512"))
    ;; Create the fixtures
    (restagraph::log-message :info ";TEST Create the fixtures")
    ;; Install the core schema in the new schema-version
    (restagraph::install-subschema *server* restagraph::*core-schema* schema-version)
    ;; Add the new resourcetypes and relationships
    (restagraph::install-subschema-resourcetype *server* from-type schema-version)
    (restagraph::install-subschema-resourcetype *server* to-type schema-version)
    (restagraph::install-subschema-relationship *server* relationship schema-version)
    (let ((schema (restagraph::fetch-current-schema *server*)))
      ;; Store the router
      (restagraph::log-message :info ";TEST Creating the resources")
      (restagraph::store-resource *server* schema (restagraph::name from-type) `(("uid" . ,from-uid)) *admin-user*)
      ;; Create the interface
      (restagraph::store-resource *server* schema (restagraph::name to-type) `(("uid" . ,to-uid)) *admin-user*)
      ;; Create a relationship between them
      (restagraph::log-message :info (format nil ";TEST Create the relationship /~A/~A/~A/~A/~A"
                                             (restagraph::name from-type)
                                             from-uid
                                             (restagraph::name relationship)
                                             (restagraph::name to-type)
                                             to-uid))
      (multiple-value-bind (result code message)
        (restagraph::create-relationship-by-path
          *server*
          (format nil "/~A/~A/~A"
                  (restagraph::name from-type)
                  from-uid
                  (restagraph::name relationship))
          (format nil "/~A/~A"
                  (restagraph::name to-type)
                  to-uid)
          schema)
        (declare (ignore result) (ignore message))
        (fiveam:is (equal 200 code)))
      ;; Confirm the relationship is there
      (restagraph::log-message
        :info
        (format nil ";TEST Confirm the list of resources at the end of /~A/~A/~A"
                (restagraph::name from-type)
                from-uid
                relationship))
      (let ((result (car (restagraph::get-resources
                           *server*
                           (format nil "/~A/~A/~A"
                                   (restagraph::name from-type)
                                   from-uid
                                   (restagraph::name relationship))))))
        (fiveam:is (equal (restagraph::name to-type)
                          (cdr (assoc :TYPE result))))
        (fiveam:is (equal to-uid (cdr (assoc :UID result)))))
      ;; Delete the relationship
      (restagraph::log-message :info (format nil ";TEST Delete the relationship from /~A/~A/~A to /~A/~A"
                                             (restagraph::name from-type)
                                             from-uid
                                             (restagraph::name relationship)
                                             (restagraph::name to-type)
                                             to-uid))
      (multiple-value-bind (result code message)
        (restagraph::delete-relationship-by-path
          *server*
          schema
          (format nil "/~A/~A/~A" (restagraph::name from-type) from-uid (restagraph::name relationship))
          (format nil "/~A/~A" (restagraph::name to-type) to-uid))
        (declare (ignore result))
        (restagraph::log-message :debug (format nil "Result of deletion request: ~A - ~A" code message))
        (fiveam:is (equal 200 code)))
      ;; Delete the router
      (restagraph::log-message :info ";TEST Cleanup: removing the resources")
      (restagraph::delete-resource-by-path *server*
                                           (format nil "/~A/~A" (restagraph::name from-type) from-uid)
                                           schema)
      ;; Delete the interface
      (restagraph::delete-resource-by-path *server*
                                           (format nil "/~A/~A" (restagraph::name to-type) to-uid)
                                           schema)
      ;; Delete the new schema-version
      (restagraph::delete-schema-version *server* schema-version))))

(fiveam:test
  relationships-to-any
  :depends-on 'relationships
  "Confirm that we can create relationships with a defined target of 'any',
   but can't create 'just any' relationship."
  (let ((source-type "Test")
        (newrel "FROBS")
        (source-uid "Whoomp")
        (target-type "Tags")
        (target-uid "ThereItIs")
        ;(invalid-sourcetype "People")
        (schema-version (restagraph::create-new-schema-version *server*)))
    ;; Install the core schema in the new schema-version
    (restagraph::install-subschema *server* restagraph::*core-schema* schema-version)
    ;; Define a relationship with a target-resource of "any".
    ;; To keep things simple, create a new type altogether as its source-type.
    (restagraph::install-subschema-resourcetype
      *server*
      (restagraph::make-incoming-rtypes :name source-type :dependent nil)
      schema-version)
    (restagraph::install-subschema-relationship
      *server*
      (restagraph::make-incoming-rels :name newrel
                                      :source-type source-type
                                      :target-type target-type
                                      :cardinality "many:many"
                                      :dependent nil
                                      :description "Test relationship to 'any'")
      schema-version)
    (let ((schema (restagraph::fetch-current-schema *server*)))
      ;; Create test instances
      (restagraph::store-resource *server* schema source-type `(("uid" . ,source-uid)) *admin-user*)
      (restagraph::store-resource *server* schema target-type `(("uid" . ,target-uid)) *admin-user*)
      ;; Create a relationship from the instance
      (fiveam:is (restagraph::create-relationship-by-path *server*
                                                          (format nil "/~A/~A/~A" source-type source-uid newrel)
                                                          (format nil "/~A/~A" target-type target-uid)
                                                          schema))
      ;; Fail to create an invalid relationship from another resourcetype
      (fiveam:signals
        restagraph::integrity-error
        (restagraph::create-relationship-by-path *server*
                                                 (format nil "/~A/~A/~A" source-type source-uid newrel)
                                                 (format nil "/~A/~A" target-type target-uid)
                                                 schema))
      ;; Delete the test instances
      (restagraph::delete-resource-by-path *server* (format nil "/~A/~A" source-type source-uid) schema)
      (restagraph::delete-resource-by-path *server* (format nil "/~A/~A" target-type target-uid) schema))
    ;; Delete the temporary schema-version
    (restagraph::delete-schema-version *server* schema-version)))

(fiveam:test
  relationships-integrity
  :depends-on 'relationships
  "Basic operations on relationships between resources"
  (let* ((schema-version (restagraph::create-new-schema-version *server*))
         (from-type (restagraph::make-incoming-rtypes :name "Routers"))
         (to-type (restagraph::make-incoming-rtypes :name "Asn"))
         (relationship
           (restagraph::make-incoming-rels
             :NAME "ASN"
             :CARDINALITY "many:many"
             :SOURCE-TYPE (restagraph::name from-type)
             :TARGET-TYPE (restagraph::name to-type)))
         (from-uid "bikini")
         (to-uid "64512"))
    ;; Create the fixtures
    (restagraph::log-message :info ";TEST Create the fixtures")
    ;; Install the core schema in the new schema-version
    (restagraph::install-subschema *server* restagraph::*core-schema* schema-version)
    ;; Install the new resourcetypes and relationship
    (restagraph::install-subschema-resourcetype *server* from-type schema-version)
    (restagraph::install-subschema-resourcetype *server* to-type schema-version)
    (restagraph::install-subschema-relationship *server* relationship schema-version)
    (let ((schema (restagraph::fetch-current-schema *server*)))
      ;; Create the resources
      (restagraph::log-message :info ";TEST Creating the resources")
      (restagraph::store-resource *server*
                                  schema
                                  (restagraph::name from-type)
                                  `(("uid" . ,from-uid))
                                  *admin-user*)
      ;; Create the interface
      (restagraph::store-resource *server*
                                  schema
                                  (restagraph::name to-type)
                                  `(("uid" . ,to-uid))
                                  *admin-user*)
      ;; Create a relationship between them
      (restagraph::log-message :info ";TEST Create a relationship between them")
      (multiple-value-bind (result code message)
        (restagraph::create-relationship-by-path
          *server*
          (format nil "/~A/~A/~A" (restagraph::name from-type) from-uid (restagraph::name relationship))
          (format nil "/~A/~A" (restagraph::name to-type) to-uid)
          schema)
        (declare (ignore result) (ignore message))
        (fiveam:is (equal 200 code)))
      ;; Confirm the relationship is there
      (restagraph::log-message :info ";TEST Confirm that the relationship is there")
      (let ((result (restagraph::get-resources *server*
                                               (format nil "/~A/~A/~A"
                                                       (restagraph::name from-type)
                                                       from-uid
                                                       (restagraph::name relationship)))))
        (restagraph::log-message :debug (format nil "Received result ~A" result))
        (fiveam:is (equal 4 (length (car result))))
        (fiveam:is (assoc :TYPE (car result) :test #'equal))
        (fiveam:is (equal (restagraph::name to-type)
                          (cdr (assoc :TYPE (car result) :test #'equal))))
        (fiveam:is (assoc :UID (car result) :test #'equal))
        (fiveam:is (equal to-uid (cdr (assoc :UID (car result) :test #'equal)))))
      ;; Confirm we get what we expect when checking what's at the end of the path
      (restagraph::log-message :info ";TEST Confirm that we get what we expect at the end of the path.")
      (let ((result (restagraph::get-resources *server*
                                               (format nil "/~A/~A/~A"
                                                       (restagraph::name from-type)
                                                       from-uid
                                                       (restagraph::name relationship)))))
        (fiveam:is (equal 4 (length (car result))))
        (fiveam:is (assoc :TYPE (car result)))
        (fiveam:is (equal (restagraph::name to-type)
                          (cdr (assoc :TYPE (car result)))))
        (fiveam:is (assoc :UID (car result)))
        (fiveam:is (equal to-uid (cdr (assoc :UID (car result))))))
      ;; Attempt to create a duplicate relationship between them
      (restagraph::log-message :info ";TEST Attempt to create a duplicate relationship.")
      (fiveam:signals (restagraph::integrity-error
                        (format nil "Relationship ~A already exists from ~A ~A to ~A ~A"
                                (restagraph::name relationship)
                                (restagraph::name from-type)
                                from-uid
                                (restagraph::name to-type)
                                to-uid))
        (restagraph::create-relationship-by-path
          *server*
          (format nil "/~A/~A/~A" (restagraph::name from-type) from-uid (restagraph::name relationship))
          (format nil "/~A/~A" (restagraph::name to-type) to-uid)
          schema))
      ;; Confirm we still only have one relationship between them
      (restagraph::log-message :info ";TEST Confirm we still only have one relationship between them.")
      (let ((result (restagraph::get-resources *server*
                                               (format nil "/~A/~A/~A"
                                                       (restagraph::name from-type)
                                                       from-uid
                                                       (restagraph::name relationship)))))
        (fiveam:is (equal 4 (length (car result))))
        (fiveam:is (assoc :TYPE (car result) :test #'equal))
        (fiveam:is (equal (restagraph::name to-type)
                          (cdr (assoc :TYPE (car result) :test #'equal))))
        (fiveam:is (assoc :UID (car result) :test #'equal))
        (fiveam:is (equal to-uid (cdr (assoc :UID (car result) :test #'equal)))))
      ;; Delete the relationship
      (restagraph::log-message :info ";TEST Delete the relationship.")
      (multiple-value-bind (result code message)
        (restagraph::delete-relationship-by-path
          *server*
          schema
          (format nil "/~A/~A/~A/" (restagraph::name from-type) from-uid (restagraph::name relationship))
          (format nil "/~A/~A/" (restagraph::name to-type) to-uid))
        (declare (ignore result) (ignore message))
        (fiveam:is (equal 200 code)))
      ;; Clean-up: delete the resources
      (restagraph::log-message :info ";TEST Cleaning up: removing the resources")
      (restagraph::delete-resource-by-path *server*
                                           (format nil "/~A/~A" (restagraph::name from-type) from-uid)
                                           schema)
      (restagraph::delete-resource-by-path *server*
                                           (format nil "/~A/~A" (restagraph::name to-type) to-uid)
                                           schema))
    ;; Delete the temporary schema-version
    (restagraph::delete-schema-version *server* schema-version)))

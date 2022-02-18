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
  bolt-side-effecting-relationships
  :description "Tests for side-effecting code pertaining to relationships."
  :in main)

(fiveam:in-suite bolt-side-effecting-relationships)

(fiveam:test
  relationships
  :depends-on 'resources-basic
  "Basic operations on relationships between resources"
  (let* ((to-type (restagraph::make-incoming-rtypes :name "Asn"))
         (from-type (restagraph::make-incoming-rtypes :name "Routers"))
         (relationship (restagraph::make-incoming-rels :NAME "ASN"
                                                     :SOURCE-TYPE (restagraph::name from-type)
                                                     :CARDINALITY "many:many"
                                                     :TARGET-TYPE (restagraph::name to-type)))
         (from-uid "bikini")
         (to-uid "64512")
         (session (neo4cl:establish-bolt-session *bolt-server*))
         (schema-version (restagraph::create-new-schema-version session)))
    ;; Create the fixtures
    (restagraph::log-message :info ";TEST Create the fixtures")
    ;; Install the core schema in the new schema-version
    (restagraph::install-subschema session restagraph::*core-schema* schema-version)
    ;; Add the new resourcetypes and relationships
    (restagraph::install-subschema-resourcetype session from-type schema-version)
    (restagraph::install-subschema-resourcetype session to-type schema-version)
    (restagraph::install-subschema-relationship session relationship schema-version)
    ;; Install the default resources
    (restagraph::install-default-resources session)
    (let ((schema (restagraph::fetch-current-schema session)))
      ;; Store the router
      (restagraph::log-message :info ";TEST Creating the resources")
      (restagraph::store-resource session
                                  schema
                                  (restagraph::name from-type)
                                  `(("uid" . ,from-uid))
                                  *admin-user*)
      ;; Create the interface
      (restagraph::store-resource session
                                  schema
                                  (restagraph::name to-type)
                                  `(("uid" . ,to-uid))
                                  *admin-user*)
      ;; Create a relationship between them
      (restagraph::log-message :info (format nil ";TEST Create the relationship /~A/~A/~A/~A/~A"
                                             (restagraph::name from-type)
                                             from-uid
                                             (restagraph::name relationship)
                                             (restagraph::name to-type)
                                             to-uid))
      (fiveam:is (null (restagraph::create-relationship-by-path
          session
          (format nil "/~A/~A/~A"
                  (restagraph::name from-type)
                  from-uid
                  (restagraph::name relationship))
          (format nil "/~A/~A"
                  (restagraph::name to-type)
                  to-uid)
          schema)))
      ;; Confirm the relationship is there
      (restagraph::log-message
        :info
        (format nil ";TEST Confirm the list of resources at the end of /~A/~A/~A"
                (restagraph::name from-type) from-uid relationship))
      (let ((result (car (restagraph::get-resources
                           session
                           (format nil "/~A/~A/~A"
                                   (restagraph::name from-type)
                                   from-uid
                                   (restagraph::name relationship))))))
        (fiveam:is (equal (restagraph::name to-type)
                          (car result)))
        (fiveam:is (equal to-uid (gethash "uid" (neo4cl:node-properties (cdr result))))))
      ;; Delete the relationship
      (restagraph::log-message :info (format nil ";TEST Delete the relationship from /~A/~A/~A to /~A/~A"
                                             (restagraph::name from-type)
                                             from-uid
                                             (restagraph::name relationship)
                                             (restagraph::name to-type)
                                             to-uid))
      (fiveam:is (null (restagraph::delete-relationship-by-path
                         session
                         schema
                         (format nil "/~A/~A/~A"
                                 (restagraph::name from-type)
                                 from-uid
                                 (restagraph::name relationship))
                         (format nil "/~A/~A" (restagraph::name to-type) to-uid))))
      ;; Delete the router
      (restagraph::log-message :info ";TEST Cleanup: removing the resources")
      (restagraph::delete-resource-by-path session
                                           (format nil "/~A/~A" (restagraph::name from-type) from-uid)
                                           schema)
      ;; Delete the interface
      (restagraph::delete-resource-by-path session
                                           (format nil "/~A/~A" (restagraph::name to-type) to-uid)
                                           schema)
      ;; Delete the new schema-version
      (restagraph::delete-schema-version session schema-version))))

(fiveam:test
  relationships-to-any
  :depends-on 'relationships
  "Confirm that we can create relationships with a defined target of 'any',
   but can't create 'just any' relationship."
  (let* ((source-type "Test")
         (newrel "FROBS")
         (source-uid "Whoomp")
         (target-type "Tags")
         (target-uid "ThereItIs")
         ;(invalid-sourcetype "People")
         (session (neo4cl:establish-bolt-session *bolt-server*))
         (schema-version (restagraph::create-new-schema-version session)))
    ;; Install the core schema in the new schema-version
    (restagraph::install-subschema session restagraph::*core-schema* schema-version)
    ;; Define a relationship with a target-resource of "any".
    ;; To keep things simple, create a new type altogether as its source-type.
    (restagraph::install-subschema-resourcetype
      session
      (restagraph::make-incoming-rtypes :name source-type :dependent nil)
      schema-version)
    (restagraph::install-subschema-relationship
      session
      (restagraph::make-incoming-rels :name newrel
                                      :source-type source-type
                                      :target-type target-type
                                      :cardinality "many:many"
                                      :dependent nil
                                      :description "Test relationship to 'any'")
      schema-version)
    (let ((schema (restagraph::fetch-current-schema session)))
      ;; Install the default resources
      (restagraph::install-default-resources session)
      ;; Create test instances
      (restagraph::store-resource session schema source-type `(("uid" . ,source-uid)) *admin-user*)
      (restagraph::store-resource session schema target-type `(("uid" . ,target-uid)) *admin-user*)
      ;; Create a relationship from the instance
      (fiveam:is (null (restagraph::create-relationship-by-path
                         session
                         (format nil "/~A/~A/~A" source-type source-uid newrel)
                         (format nil "/~A/~A" target-type target-uid)
                         schema)))
      ;; Fail to create an invalid relationship from another resourcetype
      (fiveam:signals
        restagraph::integrity-error
        (restagraph::create-relationship-by-path session
                                                 (format nil "/~A/~A/~A" source-type source-uid newrel)
                                                 (format nil "/~A/~A" target-type target-uid)
                                                 schema))
      ;; Delete the test instances
      (restagraph::delete-resource-by-path session (format nil "/~A/~A" source-type source-uid) schema)
      (restagraph::delete-resource-by-path session (format nil "/~A/~A" target-type target-uid) schema))
    ;; Delete the temporary schema-version
    (restagraph::delete-schema-version session schema-version)
    ;; Clean up the Bolt session
    (neo4cl:disconnect session)))

(fiveam:test
  relationships-integrity
  :depends-on 'relationships
  "Basic operations on relationships between resources"
  (let* ((from-type (restagraph::make-incoming-rtypes :name "Routers"))
         (to-type (restagraph::make-incoming-rtypes :name "Asn"))
         (relationship
           (restagraph::make-incoming-rels
             :NAME "ASN"
             :CARDINALITY "many:many"
             :SOURCE-TYPE (restagraph::name from-type)
             :TARGET-TYPE (restagraph::name to-type)))
         (from-uid "bikini")
         (to-uid "64512")
         (session (neo4cl:establish-bolt-session *bolt-server*))
         (schema-version (restagraph::create-new-schema-version session)))
    ;; Create the fixtures
    (restagraph::log-message :info ";TEST Create the fixtures")
    ;; Install the core schema in the new schema-version
    (restagraph::install-subschema session restagraph::*core-schema* schema-version)
    ;; Install the new resourcetypes and relationship
    (restagraph::install-subschema-resourcetype session from-type schema-version)
    (restagraph::install-subschema-resourcetype session to-type schema-version)
    (restagraph::install-subschema-relationship session relationship schema-version)
    (let ((schema (restagraph::fetch-current-schema session)))
      ;; Install the default resources
      (restagraph::install-default-resources session)
      ;; Create the resources
      (restagraph::log-message :info ";TEST Creating the resources")
      (restagraph::store-resource session
                                  schema
                                  (restagraph::name from-type)
                                  `(("uid" . ,from-uid))
                                  *admin-user*)
      ;; Create the interface
      (restagraph::store-resource session
                                  schema
                                  (restagraph::name to-type)
                                  `(("uid" . ,to-uid))
                                  *admin-user*)
      ;; Create a relationship between them
      (restagraph::log-message :info ";TEST Create a relationship between them")
      (fiveam:is (null (restagraph::create-relationship-by-path
          session
          (format nil "/~A/~A/~A" (restagraph::name from-type) from-uid (restagraph::name relationship))
          (format nil "/~A/~A" (restagraph::name to-type) to-uid)
          schema)))
      ;; Confirm the relationship is there
      (restagraph::log-message :info ";TEST Confirm that the relationship is there")
      (let ((result (car (restagraph::get-resources session
                                                    (format nil "/~A/~A/~A"
                                                            (restagraph::name from-type)
                                                            from-uid
                                                            (restagraph::name relationship))))))
        (restagraph::log-message :debug (format nil "Received result ~A" result))
        (fiveam:is (equal 3 (hash-table-count (neo4cl:node-properties (cdr result)))))
        (fiveam:is (not (null (car result))))
        (fiveam:is (equal (restagraph::name to-type)
                          (car result)))
        (fiveam:is (not (null (gethash "uid" (neo4cl:node-properties (cdr result))))))
        (fiveam:is (equal to-uid
                          (gethash "uid" (neo4cl:node-properties (cdr result))))))
      ;; Confirm we get what we expect when checking what's at the end of the path
      (restagraph::log-message :info ";TEST Confirm that we get what we expect at the end of the path.")
      (let ((result (car (restagraph::get-resources session
                                                    (format nil "/~A/~A/~A"
                                                            (restagraph::name from-type)
                                                            from-uid
                                                            (restagraph::name relationship))))))
        (fiveam:is (equal 3 (hash-table-count (neo4cl:node-properties (cdr result)))))
        (fiveam:is (not (null (car result))))
        (fiveam:is (equal (restagraph::name to-type)
                          (car result)))
        (fiveam:is (not (null (gethash "uid" (neo4cl:node-properties (cdr result))))))
        (fiveam:is (equal to-uid
                          (gethash "uid" (neo4cl:node-properties (cdr result))))))
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
          session
          (format nil "/~A/~A/~A" (restagraph::name from-type) from-uid (restagraph::name relationship))
          (format nil "/~A/~A" (restagraph::name to-type) to-uid)
          schema))
      ;; Confirm we still only have one relationship between them
      (restagraph::log-message :info ";TEST Confirm we still only have one relationship between them.")
      (let ((result (car (restagraph::get-resources session
                                                    (format nil "/~A/~A/~A"
                                                            (restagraph::name from-type)
                                                            from-uid
                                                            (restagraph::name relationship))))))
        (fiveam:is (equal 3 (hash-table-count (neo4cl:node-properties (cdr result)))))
        (fiveam:is (not (null (car result))))
        (fiveam:is (equal (restagraph::name to-type)
                          (car result)))
        (fiveam:is (not (null (gethash "uid" (neo4cl:node-properties (cdr result))))))
        (fiveam:is (equal to-uid
                          (gethash "uid" (neo4cl:node-properties (cdr result))))))
      ;; Delete the relationship
      (restagraph::log-message :info ";TEST Delete the relationship.")
      (fiveam:is (null (restagraph::delete-relationship-by-path
          session
          schema
          (format nil "/~A/~A/~A/" (restagraph::name from-type) from-uid (restagraph::name relationship))
          (format nil "/~A/~A/" (restagraph::name to-type) to-uid))))
      ;; Clean-up: delete the resources
      (restagraph::log-message :info ";TEST Cleaning up: removing the resources")
      (restagraph::delete-resource-by-path session
                                           (format nil "/~A/~A" (restagraph::name from-type) from-uid)
                                           schema)
      (restagraph::delete-resource-by-path session
                                           (format nil "/~A/~A" (restagraph::name to-type) to-uid)
                                           schema))
    ;; Delete the temporary schema-version
    (restagraph::delete-schema-version session schema-version)))

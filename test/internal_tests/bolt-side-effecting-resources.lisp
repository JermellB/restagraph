;   Copyright 2021-2022 James Fleming <james@electronic-quill.net>
;
;   Licensed under the GNU General Public License
;   - for details, see LICENSE.txt in the top-level directory


;;;; Test suite for side-effecting code pertaining mostly to resources.

(in-package #:restagraph-test)

(declaim (optimize (compilation-speed 0)
                   (speed 2)
                   (safety 3)
                   (debug 3)))


(fiveam:def-suite
  bolt-side-effecting-resources
  :description "Tests for side-effecting code pertaining mostly to resources."
  :in main)

(fiveam:in-suite bolt-side-effecting-resources)

(fiveam:test
  default-resources
  "Test installation of default resources."
  :depends-on 'process-filter
  (let* ((session (neo4cl:establish-bolt-session *bolt-server*))
         ;; Ensure there's a new schema-version to work from
         (schema-version (restagraph::create-new-schema-version session)))
    ;; Install the core schema into that new schema-version
    (restagraph::install-subschema session restagraph::*core-schema* schema-version)
    ;; Install the default resources
    (restagraph::install-default-resources session)
    ;; Check that the resources are actually there
    (fiveam:is (equal *admin-user*
                      (gethash "uid"
                               (restagraph::get-resources
                                 session
                                 (format nil "/People/~A" *admin-user*)))))
    ;; Remove the schema we added for this test
    (restagraph::delete-schema-version session schema-version)
    ;; Clean up the session
    (neo4cl:disconnect session)))

(fiveam:test
  resources-basic
  "Basic operations on resources"
  :depends-on 'default-resources
  (let* ((restype "People")
         (uid "Cally")
         (tag1 "telepath")
         (tag2 "crewmember")
         (invalid-type "interfaces")
         (invalid-uid "eth0")
         (session (neo4cl:establish-bolt-session *bolt-server*))
         ;; Ensure there's a new schema-version to work from
         (schema-version (restagraph::create-new-schema-version session)))
    ;; Install the core schema into that new schema-version
    (restagraph::install-subschema session restagraph::*core-schema* schema-version)
    ;; Fetch the schema into memory
    (let ((schema (restagraph::fetch-current-schema session)))
      ;; Install the default resources
      (restagraph::install-default-resources session)
      ;; Confirm the resource isn't already present
      (restagraph::log-message :info ";TEST Confirm the resource isn't already present")
      (fiveam:is (null (restagraph::get-resources session (format nil "/~A/~A" restype uid))))
      ;; Store the resource
      (restagraph::log-message :info ";TEST Store the resource")
      (fiveam:is (equal (restagraph::sanitise-uid uid)
                        (restagraph::store-resource
                          session
                          schema
                          restype
                          `(("uid" . ,uid))
                          *admin-user*)))
      ;; Confirm it's there
      ;; (Test get-resources with mod/3 == 2)
      (restagraph::log-message :info ";TEST Confirm the resource is present")
      (let ((result (restagraph::get-resources session (format nil "/~A/~A" restype uid))))
        (fiveam:is (gethash "uid" result))
        (fiveam:is (equal (restagraph::sanitise-uid uid)
                          (gethash "uid" result)))
        (fiveam:is (gethash "original_uid" result))
        (fiveam:is (equal uid (gethash "original_uid" result))))
      ;; Confirm we can't create a duplicate
      (restagraph::log-message :info ";TEST Confirm refusal to create duplicate resources")
      (fiveam:signals
        (restagraph::integrity-error "Path already exists; refusing to create a duplicate.")
        (restagraph::store-resource session schema restype `(("uid" . ,uid)) *admin-user*))
      ;; Confirm that there are two people (RgAdmin should be the other)
      ;; (Test get-resources with mod/3 == 1)
      (let ((people-list (restagraph::get-resources session (format nil "/~A" restype))))
        (restagraph::log-message :info (format nil ";TEST-INFO fetched list of people: ~A" people-list))
        (fiveam:is (equal 2 (length people-list))))
      ;; Confirm that we get all resource at the end of a relationship
      ;; First, create the tags.
      (restagraph::store-resource session schema "Tags" `(("uid" . ,tag1)) *admin-user*)
      (restagraph::store-resource session schema "Tags" `(("uid" . ,tag2)) *admin-user*)
      (fiveam:is (equal tag1
                        (gethash "uid"
                                 (restagraph::get-resources
                                   session
                                   (format nil "/Tags/~A" tag1)))))
      (fiveam:is (equal tag2
                        (gethash "uid"
                                 (restagraph::get-resources
                                   session
                                   (format nil "/Tags/~A" tag2)))))
      ;; Tag the user
      (restagraph::create-relationship-by-path session
                                               (format nil "/People/~A/TAGS" uid)
                                               (format nil "/Tags/~A" tag1)
                                               schema)
      (restagraph::create-relationship-by-path session
                                               (format nil "/People/~A/TAGS" uid)
                                               (format nil "/Tags/~A" tag2)
                                               schema)
      ;; Confirm we get both tags back
      (fiveam:is (equal 2
                        (length (restagraph::get-resources
                                  session
                                  (format nil "/~A/~A/TAGS" restype uid)))))
      ;; Remove the _relationship_ to one of the tags
      (fiveam:is (null (restagraph::delete-relationship-by-path
                         session
                         schema
                         (format nil "/People/~A/TAGS" uid)
                         (format nil "/Tags/~A" tag1))))
      ;; Confirm we now only have one tag there
      (fiveam:is (equal 1 (length (restagraph::get-resources session (format nil "/~A/~A/TAGS" restype uid)))))
      ;; Delete both tags
      (restagraph::delete-resource-by-path session (format nil "/Tags/~A" tag1) schema)
      (restagraph::delete-resource-by-path session (format nil "/Tags/~A" tag2) schema)
      ;; Delete it
      (restagraph::log-message :info ";TEST Delete the resource")
      (fiveam:is (null (restagraph::delete-resource-by-path
                         session
                         (format nil "/~A/~A" restype uid)
                         schema)))
      ;; Confirm it's gone again
      (restagraph::log-message :info ";TEST Confirm the resource is gone")
      (fiveam:is (null (restagraph::get-resources session (format nil "/~A/~A" restype uid))))
      ;; Ensure we can't create an invalid type
      (restagraph::log-message :info ";TEST Ensure we can't create an invalid type")
      (fiveam:signals
        (restagraph::client-error "No such resourcetype.")
        (restagraph::store-resource session
                                    schema
                                    invalid-type
                                    `(("uid" . ,invalid-uid))
                                    *admin-user*))
      ;; Remove the schema we added for this test
      ;; Clean up the session
      (neo4cl:disconnect session))))

(fiveam:test
  errors-basic
  :depends-on 'resources-basic
  "Errors that can be triggered just by making a bad request"
  (let ((schema (restagraph::make-schema-hash-table))
        (invalid-resourcetype "IjustMadeThisUpNow")
        (valid-resourcetype "People")
        (session (neo4cl:establish-bolt-session *bolt-server*)))
    ;; Create a resource of an invalid type
    (restagraph::log-message :info ";TEST Creating a resource of an invalid type")
    (fiveam:signals (restagraph::client-error
                      (format nil "Requested resource type ~A is not valid."
                              invalid-resourcetype))
                    (restagraph::store-resource session
                                                schema
                                                invalid-resourcetype
                                                '((:foo . "bar"))
                                                *admin-user*))
    ;; Create a resource of a valid type, but without a UID
    (restagraph::log-message :info ";TEST Creating a valid resource without a UID")
    (fiveam:signals (restagraph::client-error "UID must be supplied")
                    (restagraph::store-resource session
                                                schema
                                                valid-resourcetype
                                                '(("foo" . "bar"))
                                                *admin-user*))
    (neo4cl:disconnect session)))

(fiveam:test
  resources-attributes-enums
  :depends-on '(resources-basic schema-versions)
  "Enumerated attributes"
  (let* ((attr1name "state")
         (attr1desc "Whether the port is up or down.")
         (attr1vals '("up" "down"))
         (restype (restagraph::make-incoming-rtypes
                    :name "switchport"
                    :attributes (list (restagraph::make-incoming-rtype-attrs
                                        (list
                                          :type "varchar"
                                          :NAME attr1name
                                          :DESCRIPTION attr1desc
                                          :VALUES attr1vals)))
                    :dependent nil))
         (uid "eth1")
         (attr1valgood "up")
         (attr1valbad "mal")
         (session (neo4cl:establish-bolt-session *bolt-server*))
         (schema-version (restagraph::create-new-schema-version session)))
    ;; Set up the fixtures
    (restagraph::log-message :info ";TEST Set up the fixtures")
    ;; Install the core schema in the new schema-version
    (restagraph::install-subschema session restagraph::*core-schema* schema-version)
    ;; Install the additional resourcetype we need for this test
    (restagraph::install-subschema-resourcetype session restype schema-version)
    ;; Fetch the schema from the database
    (let ((schema (restagraph::fetch-current-schema session)))
      ;; Install the default resources
      (restagraph::install-default-resources session)
      ;; Compare the definition in the schema with what we intended it to be
      (restagraph::log-message :info ";TEST Check the schema for the enum attribute")
      (let ((reference (make-instance 'restagraph::schema-rtype-attr-varchar
                                      :NAME attr1name
                                      :DESCRIPTION attr1desc
                                      :ATTRVALUES attr1vals))
            (retrieved (elt (restagraph::attributes (gethash (restagraph::name restype) schema)) 0)))
        (fiveam:is (equalp (restagraph::name reference) (restagraph::name retrieved)))
        (fiveam:is (equalp (restagraph::description reference) (restagraph::description retrieved)))
        (fiveam:is (equalp (restagraph::attrvalues reference) (restagraph::attrvalues retrieved))))
      ;; Make sure the test resource doesn't already exist
      (restagraph::delete-resource-by-path session
                                           (format nil "/~A/~A"
                                                   (restagraph::name restype)
                                                   uid)
                                           schema)
      ;; Fail to create a resource with an invalid value for the enum
      (restagraph::log-message :info ";TEST Fail to create a resource with an invalid attribute")
      (fiveam:signals restagraph::client-error
                      (restagraph::store-resource session
                                                  schema
                                                  (restagraph::name restype)
                                                  `(("uid" . ,uid)
                                                    (,attr1name . ,attr1valbad))
                                                  *admin-user*))
      ;; Create a resource with a valid value for the enum
      (restagraph::log-message :info ";TEST Create a resource with a valid attribute")
      (fiveam:is
        (equal
          uid
          (restagraph::store-resource session
                                      schema
                                      (restagraph::name restype)
                                      `(("uid" . ,uid) (,attr1name . ,attr1valgood))
                                      *admin-user*)))
      ;; Confirm that we now have this resource with the expected value
      (restagraph::log-message :info ";TEST Check that the resource was created")
      (let ((attr-test (restagraph::get-resources
                         session
                         (format nil "/~A/~A" (restagraph::name restype) uid))))
        (fiveam:is (equal uid
                          (gethash "uid" attr-test)))
        (fiveam:is (equal attr1valgood
                          (gethash attr1name attr-test))))
      ;; Remove it again
      (restagraph::delete-resource-by-path session
                                           (format nil "/~A/~A"
                                                   (restagraph::name restype)
                                                   uid)
                                           schema)
      ;; Create it without the attribute
      (restagraph::store-resource session
                                  schema
                                  (restagraph::name restype)
                                  `(("uid" . ,uid))
                                  *admin-user*)
      ;; Add the attribute
      (fiveam:is (null (restagraph::update-resource-attributes
                         session
                         schema
                         (list (restagraph::name restype) uid)
                         `((,attr1name . ,attr1valgood)))))
      ;;Confirm that the attribute is present
      (let ((attr-test (restagraph::get-resources
                         session
                         (format nil "/~A/~A"
                                 (restagraph::name restype)
                                 uid))))
        (fiveam:is (equal uid
                          (gethash "uid" attr-test)))
        (fiveam:is (equal attr1valgood
                          (gethash attr1name attr-test))))
      ;; Remove the resource again
      (restagraph::delete-resource-by-path session
                                           (format nil "/~A/~A"
                                                   (restagraph::name restype)
                                                   uid)
                                           schema)
      ;; Confirm it's gone
      (restagraph::delete-resource-by-path session
                                           (format nil "/~A/~A"
                                                   (restagraph::name restype)
                                                   uid)
                                           schema)
      ;; Remove the fixtures
      (restagraph::log-message :info ";TEST Remove the fixtures")
      (restagraph::delete-resource-by-path session
                                           (format nil "/~A/~A"
                                                   (restagraph::name restype) uid)
                                           schema)
      (restagraph::delete-schema-version session schema-version))
    ;; Clean up the db session
    (neo4cl:disconnect session)))

(fiveam:test
  resources-attributes-read-only
  :depends-on '(resources-attributes-enums any-readonly-attrs)
  "Confirm that read-only attributes are correctly recorded as such in the database."
  (let* ((session (neo4cl:establish-bolt-session *bolt-server*))
         (schema-version (restagraph::create-new-schema-version session)))
    ;; Pull out specifically the Files resourcetype
    (restagraph::install-subschema session restagraph::*core-schema* schema-version)
    (let ((files-attrs (restagraph::attributes
                         (gethash "Files" (restagraph::fetch-current-schema session)))))
      (fiveam:is (restagraph::readonly
                   (elt (remove-if-not #'(lambda (attr) (equal "mimetype" (restagraph::name attr)))
                                       files-attrs)
                        0)))
      (fiveam:is (not (restagraph::readonly
                        (elt (remove-if-not #'(lambda (attr) (equal "title" (restagraph::name attr)))
                                            files-attrs)
                             0)))))
    ;; Clean up
    (restagraph::delete-schema-version session schema-version)
    (neo4cl:disconnect session)))

(fiveam:test
  resources-dependent-simple
  :depends-on 'resources-basic
  "Basic operations on dependent resources"
  (let* ((child-type (restagraph::make-incoming-rtypes :name "Interfaces"
                                                       :dependent t))
         (relationship "INTERFACES")
         (parent-type (restagraph::make-incoming-rtypes :name "Routers"))
         (parent-rel (restagraph::make-incoming-rels :source-type (restagraph::name parent-type)
                                                     :name relationship
                                                     :reltype "dependent"
                                                     :cardinality "1:many"
                                                     :target-type (restagraph::name child-type)))
         (parent-uid "trinity")
         (child-uid "eth0")
         (invalid-child-type "routers")
         (invalid-child-uid "whitesands")
         (session (neo4cl:establish-bolt-session *bolt-server*))
         (schema-version (restagraph::create-new-schema-version session)))
    ;; Create the fixtures
    (restagraph::log-message :info ";TEST Create the fixtures")
    ;; Install the core schema in the new schema-version
    (restagraph::install-subschema session restagraph::*core-schema* schema-version)
    ;; Install the new resourcetypes and relationship
    (restagraph::install-subschema-resourcetype session child-type schema-version)
    (restagraph::install-subschema-resourcetype session parent-type schema-version)
    (restagraph::install-subschema-relationship session parent-rel schema-version)
    (let ((schema (restagraph::fetch-current-schema session)))
      ;; Install the default resources
      (restagraph::install-default-resources session)
      ;; Fail to create a dependent resourcetype in isolation
      (restagraph::log-message :info ";TEST Fail to create a dependent resource in isolation.")
      (fiveam:signals restagraph::integrity-error
                      (restagraph::store-resource session
                                                  schema
                                                  (restagraph::name child-type)
                                                  `(("uid" . ,child-uid))
                                                  *admin-user*))
      ;; Create the parent resource
      (restagraph::log-message :info ";TEST Create the parent resource.")
      (restagraph::store-resource session
                                  schema
                                  (restagraph::name parent-type)
                                  `(("uid" . ,parent-uid))
                                  *admin-user*)
      ;; Create the dependent resource
      (restagraph::log-message :debug (format nil ";TEST Create the dependent resource /~A/~A/~A/~A"
                                              (restagraph::name parent-type)
                                              parent-uid
                                              relationship
                                              (restagraph::name child-type)))
      (fiveam:is (null (restagraph::store-dependent-resource
                         session
                         schema
                         (format nil "/~A/~A/~A/~A"
                                 (restagraph::name parent-type)
                                 parent-uid
                                 relationship
                                 (restagraph::name child-type))
                         `(("uid" . ,child-uid))
                         *admin-user*)))
      ;; Confirm it's the only member of the parent's dependents
      (restagraph::log-message :debug ";TEST confirm this is an only child")
      (fiveam:is (equal `((,relationship
                            ,(restagraph::name child-type)
                            ,child-uid))
                        (restagraph::get-dependent-resources
                          session
                          schema
                          (list (restagraph::name parent-type) parent-uid))))
      ;; Confirm we get the type when asking for all things with that relationship
      (restagraph::log-message :debug ";TEST Confirm listing of types with all things with this relationship")
      (let ((result (car (restagraph::get-resources
                           session
                           (format nil "/~A/~A/~A"
                                   (restagraph::name parent-type)
                                   parent-uid
                                   relationship)))))
        (fiveam:is (not (null (gethash "type" result))))
        (fiveam:is (equal (restagraph::sanitise-uid (restagraph::name child-type))
                          (gethash "type" result)))
        (fiveam:is (gethash "uid" result))
        (fiveam:is (equal (restagraph::sanitise-uid child-uid)
                          (gethash "uid" result)))
        (fiveam:is (gethash "original_uid" result))
        (fiveam:is (equal (restagraph::sanitise-uid child-uid)
                          (gethash "original_uid" result))))
      ;; Delete the dependent resource
      (restagraph::log-message :debug ";TEST Delete the dependent resource")
      (let ((child-path (format nil "/~A/~A/~A/~A/~A"
                                (restagraph::name parent-type)
                                parent-uid
                                relationship
                                (restagraph::name child-type)
                                child-uid)))
        (fiveam:is (null (restagraph::delete-resource-by-path
                           session
                           child-path
                           schema)))
        ;; Confirm the dependent resource is gone
        (restagraph::log-message :debug ";TEST Confirm the dependent resource is gone.")
        (fiveam:is (null (restagraph::get-resources session child-path))))
      ;; Attempt to create a child resource that isn't of a dependent type
      (restagraph::log-message
        :debug
        (format nil ";TEST Fail to create a non-dependent child resource /~A/~A/~A/~A"
                (restagraph::name parent-type)
                parent-uid
                relationship
                invalid-child-type))
      (fiveam:signals (restagraph::client-error "This is not a dependent resource type")
                      (restagraph::store-dependent-resource
                        session
                        schema
                        (format nil "/~A/~A/~A/~A"
                                (restagraph::name parent-type)
                                parent-uid
                                relationship
                                invalid-child-type)
                        `(("uid" . ,invalid-child-uid))
                        *admin-user*))
      ;; Create the dependent resource yet again
      (restagraph::log-message :debug ";TEST Sucessfully re-create the dependent resource")
      (restagraph::store-dependent-resource
        session
        schema
        (format nil "/~A/~A/~A/~A"
                (restagraph::name parent-type)
                parent-uid
                relationship
                (restagraph::name child-type))
        `(("uid" . ,child-uid))
        *admin-user*)
      ;; Delete the parent resource
      (restagraph::log-message :info ";TEST Recursively deleting the parent resource")
      (restagraph::delete-resource-by-path
        session
        (format nil "/~A/~A" (restagraph::name parent-type) parent-uid)
        schema
        :recursive t)
      ;; Confirm the dependent resource was recursively deleted with it
      (restagraph::log-message :info ";TEST Confirm the parent resource is gone")
      (fiveam:is (null (restagraph::get-resources
                         session
                         (format nil "/~A/~A" (restagraph::name parent-type)
                                 parent-uid))))
      (restagraph::log-message :info ";TEST Confirm the dependent resource is gone")
      (fiveam:is (null (restagraph::get-resources
                         session
                         (format nil "/~A/~A" (restagraph::name child-type)
                                 child-uid))))
      ;; Remove the newly-created schema-version
      (restagraph::delete-schema-version session schema-version))
    ;; Clean up the Bolt session
    (neo4cl:disconnect session)))

(fiveam:test
  resources-dependent-single-parent
  :depends-on 'resources-dependent-simple
  "Ensure dependent resources can only have a single parent."
  (let* ((child1-type (restagraph::make-incoming-rtypes :name "Models"
                                                        :dependent t))
         (child1-uid "Synthetics")
         (parent1-type (restagraph::make-incoming-rtypes :name "Makes"))
         (parent1-rel (restagraph::make-incoming-rels :name "PRODUCES"
                                                      :source-type (restagraph::name parent1-type)
                                                      :target-type (restagraph::name child1-type)
                                                      :reltype "dependent"
                                                      :cardinality "1:many"))
         (parent1-uid "Weyland-Yutani")
         (parent2-uid "Tyrell")
         (session (neo4cl:establish-bolt-session *bolt-server*))
         (schema-version (restagraph::create-new-schema-version session)))
    (restagraph::log-message :info ";TEST Create the fixtures")
    ;; Install the core schema in the new schema-version
    (restagraph::install-subschema session restagraph::*core-schema* schema-version)
    ;; Add the test-specific resources and relationships
    (restagraph::install-subschema-resourcetype session child1-type schema-version)
    (restagraph::install-subschema-resourcetype session parent1-type schema-version)
    (restagraph::install-subschema-relationship session parent1-rel schema-version)
    ;; Fetch the updated schema definition and start the tests
    (let ((schema (restagraph::fetch-current-schema session)))
      ;; Install the default resources
      (restagraph::install-default-resources session)
      ;; Create the feasible parent
      (restagraph::store-resource session
                                  schema
                                  (restagraph::name parent1-type)
                                  `(("uid" . ,parent1-uid))
                                  *admin-user*)
      ;; Create the child
      (restagraph::store-dependent-resource
        session
        schema
        (format nil "/~A/~A/~A/~A"
                (restagraph::name parent1-type)
                parent1-uid
                (restagraph::name parent1-rel)
                (restagraph::name child1-type))
        `(("uid" . ,child1-uid))
        *admin-user*)
      ;; Confirm the child is there
      (fiveam:is (restagraph::get-resources
                   session
                   (format nil "/~A/~A/~A/~A/~A"
                           (restagraph::name parent1-type)
                           parent1-uid
                           (restagraph::name parent1-rel)
                           (restagraph::name child1-type)
                           child1-uid)))
      ;; Create the new parent
      (restagraph::store-resource session
                                  schema
                                  (restagraph::name parent1-type)
                                  `(("uid" . ,parent2-uid))
                                  *admin-user*)
      ;; Fail to add a duplicate parent-child relationship
      (fiveam:signals
        restagraph::integrity-error
        (restagraph::create-relationship-by-path
          session
          (format nil "/~A/~A/~A"
                  (restagraph::name parent1-type)
                  parent2-uid
                  (restagraph::name parent1-rel))
          (format nil "/~A/~A/~A/~A/~A"
                  (restagraph::name parent1-type)
                  parent1-uid
                  (restagraph::name parent1-rel)
                  (restagraph::name child1-type)
                  child1-uid)
          schema))
      ;; Move the child to the new parent
      (fiveam:is
        (null
          (restagraph::move-dependent-resource
            session
            schema
            (format nil "/~A/~A/~A/~A/~A"
                    (restagraph::name parent1-type)
                    parent1-uid
                    (restagraph::name parent1-rel)
                    (restagraph::name child1-type)
                    child1-uid)
            (format nil "/~A/~A/~A"
                    (restagraph::name parent1-type)
                    parent2-uid
                    (restagraph::name parent1-rel)))))
      ;; Confirm the child is now reachable under the new parent
      (fiveam:is (restagraph::get-resources
                   session
                   (format nil "/~A/~A/~A/~A/~A"
                           (restagraph::name parent1-type)
                           parent2-uid
                           (restagraph::name parent1-rel)
                           (restagraph::name child1-type)
                           child1-uid)))
      ;; Confirm the child is no longer reachable under the previous parent
      (fiveam:is
        (null
          (restagraph::get-resources
            session
            (format nil "/~A/~A/~A/~A/~A"
                    (restagraph::name parent1-type)
                    parent1-uid
                    (restagraph::name parent1-rel)
                    (restagraph::name child1-type)
                    child1-uid))))
      ;; Clean up
      (restagraph::log-message :info ";TEST Delete the fixtures")
      (restagraph::delete-resource-by-path
        session
        (format nil "/~A/~A" (restagraph::name parent1-type) parent1-uid)
        schema
        :recursive t)
      (restagraph::delete-resource-by-path
        session
        (format nil "/~A/~A" (restagraph::name parent1-type) parent2-uid)
        schema
        :recursive t)
      (restagraph::delete-schema-version session schema-version)
      (neo4cl:disconnect session))))

(fiveam:test
  resources-dependent-errors
  :depends-on 'resources-dependent-simple
  "Error conditions around creating/moving dependent resources"
  (let* ((child1-type (restagraph::make-incoming-rtypes :name "Models"
                                                        :dependent t))
         (child1-uid "Synthetics")
         (parent1-type (restagraph::make-incoming-rtypes :name "Makes"))
         (parent1-rel (restagraph::make-incoming-rels :name "PRODUCES"
                                                      :source-type (restagraph::name parent1-type)
                                                      :target-type (restagraph::name child1-type)
                                                      :reltype "dependent"
                                                      :cardinality "1:many"))
         (parent1-uid "Weyland-Yutani")
         (bad-parent1-type (restagraph::make-incoming-rtypes :name "NewGroups"))
         (bad-parent1-uid "Replicants")
         (session (neo4cl:establish-bolt-session *bolt-server*))
         (schema-version (restagraph::create-new-schema-version session)))
    (restagraph::log-message :info ";TEST Create the fixtures")
    ;; Install the core schema in the new schema-version
    (restagraph::install-subschema session restagraph::*core-schema* schema-version)
    ;; Add the test-specific resources and relationships
    (restagraph::install-subschema-resourcetype session child1-type schema-version)
    (restagraph::install-subschema-resourcetype session parent1-type schema-version)
    (restagraph::install-subschema-relationship session parent1-rel schema-version)
    (restagraph::install-subschema-resourcetype session bad-parent1-type schema-version)
    ;; Fetch the updated schema definition and start the tests
    (let ((schema (restagraph::fetch-current-schema session)))
      ;; Install the default resources
      (restagraph::install-default-resources session)
      ;; Create the feasible parent
      (restagraph::store-resource session
                                  schema
                                  (restagraph::name parent1-type)
                                  `(("uid" . ,parent1-uid))
                                  *admin-user*)
      ;; Create the child
      (restagraph::store-dependent-resource
        session
        schema
        (format nil "/~A/~A/~A/~A"
                (restagraph::name parent1-type)
                parent1-uid
                (restagraph::name parent1-rel)
                (restagraph::name child1-type))
        `(("uid" . ,child1-uid))
        *admin-user*)
      ;; Confirm the child is there
      (fiveam:is (restagraph::get-resources
                   session
                   (format nil "/~A/~A/~A/~A/~A"
                           (restagraph::name parent1-type)
                           parent1-uid
                           (restagraph::name parent1-rel)
                           (restagraph::name child1-type)
                           child1-uid)))
      ;; Create the infeasible parent
      (restagraph::store-resource session
                                  schema
                                  (restagraph::name bad-parent1-type)
                                  `(("uid" . ,bad-parent1-uid))
                                  *admin-user*)
      (restagraph::log-message :info ";TEST Try to move the child to an invalid parent")
      (fiveam:signals
        restagraph::client-error
        (restagraph::move-dependent-resource
          session
          schema
          (format nil "/~A/~A/~A/~A/~A"
                  (restagraph::name parent1-type)
                  parent1-uid
                  (restagraph::name parent1-rel)
                  (restagraph::name child1-type)
                  child1-uid)
          (format nil "/~A/~A"
                  (restagraph::name bad-parent1-type)
                  bad-parent1-uid)))
      ;; Clean up
      (restagraph::log-message :info ";TEST Delete the fixtures")
      (restagraph::delete-resource-by-path
        session
        (format nil "/~A/~A" (restagraph::name parent1-type) parent1-uid)
        schema
        :recursive t)
      (restagraph::delete-resource-by-path
        session
        (format nil "/~A/~A" (restagraph::name bad-parent1-type) bad-parent1-uid)
        schema)
      (restagraph::delete-schema-version session schema-version)
      (neo4cl:disconnect session))))

(fiveam:test
  resources-dependent-compound
  :depends-on 'resources-dependent-errors
  "Basic operations on 2-layered dependent resources"
  (let* ((parent-type (restagraph::make-incoming-rtypes :NAME "Routers"))
         (child-type (restagraph::make-incoming-rtypes :NAME "Interfaces"
                                                       :DEPENDENT t))
         (grandchild-type (restagraph::make-incoming-rtypes :NAME "Ipv4Addresses"
                                                            :DEPENDENT t))
             (relationship (restagraph::make-incoming-rels
                             :name "INTERFACES"
                             :reltype "dependent"
                             :source-type (restagraph::name parent-type)
                             :target-type (restagraph::name child-type)
                             :cardinality "1:many"))
         (child-relationship (restagraph::make-incoming-rels
                               :name "ADDRESSES"
                               :reltype "dependent"
                               :source-type (restagraph::name child-type)
                               :target-type (restagraph::name grandchild-type)
                               :cardinality "1:many"))
         (parent-uid "marshall")
         (child-uid "eth0")
         (grandchild-uid "192.168.24.1")
         (session (neo4cl:establish-bolt-session *bolt-server*))
         (schema-version (restagraph::create-new-schema-version session)))
    ;; Create the fixtures
    (restagraph::log-message :info ";TEST Create the fixtures")
    ;; Install the core schema in the new schema-version
    (restagraph::install-subschema session restagraph::*core-schema* schema-version)
    ;; Install the new resourcetypes and relationship
    (restagraph::install-subschema-resourcetype session grandchild-type schema-version)
    (restagraph::install-subschema-resourcetype session child-type schema-version)
    (restagraph::install-subschema-resourcetype session parent-type schema-version)
    (restagraph::install-subschema-relationship session relationship schema-version)
    (restagraph::install-subschema-relationship session child-relationship schema-version)
    (let ((schema (restagraph::fetch-current-schema session)))
      ;; Install the default resources
      (restagraph::install-default-resources session)
      ;; Create the parent resource
      (restagraph::log-message :info ";TEST Create the parent resource")
      (restagraph::store-resource session
                                  schema
                                  (restagraph::name parent-type)
                                  `(("uid" . ,parent-uid))
                                  *admin-user*)
      ;; Create the child resource
      (restagraph::log-message :info ";TEST Create the child resource")
      (restagraph::store-dependent-resource session
                                            schema
                                            (format nil "/~A/~A/~A/~A"
                                                    (restagraph::name parent-type)
                                                    parent-uid
                                                    (restagraph::name relationship)
                                                    (restagraph::name child-type))
                                            `(("uid" . ,child-uid))
                                            *admin-user*)
      ;; Create the grandchild resource
      (restagraph::log-message :info ";TEST Create the grandchild resource")
      (restagraph::store-dependent-resource
        session
        schema
        (format nil "/~A/~A/~A/~A/~A/~A/~A"
                (restagraph::name parent-type)
                parent-uid
                (restagraph::name relationship)
                (restagraph::name child-type)
                child-uid
                (restagraph::name child-relationship)
                (restagraph::name grandchild-type))
        `(("uid" . ,grandchild-uid))
        *admin-user*)
      ;; Delete the parent resource
      (restagraph::log-message :info ";TEST Recursively deleting the parent resource")
      (restagraph::delete-resource-by-path
        session
        (format nil "/~A/~A" (restagraph::name parent-type) parent-uid)
        schema
        :recursive t)
      ;; Confirm the dependent resources were recursively deleted with it
      (restagraph::log-message :info ";TEST Confirm the dependent resource is gone")
      (fiveam:is (null (restagraph::get-resources
                         session
                         (format nil "/~A/~A/~A/~A/~A"
                                 (restagraph::name parent-type)
                                 parent-uid
                                 (restagraph::name relationship)
                                 (restagraph::name child-type)
                                 child-uid))))
      (restagraph::log-message :info ";TEST Confirm the grandchild resource is gone")
      (fiveam:is (null
                   (restagraph::get-resources
                     session
                     (format nil "/~A/~A/~A/~A/~A/~A/~A/~A"
                             (restagraph::name parent-type)
                             parent-uid
                             (restagraph::name relationship)
                             (restagraph::name child-type)
                             child-uid
                             (restagraph::name child-relationship)
                             (restagraph::name grandchild-type)
                             grandchild-uid))))
      ;; Delete the temporary schema-version
      (restagraph::delete-schema-version session schema-version))
    ;; Clean up the Bolt session
    (neo4cl:disconnect session)))

(fiveam:test
  resources-dependent-moving
  :depends-on 'resources-dependent-compound
  "Moving a dependent resource to a new parent"
  (let* ((target-type (restagraph::make-incoming-rtypes :NAME "Ipv4Addresses"
                                                        :DEPENDENT t))
         (p2-type (restagraph::make-incoming-rtypes :name "Interfaces"
                                                    :dependent t))
         (p1-type (restagraph::make-incoming-rtypes :name "Routers"))
         (p1-target-rel (restagraph::make-incoming-rels
                          :name "ADDRESSES"
                          :reltype "dependent"
                          :source-type (restagraph::name p1-type)
                          :target-type (restagraph::name target-type)
                          :cardinality "1:many"))
         (p2-target-rel (restagraph::make-incoming-rels
                          :name "ADDRESSES"
                          :reltype "dependent"
                          :source-type (restagraph::name p2-type)
                          :target-type (restagraph::name target-type)
                          :cardinality "1:many"))
         (p1-p2-rel (restagraph::make-incoming-rels
                      :name "INTERFACES"
                      :reltype "dependent"
                      :source-type (restagraph::name p1-type)
                      :target-type (restagraph::name p2-type)
                      :cardinality "1:many"))
         (p1-uid "woomera")
         (p2-uid "eth2")
         (target-uid "172.20.0.1")
         (session (neo4cl:establish-bolt-session *bolt-server*))
         (schema-version (restagraph::create-new-schema-version session)))
    ;; Create the fixtures
    (restagraph::log-message :info ";TEST Create the fixtures")
    ;; Install the core schema in the new schema-version
    (restagraph::install-subschema session restagraph::*core-schema* schema-version)
    ;; Install the new resourcetypes and relationship
    (restagraph::log-message :info ";TEST Install additional resourcetypes")
    (restagraph::install-subschema-resourcetype session p1-type schema-version)
    (restagraph::install-subschema-resourcetype session p2-type schema-version)
    (restagraph::install-subschema-resourcetype session target-type schema-version)
    (restagraph::log-message :info ";TEST Install additional relationships")
    (restagraph::install-subschema-relationship session p1-target-rel schema-version)
    (restagraph::install-subschema-relationship session p2-target-rel schema-version)
    (restagraph::install-subschema-relationship session p1-p2-rel schema-version)
    ;; Get the complete schema
    (restagraph::log-message :info ";TEST Fetch updated schema")
    (let ((schema (restagraph::fetch-current-schema session)))
      ;; Install the default resources
      (restagraph::log-message :info ";TEST Install default resources")
      (restagraph::install-default-resources session)
      ;; Create initial parent
      (restagraph::log-message :info ";TEST Create initial parent resource")
      (restagraph::store-resource session
                                  schema
                                  (restagraph::name p1-type)
                                  `(("uid" . ,p1-uid))
                                  *admin-user*)
      ;; Create second parent as dependent on the initial
      (restagraph::store-dependent-resource
        session
        schema
        (format nil "/~A/~A/~A/~A"
                (restagraph::name p1-type)
                p1-uid
                (restagraph::name p1-p2-rel)
                (restagraph::name p2-type))
        `(("uid" . ,p2-uid))
        *admin-user*)
      ;; Create the dependent resource to be moved
      (restagraph::store-dependent-resource
        session
        schema
        (format nil "/~A/~A/~A/~A"
                (restagraph::name p1-type)
                p1-uid
                (restagraph::name p1-target-rel)
                (restagraph::name target-type))
        `(("uid" . ,target-uid))
        *admin-user*)
      ;; Move the resource
      (restagraph::log-message
        :info
        (format nil ";TEST Move dependent resource /~A/~A/~A/~A/~A to new parent /~A/~A/~A/~A/~A/~A"
                (restagraph::name p1-type)
                p1-uid
                (restagraph::name p1-target-rel)
                (restagraph::name target-type)
                target-uid
                (restagraph::name p1-type)
                p1-uid
                (restagraph::name p1-p2-rel)
                (restagraph::name p2-type)
                p2-uid
                (restagraph::name p2-target-rel)))
      (let ((result (restagraph::move-dependent-resource
                      session
                      schema
                      ;; URI
                      (format nil "/~A/~A/~A/~A/~A"
                              (restagraph::name p1-type)
                              p1-uid
                              (restagraph::name p1-target-rel)
                              (restagraph::name target-type)
                              target-uid)
                      ;; New parent
                      (format nil "/~A/~A/~A/~A/~A/~A"
                              (restagraph::name p1-type)
                              p1-uid
                              (restagraph::name p1-p2-rel)
                              (restagraph::name p2-type)
                              p2-uid
                              (restagraph::name p2-target-rel)))))
        (restagraph::log-message :debug (format nil "Result was: ~A" result))
        (fiveam:is (null result)))
      ;; Confirm the target resource is now at the new target path
      (let ((result (restagraph::get-resources
                      session
                      (format nil "/~A/~A/~A/~A/~A/~A/~A/~A"
                              (restagraph::name p1-type)
                              p1-uid
                              (restagraph::name p1-p2-rel)
                              (restagraph::name p2-type)
                              p2-uid
                              (restagraph::name p2-target-rel)
                              (restagraph::name target-type)
                              target-uid))))
        (fiveam:is (gethash "uid" result))
        (fiveam:is (equal (restagraph::sanitise-uid target-uid)
                          (gethash "uid" result)))
        (fiveam:is (gethash "original_uid" result))
        (fiveam:is (equal (restagraph::sanitise-uid target-uid)
                          (gethash "original_uid" result))))
      (let ((result (restagraph::get-resources
                      session
                      (format nil "/~A/~A/~A/~A/~A/~A/~A/~A"
                              (restagraph::name p1-type)
                              p1-uid
                              (restagraph::name p1-p2-rel)
                              (restagraph::name p2-type)
                              p2-uid
                              (restagraph::name p2-target-rel)
                              (restagraph::name target-type)
                              target-uid))))
        (fiveam:is (equal target-uid
                          (gethash "uid" result)))
        (fiveam:is (equal target-uid
                          (gethash "original_uid" result))))
      ;; Confirm the target resource is no longer present at the original path
      (fiveam:is
        (null
          (restagraph::get-resources
            session
            (format nil "/~A/~A/~A/~A/~A"
                    (restagraph::name p1-type)
                    p1-uid
                    (restagraph::name p1-target-rel)
                    (restagraph::name target-type)
                    target-uid))))
      ;; Delete the parent resource
      (restagraph::delete-resource-by-path
        session
        (format nil "/~A/~A" (restagraph::name p1-type) p1-uid)
        schema
        :recursive t)
      ;; Confirm stuff is gone
      (fiveam:is (null
                   (restagraph::get-resources
                     session
                     (format nil "/~A/~A" (restagraph::name p1-type) p1-uid))))
      ;; Remove the temporary schema-version
      (restagraph::delete-schema-version session schema-version))
    ;; Clean up the session
    (neo4cl:disconnect session)))

(fiveam:test
  resources-dependent-canonicalised
  :depends-on 'resources-dependent-moving
  "Confirm that canonicalisation works as expected."
  (let* ((res1type (restagraph::make-incoming-rtypes :name "Factories"))
         (res1uid "Fac1")
         (res2typename "People")
         (res2uid "George")
         (rel1 (restagraph::make-incoming-rels :source-type "People"
                                               :name "OWNS"
                                               :target-type "Factories"
                                               :reltype "any"
                                               :cardinality "many:many"))
         (depres1type (restagraph::make-incoming-rtypes :name "Robots"
                                                        :dependent t))
         (depres1uid "Bender")
         (deprel1 (restagraph::make-incoming-rels :source-type "Factories"
                                                  :name "CONTAINS"
                                                  :target-type "Robots"
                                                  :reltype "dependent"
                                                  :cardinality "1:many"))
         (depres2type (restagraph::make-incoming-rtypes :name "Components"
                                                        :dependent t))
         (depres2uid "PowerCable")
         (deprel2 (restagraph::make-incoming-rels :source-type "Robots"
                                                  :name "HAS"
                                                  :target-type "Components"
                                                  :reltype "dependent"
                                                  :cardinality "1:many"))
         (session (the neo4cl::bolt-session (neo4cl:establish-bolt-session *bolt-server*)))
         (schema-version (restagraph::create-new-schema-version session)))
    ;; Set up the fixtures
    (restagraph::log-message :info ";TEST Set up the fixtures")
    ;; Install the core schema in the new schema-version
    (restagraph::install-subschema session restagraph::*core-schema* schema-version)
    ;; Install the new resourcetypes and relationship
    (restagraph::install-subschema-resourcetype session res1type schema-version)
    (restagraph::install-subschema-relationship session rel1 schema-version)
    (restagraph::install-subschema-resourcetype session depres1type schema-version)
    (restagraph::install-subschema-relationship session deprel1 schema-version)
    (restagraph::install-subschema-resourcetype session depres2type schema-version)
    (restagraph::install-subschema-relationship session deprel2 schema-version)
    (let ((schema (the hash-table (restagraph::fetch-current-schema session))))
      ;; Fail on targets with the wrong lengths
      (fiveam:signals error (restagraph::canonicalise-path schema '() '()))
      (fiveam:signals error (restagraph::canonicalise-path schema '() '("foo")))
      (fiveam:signals error (restagraph::canonicalise-path schema '() '("foo" "bar" "baz")))
      (fiveam:signals error (restagraph::get-canonical-path session schema '("foo" "bar" "baz")))
      ;; Fail on paths with invalid lengths
      (fiveam:signals error (restagraph::canonicalise-path schema '("eenie") '("foo" "bar")))
      (fiveam:signals error (restagraph::canonicalise-path schema '("eenie" "meenie") '("foo" "bar")))
      ;; Validate a basic path to a primary resource
      (fiveam:is
        (equal (list (restagraph::name res1type) res1uid)
               (restagraph::canonicalise-path
                 schema
                 '()
                 (list (restagraph::name res1type) res1uid))))
      ;; Fail to get the path to a primary resource that doesn't exist
      (fiveam:signals
        restagraph::client-error
        (restagraph::get-canonical-path session
                                        schema
                                        (format nil "/~A/~A" (restagraph::name res1type) res1uid)))
      ;; Create that resource, then succeed in getting its path
      (restagraph::store-resource session
                                  schema
                                  (restagraph::name res1type)
                                  `(("uid" . ,res1uid))
                                  *admin-user*)
      (fiveam:is
        (equal (list (format nil "/~A/~A" (restagraph::name res1type) res1uid))
               (restagraph::get-canonical-path
                 session
                 schema
                 (format nil "/~A/~A" (restagraph::name res1type) res1uid))))
      ;; Validate a simple path to a dependent resource
      (fiveam:is
        (equal (list (restagraph::name res1type)
                     res1uid
                     (restagraph::name deprel1)
                     (restagraph::name depres1type)
                     depres1uid)
               (restagraph::canonicalise-path
                 schema
                 (list (restagraph::name res1type) res1uid (restagraph::name deprel1))
                 (list (restagraph::name depres1type) depres1uid))))
      ;; Fail to get the path to a dependent resource that doesn't exist
      (fiveam:signals
        restagraph::client-error
        (restagraph::get-canonical-path
          session
          schema
          (format nil "/~A/~A/~A/~A/~A"
                  (restagraph::name res1type)
                  res1uid
                  (restagraph::name deprel1)
                  (restagraph::name depres1type)
                  depres1uid)))
      ;; Create that resource, then succeed in getting its path
      (restagraph::log-message :debug ";TEST Store dependent resource")
      (restagraph::store-dependent-resource session
                                            schema
                                            (format nil "/~A/~A/~A/~A"
                                                    (restagraph::name res1type)
                                                    res1uid
                                                    (restagraph::name deprel1)
                                                    (restagraph::name depres1type))
                                            `(("uid" . ,depres1uid))
                                            *admin-user*)
      (restagraph::log-message :debug ";TEST Fetch dependent resource")
      (fiveam:is
        (equal (list (format nil "/~A/~A/~A/~A/~A" (restagraph::name res1type)
                             res1uid
                             (restagraph::name deprel1)
                             (restagraph::name depres1type)
                             depres1uid))
               (restagraph::get-canonical-path
                 session
                 schema
                 (format nil "/~A/~A/~A/~A/~A"
                         (restagraph::name res1type)
                         res1uid
                         (restagraph::name deprel1)
                         (restagraph::name depres1type)
                         depres1uid))))
      ;; Validate a chain of dependent resources
      (restagraph::log-message :debug ";TEST Validate a chain of dependent resources")
      (fiveam:is
        (equal (list (restagraph::name res1type)
                     res1uid
                     (restagraph::name deprel1)
                     (restagraph::name depres1type)
                     depres1uid
                     (restagraph::name deprel2)
                     (restagraph::name depres2type)
                     depres2uid)
               (restagraph::canonicalise-path
                 schema
                 (list (restagraph::name res1type) res1uid (restagraph::name deprel1)
                       (restagraph::name depres1type) depres1uid (restagraph::name deprel2))
                 (list (restagraph::name depres2type) depres2uid))))
      ;; Fail to get the path to a doubly-dependent resource that doesn't exist
      (fiveam:signals
        restagraph::client-error
        (restagraph::get-canonical-path
          session
          schema
          (format nil "/~A/~A/~A/~A/~A/~A/~A/~A"
                  (restagraph::name res1type)
                  res1uid
                  (restagraph::name deprel1)
                  (restagraph::name depres1type)
                  depres1uid
                  (restagraph::name deprel2)
                  (restagraph::name depres2type)
                  depres2uid)))
      ;; Create that resource, then succeed in getting its path
      (restagraph::log-message :debug ";TEST Store doubly-dependent resource")
      (restagraph::store-dependent-resource session
                                            schema
                                            (format nil "/~A/~A/~A/~A/~A/~A/~A"
                                                    (restagraph::name res1type)
                                                    res1uid
                                                    (restagraph::name deprel1)
                                                    (restagraph::name depres1type)
                                                    depres1uid
                                                    (restagraph::name deprel2)
                                                    (restagraph::name depres2type))
                                            `(("uid" . ,depres2uid))
                                            *admin-user*)
      (restagraph::log-message :debug ";TEST Fetch doubly-dependent resource")
      (fiveam:is
        (equal (list (format nil "/~A/~A/~A/~A/~A/~A/~A/~A"
                             (restagraph::name res1type)
                             res1uid
                             (restagraph::name deprel1)
                             (restagraph::name depres1type)
                             depres1uid
                             (restagraph::name deprel2)
                             (restagraph::name depres2type)
                             depres2uid))
               (restagraph::get-canonical-path
                 session
                 schema
                 (format nil "/~A/~A/~A/~A/~A/~A/~A/~A"
                         (restagraph::name res1type)
                         res1uid
                         (restagraph::name deprel1)
                         (restagraph::name depres1type)
                         depres1uid
                         (restagraph::name deprel2)
                         (restagraph::name depres2type)
                         depres2uid))))
      ;; Validate a more indirect path to a dependent resource
      (restagraph::log-message :debug ";TEST Validate a more indirect path to a dependent resource")
      (fiveam:is
        (equal (list (restagraph::name res1type)
                     res1uid
                     (restagraph::name deprel1)
                     (restagraph::name depres1type)
                     depres1uid)
               (restagraph::canonicalise-path
                 schema
                 (list
                   res2typename
                   res2uid
                   (restagraph::name rel1)
                   (restagraph::name res1type)
                   res1uid
                   (restagraph::name deprel1))
                 (list (restagraph::name depres1type) depres1uid))))
      ;; Fail to get a nonexistent indirect path to a dependent resource
      (fiveam:signals
        restagraph::client-error
        (restagraph::get-canonical-path
          session
          schema
          (format nil "/~A/~A/~A/~A/~A/~A/~A/~A"
                  res2typename
                  res2uid
                  (restagraph::name rel1)
                  (restagraph::name res1type)
                  res1uid
                  (restagraph::name deprel1)
                  (restagraph::name depres1type)
                  depres1uid)))
      ;; Create the head of that path, then succeed in getting its path
      (restagraph::log-message :debug ";TEST Store indirect path")
      (restagraph::store-resource session
                                  schema
                                  res2typename
                                  `(("uid" . ,res2uid))
                                  *admin-user*)
      (restagraph::create-relationship-by-path
        session
        (format nil "/~A/~A/~A"
                res2typename
                res2uid
                (restagraph::name rel1))
        (format nil "/~A/~A"
                (restagraph::name res1type)
                res1uid)
        schema)
      (restagraph::log-message :debug ";TEST Fetch dependent resource")
      (fiveam:is
        (equal (list (format nil "/~A/~A/~A/~A/~A" (restagraph::name res1type)
                             res1uid
                             (restagraph::name deprel1)
                             (restagraph::name depres1type)
                             depres1uid))
               (restagraph::get-canonical-path
                 session
                 schema
                 (format nil "/~A/~A/~A/~A/~A"
                         (restagraph::name res1type)
                         res1uid
                         (restagraph::name deprel1)
                         (restagraph::name depres1type)
                         depres1uid))))
      ;; Clean up the resources we created
      (restagraph::log-message :debug ";TEST Clean up the resources for this test")
      (restagraph::delete-resource-by-path session
                                           (format nil "/~A/~A" (restagraph::name res1type) res1uid)
                                           schema
                                           :recursive t)
      (restagraph::delete-resource-by-path session
                                           (format nil "/~A/~A" res2typename res2uid)
                                           schema
                                           :recursive t)
      (restagraph::delete-resource-by-path session
                                           (format nil "/~A/~A/~A/~A/~A"
                                                   (restagraph::name res1type)
                                                   res1uid
                                                   (restagraph::name deprel1)
                                                   (restagraph::name depres1type)
                                                   depres1uid)
                                           schema
                                           :recursive t)
      ;; Remove the temporary schema-version
      (restagraph::delete-schema-version session schema-version))
    ;; Clean up the session
    (neo4cl:disconnect session)))

(fiveam:test
  resources-multiple
  :depends-on 'resources-basic
  "Confirm we can retrieve all resources of a given type"
  (let* ((resourcetype (restagraph::make-incoming-rtypes :name "Routers"))
         (res1uid "amchitka")
         (res2uid "bikini")
         (res3uid "mururoa")
         (session (neo4cl:establish-bolt-session *bolt-server*))
         (schema-version (restagraph::create-new-schema-version session)))
    ;; Set up the fixtures
    (restagraph::log-message :info ";TEST Set up the fixtures")
    ;; Install the core schema in the new schema-version
    (restagraph::install-subschema session restagraph::*core-schema* schema-version)
    ;; Install the new resourcetypes and relationship
    (restagraph::install-subschema-resourcetype session resourcetype schema-version)
    (let ((schema (restagraph::fetch-current-schema session)))
      ;; Install the default resources
      (restagraph::install-default-resources session)
      ;; Confirm we have no instances of that resource in place now
      (fiveam:is (null (restagraph::get-resources
                         session
                         (format nil "/~A" (restagraph::name resourcetype)))))
      ;; Add one of that kind of resource
      (restagraph::store-resource session
                                  schema
                                  (restagraph::name resourcetype)
                                  `(("uid" . ,res1uid))
                                  *admin-user*)
      ;; Confirm we now get a list containing exactly that resource
      (let ((result (car (restagraph::get-resources
                           session
                           (format nil "/~A" (restagraph::name resourcetype))))))
        (fiveam:is (equal 3 (hash-table-count result)))
        (fiveam:is (not (null (gethash "original_uid" result))))
        (fiveam:is (not (null (gethash "createddate" result))))
        (fiveam:is (integerp (gethash "createddate" result)))
        (fiveam:is (not (null (gethash "uid" result))))
        (fiveam:is (equal res1uid (gethash "uid" result))))
      ;; Add a second of that kind of resource
      (restagraph::store-resource session
                                  schema
                                  (restagraph::name resourcetype)
                                  `(("uid" . ,res2uid))
                                  *admin-user*)
      ;; Confirm we now get a list containing both resources
      (let ((result (restagraph::get-resources
                      session
                      (format nil "/~A" (restagraph::name resourcetype)))))
        (fiveam:is (equal (list (restagraph::sanitise-uid res1uid)
                                (restagraph::sanitise-uid res2uid))
                          (sort (mapcar #'(lambda (res)
                                            (gethash "uid" res))
                                        result)
                                #'string<)))
        (fiveam:is (equal (restagraph::sanitise-uid res1uid)
                          (gethash "uid" (first result))))
        (fiveam:is (equal (restagraph::sanitise-uid res2uid)
                          (gethash "uid" (second result)))))
      ;; Add a third of that kind of resource
      (restagraph::store-resource session
                                  schema
                                  (restagraph::name resourcetype)
                                  `(("uid" . ,res3uid))
                                  *admin-user*)
      ;; Confirm we now get a list containing all three resources
      (let ((result (restagraph::get-resources
                      session
                      (format nil "/~A" (restagraph::name resourcetype)))))
        (fiveam:is (equal (list (restagraph::sanitise-uid res1uid)
                                (restagraph::sanitise-uid res2uid)
                                (restagraph::sanitise-uid res3uid))
                          (sort (mapcar #'(lambda (res)
                                            (gethash "uid" res))
                                        result)
                                #'string<))))
      ;; Delete all the resources we added
      (restagraph::delete-resource-by-path
        session
        (format nil "/~A/~A" (restagraph::name resourcetype) res1uid)
        schema)
      (restagraph::delete-resource-by-path
        session
        (format nil "/~A/~A" (restagraph::name resourcetype) res2uid)
        schema)
      (restagraph::delete-resource-by-path
        session
        (format nil "/~A/~A" (restagraph::name resourcetype) res3uid)
        schema)
      ;; Remove the schema-version we created for this test
      (restagraph::delete-schema-version session schema-version))
    ;; Clean up the session
    (neo4cl:disconnect session)))

(fiveam:test
  resources-filtering
  :depends-on 'resources-multiple
  "Filtering while searching for resources"
  (let* ((r1type (restagraph::make-incoming-rtypes :name "Routers"))
         (r2type (restagraph::make-incoming-rtypes :name "Interfaces"
                                                   :dependent t))
         (rel
           (restagraph::make-incoming-rels :name "INTERFACES"
                                           :reltype "dependent"
                                           :source-type (restagraph::name r1type)
                                           :target-type (restagraph::name r2type)
                                           :cardinality "1:many"))
         (r1uid "upshot")
         (r1partial "upsh.*")
         (r2uid "eth1/41")
         (r2partial "eth1.*")
         (session (neo4cl:establish-bolt-session *bolt-server*))
         (schema-version (restagraph::create-new-schema-version session)))
    ;; Create the fixtures
    (restagraph::log-message :info ";TEST Create the fixtures")
    ;; Install the core schema in the new schema-version
    (restagraph::install-subschema session restagraph::*core-schema* schema-version)
    ;; Install the new resourcetypes and relationship
    (restagraph::log-message :info ";TEST Install new resourcetypes and relationship")
    (restagraph::install-subschema-resourcetype session r1type schema-version)
    (restagraph::install-subschema-resourcetype session r2type schema-version)
    (restagraph::install-subschema-relationship session rel schema-version)
    (restagraph::log-message :info ";TEST Fetch the current schema")
    (let ((schema (restagraph::fetch-current-schema session)))
      ;; Install the default resources
      (restagraph::log-message :info ";TEST Install default resources")
      (restagraph::install-default-resources session)
      ;; Do the filters do what we expect?
      ;; Store a resource to check on
      (restagraph::log-message :info ";TEST Creating the primary resource")
      (restagraph::store-resource session
                                  schema
                                  (restagraph::name r1type)
                                  `(("uid" . ,r1uid))
                                  *admin-user*)
      ;; Search for it by type and exact UID
      (restagraph::log-message :info ";TEST Searching for the primary resource by name")
      (let ((result (car (restagraph::get-resources
                           session
                           (format nil "/~A" (restagraph::name r1type))
                           :filters (restagraph::process-filters
                                      `(("uid" . ,r1uid))
                                      schema
                                      (restagraph::name r1type))))))
        (fiveam:is (equal 3 (hash-table-count result)))
        (fiveam:is (not (null (gethash "uid" result))))
        (fiveam:is (equal r1uid (gethash "uid" result)))
        (fiveam:is (not (null (gethash "original_uid" result))))
        (fiveam:is (not (null (gethash "createddate" result)))))
      ;; Search for it by type and partial UID
      (restagraph::log-message :info ";TEST Searching for the primary resource by type and partial UID")
      (let ((result (car (restagraph::get-resources
                           session
                           (format nil "/~A" (restagraph::name r1type))
                           :filters (restagraph::process-filters
                                      `(("uid" . ,r1partial))
                                      schema
                                      (restagraph::name r1type))))))
        (fiveam:is (equal 3 (hash-table-count result)))
        (fiveam:is (not (null (gethash "original_uid" result))))
        (fiveam:is (not (null (gethash "createddate" result))))
        (fiveam:is (not (null (gethash "uid" result))))
        (fiveam:is (equal r1uid (gethash "uid" result))))
      ;; Add a dependent resource to search for
      (restagraph::log-message :info ";TEST Creating the secondary resource")
      (restagraph::store-dependent-resource
        session
        schema
        (format nil "/~A/~A/~A/~A"
                (restagraph::name r1type)
                r1uid
                (restagraph::name rel)
                (restagraph::name r2type))
        `(("uid" . ,r2uid))
        *admin-user*)
      ;; Confirm it's actually there
      (fiveam:is (equal `((,(restagraph::name rel)
                            ,(restagraph::name r2type)
                            ,(restagraph::sanitise-uid r2uid)))
                        (restagraph::get-dependent-resources
                          session
                          schema
                          (list (restagraph::name r1type) r1uid))))
      ;; Search for it by relationship to parent and exact UID
      (restagraph::log-message :info ";TEST Searching for the secondary resource")
      (let ((result (car (restagraph::get-resources
                           session
                           (format nil "/~A/~A/~A/~A"
                                   (restagraph::name r1type)
                                   r1uid
                                   (restagraph::name rel)
                                   (restagraph::name r2type))
                           :filters (restagraph::process-filters
                                      `(("uid" . ,(restagraph::sanitise-uid r2uid)))
                                      schema
                                      (restagraph::name r2type))))))
        (fiveam:is (equal 4 (hash-table-count result)))
        (fiveam:is (not (null (gethash "original_uid" result))))
        (fiveam:is (not (null (gethash "createddate" result))))
        (fiveam:is (not (null (gethash "uid" result))))
        (fiveam:is (equal (restagraph::sanitise-uid r2uid)
                          (gethash "uid" result)))
        (fiveam:is (equal r2uid (gethash "original_uid" result))))
      ;; Search for it by type and partial UID
      (let ((result (car (restagraph::get-resources
                           session
                           (format nil "/~A/~A/~A/~A"
                                   (restagraph::name r1type)
                                   r1uid
                                   (restagraph::name rel)
                                   (restagraph::name r2type))
                           :filters (restagraph::process-filters
                                      `(("uid" . ,r2partial))
                                      schema
                                      (restagraph::name r2type))))))
        (fiveam:is (equal 4 (hash-table-count result)))
        (fiveam:is (not (null (gethash "original_uid" result))))
        (fiveam:is (not (null (gethash "createddate" result))))
        (fiveam:is (not (null (gethash "uid" result))))
        (fiveam:is (equal (restagraph::sanitise-uid r2uid)
                          (gethash "uid" result)))
        (fiveam:is (equal r2uid (gethash "original_uid" result))))
      ;; Clean up: delete the primary and dependent resources.
      (restagraph::log-message :info ";TEST Cleanup: removing the resources")
      (restagraph::delete-resource-by-path
        session
        (format nil "/~A/~A" (restagraph::name r1type) r1uid)
        schema
        :recursive t)
      ;; Delete the temporary schema-version
      (restagraph::delete-schema-version session schema-version))
    ;; Clean up the session
    (neo4cl:disconnect session)))

(fiveam:test
  character-encoding
  :depends-on 'resources-basic
  "Check handling of non-ASCII characters."
  (let* ((attr1name "fullname")
         (attr1val "Röver Edwárd Petrusky De'Ath the fourth")
         ;(attr1val "Röver")
         (restype (restagraph::make-incoming-rtypes
                    :name "Dogs"
                    :attributes (list (restagraph::make-incoming-rtype-attrs
                                        (list :name attr1name)))))
         (uid "Spot")
         (session (neo4cl:establish-bolt-session *bolt-server*))
         (schema-version (restagraph::create-new-schema-version session)))
    ;; Create a resource
    (restagraph::log-message :info ";TEST Set up the fixtures")
    ;; Install the core schema in the new schema-version
    (restagraph::install-subschema session restagraph::*core-schema* schema-version)
    (restagraph::install-subschema-resourcetype session restype schema-version)
    (let ((schema (restagraph::fetch-current-schema session)))
      ;; Install the default resources
      (restagraph::install-default-resources session)
      ;; Create a resource with a non-contentious UID
      (restagraph::store-resource session
                                  schema
                                  (restagraph::name restype)
                                  `(("uid" . ,uid))
                                  *admin-user*)
      ;; Add an attribute whose value has non-ASCII characters
      (restagraph::log-message :info ";TEST Try to set the attribute")
      (fiveam:is (null (restagraph::update-resource-attributes
                         session
                         schema
                         (list (restagraph::name restype) uid)
                         `((,attr1name . ,attr1val)))))
      ;; Verify that the same string is returned when we query it
      (let ((result (restagraph::get-resources
                      session (format nil "/~A/~A"
                                      (restagraph::name restype) uid))))
        (fiveam:is (not (null (gethash "uid" result))))
        (fiveam:is (equal (restagraph::sanitise-uid uid)
                          (gethash "uid" result)))
        (fiveam:is (not (null (gethash "original_uid" result))))
        (fiveam:is (equal uid (gethash "original_uid" result)))
        (fiveam:is (not (null (gethash attr1name result))))
        (fiveam:is (equal attr1val (gethash attr1name result))))
      ;; Delete the resource
      (restagraph::log-message :info ";TEST Remove the fixtures")
      (restagraph::delete-resource-by-path session
                                           (format nil "/~A/~A" (restagraph::name restype) uid)
                                           schema)
      (restagraph::delete-schema-version session schema-version))
    ;; Clean up the session
    (neo4cl:disconnect session)))

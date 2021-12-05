;   Copyright 2021 James Fleming <james@electronic-quill.net>
;
;   Licensed under the GNU General Public License
;   - for details, see LICENSE.txt in the top-level directory


;;;; Test suite for side-effecting code.

(in-package #:restagraph-test)

(declaim (optimize (compilation-speed 0)
                   (speed 2)
                   (safety 3)
                   (debug 3)))


(fiveam:def-suite
  side-effecting
  :description "Tests for side-effecting code."
  :in main)

(fiveam:in-suite side-effecting)

(fiveam:test
  schema-versions
  "Basic operations on schema versions"
  (let ((current-version-list (restagraph::list-schema-versions *server*))
        (current-version (restagraph::current-schema-version *server*)))
    ;; Create a new schema version
    (restagraph::log-message :info ";TEST Add a schema version")
    (fiveam:is (restagraph::create-new-schema-version *server*))
    ;; Do we have one more version than before?
    (fiveam:is (= (+ (length (assoc :VERSIONS current-version-list)) 1)
                  (length (assoc :VERSIONS (restagraph::list-schema-versions *server*)))))
    (let ((new-current-version (restagraph::current-schema-version *server*)))
      ;; Is the new current version newer than the previous one?
      (fiveam:is (> new-current-version current-version))
      (restagraph::log-message :info ";TEST Set schema version back and forward")
      ;; Set the version back to the previous one
      (fiveam:is (restagraph::set-current-schema-version *server* current-version))
      (fiveam:is (= current-version (restagraph::current-schema-version *server*)))
      ;; Roll forward to the newer one
      (fiveam:is (restagraph::set-current-schema-version *server* new-current-version))
      (fiveam:is (= new-current-version (restagraph::current-schema-version *server*)))
      ;; Delete the new version
      (restagraph::log-message :info ";TEST Delete a schema version")
      (fiveam:is (restagraph::delete-schema-version *server* new-current-version))
      ;; Are we back to the original current-version?
      (fiveam:is (equal current-version (restagraph::current-schema-version *server*)))
      ;; Are we back to the original number of versions?
      (fiveam:is (= (length (assoc :VERSIONS current-version-list))
                    (length (assoc :VERSIONS (restagraph::list-schema-versions *server*))))))))

(fiveam:test
  resources-basic
  :depends-on 'confirm-db-is-running
  "Basic operations on resources"
  (let ((restype "People")
        (uid "Cally")
        (tag1 "telepath")
        (tag2 "crewmember")
        (invalid-type "interfaces")
        (invalid-uid "eth0")
        (admin-user "RgAdmin")
        (schema (restagraph::fetch-current-schema *server*)))
    ;; Confirm the resource isn't already present
    (restagraph::log-message :info ";TEST Confirm the resource isn't already present")
    (fiveam:is (null (restagraph::get-resources *server* (format nil "/~A/~A" restype uid))))
    ;; Store the resource
    (restagraph::log-message :info ";TEST Store the resource")
    (fiveam:is (equal (restagraph::sanitise-uid uid)
                      (restagraph::store-resource *server* schema restype `(("uid" . ,uid)) admin-user)))
    ;; Confirm it's there
    ;; (Test get-resources with mod/3 == 2)
    (restagraph::log-message :info ";TEST Confirm the resource is present")
    (let ((result (restagraph::get-resources *server* (format nil "/~A/~A" restype uid))))
      (fiveam:is (assoc :UID result))
      (fiveam:is (equal (restagraph::sanitise-uid uid)
                        (cdr (assoc :UID result))))
      (fiveam:is (assoc :ORIGINAL_UID result))
      (fiveam:is (equal uid
                        (cdr (assoc :ORIGINAL_UID result)))))
    ;; Confirm we can't create a duplicate
    (restagraph::log-message :info ";TEST Confirm refusal to create duplicate resources")
    (fiveam:signals
      (restagraph::integrity-error "Path already exists; refusing to create a duplicate.")
      (restagraph::store-resource *server* schema restype `(("uid" . ,uid)) admin-user))
    ;; Confirm that there are two people (RgAdmin should be the other)
    ;; (Test get-resources with mod/3 == 1)
    (let ((people-list (restagraph::get-resources *server* (format nil "/~A" restype))))
      (restagraph::log-message :info (format nil ";TEST-INFO fetched list of people: ~A" people-list))
    (fiveam:is (equal 2 (length people-list))))
    ;; Confirm that we get all resource at the end of a relationship
    ;; First, create the tags.
    (restagraph::store-resource *server* schema "Tags" `(("uid" . ,tag1)) admin-user)
    (restagraph::store-resource *server* schema "Tags" `(("uid" . ,tag2)) admin-user)
    (fiveam:is (equal tag1
                      (cdr (assoc :UID (restagraph::get-resources *server* (format nil "/Tags/~A" tag1))))))
    (fiveam:is (equal tag2
                      (cdr (assoc :UID (restagraph::get-resources *server* (format nil "/Tags/~A" tag2))))))
    ;; Tag the user
    (restagraph::create-relationship-by-path *server*
                                             (format nil "/People/~A/TAGS" uid)
                                             (format nil "/Tags/~A" tag1)
                                             schema)
    (restagraph::create-relationship-by-path *server*
                                             (format nil "/People/~A/TAGS" uid)
                                             (format nil "/Tags/~A" tag2)
                                             schema)
    ;; Confirm we get both tags back
    (fiveam:is (equal 2 (length (restagraph::get-resources *server* (format nil "/~A/~A/TAGS" restype uid)))))
    ;; Remove the _relationship_ to one of the tags
    (fiveam:is (restagraph::delete-relationship-by-path *server*
                                                        schema
                                                        (format nil "/People/~A/TAGS" uid)
                                                        (format nil "/Tags/~A" tag1)))
    ;; Confirm we now only have one tag there
    (fiveam:is (equal 1 (length (restagraph::get-resources *server* (format nil "/~A/~A/TAGS" restype uid)))))
    ;; Delete both tags
    (restagraph::delete-resource-by-path *server* (format nil "/Tags/~A" tag1) schema)
    (restagraph::delete-resource-by-path *server* (format nil "/Tags/~A" tag2) schema)
    ;; Delete it
    (restagraph::log-message :info ";TEST Delete the resource")
    (multiple-value-bind (result code message)
      (restagraph::delete-resource-by-path *server* (format nil "/~A/~A" restype uid) schema)
      (declare (ignore result) (ignore message))
      (fiveam:is (equal 200 code)))
    ;; Confirm it's gone again
    (restagraph::log-message :info ";TEST Confirm the resource is gone")
    (fiveam:is (null (restagraph::get-resources *server* (format nil "/~A/~A" restype uid))))
    ;; Ensure we can't create an invalid type
    (restagraph::log-message :info ";TEST Ensure we can't create an invalid type")
    (fiveam:signals
      (restagraph::client-error "No such resourcetype.")
      (restagraph::store-resource *server* schema invalid-type `(("uid" . ,invalid-uid)) admin-user))))

(fiveam:test
  resources-attributes-enums
  :depends-on (and . ('resources-basic 'schema-versions))
  "Enumerated attributes"
  (let* ((schema-version (restagraph::create-new-schema-version *server*))
         (admin-user "RgAdmin")
         (attr1name "state")
         (attr1desc "Whether the port is up or down.")
         (attr1vals '("up" "down"))
         (restype (restagraph::make-incoming-rtypes
                    :name "switchport"
                    :attributes (list (restagraph::make-incoming-rtype-attrs
                                        :NAME attr1name
                                        :DESCRIPTION attr1desc
                                        :ATTR-VALUES attr1vals))))
         (uid "eth1")
         (attr1valgood "up")
         (attr1valbad "mal"))
    ;; Set up the fixtures
    (restagraph::log-message :info ";TEST Set up the fixtures")
    ;; Install the core schema in the new schema-version
    (restagraph::install-subschema *server* restagraph::*core-schema* schema-version)
    ;; Ensure we have this resourcetype
    (restagraph::install-subschema-resourcetype *server* restype schema-version)
    (let ((schema (restagraph::fetch-current-schema *server*)))
      ;; Check the definition in the schema
      (restagraph::log-message :info ";TEST Check the schema for the enum attribute")
      (let ((reference (restagraph::make-schema-rtype-attrs
                         :NAME attr1name
                         :DESCRIPTION attr1desc
                         :ATTR-VALUES attr1vals))
            (retrieved (elt (restagraph::attributes (gethash (restagraph::name restype) schema)) 0)))
        (fiveam:is (equalp (restagraph::name reference) (restagraph::name retrieved)))
        (fiveam:is (equalp (restagraph::description reference) (restagraph::description retrieved)))
        (fiveam:is (equalp (restagraph::attr-values reference) (restagraph::attr-values retrieved))))
      ;; Make sure the test resource doesn't already exist
      (restagraph::delete-resource-by-path *server*
                                           (format nil "/~A/~A"
                                                   (restagraph::name restype)
                                                   uid)
                                           schema)
      ;; Fail to create a resource with an invalid value for the enum
      (restagraph::log-message :info ";TEST Fail to create a resource with an invalid attribute")
      (fiveam:signals restagraph::client-error
        (restagraph::store-resource *server*
                                    schema
                                    (restagraph::name restype)
                                    `(("uid" . ,uid)
                                      (,attr1name . ,attr1valbad))
                                    admin-user))
      ;; Create a resource with a valid value for the enum
      (restagraph::log-message :info ";TEST Create a resource with a valid attribute")
      (fiveam:is
        (restagraph::store-resource *server*
                                    schema
                                    (restagraph::name restype)
                                    `(("uid" . ,uid) (,attr1name . ,attr1valgood))
                                    admin-user))
      ;; Confirm that we now have this resource with the expected value
      (let ((attr-test (restagraph::get-resources
                         *server*
                         (format nil "/~A/~A"
                                 (restagraph::name restype)
                                 uid))))
        (fiveam:is (equal uid
                          (cdr (assoc :uid attr-test))))
        (fiveam:is (equal attr1valgood
                          (cdr (assoc
                                 (intern (string-upcase attr1name) 'keyword)
                                 attr-test)))))
      ;; Remove it again
      (restagraph::delete-resource-by-path *server*
                                           (format nil "/~A/~A"
                                                   (restagraph::name restype)
                                                   uid)
                                           schema)
      ;; Create it without the attribute
      (restagraph::store-resource *server*
                                  schema
                                  (restagraph::name restype)
                                  `(("uid" . ,uid))
                                  admin-user)
      ;; Add the attribute
      (fiveam:is (restagraph::update-resource-attributes
                   *server*
                   schema
                   (list (restagraph::name restype) uid)
                   `((,attr1name . ,attr1valgood))))
      ;;Confirm that the attribute is present
      (let ((attr-test (restagraph::get-resources
                         *server*
                         (format nil "/~A/~A"
                                 (restagraph::name restype)
                                 uid))))
        (fiveam:is (equal uid
                          (cdr (assoc :uid attr-test))))
        (fiveam:is (equal attr1valgood
                          (cdr (assoc
                                 (intern (string-upcase attr1name) 'keyword)
                                 attr-test)))))
      ;; Remove the resource again
      (restagraph::delete-resource-by-path *server*
                                           (format nil "/~A/~A"
                                                   (restagraph::name restype)
                                                   uid)
                                           schema)
      ;; Confirm it's gone
      (restagraph::delete-resource-by-path *server*
                                           (format nil "/~A/~A"
                                                   (restagraph::name restype)
                                                   uid)
                                           schema)
      ;; Remove the fixtures
      (restagraph::log-message :info ";TEST Remove the fixtures")
      (restagraph::delete-resource-by-path *server*
                                           (format nil "/~A/~A"
                                                   (restagraph::name restype) uid)
                                           schema)
      (restagraph::delete-schema-version *server* schema-version))))

(fiveam:test
  resources-dependent-simple
  :depends-on 'resources-basic
  "Basic operations on dependent resources"
  (let* ((schema-version (restagraph::create-new-schema-version *server*))
         (admin-user "RgAdmin")
         (child-type (restagraph::make-incoming-rtypes :name "Interfaces"
                                                       :dependent t))
         (relationship "INTERFACES")
         (parent-type (restagraph::make-incoming-rtypes :name "Routers"))
         (parent-rel (restagraph::make-incoming-rels :source-type (restagraph::name parent-type)
                                                     :name relationship
                                                     :dependent t
                                                     :cardinality "1:many"
                                                     :target-type (restagraph::name child-type)))
         (parent-uid "bikini")
         (child-uid "eth0")
         (invalid-child-type "routers")
         (invalid-child-uid "whitesands"))
    ;; Create the fixtures
    (restagraph::log-message :info ";TEST Create the fixtures")
    ;; Install the core schema in the new schema-version
    (restagraph::install-subschema *server* restagraph::*core-schema* schema-version)
    ;; Install the new resourcetypes and relationship
    (restagraph::install-subschema-resourcetype *server* child-type schema-version)
    (restagraph::install-subschema-resourcetype *server* parent-type schema-version)
    (restagraph::install-subschema-relationship *server* parent-rel schema-version)
    (let ((schema (restagraph::fetch-current-schema *server*)))
      ;; Fail to create a dependent resourcetype in isolation
      (restagraph::log-message :info ";TEST Fail to create a dependent resource in isolation.")
      (fiveam:signals restagraph::integrity-error
        (restagraph::store-resource *server*
                                    schema
                                    (restagraph::name child-type)
                                    `(("uid" . ,child-uid))
                                    admin-user))
      ;; Create the parent resource
      (restagraph::log-message :info ";TEST Create the parent resource.")
      (restagraph::store-resource *server*
                                  schema
                                  (restagraph::name parent-type)
                                  `(("uid" . ,parent-uid))
                                  admin-user)
      ;; Create the dependent resource
      (restagraph::log-message :debug (format nil ";TEST Create the dependent resource /~A/~A/~A/~A"
                                              (restagraph::name parent-type)
                                              parent-uid
                                              relationship
                                              (restagraph::name child-type)))
      (multiple-value-bind (result code message)
        (restagraph::store-dependent-resource
          *server*
          schema
          (format nil "/~A/~A/~A/~A"
                  (restagraph::name parent-type)
                  parent-uid
                  relationship
                  (restagraph::name child-type))
          `(("uid" . ,child-uid))
          admin-user)
        (declare (ignore result) (ignore message))
        (fiveam:is (equal 200 code)))
      ;; Confirm it's the only member of the parent's dependents
      (restagraph::log-message :debug ";TEST confirm this is an only child")
      (fiveam:is (equal `((,relationship
                            ,(restagraph::name child-type)
                            ,child-uid))
                        (restagraph::get-dependent-resources
                          *server*
                          schema
                          (list (restagraph::name parent-type) parent-uid))))
      ;; Confirm we get the type when asking for all things with that relationship
      (restagraph::log-message :debug ";TEST Confirm listing of types with all things with this relationship")
      (let ((result (car
                      (restagraph::get-resources
                        *server*
                        (format nil "/~A/~A/~A"
                                (restagraph::name parent-type)
                                parent-uid
                                relationship)))))
        (fiveam:is (assoc :TYPE result))
        (fiveam:is (equal (restagraph::sanitise-uid (restagraph::name child-type))
                          (cdr (assoc :TYPE result))))
        (fiveam:is (assoc :UID result))
        (fiveam:is (equal (restagraph::sanitise-uid child-uid)
                          (cdr (assoc :UID result))))
        (fiveam:is (assoc :ORIGINAL_UID result))
        (fiveam:is (equal child-uid
                          (cdr (assoc :ORIGINAL_UID result)))))
      ;; Delete the dependent resource
      (restagraph::log-message :debug ";TEST Delete the dependent resource")
      (let ((child-path (format nil "/~A/~A/~A/~A/~A"
                                (restagraph::name parent-type)
                                parent-uid
                                relationship
                                (restagraph::name child-type)
                                child-uid)))
        (multiple-value-bind (result code message)
          (restagraph::delete-resource-by-path *server* child-path
                                               schema)
          (declare (ignore result) (ignore message))
          (fiveam:is (equal 200 code)))
        ;; Confirm the dependent resource is gone
        (restagraph::log-message :debug ";TEST Confirm the dependent resource is gone.")
        (fiveam:is (null (restagraph::get-resources *server* child-path))))
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
          *server*
          schema
          (format nil "/~A/~A/~A/~A"
                  (restagraph::name parent-type)
                  parent-uid
                  relationship
                  invalid-child-type)
          `(("uid" . ,invalid-child-uid))
          admin-user))
      ;; Create the dependent resource yet again
      (restagraph::log-message :debug ";TEST Sucessfully re-create the dependent resource")
      (restagraph::store-dependent-resource
        *server*
        schema
        (format nil "/~A/~A/~A/~A"
                (restagraph::name parent-type)
                parent-uid
                relationship
                (restagraph::name child-type))
        `(("uid" . ,child-uid))
        admin-user)
      ;; Delete the parent resource
      (restagraph::log-message :info ";TEST Recursively deleting the parent resource")
      (restagraph::delete-resource-by-path
        *server*
        (format nil "/~A/~A" (restagraph::name parent-type) parent-uid)
        schema
        :recursive t)
      ;; Confirm the dependent resource was recursively deleted with it
      (restagraph::log-message :info ";TEST Confirm the parent resource is gone")
      (fiveam:is (null (restagraph::get-resources
                         *server*
                         (format nil "/~A/~A" (restagraph::name parent-type)
                                 parent-uid))))
      (restagraph::log-message :info ";TEST Confirm the dependent resource is gone")
      (fiveam:is (null (restagraph::get-resources
                         *server*
                         (format nil "/~A/~A" (restagraph::name child-type)
                                 child-uid))))
      ;; Remove the newly-created schema-version
      (restagraph::delete-schema-version *server* schema-version))))

(fiveam:test
  resources-dependent-errors
  :depends-on 'resources-dependent-simple
  "Error conditions around creating/moving dependent resources"
  (let* ((schema-version (restagraph::create-new-schema-version *server*))
         (admin-user "RgAdmin")
         (child1-type (restagraph::make-incoming-rtypes :name "Models"
                                                        :dependent t))
         (parent1-type (restagraph::make-incoming-rtypes :name "Makes"))
         (parent1-rel (restagraph::make-incoming-rels :name "PRODUCES"
                                                      :source-type (restagraph::name parent1-type)
                                                      :target-type (restagraph::name child1-type)
                                                      :dependent t
                                                      :cardinality "1:many"))
         (child1-uid "Synthetics")
         (parent1-uid "Weyland-Yutani")
         (bad-parent1-type (restagraph::make-incoming-rtypes :name "NewGroups"))
         (bad-parent1-uid "Replicants"))
    (restagraph::log-message :info ";TEST Create the fixtures")
    ;; Install the core schema in the new schema-version
    (restagraph::install-subschema *server* restagraph::*core-schema* schema-version)
    ;; Add the test-specific resources and relationships
    (restagraph::install-subschema-resourcetype *server* child1-type schema-version)
    (restagraph::install-subschema-resourcetype *server* parent1-type schema-version)
    (restagraph::install-subschema-relationship *server* parent1-rel schema-version)
    (restagraph::install-subschema-resourcetype *server* bad-parent1-type schema-version)
    ;; Fetch the updated schema definition and start the tests
    (let ((schema (restagraph::fetch-current-schema *server*)))
      ;; Create the feasible parent
      (restagraph::store-resource *server*
                                  schema
                                  (restagraph::name parent1-type)
                                  `(("uid" . ,parent1-uid))
                                  admin-user)
      ;; Create the child
      (restagraph::store-dependent-resource
        *server*
        schema
        (format nil "/~A/~A/~A/~A"
                (restagraph::name parent1-type)
                parent1-uid
                (restagraph::name parent1-rel)
                (restagraph::name child1-type))
        `(("uid" . ,child1-uid))
        admin-user)
      ;; Create the infeasible parent
      (restagraph::store-resource *server*
                                  schema
                                  (restagraph::name bad-parent1-type)
                                  `(("uid" . ,bad-parent1-uid))
                                  admin-user)
      (restagraph::log-message :info ";TEST Try to move the child to an invalid parent")
      (fiveam:signals
        restagraph::client-error
        (restagraph::move-dependent-resource
          *server*
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
        *server*
        (format nil "/~A/~A" (restagraph::name parent1-type) parent1-uid)
        schema
        :recursive t)
      (restagraph::delete-resource-by-path
        *server*
        (format nil "/~A/~A" (restagraph::name bad-parent1-type) bad-parent1-uid)
        schema)
      (restagraph::delete-schema-version *server* schema-version))))

(fiveam:test
  character-encoding
  :depends-on 'resources-basic
  "Check handling of non-ASCII characters."
  (let* ((schema-version (restagraph::create-new-schema-version *server*))
         (admin-user "RgAdmin")
         (attr1name "fullname")
         (restype (restagraph::make-incoming-rtypes
                    :name "Dogs"
                    :attributes (list (restagraph::make-incoming-rtype-attrs :name attr1name))))
         (uid "Spot")
         (attr1val "Röver Edwárd Petrusky the fourth"))
    ;; Create a resource
    (restagraph::log-message :info ";TEST Set up the fixtures")
    ;; Install the core schema in the new schema-version
    (restagraph::install-subschema *server* restagraph::*core-schema* schema-version)
    (restagraph::install-subschema-resourcetype *server* restype schema-version)
    (let ((schema (restagraph::fetch-current-schema *server*)))
      (restagraph::store-resource *server*
                                  schema
                                  (restagraph::name restype)
                                  `(("uid" . ,uid))
                                  admin-user)
      ;; Add an attribute whose value has non-ASCII characters
      (restagraph::log-message :info ";TEST Try to set the attribute")
      (fiveam:is (restagraph::update-resource-attributes
                   *server*
                   schema
                   (list (restagraph::name restype) uid)
                   `((,attr1name . ,attr1val))))
      ;; Verify that the same string is returned when we query it
      (let ((result (restagraph::get-resources
                      *server* (format nil "/~A/~A"
                                       (restagraph::name restype) uid)))
            (attr1kw (intern (string-upcase attr1name) 'keyword)))
        (fiveam:is (assoc :UID result))
        (fiveam:is (equal (restagraph::sanitise-uid uid)
                          (cdr (assoc :UID result))))
        (fiveam:is (assoc :ORIGINAL_UID result))
        (fiveam:is (equal uid
                          (cdr (assoc :ORIGINAL_UID result))))
        (fiveam:is (assoc attr1kw result))
        (fiveam:is (equal attr1val (cdr (assoc attr1kw result)))))
      ;; Delete the resource
      (restagraph::log-message :info ";TEST Remove the fixtures")
      (restagraph::delete-resource-by-path *server*
                                           (format nil "/~A/~A" (restagraph::name restype) uid)
                                           schema)
      (restagraph::delete-schema-version *server* schema-version))))

(fiveam:test
  relationships
  :depends-on 'resources-basic
  "Basic operations on relationships between resources"
  (let* ((schema-version (restagraph::create-new-schema-version *server*))
         (admin-user "RgAdmin")
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
      (restagraph::store-resource *server* schema (restagraph::name from-type) `(("uid" . ,from-uid)) admin-user)
      ;; Create the interface
      (restagraph::store-resource *server* schema (restagraph::name to-type) `(("uid" . ,to-uid)) admin-user)
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
        (admin-user "RgAdmin")
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
      (restagraph::store-resource *server* schema source-type `(("uid" . ,source-uid)) admin-user)
      (restagraph::store-resource *server* schema target-type `(("uid" . ,target-uid)) admin-user)
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
         (admin-user "RgAdmin")
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
                                  admin-user)
      ;; Create the interface
      (restagraph::store-resource *server*
                                  schema
                                  (restagraph::name to-type)
                                  `(("uid" . ,to-uid))
                                  admin-user)
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

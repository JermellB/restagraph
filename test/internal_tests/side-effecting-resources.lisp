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
  side-effecting-resources
  :description "Tests for side-effecting code pertaining mostly to resources."
  :in main)

(fiveam:in-suite side-effecting-resources)

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
        (schema (restagraph::fetch-current-schema *server*)))
    ;; Confirm the resource isn't already present
    (restagraph::log-message :info ";TEST Confirm the resource isn't already present")
    (fiveam:is (null (restagraph::get-resources *server* (format nil "/~A/~A" restype uid))))
    ;; Store the resource
    (restagraph::log-message :info ";TEST Store the resource")
    (fiveam:is (equal (restagraph::sanitise-uid uid)
                      (restagraph::store-resource *server* schema restype `(("uid" . ,uid)) *admin-user*)))
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
      (restagraph::store-resource *server* schema restype `(("uid" . ,uid)) *admin-user*))
    ;; Confirm that there are two people (RgAdmin should be the other)
    ;; (Test get-resources with mod/3 == 1)
    (let ((people-list (restagraph::get-resources *server* (format nil "/~A" restype))))
      (restagraph::log-message :info (format nil ";TEST-INFO fetched list of people: ~A" people-list))
    (fiveam:is (equal 2 (length people-list))))
    ;; Confirm that we get all resource at the end of a relationship
    ;; First, create the tags.
    (restagraph::store-resource *server* schema "Tags" `(("uid" . ,tag1)) *admin-user*)
    (restagraph::store-resource *server* schema "Tags" `(("uid" . ,tag2)) *admin-user*)
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
      (restagraph::store-resource *server* schema invalid-type `(("uid" . ,invalid-uid)) *admin-user*))))

(fiveam:test
  errors-basic
  :depends-on 'resources-basic
  "Errors that can be triggered just by making a bad request"
  (let ((schema (restagraph::make-schema-hash-table))
        (invalid-resourcetype "IjustMadeThisUpNow")
        (valid-resourcetype "People"))
    ;; Create a resource of an invalid type
    (restagraph::log-message :info ";TEST Creating a resource of an invalid type")
    (fiveam:signals (restagraph::client-error
                      (format nil "Requested resource type ~A is not valid."
                              invalid-resourcetype))
      (restagraph::store-resource *server*
                                  schema
                                  invalid-resourcetype
                                  '((:foo . "bar"))
                                  *admin-user*))
    ;; Create a resource of a valid type, but without a UID
    (restagraph::log-message :info ";TEST Creating a valid resource without a UID")
    (fiveam:signals (restagraph::client-error "UID must be supplied")
      (restagraph::store-resource *server*
                                  schema
                                  valid-resourcetype
                                  '(("foo" . "bar"))
                                  *admin-user*))))

(fiveam:test
  resources-attributes-enums
  :depends-on (and . ('resources-basic 'schema-versions))
  "Enumerated attributes"
  (let* ((schema-version (restagraph::create-new-schema-version *server*))
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
                                    *admin-user*))
      ;; Create a resource with a valid value for the enum
      (restagraph::log-message :info ";TEST Create a resource with a valid attribute")
      (fiveam:is
        (restagraph::store-resource *server*
                                    schema
                                    (restagraph::name restype)
                                    `(("uid" . ,uid) (,attr1name . ,attr1valgood))
                                    *admin-user*))
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
                                  *admin-user*)
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
  resources-attributes-read-only
  :depends-on '(resources-attributes-enums any-readonly-attrs)
  "Confirm that read-only attributes are correctly recorded as such in the database."
  (let ((files-attrs (restagraph::attributes
                       (gethash "Files" (restagraph::fetch-current-schema *server*)))))
    (fiveam:is (restagraph::read-only
                 (elt (remove-if-not #'(lambda (attr) (equal "mimetype" (restagraph::name attr)))
                                     files-attrs)
                      0)))
    (fiveam:is (not (restagraph::read-only
                      (elt (remove-if-not #'(lambda (attr) (equal "title" (restagraph::name attr)))
                                          files-attrs)
                           0))))))

(fiveam:test
  resources-dependent-simple
  :depends-on 'resources-basic
  "Basic operations on dependent resources"
  (let* ((schema-version (restagraph::create-new-schema-version *server*))
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
                                    *admin-user*))
      ;; Create the parent resource
      (restagraph::log-message :info ";TEST Create the parent resource.")
      (restagraph::store-resource *server*
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
          *admin-user*)
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
          *admin-user*))
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
        *admin-user*)
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
                                  *admin-user*)
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
        *admin-user*)
      ;; Create the infeasible parent
      (restagraph::store-resource *server*
                                  schema
                                  (restagraph::name bad-parent1-type)
                                  `(("uid" . ,bad-parent1-uid))
                                  *admin-user*)
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
  resources-dependent-compound
  :depends-on 'resources-dependent-errors
  "Basic operations on 2-layered dependent resources"
  (let* ((schema-version (restagraph::create-new-schema-version *server*))
         (parent-type (restagraph::make-incoming-rtypes :NAME "Routers"))
         (child-type (restagraph::make-incoming-rtypes :NAME "Interfaces"
                                                       :DEPENDENT t))
         (grandchild-type (restagraph::make-incoming-rtypes :NAME "Ipv4Addresses"
                                                            :DEPENDENT t))
         (relationship (restagraph::make-incoming-rels
                         :NAME "INTERFACES"
                         :DEPENDENT t
                         :SOURCE-TYPE (restagraph::name parent-type)
                         :TARGET-TYPE (restagraph::name child-type)))
         (child-relationship (restagraph::make-incoming-rels
                               :NAME "ADDRESSES"
                               :DEPENDENT t
                               :SOURCE-TYPE (restagraph::name child-type)
                               :TARGET-TYPE (restagraph::name grandchild-type)))
         (parent-uid "bikini")
         (child-uid "eth0")
         (grandchild-uid "192.168.24.1"))
    ;; Create the fixtures
    (restagraph::log-message :info ";TEST Create the fixtures")
    ;; Install the core schema in the new schema-version
    (restagraph::install-subschema *server* restagraph::*core-schema* schema-version)
    ;; Install the new resourcetypes and relationship
    (restagraph::install-subschema-resourcetype *server* grandchild-type schema-version)
    (restagraph::install-subschema-resourcetype *server* child-type schema-version)
    (restagraph::install-subschema-resourcetype *server* parent-type schema-version)
    (restagraph::install-subschema-relationship *server* relationship schema-version)
    (restagraph::install-subschema-relationship *server* child-relationship schema-version)
    (let ((schema (restagraph::fetch-current-schema *server*)))
      ;; Create the parent resource
      (restagraph::log-message :info ";TEST Create the parent resource")
      (restagraph::store-resource *server*
                                  schema
                                  (restagraph::name parent-type)
                                  `(("uid" . ,parent-uid))
                                  *admin-user*)
      ;; Create the child resource
      (restagraph::log-message :info ";TEST Create the child resource")
      (restagraph::store-dependent-resource *server*
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
        *server*
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
        *server*
        (format nil "/~A/~A" (restagraph::name parent-type) parent-uid)
        schema
        :recursive t)
      ;; Confirm the dependent resources were recursively deleted with it
      (restagraph::log-message :info ";TEST Confirm the dependent resource is gone")
      (fiveam:is (null (restagraph::get-resources
                         *server*
                         (format nil "/~A/~A/~A/~A/~A"
                                 (restagraph::name parent-type)
                                 parent-uid
                                 (restagraph::name relationship)
                                 (restagraph::name child-type)
                                 child-uid))))
      (restagraph::log-message :info ";TEST Confirm the grandchild resource is gone")
      (fiveam:is (null
                   (restagraph::get-resources
                     *server*
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
      (restagraph::delete-schema-version *server* schema-version))))

(fiveam:test
  resources-dependent-moving
  :depends-on 'resources-dependent-compound
  "Moving a dependent resource to a new parent"
  (let* ((schema-version (restagraph::create-new-schema-version *server*))
         (target-type (restagraph::make-incoming-rtypes :NAME "Ipv4Addresses"
                                                        :DEPENDENT t))
         (p2-type (restagraph::make-incoming-rtypes :name "Interfaces"
                                                    :dependent t))
         (p1-type (restagraph::make-incoming-rtypes :name "Routers"))
         (p1-target-rel (restagraph::make-incoming-rels
                          :NAME "ADDRESSES"
                          :DEPENDENT t
                          :SOURCE-TYPE (restagraph::name p1-type)
                          :TARGET-TYPE (restagraph::name target-type)))
         (p2-target-rel (restagraph::make-incoming-rels
                          :NAME "ADDRESSES"
                          :DEPENDENT t
                          :SOURCE-TYPE (restagraph::name p2-type)
                          :TARGET-TYPE (restagraph::name target-type)))
         (p1-p2-rel (restagraph::make-incoming-rels
                      :name "INTERFACES"
                      :dependent t
                      :source-type (restagraph::name p1-type)
                      :target-type (restagraph::name p2-type)))
         (p1-uid "woomera")
         (p2-uid "eth2")
         (target-uid "172.20.0.1"))
    ;; Create the fixtures
    (restagraph::log-message :info ";TEST Create the fixtures")
    ;; Install the core schema in the new schema-version
    (restagraph::install-subschema *server* restagraph::*core-schema* schema-version)
    ;; Install the new resourcetypes and relationship
    (restagraph::install-subschema-resourcetype *server* p1-type schema-version)
    (restagraph::install-subschema-resourcetype *server* p2-type schema-version)
    (restagraph::install-subschema-resourcetype *server* target-type schema-version)
    (restagraph::install-subschema-relationship *server* p1-target-rel schema-version)
    (restagraph::install-subschema-relationship *server* p2-target-rel schema-version)
    (restagraph::install-subschema-relationship *server* p1-p2-rel schema-version)
    (let ((schema (restagraph::fetch-current-schema *server*)))
      ;; Create initial parent
      (restagraph::store-resource *server*
                                 schema
                                 (restagraph::name p1-type)
                                 `(("uid" . ,p1-uid))
                                 *admin-user*)
      ;; Create second parent as dependent on the initial
      (restagraph::store-dependent-resource
        *server*
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
        *server*
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
      (let ((result (neo4cl:extract-data-from-get-request
                      (restagraph::move-dependent-resource
                        *server*
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
                                (restagraph::name p2-target-rel))))))
        (restagraph::log-message :debug (format nil "Result was: ~A" result))
        (fiveam:is (null result)))
      ;; Confirm the target resource is now at the new target path
      (let ((result (restagraph::get-resources
                      *server*
                      (format nil "/~A/~A/~A/~A/~A/~A/~A/~A"
                              (restagraph::name p1-type)
                              p1-uid
                              (restagraph::name p1-p2-rel)
                              (restagraph::name p2-type)
                              p2-uid
                              (restagraph::name p2-target-rel)
                              (restagraph::name target-type)
                              target-uid))))
        (fiveam:is (assoc :UID result))
        (fiveam:is (equal (restagraph::sanitise-uid target-uid)
                          (cdr (assoc :UID result))))
        (fiveam:is (assoc :ORIGINAL_UID result))
        (fiveam:is (equal target-uid
                          (cdr (assoc :ORIGINAL_UID result)))))
      (let ((result (restagraph::get-resources
                      *server*
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
                          (cdr (assoc :UID result))))
        (fiveam:is (equal target-uid
                          (cdr (assoc :ORIGINAL_UID result)))))
      ;; Confirm the target resource is no longer present at the original path
      (fiveam:is
        (null
          (restagraph::get-resources
            *server*
            (format nil "/~A/~A/~A/~A/~A"
                    (restagraph::name p1-type)
                    p1-uid
                    (restagraph::name p1-target-rel)
                    (restagraph::name target-type)
                    target-uid))))
      ;; Delete the parent resource
      (restagraph::delete-resource-by-path
        *server*
        (format nil "/~A/~A" (restagraph::name p1-type) p1-uid)
        schema
        :recursive t)
      ;; Confirm stuff is gone
      (fiveam:is (null
                   (restagraph::get-resources
                     *server*
                     (format nil "/~A/~A" (restagraph::name p1-type) p1-uid))))
      ;; Remove the temporary schema-version
      (restagraph::delete-schema-version *server* schema-version))))

(fiveam:test
  resources-multiple
  :depends-on 'resources-basic
  "Confirm we can retrieve all resources of a given type"
  (let* ((schema-version (restagraph::create-new-schema-version *server*))
         (resourcetype (restagraph::make-incoming-rtypes :name "Routers"))
         (res1uid "amchitka")
         (res2uid "bikini")
         (res3uid "mururoa"))
    ;; Set up the fixtures
    (restagraph::log-message :info ";TEST Set up the fixtures")
    ;; Install the core schema in the new schema-version
    (restagraph::install-subschema *server* restagraph::*core-schema* schema-version)
    ;; Install the new resourcetypes and relationship
    (restagraph::install-subschema-resourcetype *server* resourcetype schema-version)
    (let ((schema (restagraph::fetch-current-schema *server*)))
      ;; Confirm we have no instances of that resource in place now
      (fiveam:is (null (restagraph::get-resources
                         *server*
                         (format nil "/~A" (restagraph::name resourcetype)))))
      ;; Add one of that kind of resource
      (restagraph::store-resource *server*
                                 schema
                                 (restagraph::name resourcetype)
                                 `(("uid" . ,res1uid))
                                 *admin-user*)
      ;; Confirm we now get a list containing exactly that resource
      (let ((result (restagraph::get-resources
                      *server*
                      (format nil "/~A" (restagraph::name resourcetype)))))
        (fiveam:is (equal 3 (length (car result))))
        (fiveam:is (assoc :ORIGINAL_UID (car result)))
        (fiveam:is (assoc :CREATEDDATE (car result)))
        (fiveam:is (assoc :UID (car result)))
        (fiveam:is (equal res1uid (cdr (assoc :UID (car result))))))
      ;; Add a second of that kind of resource
      (restagraph::store-resource *server*
                                  schema
                                  (restagraph::name resourcetype)
                                  `(("uid" . ,res2uid))
                                  *admin-user*)
      ;; Confirm we now get a list containing both resources
      (let ((result (restagraph::get-resources
                      *server*
                      (format nil "/~A" (restagraph::name resourcetype)))))
        (fiveam:is (equal (list (restagraph::sanitise-uid res1uid)
                                (restagraph::sanitise-uid res2uid))
                          (sort (mapcar #'(lambda (res)
                                            (cdr (assoc :UID res)))
                                        result)
                                #'string<)))
        (fiveam:is (equal (restagraph::sanitise-uid res1uid)
                          (cdr (assoc :UID (first result)))))
        (fiveam:is (equal (restagraph::sanitise-uid res2uid)
                          (cdr (assoc :UID (second result))))))
      ;; Add a third of that kind of resource
      (restagraph::store-resource *server*
                                  schema
                                  (restagraph::name resourcetype)
                                  `(("uid" . ,res3uid))
                                  *admin-user*)
      ;; Confirm we now get a list containing all three resources
      (let ((result (restagraph::get-resources
                      *server*
                      (format nil "/~A" (restagraph::name resourcetype)))))
        (fiveam:is (equal (list (restagraph::sanitise-uid res1uid)
                                (restagraph::sanitise-uid res2uid)
                                (restagraph::sanitise-uid res3uid))
                          (sort (mapcar #'(lambda (res)
                                            (cdr (assoc :UID res)))
                                        result)
                                #'string<))))
      ;; Delete all the resources we added
      (restagraph::delete-resource-by-path
        *server*
        (format nil "/~A/~A" (restagraph::name resourcetype) res1uid)
        schema)
      (restagraph::delete-resource-by-path
        *server*
        (format nil "/~A/~A" (restagraph::name resourcetype) res2uid)
        schema)
      (restagraph::delete-resource-by-path
        *server*
        (format nil "/~A/~A" (restagraph::name resourcetype) res3uid)
        schema)
      (restagraph::delete-schema-version *server* schema-version))))

(fiveam:test
  resources-filtering
  :depends-on 'resources-multiple
  "Filtering while searching for resources"
  (let* ((schema-version (restagraph::create-new-schema-version *server*))
         (r1type (restagraph::make-incoming-rtypes :name "Routers"))
         (r2type (restagraph::make-incoming-rtypes :name "Interfaces"
                                                   :dependent t))
         (rel
           (restagraph::make-incoming-rels :name "INTERFACES"
                                           :dependent t
                                           :source-type (restagraph::name r1type)
                                           :target-type (restagraph::name r2type)))
         (r1uid "upshot")
         (r1partial "upsh.*")
         (r2uid "eth1/41")
         (r2partial "eth1.*"))
    ;; Create the fixtures
    (restagraph::log-message :info ";TEST Create the fixtures")
    ;; Install the core schema in the new schema-version
    (restagraph::install-subschema *server* restagraph::*core-schema* schema-version)
    ;; Install the new resourcetypes and relationship
    (restagraph::install-subschema-resourcetype *server* r1type schema-version)
    (restagraph::install-subschema-resourcetype *server* r2type schema-version)
    (restagraph::install-subschema-relationship *server* rel schema-version)
    (let ((schema (restagraph::fetch-current-schema *server*)))
      ;; Do the filters do what we expect?
      ;; Store a resource to check on
      (restagraph::log-message :info ";TEST Creating the primary resource")
      (restagraph::store-resource *server*
                                  schema
                                  (restagraph::name r1type)
                                  `(("uid" . ,r1uid))
                                  *admin-user*)
      ;; Search for it by type and exact UID
      (restagraph::log-message :info ";TEST Searching for the primary resource")
      (let ((result (restagraph::get-resources
                      *server*
                      (format nil "/~A" (restagraph::name r1type))
                      :filters (restagraph::process-filters
                                 `(("uid" . ,r1uid))
                                 schema
                                 (restagraph::name r1type)))))
        (fiveam:is (equal 3 (length (car result))))
        (fiveam:is (assoc :ORIGINAL_UID (car result)))
        (fiveam:is (assoc :CREATEDDATE (car result)))
        (fiveam:is (assoc :UID (car result)))
        (fiveam:is (equal r1uid (cdr (assoc :UID (car result))))))
      ;; Search for it by type and partial UID
      (let ((result (restagraph::get-resources
                      *server*
                      (format nil "/~A" (restagraph::name r1type))
                      :filters (restagraph::process-filters
                                 `(("uid" . ,r1partial))
                                 schema
                                 (restagraph::name r1type)))))
        (fiveam:is (equal 3 (length (car result))))
        (fiveam:is (assoc :ORIGINAL_UID (car result)))
        (fiveam:is (assoc :UID (car result)))
        (fiveam:is (assoc :CREATEDDATE (car result)))
        (fiveam:is (equal r1uid (cdr (assoc :UID (car result))))))
      ;; Add a dependent resource to search for
      (restagraph::log-message :info ";TEST Creating the secondary resource")
      (restagraph::store-dependent-resource
        *server*
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
                          *server*
                          schema
                          (list (restagraph::name r1type) r1uid))))
      ;; Search for it by relationship to parent and exact UID
      (restagraph::log-message :info ";TEST Searching for the secondary resource")
      (let ((result (restagraph::get-resources
                      *server*
                      (format nil "/~A/~A/~A/~A"
                              (restagraph::name r1type)
                              r1uid
                              (restagraph::name rel)
                              (restagraph::name r2type))
                      :filters (restagraph::process-filters
                                 `(("uid" . ,(restagraph::sanitise-uid r2uid)))
                                 schema
                                 (restagraph::name r2type)))))
        (fiveam:is (equal 3 (length (car result))))
        (fiveam:is (assoc :ORIGINAL_UID (car result)))
        (fiveam:is (equal r2uid (cdr (assoc :ORIGINAL_UID (car result)))))
        (fiveam:is (assoc :UID (car result)))
        (fiveam:is (assoc :CREATEDDATE (car result)))
        (fiveam:is (equal (restagraph::sanitise-uid r2uid)
                          (cdr (assoc :UID (car result))))))
      ;; Search for it by type and partial UID
      (let ((result (restagraph::get-resources
                      *server*
                      (format nil "/~A/~A/~A/~A"
                              (restagraph::name r1type)
                              r1uid
                              (restagraph::name rel)
                              (restagraph::name r2type))
                      :filters (restagraph::process-filters
                                 `(("uid" . ,r2partial))
                                 schema
                                 (restagraph::name r2type)))))
        (fiveam:is (equal 3 (length (car result))))
        (fiveam:is (assoc :ORIGINAL_UID (car result)))
        (fiveam:is (equal r2uid (cdr (assoc :ORIGINAL_UID (car result)))))
        (fiveam:is (assoc :UID (car result)))
        (fiveam:is (assoc :CREATEDDATE (car result)))
        (fiveam:is (equal (restagraph::sanitise-uid r2uid)
                          (cdr (assoc :UID (car result))))))
      ;; Clean up: delete the primary and dependent resources.
      (restagraph::log-message :info ";TEST Cleanup: removing the resources")
      (restagraph::delete-resource-by-path
        *server*
        (format nil "/~A/~A" (restagraph::name r1type) r1uid)
        schema
        :recursive t)
      ;; Delete the temporary schema-version
      (restagraph::delete-schema-version *server* schema-version))))

(fiveam:test
  character-encoding
  :depends-on 'resources-basic
  "Check handling of non-ASCII characters."
  (let* ((schema-version (restagraph::create-new-schema-version *server*))
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
      ;; Create a resource with a non-contentious UID
      (restagraph::store-resource *server*
                                  schema
                                  (restagraph::name restype)
                                  `(("uid" . ,uid))
                                  *admin-user*)
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

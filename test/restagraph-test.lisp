;   Copyright 2017 James Fleming <james@electronic-quill.net>
;
;   Licensed under the GNU General Public License
;   - for details, see LICENSE.txt in the top-level directory


;;;; Test suite for all of restagraph
;;;;
;;;; Beware: it currently only tests _expected_ cases,
;;;; and does not test edge-cases or wrong input.

(in-package #:restagraph-test)

(defparameter *server*
  (make-instance 'neo4cl:neo4j-rest-server
                 :hostname (getf restagraph::*config-vars* :dbhostname)
                 :dbname (getf restagraph::*config-vars* :dbname)
                 :dbuser (getf restagraph::*config-vars* :dbusername)
                 :dbpasswd (getf restagraph::*config-vars* :dbpasswd)))

(fiveam:def-suite main)
(fiveam:in-suite main)

(defun sort-results (results)
  (sort results
        #'string-lessp
        :key #'(lambda (f)
                 (cdr (assoc :uid f :test #'equal)))))

(fiveam:test
  schema-hash-basic
  "Basic tests of functions and methods on a schema implemented as a hash-table."
  (let ((schema (restagraph::make-schema-hash-table))
        (rtype1 (restagraph::make-incoming-rtypes :name "foo"
                                                  :dependent nil
                                                  :notes nil
                                                  :attributes nil
                                                  :relationships nil))
        (rtype2 (restagraph::make-incoming-rtypes
                  :name "foo"
                  :dependent nil
                  :notes "Should be ignored"
                  :attributes (list (restagraph::make-incoming-rtype-attrs
                                      :name "bar"
                                      :description "Hell if I know."))
                  :relationships nil))
        (rtype3 (restagraph::make-incoming-rtypes
                  :name "foo"
                  :dependent nil
                  :notes "Should be ignored"
                  :attributes (list (restagraph::make-incoming-rtype-attrs
                                      :name "bar"
                                      :description "Should be ignored."
                                      :values '("baz" "quux"))
                                    (restagraph::make-incoming-rtype-attrs
                                      :name "seasons"
                                      :description "This one has values."
                                      :values '("Spring" "Summer" "Autumn" "Winter")))
                  :relationships nil))
        (rtype4 (restagraph::make-incoming-rtypes
                  :name "bar"
                  :dependent nil
                  :notes "For testing attribute validation"
                  :attributes (list (restagraph::make-incoming-rtype-attrs
                                      :name "baz"
                                      :description "Valid attribute.")
                                    (restagraph::make-incoming-rtype-attrs
                                      :name "baz"
                                      :description "Duplicate. Shouldn't see this one.")))))
    ;; Confirm we can't retrieve this definition
    (restagraph:log-message :info "; TEST Confirm the schema is empty")
    (fiveam:is (equal nil
                      (restagraph::get-resourcetype-names schema)))
    (fiveam:is (equal ()
                      (restagraph::resourcetype-exists-p
                        schema
                        (restagraph::incoming-rtypes-name rtype1))))
    ;; Add a resourcetype to the schema
    (restagraph:log-message :info "; TEST Add one resourcetype")
    (fiveam:is (restagraph::add-resource-to-schema schema rtype1))
    ;; Confirm the resourcetype is there
    (fiveam:is (equalp (restagraph::make-schema-rtypes
                         :name (restagraph::incoming-rtypes-name rtype1)
                         :dependent (restagraph::incoming-rtypes-dependent rtype1)
                         :notes (restagraph::incoming-rtypes-notes rtype1)
                         :attributes (restagraph::incoming-rtypes-attributes rtype1)
                         :relationships (restagraph::incoming-rtypes-relationships rtype1))
                       (restagraph::resourcetype-exists-p
                         schema
                         (restagraph::incoming-rtypes-name rtype1))))
    ;; Confirm it's the only resourcetype in there
    (fiveam:is (equal (list (restagraph::incoming-rtypes-name rtype1))
                      (restagraph::get-resourcetype-names schema)))
    ;; Confirm it isn't dependent
    (fiveam:is (null (restagraph::dependent-resource-p
                       schema
                       (restagraph::incoming-rtypes-name rtype1))))
    ;; Check its attributes
    (fiveam:is (equalp (restagraph::incoming-rtypes-attributes rtype1)
                       (restagraph::get-resource-attributes-from-db
                         schema
                         (restagraph::incoming-rtypes-name rtype1))))
    ;; Dump it to an alist for conversion to JSON
    (fiveam:is (equal '((:NAME . "foo")
                        (:ATTRIBUTES)
                        (:DEPENDENT . "false")
                        (:NOTES . "")
                        (:RELATIONSHIPS))
                      (restagraph::describe-resource-type
                        schema
                        (restagraph::incoming-rtypes-name rtype1))))
    ;; Merge in new attributes for the resourcetype.
    (restagraph:log-message :info "; TEST Merge new attributes to the resourcetype.")
    (fiveam:is (restagraph::add-resource-to-schema schema rtype2))
    ;; Confirm they were merged correctly.
    (fiveam:is (equalp (restagraph::make-schema-rtypes
                         :name (restagraph::incoming-rtypes-name rtype1)
                         :dependent (restagraph::incoming-rtypes-dependent rtype1)
                         :notes (restagraph::incoming-rtypes-notes rtype1)
                         :attributes (list (restagraph::make-schema-rtype-attrs
                                             :name "bar"
                                             :description "Hell if I know."))
                         :relationships nil)
                       (restagraph::resourcetype-exists-p
                         schema
                         (restagraph::incoming-rtypes-name rtype1))))
    ;; Confirm it's still the only resourcetype in there
    (fiveam:is (equal (list (restagraph::incoming-rtypes-name rtype1))
                      (restagraph::get-resourcetype-names schema)))
    ;; Merge in more attributes, but now with a collision with the existing ones.
    (restagraph:log-message :info "; TEST Merge more attributes to the resourcetype. Expect a collision.")
    (fiveam:is (restagraph::add-resource-to-schema schema rtype3))
    ;; Confirm they were merged correctly
    (fiveam:is (equalp (restagraph::make-schema-rtypes
                         :name "foo"
                         :dependent nil
                         :notes nil
                         :attributes (list (restagraph::make-schema-rtype-attrs
                                             :name "seasons"
                                             :description "This one has values."
                                             :values '("Spring"
                                                       "Summer"
                                                       "Autumn"
                                                       "Winter"))
                                           (restagraph::make-schema-rtype-attrs
                                             :name "bar"
                                             :description "Hell if I know."))
                         :relationships nil)
                       (restagraph::resourcetype-exists-p
                         schema
                         (restagraph::incoming-rtypes-name rtype1))))
    ;; Confirm it's still the only resourcetype in there
    (fiveam:is (equal (list (restagraph::incoming-rtypes-name rtype1))
                      (restagraph::get-resourcetype-names schema)))
    ;; Add a type with a duplicate attribute already in it.
    (restagraph:log-message :info "; TEST Add a new type with a duplicate attribute.")
    (fiveam:is (restagraph::add-resource-to-schema schema rtype4))
    ;; Confirm it's among those that are present.
    (fiveam:is (equal (list (restagraph::incoming-rtypes-name rtype1)
                            (restagraph::incoming-rtypes-name rtype4))
                      (restagraph::get-resourcetype-names schema)))
    ;; Confirm that the duplicate attribute was _not_ applied.
    (fiveam:is (equalp (restagraph::make-schema-rtypes
                         :name "bar"
                         :dependent nil
                         :notes "For testing attribute validation"
                         :attributes (list (restagraph::make-schema-rtype-attrs
                                             :name "baz"
                                             :description "Valid attribute.")))
                       (restagraph::resourcetype-exists-p
                         schema
                         (restagraph::incoming-rtypes-name rtype4))))))

(fiveam:test
  schema-hash-relationships
  :depends-on 'schema-hash-basic
  "Test basic relationships operations in a hash-table schema"
  (let ((schema (restagraph::make-schema-hash-table))
        (rtype1 (restagraph::make-incoming-rtypes
                  :name "foo"
                  :dependent nil
                  :notes nil
                  :attributes nil
                  :relationships
                  ;; Test for both valid and invalid relationships, both dependent and non-dependent.
                  ;; The invalid ones go first to prevent them getting pre-empted by duplicate detection.
                  (list
                    ;; Invalid non-dependent relationship
                    (restagraph::make-incoming-rels
                      :relationship "Owns"
                      :target-type "bar"
                      :dependent t
                      :cardinality "1:many")
                    ;; Invalid dependent relationship
                    (restagraph::make-incoming-rels
                      :relationship "Has"
                      :target-type "baz"
                      :dependent nil
                      :cardinality "1:1")
                    ;; Valid non-dependent relationship
                    (restagraph::make-incoming-rels
                      :relationship "Owns"
                      :target-type "bar"
                      :dependent nil
                      :cardinality "1:many")
                    ;; Valid dependent relationship
                    (restagraph::make-incoming-rels
                      :relationship "Has"
                      :target-type "baz"
                      :dependent t
                      :cardinality "1:1"))))
        (rtype2 (restagraph::make-incoming-rtypes :name "bar"
                                                  :dependent nil
                                                  :notes nil
                                                  :attributes nil
                                                  :relationships nil))
        (rtype3 (restagraph::make-incoming-rtypes :name "baz"
                                                  :dependent t
                                                  :notes nil
                                                  :attributes nil
                                                  :relationships nil)))
    ;; Create the types.
    ;; Note the repeat of rtype1 - the first attempt should fail,
    ;; due to its relationships' dependencies on types that aren't yet present.
    (restagraph:log-message :info "; TEST Add resourcetype rtype1 - this should fail")
    (fiveam:is (restagraph::add-resource-to-schema schema rtype1))
    (fiveam:is (equal ()
                      (restagraph::schema-rtypes-relationships
                        (restagraph::resourcetype-exists-p
                          schema
                          (restagraph::incoming-rtypes-name rtype1)))))
    (restagraph:log-message :info "; TEST Add resourcetype rtype2.")
    (fiveam:is (restagraph::add-resource-to-schema schema rtype2))
    (restagraph:log-message :info "; TEST Add resourcetype rtype3.")
    (fiveam:is (restagraph::add-resource-to-schema schema rtype3))
    ;; This test-case covers the creation of valid dependent and non-dependent relationships.
    (restagraph:log-message :info "; TEST Add resourcetype rtype1 - this should now succeed.")
    (fiveam:is (restagraph::add-resource-to-schema schema rtype1))
    (restagraph:log-message :info "; TEST Check the state of the whole list of relationships for rtype1")
    (fiveam:is (equalp (list (restagraph::make-schema-rels
                               :relationship "Owns"
                               :target-type (restagraph::make-schema-rtypes
                                              :name (restagraph::incoming-rtypes-name rtype2)
                                              :dependent (restagraph::incoming-rtypes-dependent rtype2)
                                              :notes (restagraph::incoming-rtypes-notes rtype2)
                                              :attributes nil
                                              :relationships nil)
                               :dependent nil
                               :cardinality "1:many")
                             (restagraph::make-schema-rels
                               :relationship "Has"
                               :target-type (restagraph::make-schema-rtypes
                                              :name (restagraph::incoming-rtypes-name rtype3)
                                              :dependent (restagraph::incoming-rtypes-dependent rtype3)
                                              :notes (restagraph::incoming-rtypes-notes rtype3)
                                              :attributes nil
                                              :relationships nil)
                               :dependent t
                               :cardinality "1:1"))
                       (restagraph::schema-rtypes-relationships
                         (restagraph::resourcetype-exists-p
                           schema
                           (restagraph::incoming-rtypes-name rtype1)))))
    (restagraph:log-message :info "; TEST Check the presence of one specific relationship from rtype1")
    (fiveam:is (equalp (list (restagraph::make-schema-rels
                               :relationship "Owns"
                               :target-type (restagraph::make-schema-rtypes
                                              :name (restagraph::incoming-rtypes-name rtype2)
                                              :dependent (restagraph::incoming-rtypes-dependent rtype2)
                                              :notes (restagraph::incoming-rtypes-notes rtype2)
                                              :attributes nil
                                              :relationships nil)
                               :dependent nil
                               :cardinality "1:many"))
                       (restagraph::get-relationship-attrs
                         schema
                         (restagraph::incoming-rtypes-name rtype1)
                         "Owns"
                         (restagraph::incoming-rtypes-name rtype2))))))

(fiveam:test
  digest-schema-yaml
  :depends-on 'schema-hash-relationships
  "Digest a YAML file into incoming-rtypes structs."
  (let* ((filepath "../test/test_schemas/01_people_and_places.yaml")
         (digest (restagraph::digest-schema-yaml (make-pathname :defaults filepath))))
    (restagraph:log-message :debug (format nil "Loaded schema file from '~A'" filepath))
    (restagraph:log-message :debug (format nil "Result was: ~A" digest))
    (fiveam:is (equal "people_and_places"
                      (getf digest :NAME)))
    (fiveam:is (equal 'restagraph::incoming-rtypes
                      (type-of (car (getf digest :RESOURCETYPES)))))
    (fiveam:is (equal 'restagraph::incoming-rels
                      (type-of (cdr (car (getf digest :RELATIONSHIPS))))))))

(fiveam:test
  digest-yaml-to-schema
  :depends-on 'digest-schema-yaml
  "Digest a YAML file into a schema"
  (let* ((filepath "../test/test_schemas/01_people_and_places.yaml")
         (schema (restagraph::make-schema-hash-table))
         (digest (restagraph::digest-schema-yaml (make-pathname :defaults filepath))))
    (fiveam:is (equal 'hash-table
                      (type-of schema)))
    (restagraph::update-hash-from-digest schema digest)
    (fiveam:is
      (equalp
        (restagraph::make-schema-rtypes
          :name "people"
          :dependent nil
          :notes "A sentient individual."
          :attributes (list (restagraph::make-schema-rtype-attrs :name "preferredname")
                            (restagraph::make-schema-rtype-attrs :name "fullname"))
          :relationships (list (restagraph::make-schema-rels
                                 :relationship "ContactMethod"
                                 :target-type
                                 (restagraph::make-schema-rtypes
                                   :name "emailAddresses"
                                   :dependent t
                                   :notes ""
                                   :attributes
                                   (list (restagraph::make-schema-rtype-attrs
                                           :name "comments"
                                           :description "Comments about this particular email address."
                                           :values nil))
                                   :relationships nil)
                                 :cardinality "1:many"
                                 :dependent t
                                 :notes nil)
                               (restagraph::make-schema-rels
                                 :relationship "Addresses"
                                 :target-type (restagraph::make-schema-rtypes
                                                :name "streetNumbers"
                                                :dependent t
                                                :notes "The UID is the street number"
                                                :attributes
                                                (list (restagraph::make-schema-rtype-attrs
                                                        :name "name"
                                                        :description ""
                                                        :values nil)
                                                      (restagraph::make-schema-rtype-attrs
                                                        :name "floor"
                                                        :description ""
                                                        :values nil))
                                                :relationships
                                                (list (restagraph::make-schema-rels
                                                        :relationship "Postcodes"
                                                        :target-type (restagraph::make-schema-rtypes
                                                                       :name "postcodes"
                                                                       :dependent nil
                                                                       :notes "What else does this need?"
                                                                       :attributes nil
                                                                       :relationships nil)
                                                        :cardinality "many:1"
                                                        :dependent nil
                                                        :notes nil)))
                                 :cardinality "1:many"
                                 :dependent nil
                                 :notes nil)))
        (gethash "people" schema)))))

(fiveam:test
  validate-attributes
  "Check the validation of attributes"
  (let ((attrs (list (restagraph::make-schema-rtype-attrs
                       :NAME "status"
                       :DESCRIPTION "Task status."
                       :values '("idea"
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
                       :values '())
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
  authentication
  :depends-on 'validate-attributes
  "Basic checks of authentication."
  ;; Success
  (fiveam:is (restagraph::ensure-db-passwd *server*))
  ;; Failure
  (fiveam:is (null (restagraph::ensure-db-passwd
                     (make-instance 'neo4cl:neo4j-rest-server
                                    :hostname (getf restagraph::*config-vars* :dbhostname)
                                    :dbname (getf restagraph::*config-vars* :dbname)
                                    :dbpasswd "This is not the password"
                                    :dbuser (getf restagraph::*config-vars* :dbusername))))))

(fiveam:test
  resources-basic
  :depends-on 'authentication
  "Basic operations on resources"
  (let ((schema (restagraph::make-schema-hash-table))
        (restype (restagraph::make-incoming-rtypes :name "routers"))
        (uid "amchitka")
        (invalid-type "interfaces")
        (invalid-uid "eth0"))
    ;; Set up the fixtures
    (restagraph:log-message :info ";TEST Set up the fixtures")
    (restagraph::add-resource-to-schema schema restype)
    ;; Confirm the resource isn't already present
    (restagraph:log-message :info ";TEST Confirm the resource isn't already present")
    (fiveam:is (null (restagraph:get-resources
                       *server* (format nil "/~A/~A"
                                        (restagraph::incoming-rtypes-name restype)
                                        uid))))
    ;; Store the resource
    (restagraph:log-message :info ";TEST Store the resource")
    (fiveam:is (equal (restagraph::sanitise-uid uid)
                      (restagraph:store-resource
                        *server*
                        (restagraph::incoming-rtypes-name restype)
                        `(("uid" . ,uid))
                        schema)))
    ;; Confirm it's there
    (restagraph:log-message :info ";TEST Confirm the resource is present")
    (let ((result (restagraph:get-resources
                    *server* (format nil "/~A/~A"
                                     (restagraph::incoming-rtypes-name restype)
                                     uid))))
      (fiveam:is (assoc :UID result))
      (fiveam:is (equal (restagraph:sanitise-uid uid)
                        (cdr (assoc :UID result))))
      (fiveam:is (assoc :ORIGINAL_UID result))
      (fiveam:is (equal uid
                        (cdr (assoc :ORIGINAL_UID result)))))
    ;; Delete it
    (restagraph:log-message :info ";TEST Delete the resource")
    (multiple-value-bind (result code message)
      (restagraph:delete-resource-by-path
        *server*
        (format nil "/~A/~A" (restagraph::incoming-rtypes-name restype) uid)
        schema)
      (declare (ignore result) (ignore message))
      (fiveam:is (equal 200 code)))
    ;; Confirm it's gone again
    (restagraph:log-message :info ";TEST Confirm the resource is gone")
    (fiveam:is (null (restagraph:get-resources
                       *server* (format nil "/~A/~A"
                                        (restagraph::incoming-rtypes-name restype) uid))))
    ;; Ensure we can't create a dependent type
    (restagraph:log-message :info ";TEST Ensure we can't create a dependent type")
    (fiveam:signals
      (restagraph:integrity-error "This is a dependent resource; it must be created as a sub-resource of an existing resource.")
      (restagraph:store-resource *server* invalid-type `(("uid" . ,invalid-uid)) schema))))

(fiveam:test
  character-encoding
  :depends-on 'resources-basic-attributes
  "Check handling of non-ASCII characters."
  (let* ((schema (restagraph::make-schema-hash-table))
         (attr1name "fullname")
         (restype (restagraph::make-incoming-rtypes
                    :name "dogs"
                    :attributes (list (restagraph::make-incoming-rtype-attrs :name attr1name))))
         (uid "Spot")
         (attr1val "Röver Edwárd Petrusky the fourth"))
    ;; Create a resource
    (restagraph:log-message :info ";TEST Set up the fixtures")
    (restagraph::add-resource-to-schema schema restype)
    (restagraph:store-resource *server*
                               (restagraph::incoming-rtypes-name restype)
                               `(("uid" . ,uid))
                               schema)
    ;; Add an attribute whose value has non-ASCII characters
    (restagraph:log-message :info ";TEST Try to set the attribute")
    (fiveam:is (restagraph:update-resource-attributes
                 *server*
                 (list (restagraph::incoming-rtypes-name restype) uid)
                 `((,attr1name . ,attr1val))
                 schema))
    ;; Verify that the same string is returned when we query it
    (let ((result (restagraph:get-resources
                    *server* (format nil "/~A/~A"
                                     (restagraph::incoming-rtypes-name restype) uid)))
          (attr1kw (intern (string-upcase attr1name) 'keyword)))
      (fiveam:is (assoc :UID result))
      (fiveam:is (equal (restagraph:sanitise-uid uid)
                        (cdr (assoc :UID result))))
      (fiveam:is (assoc :ORIGINAL_UID result))
      (fiveam:is (equal uid
                        (cdr (assoc :ORIGINAL_UID result))))
      (fiveam:is (assoc attr1kw result))
      (fiveam:is (equal attr1val (cdr (assoc attr1kw result)))))
    ;; Delete the resource
    (restagraph:log-message :info ";TEST Remove the fixtures")
    (restagraph:delete-resource-by-path *server*
                                        (format nil "/~A/~A"
                                                (restagraph::incoming-rtypes-name restype)
                                                uid)
                                        schema)))

(fiveam:test
  resources-attributes-enums
  :depends-on 'resources-basic-attributes
  "Enumerated attributes"
  (let* ((schema (restagraph::make-schema-hash-table))
         (attr1name "state")
         (attr1desc "Whether the port is up or down.")
         (attr1vals '("up" "down"))
         (restype (restagraph::make-incoming-rtypes
                    :name "switchport"
                    :attributes (list (restagraph::make-incoming-rtype-attrs
                                        :name attr1name
                                        :description attr1desc
                                        :values attr1vals))))
         (uid "eth1")
         (attr1valgood "up")
         (attr1valbad "mal"))
    ;; Set up the fixtures
    (restagraph:log-message :info ";TEST Set up the fixtures")
    ;; Ensure we have this resourcetype
    (restagraph::add-resource-to-schema schema restype)
    ;; Check the definition in the schema
    (restagraph:log-message :info ";TEST Check the schema for the enum attribute")
    (fiveam:is (equalp
                 (list (restagraph::make-schema-rtype-attrs
                         :values attr1vals
                         :NAME attr1name
                         :DESCRIPTION attr1desc))
                 (restagraph::get-resource-attributes-from-db
                   schema
                   (restagraph::incoming-rtypes-name restype))))
    ;; Make sure the test resource doesn't already exist
    (restagraph:delete-resource-by-path *server*
                                        (format nil "/~A/~A"
                                                (restagraph::incoming-rtypes-name restype)
                                                uid)
                                        schema)
    ;; Fail to create a resource with an invalid value for the enum
    (restagraph:log-message :info ";TEST Fail to create a resource with an invalid attribute")
    (fiveam:signals restagraph:client-error
                    (restagraph:store-resource *server*
                                               (restagraph::incoming-rtypes-name restype)
                                               `(("uid" . ,uid)
                                                 (,attr1name . ,attr1valbad))
                                               schema))
    ;; Create a resource with a valid value for the enum
    (restagraph:log-message :info ";TEST Create a resource with a valid attribute")
    (fiveam:is
      (restagraph:store-resource *server*
                                 (restagraph::incoming-rtypes-name restype)
                                 `(("uid" . ,uid) (,attr1name . ,attr1valgood))
                                 schema))
    ;; Remove it again
    (restagraph:delete-resource-by-path *server*
                                        (format nil "/~A/~A"
                                                (restagraph::incoming-rtypes-name restype)
                                                uid)
                                        schema)
    ;; Create it without the attribute
    (restagraph:store-resource *server*
                               (restagraph::incoming-rtypes-name restype)
                               `(("uid" . ,uid))
                               schema)
    ;; Add the attribute
    (fiveam:is (restagraph:update-resource-attributes
                 *server*
                 (list (restagraph::incoming-rtypes-name restype) uid)
                 `((,attr1name . ,attr1valgood))
                 schema))
    ;; Remove the fixtures
    (restagraph:log-message :info ";TEST Remove the fixtures")
    (restagraph:delete-resource-by-path *server*
                                        (format nil "/~A/~A"
                                                (restagraph::incoming-rtypes-name restype) uid)
                                        schema)))

(fiveam:test
  resources-dependent-simple
  :depends-on 'resources-basic
  "Basic operations on dependent resources"
  (let* ((schema (restagraph::make-schema-hash-table))
         (child-type (restagraph::make-incoming-rtypes :name "interfaces"
                                                       :dependent t))
         (relationship "Interfaces")
         (parent-type (restagraph::make-incoming-rtypes
                        :name "routers"
                        :relationships (list (restagraph::make-incoming-rels
                                               :relationship relationship
                                               :dependent t
                                               :target-type "interfaces"))))
         (parent-uid "bikini")
         (child-uid "eth0")
         (invalid-child-type "routers")
         (invalid-child-uid "whitesands"))
    ;; Create the fixtures
    (restagraph:log-message :info ";TEST Create the fixtures")
    (restagraph::add-resource-to-schema schema child-type)
    (restagraph::add-resource-to-schema schema parent-type)
    ;; Fail to create a dependent resourcetype in isolation
    (restagraph:log-message :info ";TEST Fail to create a dependent resource in isolation.")
    (fiveam:signals restagraph:integrity-error
      (restagraph:store-resource *server*
                                 (restagraph::incoming-rtypes-name child-type)
                                 `(("uid" . ,child-uid))
                                 schema))
    ;; Create the parent resource
    (restagraph:log-message :info ";TEST Create the parent resource.")
    (restagraph:store-resource *server*
                               (restagraph::incoming-rtypes-name parent-type)
                               `(("uid" . ,parent-uid))
                               schema)
    ;; Create the dependent resource
    (restagraph:log-message :debug (format nil ";TEST Create the dependent resource /~A/~A/~A/~A"
                                           (restagraph::incoming-rtypes-name parent-type)
                                           parent-uid
                                           relationship
                                           (restagraph::incoming-rtypes-name child-type)))
    (multiple-value-bind (result code message)
      (restagraph:store-dependent-resource
        *server*
        (format nil "/~A/~A/~A/~A"
                (restagraph::incoming-rtypes-name parent-type)
                parent-uid
                relationship
                (restagraph::incoming-rtypes-name child-type))
        `(("uid" . ,child-uid))
        schema)
      (declare (ignore result) (ignore message))
      (fiveam:is (equal 200 code)))
    ;; Confirm it's the only member of the parent's dependents
    (restagraph:log-message :debug ";TEST confirm this is an only child")
    (fiveam:is (equal `((,relationship
                          ,(restagraph::incoming-rtypes-name child-type)
                          ,child-uid))
                      (restagraph:get-dependent-resources
                        *server*
                        (list (restagraph::incoming-rtypes-name parent-type) parent-uid)
                        schema)))
    ;; Confirm we get the type when asking for all things with that relationship
    (restagraph:log-message :debug ";TEST Confirm listing of types with all things with this relationship")
    (let ((result (car
                    (restagraph:get-resources
                      *server*
                      (format nil "/~A/~A/~A"
                              (restagraph::incoming-rtypes-name parent-type)
                              parent-uid
                              relationship)))))
      (fiveam:is (assoc :TYPE result))
      (fiveam:is (equal (restagraph:sanitise-uid (restagraph::incoming-rtypes-name child-type))
                        (cdr (assoc :TYPE result))))
      (fiveam:is (assoc :UID result))
      (fiveam:is (equal (restagraph:sanitise-uid child-uid)
                        (cdr (assoc :UID result))))
      (fiveam:is (assoc :ORIGINAL_UID result))
      (fiveam:is (equal child-uid
                        (cdr (assoc :ORIGINAL_UID result)))))
    ;; Delete the dependent resource
    (restagraph:log-message :debug ";TEST Delete the dependent resource")
    (let ((child-path (format nil "/~A/~A/~A/~A/~A"
                              (restagraph::incoming-rtypes-name parent-type)
                              parent-uid
                              relationship
                              (restagraph::incoming-rtypes-name child-type)
                              child-uid)))
      (multiple-value-bind (result code message)
        (restagraph:delete-resource-by-path *server* child-path
                                            schema)
        (declare (ignore result) (ignore message))
        (fiveam:is (equal 200 code)))
      ;; Confirm the dependent resource is gone
      (restagraph:log-message :debug ";TEST Confirm the dependent resource is gone.")
      (fiveam:is (null (restagraph:get-resources *server* child-path))))
    ;; Attempt to create a child resource that isn't of a dependent type
    (restagraph:log-message
      :debug
      (format nil ";TEST Fail to create a non-dependent child resource /~A/~A/~A/~A"
              (restagraph::incoming-rtypes-name parent-type)
              parent-uid
              relationship
              invalid-child-type))
    (fiveam:signals (restagraph:client-error "This is not a dependent resource type")
                    (restagraph:store-dependent-resource
                      *server*
                      (format nil "/~A/~A/~A/~A"
                              (restagraph::incoming-rtypes-name parent-type)
                              parent-uid
                              relationship
                              invalid-child-type)
                      `(("uid" . ,invalid-child-uid))
                      schema))
    ;; Create the dependent resource yet again
    (restagraph:log-message :debug ";TEST Sucessfully re-create the dependent resource")
    (restagraph:store-dependent-resource
      *server*
      (format nil "/~A/~A/~A/~A"
              (restagraph::incoming-rtypes-name parent-type)
              parent-uid
              relationship
              (restagraph::incoming-rtypes-name child-type))
      `(("uid" . ,child-uid))
      schema)
    ;; Delete the parent resource
    (restagraph:log-message :info ";TEST Recursively deleting the parent resource")
    (restagraph:delete-resource-by-path
      *server*
      (format nil "/~A/~A" (restagraph::incoming-rtypes-name parent-type) parent-uid)
      schema
      :recursive t)
    ;; Confirm the dependent resource was recursively deleted with it
    (restagraph:log-message :info ";TEST Confirm the parent resource is gone")
    (fiveam:is (null (restagraph:get-resources
                       *server*
                       (format nil "/~A/~A" (restagraph::incoming-rtypes-name parent-type)
                               parent-uid))))
    (restagraph:log-message :info ";TEST Confirm the dependent resource is gone")
    (fiveam:is (null (restagraph:get-resources
                       *server*
                       (format nil "/~A/~A" (restagraph::incoming-rtypes-name child-type)
                               child-uid))))))

(fiveam:test
  resources-dependent-errors
  :depends-on 'resources-dependent-simple
  "Error conditions around creating/moving dependent resources"
  (let* ((schema (restagraph::make-schema-hash-table))
         (child1-type (restagraph::make-incoming-rtypes :name "models"
                                                        :dependent t))
         (parent1-rel "Produces")
         (parent1-type (restagraph::make-incoming-rtypes
                         :name "makes"
                         :relationships
                         (list (restagraph::make-incoming-rels
                                 :relationship parent1-rel
                                 :dependent t
                                 :cardinality "1:many"
                                 :target-type (restagraph::incoming-rtypes-name child1-type)))))
         (child1-uid "Synthetics")
         (parent1-uid "Weyland-Yutani")
         (bad-parent1-type (restagraph::make-incoming-rtypes :name "groups"))
         (bad-parent1-uid "Replicants"))
    (restagraph:log-message :info ";TEST Create the fixtures")
    ;; Initial dependent parent/child
    (restagraph::add-resource-to-schema schema child1-type)
    (restagraph::add-resource-to-schema schema parent1-type)
    (restagraph:store-resource *server*
                               (restagraph::incoming-rtypes-name parent1-type)
                               `(("uid" . ,parent1-uid))
                               schema)
    (restagraph:store-dependent-resource
      *server*
      (format nil "/~A/~A/~A/~A"
              (restagraph::incoming-rtypes-name parent1-type)
              parent1-uid
              parent1-rel
              (restagraph::incoming-rtypes-name child1-type))
      `(("uid" . ,child1-uid))
      schema)
    ;; Infeasible parent
    (restagraph::add-resource-to-schema schema bad-parent1-type)
    (restagraph:store-resource *server*
                               (restagraph::incoming-rtypes-name bad-parent1-type)
                               `(("uid" . ,bad-parent1-uid))
                               schema)
    (restagraph:log-message :info ";TEST Try to move the child to an invalid parent")
    (fiveam:signals
      restagraph:client-error
      (restagraph:move-dependent-resource
        *server*
        (format nil "/~A/~A/~A/~A/~A"
                (restagraph::incoming-rtypes-name parent1-type)
                parent1-uid
                parent1-rel
                (restagraph::incoming-rtypes-name child1-type)
                child1-uid)
        (format nil "/~A/~A"
                (restagraph::incoming-rtypes-name bad-parent1-type)
                bad-parent1-uid)
        schema))
    (restagraph:log-message :info ";TEST Delete the fixtures")
    (restagraph:delete-resource-by-path
      *server*
      (format nil "/~A/~A" (restagraph::incoming-rtypes-name parent1-type) parent1-uid)
      schema
      :recursive t)
    (restagraph:delete-resource-by-path
      *server*
      (format nil "/~A/~A"
              (restagraph::incoming-rtypes-name bad-parent1-type)
              bad-parent1-uid)
      schema)))

(fiveam:test
  resources-dependent-compound
  :depends-on 'resources-dependent-errors
  "Basic operations on 2-layered dependent resources"
  (let* ((schema (restagraph::make-schema-hash-table))
         (grandchild-type (restagraph::make-incoming-rtypes :name "ipv4Addresses"
                                                            :dependent t))
         (child-relationship "Addresses")
         (child-type (restagraph::make-incoming-rtypes
                       :name "interfaces"
                       :dependent t
                       :relationships
                       (list (restagraph::make-incoming-rels
                               :relationship child-relationship
                               :dependent t
                               :target-type (restagraph::incoming-rtypes-name grandchild-type)))))
         (relationship "Interfaces")
         (parent-type (restagraph::make-incoming-rtypes
                        :name "routers"
                        :relationships
                        (list (restagraph::make-incoming-rels
                                :relationship relationship
                                :dependent t
                                :target-type (restagraph::incoming-rtypes-name child-type)))))
         (parent-uid "bikini")
         (child-uid "eth0")
         (grandchild-uid "192.168.24.1"))
    ;; Create the fixtures
    (restagraph:log-message :info ";TEST Create the fixtures")
    (restagraph::add-resource-to-schema schema grandchild-type)
    (restagraph::add-resource-to-schema schema child-type)
    (restagraph::add-resource-to-schema schema parent-type)
    ;; Create the parent resource
    (restagraph:log-message :info ";TEST Create the parent resource")
    (restagraph:store-resource *server*
                               (restagraph::incoming-rtypes-name parent-type)
                               `(("uid" . ,parent-uid))
                               schema)
    ;; Create the child resource
    (restagraph:log-message :info ";TEST Create the child resource")
    (restagraph:store-dependent-resource
      *server*
      (format nil "/~A/~A/~A/~A"
              (restagraph::incoming-rtypes-name parent-type)
              parent-uid
              relationship
              (restagraph::incoming-rtypes-name child-type))
      `(("uid" . ,child-uid))
      schema)
    ;; Create the grandchild resource
    (restagraph:log-message :info ";TEST Create the grandchild resource")
    (restagraph:store-dependent-resource
      *server*
      (format nil "/~A/~A/~A/~A/~A/~A/~A"
              (restagraph::incoming-rtypes-name parent-type)
              parent-uid
              relationship
              (restagraph::incoming-rtypes-name child-type)
              child-uid
              child-relationship
              (restagraph::incoming-rtypes-name grandchild-type))
      `(("uid" . ,grandchild-uid))
      schema)
    ;; Delete the parent resource
    (restagraph:log-message :info ";TEST Recursively deleting the parent resource")
    (restagraph:delete-resource-by-path
      *server*
      (format nil "/~A/~A" (restagraph::incoming-rtypes-name parent-type) parent-uid)
      schema
      :recursive t)
    ;; Confirm the dependent resources were recursively deleted with it
    (restagraph:log-message :info ";TEST Confirm the dependent resource is gone")
    (fiveam:is (null (restagraph:get-resources
                       *server*
                       (format nil "/~A/~A/~A/~A/~A"
                               (restagraph::incoming-rtypes-name parent-type)
                               parent-uid
                               relationship
                               (restagraph::incoming-rtypes-name child-type)
                               child-uid))))
    (restagraph:log-message :info ";TEST Confirm the grandchild resource is gone")
    (fiveam:is (null
                 (restagraph:get-resources
                   *server*
                   (format nil "/~A/~A/~A/~A/~A/~A/~A/~A"
                           (restagraph::incoming-rtypes-name parent-type)
                           parent-uid
                           relationship
                           (restagraph::incoming-rtypes-name child-type)
                           child-uid
                           child-relationship
                           (restagraph::incoming-rtypes-name grandchild-type)
                           grandchild-uid))))))

(fiveam:test
  resources-dependent-moving
  :depends-on 'resources-dependent-compound
  "Moving a dependent resource to a new parent"
  (let* ((schema (restagraph::make-schema-hash-table))
         (target-type (restagraph::make-incoming-rtypes
                        :name "ipv4Addresses"
                        :dependent t))
         (p2-target-rel "Addresses")
         (p2-type (restagraph::make-incoming-rtypes
                    :name "interfaces"
                    :dependent t
                    :relationships
                    (list (restagraph::make-incoming-rels
                            :relationship p2-target-rel
                            :dependent t
                            :target-type (restagraph::incoming-rtypes-name target-type)))))
         (p1-target-rel "Addresses")
         (p1-p2-rel "Interfaces")
         (p1-type (restagraph::make-incoming-rtypes
                    :name "routers"
                    :relationships
                    (list (restagraph::make-incoming-rels
                            :relationship p1-target-rel
                            :dependent t
                            :target-type (restagraph::incoming-rtypes-name target-type))
                          (restagraph::make-incoming-rels
                            :relationship p1-p2-rel
                            :dependent t
                            :target-type (restagraph::incoming-rtypes-name p2-type)))))
         (p1-uid "woomera")
         (p2-uid "eth2")
         (target-uid "172.20.0.1"))
    ;; Create the fixtures
    (restagraph:log-message :info ";TEST Create the fixtures")
    (restagraph::add-resource-to-schema schema target-type)
    (restagraph::add-resource-to-schema schema p2-type)
    (restagraph::add-resource-to-schema schema p1-type)
    ;; Create initial parent
    (restagraph:store-resource
      *server*
      (restagraph::incoming-rtypes-name p1-type)
      `(("uid" . ,p1-uid)) schema)
    ;; Create second parent as dependent on the initial
    (restagraph:store-dependent-resource
      *server*
      (format nil "/~A/~A/~A/~A"
              (restagraph::incoming-rtypes-name p1-type)
              p1-uid
              p1-p2-rel
              (restagraph::incoming-rtypes-name p2-type))
      `(("uid" . ,p2-uid))
      schema)
    ;; Create the dependent resource to be moved
    (restagraph:store-dependent-resource
      *server*
      (format nil "/~A/~A/~A/~A"
              (restagraph::incoming-rtypes-name p1-type)
              p1-uid
              p1-target-rel
              (restagraph::incoming-rtypes-name target-type))
      `(("uid" . ,target-uid))
      schema)
    ;; Move the resource
    (restagraph:log-message
      :info
      (format nil ";TEST Move dependent resource /~A/~A/~A/~A/~A to new parent /~A/~A/~A/~A/~A/~A"
              (restagraph::incoming-rtypes-name p1-type)
              p1-uid
              p1-target-rel
              (restagraph::incoming-rtypes-name target-type)
              target-uid
              (restagraph::incoming-rtypes-name p1-type)
              p1-uid
              p1-p2-rel
              (restagraph::incoming-rtypes-name p2-type)
              p2-uid
              p2-target-rel))
    (let ((result (neo4cl:extract-data-from-get-request
                    (restagraph:move-dependent-resource
                      *server*
                      ;; URI
                      (format nil "/~A/~A/~A/~A/~A"
                              (restagraph::incoming-rtypes-name p1-type)
                              p1-uid
                              p1-target-rel
                              (restagraph::incoming-rtypes-name target-type)
                              target-uid)
                      ;; New parent
                      (format nil "/~A/~A/~A/~A/~A/~A"
                              (restagraph::incoming-rtypes-name p1-type)
                              p1-uid
                              p1-p2-rel
                              (restagraph::incoming-rtypes-name p2-type)
                              p2-uid
                              p2-target-rel)
                      schema))))
      (restagraph:log-message :debug (format nil "Result was: ~A" result))
      (fiveam:is (null result)))
    ;; Confirm the target resource is now at the new target path
    (let ((result (restagraph:get-resources
                    *server*
                    (format nil "/~A/~A/~A/~A/~A/~A/~A/~A"
                            (restagraph::incoming-rtypes-name p1-type)
                            p1-uid
                            p1-p2-rel
                            (restagraph::incoming-rtypes-name p2-type)
                            p2-uid
                            p2-target-rel
                            (restagraph::incoming-rtypes-name target-type)
                            target-uid))))
      (fiveam:is (assoc :UID result))
      (fiveam:is (equal (restagraph:sanitise-uid target-uid)
                        (cdr (assoc :UID result))))
      (fiveam:is (assoc :ORIGINAL_UID result))
      (fiveam:is (equal target-uid
                        (cdr (assoc :ORIGINAL_UID result)))))
    (let ((result (restagraph:get-resources
                    *server*
                    (format nil "/~A/~A/~A/~A/~A/~A/~A/~A"
                            (restagraph::incoming-rtypes-name p1-type)
                            p1-uid
                            p1-p2-rel
                            (restagraph::incoming-rtypes-name p2-type)
                            p2-uid
                            p2-target-rel
                            (restagraph::incoming-rtypes-name target-type)
                            target-uid))))
      (fiveam:is (equal target-uid
                        (cdr (assoc :UID result))))
      (fiveam:is (equal target-uid
                        (cdr (assoc :ORIGINAL_UID result)))))
    ;; Confirm the target resource is no longer present at the original path
    (fiveam:is
      (null
        (restagraph:get-resources
          *server*
          (format nil "/~A/~A/~A/~A/~A"
                  (restagraph::incoming-rtypes-name p1-type)
                  p1-uid
                  p1-target-rel
                  (restagraph::incoming-rtypes-name target-type)
                  target-uid))))
    ;; Delete the parent resource
    (restagraph:delete-resource-by-path
      *server*
      (format nil "/~A/~A"
              (restagraph::incoming-rtypes-name p1-type)
              p1-uid)
      schema
      :recursive t)
    ;; Confirm stuff is gone
    (fiveam:is (null
                 (restagraph:get-resources
                   *server*
                   (format nil "/~A/~A"
                           (restagraph::incoming-rtypes-name p1-type)
                           p1-uid))))))

(fiveam:test
  resources-multiple
  :depends-on 'resources-basic
  "Confirm we can retrieve all resources of a given type"
  (let* ((schema (restagraph::make-schema-hash-table))
         (resourcetype (restagraph::make-incoming-rtypes :name "routers"))
         (res1uid "amchitka")
         (res2uid "bikini")
         (res3uid "mururoa"))
    ;; Set up the fixtures
    (restagraph:log-message :info ";TEST Set up the fixtures")
    (restagraph::add-resource-to-schema schema resourcetype)
    ;; Confirm we have no instances of that resource in place now
    (fiveam:is (null (restagraph:get-resources
                       *server*
                       (format nil "/~A" (restagraph::incoming-rtypes-name resourcetype)))))
    ;; Add one of that kind of resource
    (restagraph:store-resource *server*
                               (restagraph::incoming-rtypes-name resourcetype)
                               `(("uid" . ,res1uid))
                               schema)
    ;; Confirm we now get a list containing exactly that resource
    (let ((result (restagraph:get-resources
                    *server*
                    (format nil "/~A"
                            (restagraph::incoming-rtypes-name resourcetype)))))
      (fiveam:is (equal 3 (length (car result))))
      (fiveam:is (assoc :ORIGINAL_UID (car result)))
      (fiveam:is (assoc :CREATEDDATE (car result)))
      (fiveam:is (assoc :UID (car result)))
      (fiveam:is (equal res1uid (cdr (assoc :UID (car result))))))
    ;; Add a second of that kind of resource
    (restagraph:store-resource *server*
                               (restagraph::incoming-rtypes-name resourcetype)
                               `(("uid" . ,res2uid))
                               schema)
    ;; Confirm we now get a list containing both resources
    (let ((result (restagraph:get-resources
                    *server*
                    (format nil "/~A" (restagraph::incoming-rtypes-name resourcetype)))))
      (fiveam:is (equal (list (restagraph:sanitise-uid res1uid)
                              (restagraph:sanitise-uid res2uid))
                        (sort (mapcar #'(lambda (res)
                                          (cdr (assoc :UID res)))
                                      result)
                              #'string<)))
      (fiveam:is (equal (restagraph:sanitise-uid res1uid)
                        (cdr (assoc :UID (first result)))))
      (fiveam:is (equal (restagraph:sanitise-uid res2uid)
                        (cdr (assoc :UID (second result))))))
    ;; Add a third of that kind of resource
    (restagraph:store-resource *server*
                               (restagraph::incoming-rtypes-name resourcetype)
                               `(("uid" . ,res3uid))
                               schema)
    ;; Confirm we now get a list containing all three resources
    (let ((result (restagraph:get-resources
                    *server*
                    (format nil "/~A"
                            (restagraph::incoming-rtypes-name resourcetype)))))
      (fiveam:is (equal (list (restagraph:sanitise-uid res1uid)
                              (restagraph:sanitise-uid res2uid)
                              (restagraph:sanitise-uid res3uid))
                        (sort (mapcar #'(lambda (res)
                                          (cdr (assoc :UID res)))
                                      result)
                              #'string<))))
    ;; Delete all the resources we added
    (restagraph:delete-resource-by-path
      *server*
      (format nil "/~A/~A" (restagraph::incoming-rtypes-name resourcetype) res1uid)
      schema)
    (restagraph:delete-resource-by-path
      *server*
      (format nil "/~A/~A" (restagraph::incoming-rtypes-name resourcetype) res2uid)
      schema)
    (restagraph:delete-resource-by-path
      *server*
      (format nil "/~A/~A" (restagraph::incoming-rtypes-name resourcetype) res3uid)
      schema)))

(fiveam:test
  resources-filtering
  :depends-on 'resources-multiple
  "Filtering while searching for resources"
  (let* ((schema (restagraph::make-schema-hash-table))
         (r2type (restagraph::make-incoming-rtypes :name "interfaces"
                                                   :dependent t))
         (rel "Interfaces")
         (r1type (restagraph::make-incoming-rtypes
                   :name "routers"
                   :relationships (list (restagraph::make-incoming-rels
                                          :relationship rel
                                          :dependent t
                                          :target-type (restagraph::incoming-rtypes-name
                                                         r2type)))))
         (r1uid "upshot")
         (r1partial "upsh.*")
         (r2uid "eth1/41")
         (r2partial "eth1.*"))
    ;; Create the fixtures
    (restagraph:log-message :info ";TEST Create the fixtures")
    (restagraph::add-resource-to-schema schema r2type)
    (restagraph::add-resource-to-schema schema r1type)
    ;; Do the filters do what we expect?
    ;; Store a resource to check on
    (restagraph:log-message :info ";TEST Creating the primary resource")
    (restagraph:store-resource *server*
                               (restagraph::incoming-rtypes-name r1type)
                               `(("uid" . ,r1uid))
                               schema)
    ;; Search for it by type and exact UID
    (restagraph:log-message :info ";TEST Searching for the primary resource")
    (let ((result (restagraph:get-resources
                    *server*
                    (format nil "/~A" (restagraph::incoming-rtypes-name r1type))
                    :filters `(("uid" . ,r1uid)))))
      (fiveam:is (equal 3 (length (car result))))
      (fiveam:is (assoc :ORIGINAL_UID (car result)))
      (fiveam:is (assoc :CREATEDDATE (car result)))
      (fiveam:is (assoc :UID (car result)))
      (fiveam:is (equal r1uid (cdr (assoc :UID (car result))))))
    ;; Search for it by type and partial UID
    (let ((result (restagraph:get-resources
                    *server*
                    (format nil "/~A" (restagraph::incoming-rtypes-name r1type))
                    :filters `(("uid" . ,r1partial)))))
      (fiveam:is (equal 3 (length (car result))))
      (fiveam:is (assoc :ORIGINAL_UID (car result)))
      (fiveam:is (assoc :UID (car result)))
      (fiveam:is (assoc :CREATEDDATE (car result)))
      (fiveam:is (equal r1uid (cdr (assoc :UID (car result))))))
    ;; Add a dependent resource to search for
    (restagraph:log-message :info ";TEST Creating the secondary resource")
    (restagraph:store-dependent-resource
      *server*
      (format nil "/~A/~A/~A/~A"
              (restagraph::incoming-rtypes-name r1type)
              r1uid
              rel
              (restagraph::incoming-rtypes-name r2type))
      `(("uid" . ,r2uid))
      schema)
    ;; Confirm it's actually there
    (fiveam:is (equal `((,rel
                          ,(restagraph::incoming-rtypes-name r2type)
                          ,(restagraph:sanitise-uid r2uid)))
                      (restagraph:get-dependent-resources
                        *server* (list (restagraph::incoming-rtypes-name r1type)
                                       r1uid)
                        schema)))
    ;; Search for it by relationship to parent and exact UID
    (restagraph:log-message :info ";TEST Searching for the secondary resource")
    (let ((result (restagraph:get-resources
                    *server*
                    (format nil "/~A/~A/~A/~A"
                            (restagraph::incoming-rtypes-name r1type)
                            r1uid
                            rel
                            (restagraph::incoming-rtypes-name r2type))
                    :filters `(("uid" . ,(restagraph:sanitise-uid r2uid))))))
      (fiveam:is (equal 3 (length (car result))))
      (fiveam:is (assoc :ORIGINAL_UID (car result)))
      (fiveam:is (equal r2uid (cdr (assoc :ORIGINAL_UID (car result)))))
      (fiveam:is (assoc :UID (car result)))
      (fiveam:is (assoc :CREATEDDATE (car result)))
      (fiveam:is (equal (restagraph::sanitise-uid r2uid)
                        (cdr (assoc :UID (car result))))))
    ;; Search for it by type and partial UID
    (let ((result (restagraph:get-resources
                    *server*
                    (format nil "/~A/~A/~A/~A"
                            (restagraph::incoming-rtypes-name r1type)
                            r1uid
                            rel
                            (restagraph::incoming-rtypes-name r2type))
                    :filters `(("uid" . ,r2partial)))))
      (fiveam:is (equal 3 (length (car result))))
      (fiveam:is (assoc :ORIGINAL_UID (car result)))
      (fiveam:is (equal r2uid (cdr (assoc :ORIGINAL_UID (car result)))))
      (fiveam:is (assoc :UID (car result)))
      (fiveam:is (assoc :CREATEDDATE (car result)))
      (fiveam:is (equal (restagraph::sanitise-uid r2uid)
                        (cdr (assoc :UID (car result))))))
    ;; Clean up: delete the primary and dependent resources.
    (restagraph:log-message :info ";TEST Cleanup: removing the resources")
    (restagraph:delete-resource-by-path
      *server*
      (format nil "/~A/~A" (restagraph::incoming-rtypes-name r1type)
              r1uid)
      schema
      :recursive t)))

(fiveam:test
  relationships
  :depends-on 'resources-basic
  "Basic operations on relationships between resources"
  (let* ((schema (restagraph::make-schema-hash-table))
         (to-type (restagraph::make-incoming-rtypes :name "asn"))
         (relationship "Asn")
         (from-type (restagraph::make-incoming-rtypes
                      :name "routers"
                      :relationships
                      (list (restagraph::make-incoming-rels
                              :relationship relationship
                              :target-type (restagraph::incoming-rtypes-name to-type)))))
         (from-uid "bikini")
         (to-uid "64512"))
    ;; Create the fixtures
    (restagraph:log-message :info ";TEST Create the fixtures")
    (restagraph::add-resource-to-schema schema to-type)
    (restagraph::add-resource-to-schema schema from-type)
    ;; Store the router
    (restagraph:log-message :info ";TEST Creating the resources")
    (restagraph:store-resource
      *server*
      (restagraph::incoming-rtypes-name from-type)
      `(("uid" . ,from-uid))
      schema)
    ;; Create the interface
    (restagraph:store-resource
      *server*
      (restagraph::incoming-rtypes-name to-type)
      `(("uid" . ,to-uid))
      schema)
    ;; Create a relationship between them
    (restagraph:log-message :info (format nil ";TEST Create the relationship /~A/~A/~A/~A/~A"
                                          (restagraph::incoming-rtypes-name from-type)
                                          from-uid
                                          relationship
                                          (restagraph::incoming-rtypes-name to-type)
                                          to-uid))
    (multiple-value-bind (result code message)
      (restagraph:create-relationship-by-path
        *server*
        (format nil "/~A/~A/~A"
                (restagraph::incoming-rtypes-name from-type)
                from-uid
                relationship)
        (format nil "/~A/~A"
                (restagraph::incoming-rtypes-name to-type)
                to-uid)
        schema)
      (declare (ignore result) (ignore message))
      (fiveam:is (equal 200 code)))
    ;; Confirm the relationship is there
    (restagraph:log-message
      :info
      (format nil ";TEST Confirm the list of resources at the end of /~A/~A/~A"
              (restagraph::incoming-rtypes-name from-type)
              from-uid
              relationship))
    (let ((result (car (restagraph:get-resources
                         *server*
                         (format nil "/~A/~A/~A"
                                 (restagraph::incoming-rtypes-name from-type)
                                 from-uid
                                 relationship)))))
      (fiveam:is (equal (restagraph::incoming-rtypes-name to-type)
                        (cdr (assoc :TYPE result))))
      (fiveam:is (equal to-uid
                        (cdr (assoc :UID result)))))
    ;; Delete the relationship
    (restagraph:log-message :info (format nil ";TEST Delete the relationship from /~A/~A/~A to /~A/~A"
                                          (restagraph::incoming-rtypes-name from-type)
                                          from-uid
                                          relationship
                                          (restagraph::incoming-rtypes-name to-type)
                                          to-uid))
    (multiple-value-bind (result code message)
      (restagraph:delete-relationship-by-path
        *server*
        (format nil "/~A/~A/~A"
                (restagraph::incoming-rtypes-name from-type)
                from-uid
                relationship)
        (format nil "/~A/~A"
                (restagraph::incoming-rtypes-name to-type)
                to-uid)
        schema)
      (declare (ignore result))
      (restagraph:log-message :debug (format nil "Result of deletion request: ~A - ~A" code message))
      (fiveam:is (equal 200 code)))
    ;; Delete the router
    (restagraph:log-message :info ";TEST Cleanup: removing the resources")
    (restagraph:delete-resource-by-path
      *server*
      (format nil "/~A/~A"
              (restagraph::incoming-rtypes-name from-type)
              from-uid)
      schema)
    ;; Delete the interface
    (restagraph:delete-resource-by-path
      *server*
      (format nil "/~A/~A"
              (restagraph::incoming-rtypes-name to-type)
              to-uid)
      schema)))

(fiveam:test
  relationships-integrity
  :depends-on 'relationships
  "Basic operations on relationships between resources"
  (let* ((schema (restagraph::make-schema-hash-table))
         (to-type (restagraph::make-incoming-rtypes :name "asn"))
         (relationship "Asn")
         (from-type (restagraph::make-incoming-rtypes
                      :name "routers"
                      :relationships
                      (list (restagraph::make-incoming-rels
                              :relationship relationship
                              :target-type (restagraph::incoming-rtypes-name to-type)))))
         (from-uid "bikini")
         (to-uid "64512"))
    ;; Create the fixtures
    (restagraph:log-message :info ";TEST Create the fixtures")
    (restagraph::add-resource-to-schema schema to-type)
    (restagraph::add-resource-to-schema schema from-type)
    ;; Create the resources
    (restagraph:log-message :info ";TEST Creating the resources")
    (restagraph:store-resource *server*
                               (restagraph::incoming-rtypes-name from-type)
                               `(("uid" . ,from-uid))
                               schema)
    ;; Create the interface
    (restagraph:store-resource *server*
                               (restagraph::incoming-rtypes-name to-type)
                               `(("uid" . ,to-uid))
                               schema)
    ;; Create a relationship between them
    (restagraph:log-message :info ";TEST Create a relationship between them")
    (multiple-value-bind (result code message)
      (restagraph:create-relationship-by-path
        *server*
        (format nil "/~A/~A/~A" (restagraph::incoming-rtypes-name from-type)
                from-uid
                relationship)
        (format nil "/~A/~A" (restagraph::incoming-rtypes-name to-type)
                to-uid)
        schema)
      (declare (ignore result) (ignore message))
      (fiveam:is (equal 200 code)))
    ;; Confirm the relationship is there
    (restagraph:log-message :info ";TEST Confirm that the relationship is there")
    (let ((result (restagraph:get-resources
                    *server*
                    (format nil "/~A/~A/~A"
                            (restagraph::incoming-rtypes-name from-type)
                            from-uid
                            relationship))))
      (restagraph:log-message :debug (format nil "Received result ~A" result))
      (fiveam:is (equal 4 (length (car result))))
      (fiveam:is (assoc :TYPE (car result) :test #'equal))
      (fiveam:is (equal (restagraph::incoming-rtypes-name to-type)
                        (cdr (assoc :TYPE (car result) :test #'equal))))
      (fiveam:is (assoc :UID (car result) :test #'equal))
      (fiveam:is (equal to-uid (cdr (assoc :UID (car result) :test #'equal)))))
    ;; Confirm we get what we expect when checking what's at the end of the path
    (restagraph:log-message :info ";TEST Confirm that we get what we expect at the end of the path.")
    (let ((result (restagraph:get-resources
                    *server*
                    (format nil "/~A/~A/~A"
                            (restagraph::incoming-rtypes-name from-type)
                            from-uid
                            relationship))))
      (fiveam:is (equal 4 (length (car result))))
      (fiveam:is (assoc :TYPE (car result)))
      (fiveam:is (equal (restagraph::incoming-rtypes-name to-type)
                        (cdr (assoc :TYPE (car result)))))
      (fiveam:is (assoc :UID (car result)))
      (fiveam:is (equal to-uid (cdr (assoc :UID (car result))))))
    ;; Attempt to create a duplicate relationship between them
    (restagraph:log-message :info ";TEST Attempt to create a duplicate relationship.")
    (fiveam:signals (restagraph:integrity-error
                      (format nil "Relationship ~A already exists from ~A ~A to ~A ~A"
                              relationship
                              (restagraph::incoming-rtypes-name from-type)
                              from-uid
                              (restagraph::incoming-rtypes-name to-type)
                              to-uid))
                    (restagraph:create-relationship-by-path
                      *server*
                      (format nil "/~A/~A/~A"
                              (restagraph::incoming-rtypes-name from-type)
                              from-uid
                              relationship)
                      (format nil "/~A/~A" (restagraph::incoming-rtypes-name to-type) to-uid)
                      schema))
    ;; Confirm we still only have one relationship between them
    (restagraph:log-message :info ";TEST Confirm we still only have one relationship between them.")
    (let ((result (restagraph:get-resources
                    *server*
                    (format nil "/~A/~A/~A"
                            (restagraph::incoming-rtypes-name from-type)
                            from-uid
                            relationship))))
      (fiveam:is (equal 4 (length (car result))))
      (fiveam:is (assoc :TYPE (car result) :test #'equal))
      (fiveam:is (equal (restagraph::incoming-rtypes-name to-type)
                        (cdr (assoc :TYPE (car result) :test #'equal))))
      (fiveam:is (assoc :UID (car result) :test #'equal))
      (fiveam:is (equal to-uid (cdr (assoc :UID (car result) :test #'equal)))))
    ;; Delete the relationship
    (restagraph:log-message :info ";TEST Delete the relationship.")
    (multiple-value-bind (result code message)
      (restagraph:delete-relationship-by-path
        *server*
        (format nil "/~A/~A/~A/" (restagraph::incoming-rtypes-name from-type)
                from-uid
                relationship)
        (format nil "/~A/~A/" (restagraph::incoming-rtypes-name to-type)
                to-uid)
        schema)
      (declare (ignore result) (ignore message))
      (fiveam:is (equal 200 code)))
    ;; Clean-up: delete the resources
    (restagraph:log-message :info ";TEST Cleaning up: removing the resources")
    (restagraph:delete-resource-by-path
      *server*
      (format nil "/~A/~A" (restagraph::incoming-rtypes-name from-type) from-uid)
      schema)
    (restagraph:delete-resource-by-path
      *server*
      (format nil "/~A/~A" (restagraph::incoming-rtypes-name to-type) to-uid)
      schema)))

(fiveam:test
  errors-basic
  :depends-on 'resources-basic
  "Errors that can be triggered just by making a bad request"
  (let ((schema (restagraph::make-schema-hash-table))
        (invalid-resourcetype (restagraph::make-incoming-rtypes :name "IjustMadeThisUpNow"))
        (valid-resourcetype (restagraph::make-incoming-rtypes :name "routers")))
    ;; Create the fixtures
    (restagraph:log-message :info ";TEST Create the fixtures")
    (restagraph::add-resource-to-schema schema valid-resourcetype)
    ;; Create a resource of an invalid type
    (restagraph:log-message :info ";TEST Creating a resource of an invalid type")
    (fiveam:signals (restagraph:client-error
                      (format nil "Requested resource type ~A is not valid."
                              (restagraph::incoming-rtypes-name invalid-resourcetype)))
                    (restagraph:store-resource *server*
                                               (restagraph::incoming-rtypes-name invalid-resourcetype)
                                               '((:foo . "bar"))
                                               schema))
    ;; Create a resource of a valid type, but without a UID
    (restagraph:log-message :info ";TEST Creating a valid resource without a UID")
    (fiveam:signals (restagraph:client-error "UID must be supplied")
                    (restagraph:store-resource *server*
                                               (restagraph::incoming-rtypes-name valid-resourcetype)
                                               '(("foo" . "bar"))
                                               schema))))

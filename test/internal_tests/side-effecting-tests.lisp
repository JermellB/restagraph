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
    ;; Delete the tags
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
  relationships-to-any
  :depends-on 'resources-basic
  "Confirm that we can create relationships with a defined target of 'any',
  but can't create 'just any' relationship."
  (let ((source-type "Test")
        (newrel "FROBS")
        (source-uid "Whoomp")
        (target-type "Tags")
        (target-uid "ThereItIs")
        ;(invalid-sourcetype "People")
        (admin-user "RgAdmin")
        (schema-version (restagraph::current-schema-version *server*)))
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
      (restagraph::delete-resource-by-path *server* (format nil "/~A/~A" target-type target-uid) schema))))

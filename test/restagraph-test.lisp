;   Copyright 2017 James Fleming <james@electronic-quill.net>
;
;   Licensed under the Apache License, Version 2.0 (the "License");
;   you may not use this file except in compliance with the License.
;   You may obtain a copy of the License at
;
;       http://www.apache.org/licenses/LICENSE-2.0
;
;   Unless required by applicable law or agreed to in writing, software
;   distributed under the License is distributed on an "AS IS" BASIS,
;   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;   See the License for the specific language governing permissions and
;   limitations under the License.


;;;; Test suite for all of restagraph
;;;;
;;;; Beware: it currently only tests _expected_ cases,
;;;; and does not test edge-cases or wrong input.

(in-package #:restagraph-test)

(defparameter *server*
  (make-instance 'neo4cl:neo4j-rest-server
                 :hostname (getf restagraph::*config-vars* :dbhostname)
                 :dbpasswd (getf restagraph::*config-vars* :dbpasswd)
                 :dbuser (getf restagraph::*config-vars* :dbusername)))

(fiveam:def-suite main)
(fiveam:in-suite main)

(defun sort-results (results)
  (sort results
        #'string-lessp
        :key #'(lambda (f)
                 (cdr (assoc :uid f :test #'equal)))))

(fiveam:test
  resources-basic
  :depends-on 'schema-relationships
  "Basic operations on resources"
  (let ((restype "routers")
        (uid "amchitka")
        (invalid-type "interfaces")
        (invalid-uid "eth0"))
    ;; Set up the fixtures
    (restagraph:log-message :info "TEST Set up the fixtures")
    (restagraph:add-resourcetype *server* restype)
    ;; Confirm the resource isn't already present
    (restagraph:log-message :info "TEST Confirm the resource isn't already present")
    (fiveam:is (null (restagraph:get-resources
                       *server* (format nil "/~A/~A" restype uid))))
    ;; Store the resource
    (restagraph:log-message :info "TEST Store the resource")
    (multiple-value-bind (result code message)
      (restagraph:store-resource *server* restype `(("uid" . ,uid)))
      (declare (ignore result) (ignore message))
      (fiveam:is (equal 200 code)))
    ;; Confirm it's there
    (restagraph:log-message :info "TEST Confirm the resource is present")
    (fiveam:is (equal
                 `((:uid . ,uid)
                   (:original--uid . ,(restagraph:sanitise-uid uid)))
                 (restagraph:get-resources
                   *server* (format nil "/~A/~A" restype uid))))
    ;; Delete it
    (restagraph:log-message :info "TEST Delete the resource")
    (multiple-value-bind (result code message)
      (restagraph:delete-resource-by-path *server* (format nil "/~A/~A" restype uid))
      (declare (ignore result) (ignore message))
      (fiveam:is (equal 200 code)))
    ;; Confirm it's gone again
    (restagraph:log-message :info "TEST Confirm the resource is gone")
    (fiveam:is (null (restagraph:get-resources
                       *server* (format nil "/~A/~A" restype uid))))
    ;; Ensure we can't create a dependent type
    (restagraph:log-message :info "TEST Ensure we can't create a dependent type")
    (fiveam:signals
      (restagraph:client-error "This is a dependent resource; it must be created as a sub-resource of an existing resource.")
      (restagraph:store-resource *server* invalid-type `(("uid" . ,invalid-uid))))
    ;; Remove the fixtures
    (restagraph:log-message :info "TEST Remove the fixtures")
    (restagraph:delete-resourcetype *server* restype)))

(fiveam:test
  resources-dependent-simple
  :depends-on 'resources-basic
  "Basic operations on dependent resources"
  (let ((parent-type "routers")
        (parent-uid "bikini")
        (relationship "Interfaces")
        (child-type "interfaces")
        (child-uid "eth0")
        (invalid-child-type "routers")
        (invalid-child-uid "whitesands"))
    ;; Create the fixtures
    (restagraph:log-message :info "TEST Create the fixtures")
    (restagraph:add-resourcetype *server* parent-type)
    (restagraph:add-resourcetype *server* child-type :dependent t)
    (restagraph:add-resource-relationship *server* parent-type relationship child-type :dependent t)
    ;; Create the parent resource
    (restagraph:store-resource *server* parent-type `(("uid" . ,parent-uid)))
    ;; Create the dependent resource
    (restagraph:log-message :debug "TEST: Create the dependent resource")
    (multiple-value-bind (result code message)
      (restagraph:store-dependent-resource
        *server*
        (format nil "/~A/~A/~A/~A" parent-type parent-uid relationship child-type)
        `(("uid" . ,child-uid)))
      (declare (ignore result) (ignore message))
      (fiveam:is (equal 200 code)))
    ;; Confirm it's the only member of the parent's dependents
    (restagraph:log-message :debug "TEST; confirm this is an only child")
    (fiveam:is (equal `((,relationship ,child-type ,child-uid))
                      (restagraph:get-dependent-resources
                        *server* (list parent-type parent-uid))))
    ;; Confirm we get the type when asking for all things with that relationship
    (restagraph:log-message :debug "TEST: Confirm listing of types with all things with this relationship")
    (fiveam:is (equal `(((:type . ,child-type)
                         (:uid . ,child-uid)
                         (:original--uid . ,child-uid)))
                      (restagraph:get-resources
                        *server*
                        (format nil "/~A/~A/~A" parent-type parent-uid relationship))))
    ;; Fail to delete the dependent resource
    (restagraph:log-message :debug "TEST: Fail to delete the dependent resource")
    (fiveam:signals
      (restagraph:integrity-error "This would delete the resource itself, and delete-dependent was not specified.")
      (restagraph:delete-resource-by-path
        *server*
        (format nil "/~A/~A/~A/~A/~A"
                parent-type parent-uid relationship child-type child-uid)))
    ;; Delete the dependent resource
    (restagraph:log-message :debug "TEST: Successfully delete the dependent resource")
    (multiple-value-bind (result code message)
      (restagraph:delete-resource-by-path
        *server*
        (format nil "/~A/~A/~A/~A/~A"
                parent-type parent-uid relationship child-type child-uid)
        :delete-dependent t)
      (declare (ignore result) (ignore message))
      (fiveam:is (equal 200 code)))
    ;; Confirm the dependent resource is gone
    (restagraph:log-message :debug "TEST: Confirm the dependent resource is gone.")
    (fiveam:is (null (restagraph:get-resources
                       *server* (format nil "/~A/~A" child-type child-uid))))
    ;; Attempt to create a child resource that isn't of a dependent type
    (restagraph:log-message :debug "TEST: Fail to create a non-dependent child resource.")
    (fiveam:signals (restagraph:client-error "This is not a dependent resource type")
      (restagraph:store-dependent-resource
        *server*
        (format nil "/~A/~A/~A/~A" parent-type parent-uid relationship invalid-child-type)
        `(("uid" . ,invalid-child-uid))))
    ;; Create the dependent resource yet again
    (restagraph:log-message :debug "TEST: Sucessfully re-create the dependent resource")
    (restagraph:store-dependent-resource
      *server*
      (format nil "/~A/~A/~A/~A" parent-type parent-uid relationship child-type)
      `(("uid" . ,child-uid)))
    ;; Delete the parent resource
    (restagraph:log-message :info "TEST Recursively deleting the parent resource")
    (restagraph:delete-resource-by-path
      *server*
      (format nil "/~A/~A" parent-type parent-uid)
      :recursive t)
    ;; Confirm the dependent resource was recursively deleted with it
    (restagraph:log-message :info "TEST Confirm the parent resource is gone")
    (fiveam:is (null (restagraph:get-resources
                       *server*
                       (format nil "/~A/~A" parent-type parent-uid))))
    (restagraph:log-message :info "TEST Confirm the dependent resource is gone")
    (fiveam:is (null (restagraph:get-resources
                       *server*
                       (format nil "/~A/~A" child-type child-uid))))
    ;; Tear down the fixtures
    (restagraph:log-message :info "TEST Remove the fixtures")
    (restagraph:delete-resourcetype *server* parent-type)
    (restagraph:delete-resourcetype *server* child-type)))

(fiveam:test
  resources-dependent-compound
  :depends-on 'resources-dependent-simple
  "Basic operations on 2-layered dependent resources"
  (let ((parent-type "routers")
        (parent-uid "bikini")
        (relationship "Interfaces")
        (child-type "interfaces")
        (child-uid "eth0")
        (child-relationship "Addresses")
        (grandchild-type "ipv4Addresses")
        (grandchild-uid "192.168.24.1"))
    ;; Create the fixtures
    (restagraph:log-message :info "TEST Create the fixtures")
    (restagraph:add-resourcetype *server* parent-type)
    (restagraph:add-resourcetype *server* child-type :dependent t)
    (restagraph:add-resourcetype *server* grandchild-type :dependent t)
    (restagraph:add-resource-relationship *server* parent-type relationship child-type :dependent t)
    (restagraph:add-resource-relationship *server* child-type child-relationship grandchild-type :dependent t)
    ;; Create the parent resource
    (restagraph:store-resource *server* parent-type `(("uid" . ,parent-uid)))
    ;; Create the child resource
    (restagraph:store-dependent-resource
      *server*
      (format nil "/~A/~A/~A/~A" parent-type parent-uid relationship child-type)
      `(("uid" . ,child-uid)))
    ;; Create the grandchild resource
    (restagraph:store-dependent-resource
      *server*
      (format nil "/~A/~A/~A/~A/~A/~A/~A"
              parent-type parent-uid
              relationship child-type child-uid
              child-relationship grandchild-type)
      `(("uid" . ,grandchild-uid)))
    ;; Delete the parent resource
    (restagraph:log-message :info "TEST Recursively deleting the parent resource")
    (restagraph:delete-resource-by-path
      *server*
      (format nil "/~A/~A" parent-type parent-uid)
      :recursive t)
    ;; Confirm the dependent resources were recursively deleted with it
    (restagraph:log-message :info "TEST Confirm the dependent resource is gone")
    (fiveam:is (null (restagraph:get-resources
                       *server*
                       (format nil "/~A/~A/~A/~A/~A"
                               parent-type parent-uid relationship child-type child-uid))))
    (restagraph:log-message :info "TEST Confirm the grandchild resource is gone")
    (fiveam:is (null
                 (restagraph:get-resources
                   *server*
                   (format nil "/~A/~A/~A/~A/~A/~A/~A/~A"
                           parent-type parent-uid relationship
                           child-type child-uid child-relationship
                           grandchild-type grandchild-uid))))
    (restagraph:log-message :info "TEST resources-dependent is complete")
    ;; Tear down the fixtures
    (restagraph:log-message :info "TEST Remove the fixtures")
    (restagraph:delete-resourcetype *server* parent-type)
    (restagraph:delete-resourcetype *server* child-type)
    (restagraph:delete-resourcetype *server* grandchild-type)))

(fiveam:test
  resources-dependent-moving
  :depends-on 'resources-dependent-compound
  "Moving a dependent resource to a new parent"
  (let ((p1-type "routers")
        (p1-uid "woomera")
        (p1-target-rel "Addresses")
        (p2-type "interfaces")
        (p2-uid "eth1")
        (p1-p2-rel "Interfaces")
        (p2-target-rel "Addresses")
        (target-type "ipv4Addresses")
        (target-uid "172.20.0.1"))
    ;; Create the fixtures
    (restagraph:log-message :info "TEST Create the fixtures")
    (restagraph:add-resourcetype *server* p1-type)
    (restagraph:add-resourcetype *server* p2-type :dependent t)
    (restagraph:add-resourcetype *server* target-type :dependent t)
    (restagraph:add-resource-relationship *server* p1-type p1-target-rel target-type :dependent t)
    (restagraph:add-resource-relationship *server* p1-type p1-p2-rel p2-type :dependent t)
    (restagraph:add-resource-relationship *server* p2-type p2-target-rel target-type :dependent t)
    ;; Create initial parent
    (restagraph:store-resource *server* p1-type `(("uid" . ,p1-uid)))
    ;; Create second parent as dependent on the initial
    (restagraph:store-dependent-resource
      *server*
      (format nil "/~A/~A/~A/~A" p1-type p1-uid p1-p2-rel p2-type)
      `(("uid" . ,p2-uid)))
    ;; Create the dependent resource to be moved
    (restagraph:store-dependent-resource
      *server*
      (format nil "/~A/~A/~A/~A" p1-type p1-uid p1-target-rel target-type)
      `(("uid" . ,target-uid)))
    ;; Move the resource
    (fiveam:is
      (null
        (neo4cl:extract-data-from-get-request
          (restagraph:move-dependent-resource
            *server*
            ;; URI
            (format nil "/~A/~A/~A/~A/~A"
                    p1-type p1-uid p1-target-rel target-type target-uid)
            ;; New parent
            (format nil "/~A/~A/~A/~A/~A/~A"
                    p1-type p1-uid p1-p2-rel p2-type p2-uid p2-target-rel)))))
    ;; Confirm the target resource is now at the new target path
    (fiveam:is
      (equal
        (restagraph:get-resources
          *server*
          (format nil "/~A/~A/~A/~A/~A/~A/~A/~A"
                  p1-type p1-uid p1-p2-rel p2-type p2-uid p2-target-rel target-type target-uid))
        `((:uid . ,target-uid) (:original--uid . ,target-uid))))
    ;; Confirm the target resource is no longer present at the original path
    (fiveam:is
      (null
        (restagraph:get-resources
          *server*
          (format nil "/~A/~A/~A/~A/~A"
                  p1-type p1-uid p1-target-rel target-type target-uid))))
    ;; Delete the parent resource
    (restagraph:delete-resource-by-path
      *server*
      (format nil "/~A/~A" p1-type p1-uid)
      :recursive t)
    ;; Confirm stuff is gone
    (fiveam:is (null
                 (restagraph:get-resources *server* (format nil "/~A/~A" p1-type p1-uid))))
    ;; Delete the fixtures
    (restagraph:log-message :info "TEST Delete the fixtures")
    (restagraph:delete-resourcetype *server* p1-type)
    (restagraph:delete-resourcetype *server* p2-type)
    (restagraph:delete-resourcetype *server* target-type)))

(fiveam:test
  resources-multiple
  :depends-on 'resources-basic
  "Confirm we can retrieve all resources of a given type"
  (let ((resourcetype "routers")
        (res1uid "amchitka")
        (res2uid "bikini")
        (res3uid "mururoa"))
    ;; Set up the fixtures
    (restagraph:log-message :info "TEST Set up the fixtures")
    (restagraph:add-resourcetype *server* resourcetype)
    ;; Confirm we have no instances of that resource in place now
    (fiveam:is (null (restagraph:get-resources *server* (format nil "/~A" resourcetype))))
    ;; Add one of that kind of resource
    (restagraph:store-resource *server* resourcetype `(("uid" . ,res1uid)))
    ;; Confirm we now get a list containing exactly that resource
    (fiveam:is (equal
                 `(((:uid . ,res1uid) (:original--uid . ,(restagraph:sanitise-uid res1uid))))
                 (restagraph:get-resources *server* (format nil "/~A" resourcetype))))
    ;; Add a second of that kind of resource
    (restagraph:store-resource *server* resourcetype `(("uid" . ,res2uid)))
    ;; Confirm we now get a list containing both resources
    (fiveam:is (equal
                 (sort-results
                   `(((:uid . ,res1uid)
                      (:original--uid . ,(restagraph:sanitise-uid res1uid)))
                     ((:uid . ,res2uid)
                      (:original--uid . ,(restagraph:sanitise-uid res2uid)))))
                 (sort-results
                   (restagraph:get-resources *server* (format nil "/~A" resourcetype)))))
    ;; Add a third of that kind of resource
    (restagraph:store-resource *server* resourcetype `(("uid" . ,res3uid)))
    ;; Confirm we now get a list containing both resources
    (fiveam:is (equal
                 (sort-results
                   `(((:uid . ,res1uid)
                      (:original--uid . ,(restagraph:sanitise-uid res1uid)))
                     ((:uid . ,res2uid)
                      (:original--uid . ,(restagraph:sanitise-uid res2uid)))
                     ((:uid . ,res3uid)
                      (:original--uid . ,(restagraph:sanitise-uid res3uid)))))
                 (sort-results
                   (restagraph:get-resources *server* (format nil "/~A" resourcetype)))))
    ;; Delete all the resources we added
    (restagraph:delete-resource-by-path *server* (format nil "/~A/~A" resourcetype res1uid))
    (restagraph:delete-resource-by-path *server* (format nil "/~A/~A" resourcetype res2uid))
    (restagraph:delete-resource-by-path *server* (format nil "/~A/~A" resourcetype res3uid))
    ;; Remove the fixtures
    (restagraph:log-message :info "TEST Remove the fixtures")
    (restagraph:delete-resourcetype *server* resourcetype)))

(fiveam:test
  resources-filtering
  :depends-on 'resources-multiple
  "Filtering while searching for resources"
  (let ((r1type "routers")
        (r1uid "upshot")
        (r1partial "upsh.*")
        (rel "Interfaces")
        (r2type "interfaces")
        (r2uid "eth1/41")
        (r2partial "eth1.*"))
    ;; Create the fixtures
    (restagraph:log-message :info "TEST Create the fixtures")
    (restagraph:add-resourcetype *server* r1type)
    (restagraph:add-resourcetype *server* r2type :dependent t)
    (restagraph:add-resource-relationship *server* r1type rel r2type :dependent t)
    ;; Do the filters do what we expect?
    ;; Store a resource to check on
    (restagraph:log-message :info "TEST Creating the primary resource")
    (restagraph:store-resource *server* r1type `(("uid" . ,r1uid)))
    ;; Search for it by type and exact UID
    (restagraph:log-message :info "TEST Searching for the primary resource")
    (fiveam:is (equal
                 `(((:UID . ,r1uid) (:ORIGINAL--UID . ,r1uid)))
                 (restagraph:get-resources
                   *server*
                   (format nil "/~A" r1type)
                   `(("uid" . ,r1uid)))))
    ;; Search for it by type and partial UID
    (fiveam:is (equal
                 `(((:UID . ,r1uid) (:ORIGINAL--UID . ,r1uid)))
                 (restagraph:get-resources
                   *server*
                   (format nil "/~A" r1type)
                   `(("uid" . ,r1partial)))))
    ;; Add a dependent resource to search for
    (restagraph:log-message :info "TEST Creating the secondary resource")
    (restagraph:store-dependent-resource
      *server*
      (format nil "/~A/~A/~A/~A" r1type r1uid rel r2type)
      `(("uid" . ,r2uid)))
    ;; Confirm it's actually there
    (fiveam:is (equal `((,rel ,r2type ,(restagraph:sanitise-uid r2uid)))
                      (restagraph:get-dependent-resources
                        *server* (list r1type r1uid))))
    ;; Search for it by relationship to parent and exact UID
    (restagraph:log-message :info "TEST Searching for the secondary resource")
    (fiveam:is (equal
                 `(((:UID . ,(restagraph:sanitise-uid r2uid)) (:ORIGINAL--UID . ,r2uid)))
                 (restagraph:get-resources
                   *server*
                   (format nil "/~A/~A/~A/~A" r1type r1uid rel r2type)
                   `(("uid" . ,(restagraph:sanitise-uid r2uid))))))
    ;; Search for it by type and partial UID
    (fiveam:is (equal
                 `(((:UID . ,(restagraph:sanitise-uid r2uid)) (:ORIGINAL--UID . ,r2uid)))
                 (restagraph:get-resources
                   *server*
                   (format nil "/~A/~A/~A/~A" r1type r1uid rel r2type)
                   `(("uid" . ,r2partial)))))
    ;; Clean up: delete the primary and dependent resources.
    (restagraph:log-message :info "TEST Cleanup: removing the resources")
    (restagraph:delete-resource-by-path
      *server*
      (format nil "/~A/~A" r1type r1uid)
      :recursive t)
    ;; Delete the fixtures
    (restagraph:log-message :info "TEST Delete the fixtures")
    (restagraph:delete-resourcetype *server* r1type)
    (restagraph:delete-resourcetype *server* r2type)))

(fiveam:test
  relationships
  :depends-on 'resources-basic
  "Basic operations on relationships between resources"
  (let ((from-type "routers")
        (from-uid "bikini")
        (relationship "Asn")
        (to-type "asn")
        (to-uid "64512"))
    ;; Create the fixtures
    (restagraph:log-message :info "TEST Create the fixtures")
    (restagraph:add-resourcetype *server* from-type)
    (restagraph:add-resourcetype *server* to-type)
    (restagraph:add-resource-relationship *server* from-type relationship to-type)
    ;; Store the router
    (restagraph:log-message :info "TEST Creating the resources")
    (restagraph:store-resource *server* from-type `(("uid" . ,from-uid)))
    ;; Create the interface
    (restagraph:store-resource *server* to-type `(("uid" . ,to-uid)))
    ;; Create a relationship between them
    (restagraph:log-message :info "TEST Create the relationship")
    (multiple-value-bind (result code message)
      (restagraph:create-relationship-by-path
        *server*
        (format nil "/~A/~A/~A" from-type from-uid relationship)
        (format nil "/~A/~A" to-type to-uid))
      (declare (ignore result) (ignore message))
      (fiveam:is (equal 200 code)))
    ;; Confirm the relationship is there
    (restagraph:log-message :info "TEST Confirm the relationship")
    (fiveam:is (equal
                 `((("resource-type" . ,to-type) ("uid" . ,to-uid)))
                 (restagraph:get-resources-with-relationship *server* from-type from-uid relationship)))
    ;; Delete the relationship
    (restagraph:log-message :info "TEST Delete the relationship")
    (multiple-value-bind (result code message)
      (restagraph:delete-relationship-by-path
        *server*
        (format nil "/~A/~A/~A"
                from-type from-uid relationship)
        (format nil "/~A/~A"
                to-type to-uid))
      (declare (ignore result) (ignore message))
      (fiveam:is (equal 200 code)))
    ;; Delete the router
    (restagraph:log-message :info "TEST Cleanup: removing the resources")
    (restagraph:delete-resource-by-path *server* (format nil "/~A/~A" from-type from-uid))
    ;; Delete the interface
    (restagraph:delete-resource-by-path *server* (format nil "/~A/~A" to-type to-uid))
    ;; Delete the fixtures
    (restagraph:log-message :info "TEST Delete the fixtures")
    (restagraph:delete-resourcetype *server* from-type)
    (restagraph:delete-resourcetype *server* to-type)))

(fiveam:test
  relationships-integrity
  :depends-on 'relationships
  "Basic operations on relationships between resources"
  (let ((from-type "routers")
        (from-uid "bikini")
        (relationship "Asn")
        (to-type "asn")
        (to-uid "64512"))
    ;; Create the fixtures
    (restagraph:log-message :info "TEST Create the fixtures")
    (restagraph:add-resourcetype *server* from-type)
    (restagraph:add-resourcetype *server* to-type)
    (restagraph:add-resource-relationship *server* from-type relationship to-type)
    ;; Create the resources
    (restagraph:log-message :info "TEST Creating the resources")
    (restagraph:store-resource *server* from-type `(("uid" . ,from-uid)))
    ;; Create the interface
    (restagraph:store-resource *server* to-type `(("uid" . ,to-uid)))
    ;; Create a relationship between them
    (multiple-value-bind (result code message)
      (restagraph:create-relationship-by-path
        *server*
        (format nil "/~A/~A/~A" from-type from-uid relationship)
        (format nil "/~A/~A" to-type to-uid))
      (declare (ignore result) (ignore message))
      (fiveam:is (equal 200 code)))
    ;; Confirm the relationship is there
    (fiveam:is (equal
                 `((("resource-type" . ,to-type) ("uid" . ,to-uid)))
                 (restagraph:get-resources-with-relationship *server* from-type from-uid relationship)))
    ;; Confirm we get what we expect when checking what's at the end of the path
    (fiveam:is (equal
                 `(((:type . ,to-type)
                    (:uid . ,to-uid)
                    (:original--uid . ,to-uid)))
                 (restagraph:get-resources
                   *server*
                   (format nil "/~A/~A/~A" from-type from-uid relationship))))
    ;; Attempt to create a duplicate relationship between them
    (fiveam:signals (restagraph:integrity-error
                      (format nil "Relationship ~A already exists from ~A ~A to ~A ~A"
                              relationship from-type from-uid to-type to-uid))
      (restagraph:create-relationship-by-path
        *server*
        (format nil "/~A/~A/~A" from-type from-uid relationship)
        (format nil "/~A/~A" to-type to-uid)))
    ;; Confirm we still only have one relationship between them
    (fiveam:is (equal
                 `((("resource-type" . ,to-type) ("uid" . ,to-uid)))
                 (restagraph:get-resources-with-relationship *server* from-type from-uid relationship)))
    ;; Delete the relationship
    (multiple-value-bind (result code message)
      (restagraph:delete-relationship-by-path
        *server*
        (format nil "/~A/~A/~A/"
                from-type from-uid relationship)
        (format nil "/~A/~A/" to-type to-uid))
      (declare (ignore result) (ignore message))
      (fiveam:is (equal 200 code)))
    ;; Clean-up: delete the resources
    (restagraph:log-message :info "TEST Cleaning up: removing the resources")
    (restagraph:delete-resource-by-path *server* (format nil "/~A/~A" from-type from-uid))
    (restagraph:delete-resource-by-path *server* (format nil "/~A/~A" to-type to-uid))
    ;; Delete the fixtures
    (restagraph:log-message :info "TEST Delete the fixtures")
    (restagraph:delete-resourcetype *server* from-type)
    (restagraph:delete-resourcetype *server* to-type)))

(fiveam:test
  errors-basic
  :depends-on 'resources-basic
  "Errors that can be triggered just by making a bad request"
  (let ((invalid-resourcetype "IjustMadeThisUpNow")
        (valid-resourcetype "routers"))
    ;; Create the fixtures
    (restagraph:log-message :info "TEST Create the fixtures")
    (restagraph:add-resourcetype *server* valid-resourcetype)
    ;; Create a resource of an invalid type
    (restagraph:log-message :info "TEST Creating a resource of an invalid type")
    (fiveam:signals (restagraph:client-error
                      (format nil "Requested resource type ~A is not valid." invalid-resourcetype))
      (restagraph:store-resource *server* invalid-resourcetype '((:foo . "bar"))))
    ;; Create a resource of a valid type, but without a UID
    (restagraph:log-message :info "TEST Creating a valid resource without a UID")
    (fiveam:signals (restagraph:client-error "UID must be supplied")
      (restagraph:store-resource *server* valid-resourcetype '(("foo" . "bar"))))
    ;; Remove the fixtures
    (restagraph:log-message :info "TEST Remove the fixtures")
    (restagraph:delete-resourcetype *server* valid-resourcetype)))

(fiveam:test
  schema-basic
  "Simple operations to create and delete resource-types and relationships between them."
  (let ((ptype1-name "foo")
        (dtype1-name "bar"))
    ;; Create one primary resource
    (restagraph:log-message :info "Create one primary resource")
    (fiveam:is (restagraph:add-resourcetype *server* ptype1-name))
    ;; Confirm it's there
    (restagraph:log-message :info "Confirm presence of single primary resource")
    (fiveam:is (equal
                 `(((:NAME . ,ptype1-name)))
                 (restagraph:get-resource-types *server*)))
    ;; Delete the single primary resource
    (restagraph:log-message :info "TEST Delete one primary resource")
    (fiveam:is (restagraph:delete-resourcetype *server* ptype1-name))
    ;; Create a single dependent resource
    (restagraph:log-message :info "TEST Create a single dependent resource")
    (fiveam:is (restagraph:add-resourcetype *server* dtype1-name :dependent t))
    ;; Confirm the presence of the single dependent resource
    (restagraph:log-message :info "TEST Confirm the presence of the single dependent resource")
    (fiveam:is (equal
                 `(((:NAME . ,dtype1-name) (:DEPENDENT . "true")))
                 (restagraph:get-resource-types *server*)))
    ;; Delete the single dependent resource
    (restagraph:log-message :info "TEST Delete the single dependent resource")
    (fiveam:is (restagraph:delete-resourcetype *server* dtype1-name))))

(fiveam:test
  schema-relationships
  :depends-on 'schema-basic
  "Relationships between resource types in the schema"
  (let ((p1type-name "rum")
        (d1type-name "cola")
        (rel1name "complements")
        ;(cardinality1 "1:1")
        )
    ;; Create the fixtures
    (restagraph:log-message :info "Creating test fixtures")
    (restagraph:add-resourcetype *server* p1type-name)
    (restagraph:add-resourcetype *server* d1type-name)
    ;; Create a simple (non-dependent) relationship between them
    (restagraph:log-message :info "TEST Create simple relationship between resources")
    (fiveam:is (restagraph:add-resource-relationship *server* p1type-name rel1name d1type-name))
    ;; Delete that simple relationship
    (restagraph:log-message :info "TEST Delete simple relationship between resources")
    (fiveam:is (restagraph:delete-resource-relationship *server* p1type-name rel1name d1type-name))
    ;; Create a dependent relationship between them
    (restagraph:log-message :info "TEST Create dependent relationship between resources")
    (fiveam:is (restagraph:add-resource-relationship *server* p1type-name rel1name d1type-name :dependent t))
    ;; Delete that dependent relationship
    (restagraph:log-message :info "TEST Delete dependent relationship between resources")
    (fiveam:is (restagraph:delete-resource-relationship *server* p1type-name rel1name d1type-name))
    ;; Delete the fixtures
    (restagraph:log-message :info "Deleting test fixtures")
    (restagraph:delete-resourcetype *server* p1type-name)
    (restagraph:delete-resourcetype *server* d1type-name)))

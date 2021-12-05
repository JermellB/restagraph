;   Copyright 2017 James Fleming <james@electronic-quill.net>
;
;   Licensed under the GNU General Public License
;   - for details, see LICENSE.txt in the top-level directory


;;;; Test suite for all of restagraph
;;;;
;;;; Beware: it currently only tests _expected_ cases,
;;;; and does not test edge-cases or wrong input.

(in-package #:restagraph-test)

(declaim (optimize (compilation-speed 0)
                   (speed 2)
                   (safety 3)
                   (debug 3)))


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

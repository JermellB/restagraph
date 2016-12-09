;;;; Test suite for all of syscat
;;;;
;;;; Beware: it currently only tests _expected_ cases,
;;;; and does not test edge-cases or wrong input.

(in-package #:restagraph-test)

(defparameter *server*
  (restagraph::datastore restagraph::*restagraph-acceptor*))

(setf (getf restagraph::*config-vars* :schema)
        (restagraph::populate-schema *server*))

(fiveam:def-suite main)
(fiveam:in-suite main)

(fiveam:test
  rg-schema
  "Check the core schema operations"
  (let ((schema (make-hash-table :test 'equal)))
    ;; Add a class to the schema
    (fiveam:is (hash-table-p (restagraph::add-resourcetype-to-schema schema "foo" (make-hash-table :test 'equal))))
    ;; Confirm the class' presence in the schema
    (fiveam:is (hash-table-p (restagraph::get-resourcetype-from-schema-by-name schema "foo")))
    ;; Add another class
    (fiveam:is (hash-table-p (restagraph::add-resourcetype-to-schema schema "bar" (make-hash-table))))
    ;; Confirm that we now have exactly the two classes in the schema that we expected
    (fiveam:is (hash-table-p (restagraph::get-resourcetype-from-schema-by-name schema "foo")))
    (fiveam:is (hash-table-p (restagraph::get-resourcetype-from-schema-by-name schema "bar")))
    ;; Add a relationship between the classes
    (fiveam:is (listp (restagraph::add-resource-relationship-to-schema schema "foo" "is-a" "bar")))
    ;; Confirm that the relationship is present
    ))

(fiveam:test
  resources-basic
  "Basic operations on resources"
  (let ((routername "amchitka")
        (routercomment "Test router #1"))
    (multiple-value-bind (result code message)
      (restagraph::store-resource *server* "routers" `(("uid" . ,routername) ("comment" . ,routercomment)))
      (declare (ignore result) (ignore message))
      (fiveam:is (equal 200 code)))
    (fiveam:is (equal
                 (format nil "{\"uid\":\"~A\",\"comment\":\"~A\"}" routername routercomment)
                 (restagraph::get-resource-by-uid *server* "routers" routername)))
    (multiple-value-bind (result code message)
      (restagraph::delete-resource-by-uid *server* "routers" routername)
      (declare (ignore result) (ignore message))
      (fiveam:is (equal 200 code)))))

(fiveam:test
  relationships
  "Basic operations on relationships between resources"
  (let ((routername "amchitka")
        (routercomment "Test-router number 1")
        (interfacename "ge-0/0/0")
        (macaddress "01:23:45:67:89:ab"))
    ;; Store the router
    (multiple-value-bind (results code message)
      (restagraph::store-resource *server* "routers" `(("uid" . ,routername) ("comment" . ,routercomment)))
      (declare (ignore results) (ignore message))
      (fiveam:is (equal 200 code)))
    ;; Confirm it's there
    (fiveam:is (equal
                 (cl-json:encode-json-alist-to-string `((uid . ,routername) (comment . ,routercomment)))
                 (restagraph::get-resource-by-uid *server* "routers" routername)))
    ;; Create the interface
    (multiple-value-bind (results code message)
      (restagraph::store-resource
        *server*
        "interfaces"
        `(("uid" . ,interfacename) ("mac-address" . ,macaddress)))
      (declare (ignore results)
               (ignore message))
      (fiveam:is (equal 200 code)))
    ;; Confirm it's there
    (fiveam:is (equal
                 (cl-json:encode-json-alist-to-string `((uid . ,interfacename) (mac-address . ,macaddress)))
                 (restagraph::get-resource-by-uid *server* "interfaces" interfacename)))
    ;; Create a relationship between them
    (multiple-value-bind (result code message)
      (restagraph::create-relationship *server* "routers" routername "Interfaces" "interfaces" interfacename)
      (declare (ignore result) (ignore message))
      (fiveam:is (equal 200 code)))
    ;; Confirm the relationship is there
    ;; Delete the relationship
    (multiple-value-bind (result code message)
      (restagraph::delete-relationship *server* "routers" routername "Interfaces" "interfaces" interfacename)
      (declare (ignore result) (ignore message))
      (fiveam:is (equal 200 code)))
    ;; Delete the router
    (multiple-value-bind (result code message)
      (restagraph::delete-resource-by-uid *server* "routers" routername)
      (declare (ignore result) (ignore message))
      (fiveam:is (equal 200 code)))
    ;; Delete the interface
    (multiple-value-bind (result code message)
      (restagraph::delete-resource-by-uid *server* "interfaces" interfacename)
      (declare (ignore result) (ignore message))
      (fiveam:is (equal 200 code)))))

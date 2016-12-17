;;;; Test suite for all of syscat
;;;;
;;;; Beware: it currently only tests _expected_ cases,
;;;; and does not test edge-cases or wrong input.

(in-package #:restagraph-test)

(defparameter *server*
  (restagraph::datastore restagraph::*restagraph-acceptor*))

(fiveam:def-suite main)
(fiveam:in-suite main)

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
    (fiveam:is (equal
                 `((("resource-type" . "interfaces") ("uid" . ,interfacename)))
                 (restagraph::get-resources-with-relationship *server* "routers" routername "Interfaces")))
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

(fiveam:test
  relationships-integrity
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
    ;; Create the interface
    (multiple-value-bind (results code message)
      (restagraph::store-resource
        *server*
        "interfaces"
        `(("uid" . ,interfacename) ("mac-address" . ,macaddress)))
      (declare (ignore results)
               (ignore message))
      (fiveam:is (equal 200 code)))
    ;; Create a relationship between them
    (multiple-value-bind (result code message)
      (restagraph::create-relationship *server* "routers" routername "Interfaces" "interfaces" interfacename)
      (declare (ignore result) (ignore message))
      (fiveam:is (equal 200 code)))
    ;; Confirm the relationship is there
    (fiveam:is (equal
                 `((("resource-type" . "interfaces") ("uid" . ,interfacename)))
                 (restagraph::get-resources-with-relationship *server* "routers" routername "Interfaces")))
    ;; Attempt to create a duplicate relationship between them
    (multiple-value-bind (result code message)
      (restagraph::create-relationship *server* "routers" routername "Interfaces" "interfaces" interfacename)
      (declare (ignore result) (ignore message))
      (fiveam:is (equal 200 code)))
    ;; Confirm we still only have one relationship between them
    (fiveam:is (equal
                 `((("resource-type" . "interfaces") ("uid" . ,interfacename)))
                 (restagraph::get-resources-with-relationship *server* "routers" routername "Interfaces")))
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

(fiveam:test
  errors-basic
  "Errors that can be triggered just by making a bad request"
  (let ((invalid-resourcetype "IjustMadeThisUpNow")
        (valid-resourcetype "routers")
        (invalid-attributes '(foo))
        (valid-attributes '("comment")))
    ;; Create a resource of an invalid type
    (fiveam:signals (restagraph:integrity-error
                      (format nil "Requested resource type ~A is not valid." invalid-resourcetype))
      (restagraph::store-resource *server* invalid-resourcetype '((:foo . "bar"))))
    ;; Create a resource of a valid type, but without a UID
    (fiveam:signals (restagraph:client-error "UID must be supplied")
      (restagraph::store-resource *server* valid-resourcetype '(("foo" . "bar"))))
    ;; Create a resource with a UID and a valid type, but another invalid attribute
    (fiveam:signals (restagraph:client-error
                      (format nil "These requested attributes are invalid for the resource-type ~A: ~{~A~^, ~}. Valid attributes are: ~{~A~^, ~}."
                              valid-resourcetype
                              invalid-attributes
                              valid-attributes))
      (restagraph::store-resource *server* valid-resourcetype '(("uid" . "amchitka") (:foo . "bar"))))))

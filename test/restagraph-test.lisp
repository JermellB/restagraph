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
  (let ((restype "routers")
        (uid "amchitka")
        (comment "Test router #1"))
    ;; Confirm the resource isn't already present
    (fiveam:is (equal "{}" (restagraph::get-resource-by-uid *server* restype uid)))
    ;; Store the resource
    (multiple-value-bind (result code message)
      (restagraph::store-resource *server* restype `(("uid" . ,uid) ("comment" . ,comment)))
      (declare (ignore result) (ignore message))
      (fiveam:is (equal 200 code)))
    ;; Confirm it's there
    (fiveam:is (equal
                 (format nil "{\"uid\":\"~A\",\"comment\":\"~A\"}" uid comment)
                 (restagraph::get-resource-by-uid *server* restype uid)))
    ;; Delete it
    (multiple-value-bind (result code message)
      (restagraph::delete-resource-by-uid *server* restype uid)
      (declare (ignore result) (ignore message))
      (fiveam:is (equal 200 code)))
    ;; Confirm it's gone again
    (fiveam:is (equal "{}" (restagraph::get-resource-by-uid *server* restype uid)))))

(fiveam:test
  resources-multiple
  "Confirm we can retrieve all resources of a given type"
  (let ((resourcetype "routers")
        (res1uid "amchitka")
        (res1attrname "comment")
        (res1attrval "Test router")
        (res2uid "bikini")
        (res3uid "mururoa"))
    ;; Confirm we have no instances of that resource in place now
    (fiveam:is (null (restagraph::search-for-resources *server* resourcetype)))
    ;; Add one of that kind of resource
    (restagraph::store-resource *server* resourcetype `(("uid" . ,res1uid) (,res1attrname . ,res1attrval)))
    ;; Confirm we now get a list containing exactly that resource
    (fiveam:is (equal
                 `((((:UID . ,res1uid) (,(intern (string-upcase res1attrname) :keyword) . ,res1attrval))))
                 (restagraph::search-for-resources *server* resourcetype)))
    ;; Add a second of that kind of resource
    (restagraph::store-resource *server* resourcetype `(("uid" . ,res2uid)))
    ;; Confirm we now get a list containing both resources
    (fiveam:is (equal
                 `((((:UID . ,res1uid) (,(intern (string-upcase res1attrname) :keyword) . ,res1attrval)))
                   (((:UID . ,res2uid))))
                 (restagraph::search-for-resources *server* resourcetype)))
    ;; Add a third of that kind of resource
    (restagraph::store-resource *server* resourcetype `(("uid" . ,res3uid)))
    ;; Confirm we now get a list containing both resources
    (fiveam:is (equal
                 `((((:UID . ,res1uid) (,(intern (string-upcase res1attrname) :keyword) . ,res1attrval)))
                   (((:UID . ,res2uid)))
                   (((:UID . ,res3uid))))
                 (restagraph::search-for-resources *server* resourcetype)))
    ;; Delete all the resources we added
    (restagraph::delete-resource-by-uid *server* resourcetype res1uid)
    (restagraph::delete-resource-by-uid *server* resourcetype res2uid)
    (restagraph::delete-resource-by-uid *server* resourcetype res3uid)))

(fiveam:test
  relationships
  "Basic operations on relationships between resources"
  (let ((routername "bikini")
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
      (restagraph::create-relationship *server* "routers" routername "Interfaces" "interfaces" interfacename nil)
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
  (let ((routername "whitesands")
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
      (restagraph::create-relationship *server* "routers" routername "Interfaces" "interfaces" interfacename nil)
      (declare (ignore result) (ignore message))
      (fiveam:is (equal 200 code)))
    ;; Confirm the relationship is there
    (fiveam:is (equal
                 `((("resource-type" . "interfaces") ("uid" . ,interfacename)))
                 (restagraph::get-resources-with-relationship *server* "routers" routername "Interfaces")))
    ;; Attempt to create a duplicate relationship between them
    (fiveam:signals (restagraph:integrity-error
                      (format nil "Relationship ~A already exists from ~A ~A to ~A ~A"
                              "Interfaces" "routers" routername "interfaces" interfacename))
      (restagraph::create-relationship *server* "routers" routername "Interfaces" "interfaces" interfacename nil))
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
  relationships-plus-resources
  "Creating resources along with the connecting relationship."
  (let ((source-type "routers")
        (source-uid "mururoa")
        (reltype "Interfaces")
        (dest-type "interfaces")
        (dest-uid "ge-0/0/0")
        (dest-attributes (cl-json:decode-json-from-string
                           "{\"enabled\": \"true\", \"mac-address\": \"ab:cd:ef:12:34:56\"}")))
    ;; Confirm that neither the source nor destination resource is present
    (fiveam:is (equal "{}" (restagraph::get-resource-by-uid *server* source-type source-uid)))
    (fiveam:is (equal "{}" (restagraph::get-resource-by-uid *server* dest-type dest-uid)))
    ;; Create the source resource
    (multiple-value-bind (results code message)
      (restagraph::store-resource *server* source-type `(("uid" . ,source-uid)))
      (declare (ignore results) (ignore message))
      (fiveam:is (equal 200 code)))
    ;; Confirm it exists
    (fiveam:is (equal
                 (format nil "{\"uid\":\"~A\"}" source-uid)
                 (restagraph::get-resource-by-uid *server* source-type source-uid)))
    ;; Create the relationship and new destination resource, with no attributes
    (multiple-value-bind (results code message)
      (restagraph::create-relationship
        *server* source-type source-uid reltype dest-type dest-uid nil)
      (declare (ignore results) (ignore message))
      (fiveam:is (equal 200 code)))
    ;; Get the listing of resources linked from the source by this relationship,
    ;; and confirm that it consists of this resource
    (fiveam:is (equal
                 `((("resource-type" . ,dest-type) ("uid" . ,dest-uid)))
                 (restagraph::get-resources-with-relationship *server* source-type source-uid reltype)))
    ;; Try to create a duplicate, and confirm that it fails
    (fiveam:signals (restagraph:integrity-error
                      (format nil
                              "Relationship ~A already exists from ~A ~A to ~A ~A"
                              reltype source-type source-uid dest-type dest-uid))
      (restagraph::create-relationship
        *server* source-type source-uid reltype dest-type dest-uid nil))
    ;; Remove the relationship
    (multiple-value-bind (result code message)
      (restagraph::delete-relationship
        *server* source-type source-uid reltype dest-type dest-uid)
      (declare (ignore result) (ignore message))
      (fiveam:is (equal 200 code)))
    ;; Confirm the relationship is gone
    (fiveam:is (equal `()
                      (restagraph::get-resources-with-relationship
                        *server* source-type source-uid reltype)))
    ;; Confirm both resources still exist
    ;; Remove the destination resource
    (multiple-value-bind (result code message)
      (restagraph::delete-resource-by-uid *server* dest-type dest-uid)
      (declare (ignore result) (ignore message))
      (fiveam:is (equal 200 code)))
    ;; Confirm it's gone
    (fiveam:is (equal "{}" (restagraph::get-resource-by-uid *server* dest-type dest-uid)))
    ;; Create the relationship plus destination resource, this time with an attribute
    (restagraph::log-message
      :debug
      "TEST: Create the relationship plus destination resource, this time with an attribute")
    (multiple-value-bind (results code message)
      (restagraph::create-relationship
        *server* source-type source-uid reltype dest-type dest-uid dest-attributes)
      (declare (ignore results) (ignore message))
      (fiveam:is (equal 200 code)))
    ;; Confirm that the new resource is present, complete with its attributes
    (restagraph::log-message
      :debug
      "TEST: Confirm that the new resource is present, complete with its attributes")
    (fiveam:is (equal
                 (cl-json:encode-json-to-string
                   (cl-json:decode-json-from-string
                     (format nil
                             "{\"uid\":\"~A\", \"mac-address\": \"ab:cd:ef:12:34:56\", \"enabled\": \"true\"}"
                             dest-uid)))
                 (restagraph::get-resource-by-uid *server* dest-type dest-uid)))
    ;; Remove the relationship
    (multiple-value-bind (result code message)
      (restagraph::delete-relationship
        *server* source-type source-uid reltype dest-type dest-uid)
      (declare (ignore result) (ignore message))
      (fiveam:is (equal 200 code)))
    ;; Confirm the relationship is gone
    (fiveam:is (equal `()
                      (restagraph::get-resources-with-relationship
                        *server* source-type source-uid reltype)))
    ;; Confirm both resources still exist
    (fiveam:is (equal
                 (format nil "{\"uid\":\"~A\"}" source-uid)
                 (restagraph::get-resource-by-uid *server* source-type source-uid)))
    ;; Delete both resources
    (multiple-value-bind (result code message)
      (restagraph::delete-resource-by-uid *server* source-type source-uid)
      (declare (ignore result) (ignore message))
      (fiveam:is (equal 200 code)))
    (multiple-value-bind (result code message)
      (restagraph::delete-resource-by-uid *server* dest-type dest-uid)
      (declare (ignore result) (ignore message))
      (fiveam:is (equal 200 code)))
    ;; Confirm that both resources are gone
    (fiveam:is (equal "{}" (restagraph::get-resource-by-uid *server* source-type source-uid)))
    (fiveam:is (equal "{}" (restagraph::get-resource-by-uid *server* dest-type dest-uid)))))

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

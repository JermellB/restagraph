(in-package #:restagraph-test)

(declaim (optimize (compilation-speed 0)
                   (speed 2)
                   (safety 3)
                   (debug 3)))


(fiveam:def-suite
  ipam
  :description "IPAM functionality"
  :in main)

(fiveam:in-suite ipam)


;; Database interactions

(fiveam:test
  ipam-subnets-no-vrf
  "Basic create/read/delete test on subnets. Depth of 1, no VRF"
  (let
    ((schema (restagraph::fetch-current-schema *server*))
     (org "internet")
     (subnet1 (ipaddress:make-ipv4-subnet "172.16.0.0/12")))
    ;; Confirm the fixtures aren't already present
    (fiveam:is (null (restagraph::get-resources *server* (format nil "/Organisations/~A" org))))
    ;; Add the fixtures
    (restagraph::store-resource *server* schema "Organisations" `(("uid" . ,org)))
    ;; Add a top-level subnet; this should return NIL.
    (fiveam:is (restagraph::insert-subnet *server* org "" subnet1 schema))
    ;; Confirm the subnet is there
    (fiveam:is (restagraph::find-subnet *server* org "" subnet1))
    ;; Remove the subnet
    (fiveam:is (restagraph::delete-subnet *server* org "" subnet1 schema))
    ;; Confirm the subnet is gone
    (fiveam:is (not (restagraph::find-subnet *server* org "" subnet1)))
    ;; Remove the fixtures
    (restagraph::delete-resource-by-path *server*
                                        (format nil "/Organisations/~A" org)
                                        schema
                                        :recursive t)))

(fiveam:test
  ipam-subnets-one-vrf
  "Basic create/read/delete test on subnets. Depth of 1, one VRF"
  (let
    ((schema (restagraph::fetch-current-schema *server*))
     (org "internet")
     (vrf "red")
     (subnet1 (ipaddress:make-ipv4-subnet "172.16.0.0/12")))
    ;; Confirm the fixtures aren't already present
    (fiveam:is (null (restagraph::get-resources *server* (format nil "/Organisations/~A" org))))
    ;; Add the fixtures
    (restagraph::store-resource *server* schema "Organisations" `(("uid" . ,org)))
    (restagraph::store-dependent-resource
      *server*
      schema
      (format nil "/Organisations/~A/VRF_GROUPS/VrfGroups" org)
      `(("uid" . ,vrf)))
    ;; Add a top-level subnet; this should return NIL.
    (fiveam:is (restagraph::insert-subnet *server* org vrf subnet1 schema))
    ;; Confirm the subnet is there
    (fiveam:is (restagraph::find-subnet *server* org vrf subnet1))
    ;; Remove the subnet
    (fiveam:is (restagraph::delete-subnet *server* org vrf subnet1 schema))
    ;; Confirm the subnet is gone
    (fiveam:is (not (restagraph::find-subnet *server* org vrf subnet1)))
    ;; Remove the fixtures
    (restagraph::delete-resource-by-path *server*
                                        (format nil "/Organisations/~A" org)
                                        schema
                                        :recursive t)))

(fiveam:test
  ipam-subnets-2-levels-no-vrf
  "Create/read/delete tests on nested subnets directly under an organisation."
  (let
    ((schema (restagraph::fetch-current-schema *server*))
     (org "testco")
     (subnet1 (ipaddress:make-ipv4-subnet "172.16.0.0/12"))
     (subnet2 (ipaddress:make-ipv4-subnet "172.18.0.0/23")))
    ;; Confirm the fixtures aren't already present
    (fiveam:is (null (restagraph::get-resources *server* (format nil "/Organisations/~A" org))))
    ;; Add the fixtures
    (restagraph::log-message :debug "TEST Creating the fixtures.")
    (restagraph::store-resource *server* schema "Organisations" `(("uid" . ,org)))
    ;; Add a top-level subnet; this should return NIL.
    (restagraph::log-message :debug "TEST Add a top-level subnet.")
    (fiveam:is (restagraph::insert-subnet *server* org "" subnet1 schema))
    ;; Confirm the subnet is there
    (restagraph::log-message :debug "TEST Confirm the top-level subnet is present.")
    (fiveam:is (restagraph::find-subnet *server* org "" subnet1))
    ;; Add another subnet
    (restagraph::log-message :debug "TEST Add a second-level subnet.")
    (fiveam:is (restagraph::insert-subnet *server* org "" subnet2 schema))
    ;; Confirm that's also there
    (restagraph::log-message :debug "TEST Confirm the second-level subnet is present.")
    (fiveam:is (restagraph::find-subnet *server* org "" subnet2))
    ;; Remove the second subnet
    (restagraph::log-message :debug "TEST Delete the second-level subnet.")
    (fiveam:is (restagraph::delete-subnet *server* org "" subnet2 schema))
    ;; Remove the top-level subnet
    (restagraph::log-message :debug "TEST Delete the top-level subnet.")
    (fiveam:is (restagraph::delete-subnet *server* org "" subnet1 schema))
    ;; Confirm the top-level subnet is gone
    (fiveam:is (not (restagraph::find-subnet *server* org "" subnet1)))
    ;; Remove the fixtures
    (restagraph::log-message :debug "TEST Deleting the fixtures.")
    (restagraph::delete-resource-by-path *server* (format nil "/Organisations/~A" org) schema :recursive t)))

(fiveam:test
  ipam-subnets-3-levels-no-vrf
  "Create/read/delete tests on nested subnets directly under an organisation."
  (let
    ((schema (restagraph::fetch-current-schema *server*))
     (org "testco")
     (subnet1 (ipaddress:make-ipv4-subnet "172.16.0.0/12"))
     (subnet2 (ipaddress:make-ipv4-subnet "172.16.19.0/24"))
     (subnet3 (ipaddress:make-ipv4-subnet "172.16.18.0/23")))
    ;; Confirm the fixtures aren't already present
    (fiveam:is (null (restagraph::get-resources *server* (format nil "/Organisations/~A" org))))
    ;; Add the fixtures
    (restagraph::log-message :debug "TEST Creating the fixtures.")
    (restagraph::store-resource *server* schema "Organisations" `(("uid" . ,org)))
    ;; Add a top-level subnet; this should return NIL.
    (restagraph::log-message :debug "TEST Add a top-level subnet.")
    (fiveam:is (restagraph::insert-subnet *server* org "" subnet1 schema))
    ;; Confirm the subnet is there
    (restagraph::log-message :debug "TEST Confirm the top-level subnet is present.")
    (fiveam:is (equal (list (restagraph::make-subnet-uid subnet1))
                      (mapcar #'restagraph::make-subnet-uid
                              (restagraph::find-subnet *server* org "" subnet1))))
    ;; Add a second subnet
    (restagraph::log-message :debug "TEST Add a second-level subnet.")
    (fiveam:is (restagraph::insert-subnet *server* org "" subnet2 schema))
    ;; Confirm that's also there
    (restagraph::log-message :debug "TEST Confirm the second-level subnet is present.")
    (fiveam:is (equal
                 (mapcar #'restagraph::make-subnet-uid
                         (list subnet1 subnet2))
                 (mapcar #'restagraph::make-subnet-uid
                         (restagraph::find-subnet *server* org "" subnet2))))
    ;; Add a third subnet
    (restagraph::log-message :debug "TEST Add a third subnet between the first two.")
    (fiveam:is (restagraph::insert-subnet *server* org "" subnet3 schema))
    ;; Confirm that's also there
    (restagraph::log-message :debug "TEST Confirm the new second-level subnet is present.")
    (fiveam:is (restagraph::find-subnet *server* org "" subnet3))
    ;; Confirm it's correctly moved the second subnet
    (restagraph::log-message :debug "TEST Confirm the original second-level subnet is now third.")
    (fiveam:is (equal
                 (mapcar #'restagraph::make-subnet-uid
                         (list subnet1 subnet3 subnet2))
                 (mapcar #'restagraph::make-subnet-uid
                         (restagraph::find-subnet *server* org "" subnet2))))
    ;; Remove the top-level subnet
    (restagraph::log-message :debug "TEST Delete the top-level subnet.")
    (fiveam:is (restagraph::delete-subnet *server* org "" subnet1 schema))
    ;; Confirm the top-level subnet is gone
    (fiveam:is (not (restagraph::find-subnet *server* org "" subnet1)))
    ;; Remove the second subnet
    (restagraph::log-message :debug "TEST Delete the now third-level subnet.")
    (fiveam:is (restagraph::delete-subnet *server* org "" subnet2 schema))
    ;; Remove the fixtures
    (restagraph::log-message :debug "TEST Deleting the fixtures.")
    (restagraph::delete-resource-by-path *server* (format nil "/Organisations/~A" org) schema :recursive t)))

(fiveam:test
  ipv4address-basic
  "Basic create/find/delete operations on an IPv4 address"
  (let ((schema (restagraph::fetch-current-schema *server*))
        (org "example")
        (address (ipaddress:make-ipv4-address "172.17.2.3"))
        (vrf "green")
        (subnet (ipaddress:make-ipv4-subnet "172.17.2.0/24")))
    ;; Ensure we're clear to start
    (fiveam:is (null (restagraph::get-resources *server* (format nil "/Organisations/~A" org))))
    ;; Create fixtures
    (restagraph::log-message :debug "Creating fixtures with one VRF")
    (restagraph::store-resource *server* schema "Organisations" `(("uid" . ,org)))
    (restagraph::store-dependent-resource
      *server*
      schema
      (format nil "/Organisations/~A/VRF_GROUPS/VrfGroups" org)
      `(("uid" . ,vrf)))
    (restagraph::insert-subnet *server* org vrf subnet schema)
    ;; Tests
    (restagraph::log-message :debug "TEST Address is absent")
    (fiveam:is (null (restagraph::find-ipaddress *server* address org vrf)))
    (restagraph::log-message :debug "TEST Insert address")
    (fiveam:is (restagraph::insert-ipaddress *server* schema address org vrf))
    (fiveam:is (equal (ipaddress:as-string address)
                      (car (last (restagraph::find-ipaddress *server* address org vrf)))))
    (restagraph::log-message :debug "TEST Delete address")
    (fiveam:is (null (restagraph::delete-ipaddress *server* schema address org vrf)))
    (fiveam:is (null (restagraph::find-ipaddress *server* address org vrf)))
    ;; Remove fixtures
    (restagraph::delete-resource-by-path *server* (format nil "/Organisations/~A" org) schema :recursive t)
    ;; Ensure the fixtures are gone
    (fiveam:is (null (restagraph::get-resources *server* (format nil "/Organisations/~A" org))))))

(fiveam:test
  ipv4-subnets-and-addresses-basic
  "Basic tests for adding and removing subnets with addresses"
  (let
    ((schema (restagraph::fetch-current-schema *server*))
     (org "sample")
     (subnet1 (ipaddress:make-ipv4-subnet "192.168.0.0/16"))
     (subnet2 (ipaddress:make-ipv4-subnet "192.168.32.0/23"))
     (address (ipaddress:make-ipv4-address "192.168.32.3")))
    ;; Confirm the fixtures aren't already present
    (fiveam:is (null (restagraph::get-resources *server* (format nil "/Organisations/~A" org))))
    ;; Add the fixtures
    (restagraph::log-message :info "TEST Creating the fixtures.")
    (restagraph::store-resource *server* schema "Organisations" `(("uid" . ,org)))
    (restagraph::insert-subnet *server* org "" subnet1 schema)
    ;; Add the IP address
    (restagraph::log-message :info "TEST Add the IP address")
    (fiveam:is (restagraph::insert-ipaddress *server* schema address org ""))
    ;; Confirm the address is there
    (restagraph::log-message :info "TEST Confirm the address is present.")
    (fiveam:is (restagraph::find-ipaddress *server* address org ""))
    (fiveam:is (equal (ipaddress:as-string address)
                      (car (last (restagraph::find-ipaddress *server* address org "")))))
    ;; Add another subnet
    (restagraph::log-message :info "TEST Add a second-level subnet.")
    (fiveam:is (restagraph::insert-subnet *server* org "" subnet2 schema))
    ;; Confirm that's also there
    (restagraph::log-message :info "TEST Confirm the second-level subnet is present.")
    (fiveam:is (equal (mapcar #'ipaddress:as-cidr (list subnet1 subnet2))
                      (mapcar #'ipaddress:as-cidr (restagraph::find-subnet *server* org "" subnet2))))
    ;; Confirm the address has the correct new path
    (restagraph::log-message :info "TEST Confirm the address has been correctly moved.")
    (let ((newpath (restagraph::find-ipaddress *server* address org "")))
      (fiveam:is (equal (mapcar #'ipaddress:as-cidr (list subnet1 subnet2))
                        (mapcar #'ipaddress:as-cidr (butlast newpath))))
      (fiveam:is (equal (ipaddress:as-string address)
                        (car (last newpath)))))
    ;; Remove the second subnet
    (restagraph::log-message :info "TEST Delete the second-level subnet.")
    (fiveam:is (restagraph::delete-subnet *server* org "" subnet2 schema))
    ;; Confirm the address has moved back again
    (restagraph::log-message :info "TEST Confirm the address is back under the top-level subnet.")
    (fiveam:is (restagraph::find-ipaddress *server* address org ""))
    (let ((newpath (restagraph::find-ipaddress *server* address org "")))
      (fiveam:is (equal (list (ipaddress:as-cidr subnet1))
                        (mapcar #'ipaddress:as-cidr (butlast newpath))))
      (fiveam:is (equal (ipaddress:as-string address)
                        (car (last newpath)))))
    ;; Remove the fixtures
    (restagraph::log-message :info "TEST Deleting the fixtures.")
    (restagraph::delete-resource-by-path *server* (format nil "/Organisations/~A" org) schema :recursive t)))

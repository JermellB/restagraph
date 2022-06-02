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
  ipam-ipv4-subnets-no-vrf
  "Basic create/read/delete test on IPv4 subnets. Depth of 1, no VRF"
  (let* ((org "internet")
         (subnet1 (ipaddress:make-ipv4-subnet "172.16.0.0/12"))
         (policy :ALLOW-ALL)
         (session (neo4cl:establish-bolt-session *bolt-server*))
         (schema-version (restagraph::create-new-schema-version session)))
    ;; Install the core schema
    (restagraph::install-subschema session restagraph::*core-schema* schema-version)
    (let ((schema (restagraph::fetch-current-schema session)))
      ;; Install the default set of resources
      (restagraph::install-default-resources session)
      ;; Confirm the fixtures aren't already present
      (fiveam:is (null (restagraph::get-resources session (format nil "/Organisations/~A" org))))
      ;; Add the fixtures
      (restagraph::store-resource session schema "Organisations" `(("uid" . ,org)) *admin-user*)
      ;; Add a top-level subnet; this should return NIL.
      (fiveam:is (restagraph::insert-subnet session org "" subnet1 schema policy))
      ;; Confirm the subnet is there
      (fiveam:is (restagraph::find-subnet session org "" subnet1))
      ;; Remove the subnet
      (fiveam:is (not (restagraph::delete-subnet session org "" subnet1 schema)))
      ;; Confirm the subnet is gone
      (fiveam:is (not (restagraph::find-subnet session org "" subnet1)))
      ;; Remove the fixtures
      (restagraph::delete-resource-by-path session
                                           (format nil "/Organisations/~A" org)
                                           schema
                                           :recursive t)
      (restagraph::delete-schema-version session schema-version))
    (neo4cl:disconnect session)))

(fiveam:test
  ipam-ipv6-subnets-no-vrf
  "Basic create/read/delete test on IPv6 subnets. Depth of 1, no VRF"
  (let* ((org "internet")
         (subnet1 (ipaddress:make-ipv6-subnet "2001:db8::/32"))
         (policy :ALLOW-ALL)
         (session (neo4cl:establish-bolt-session *bolt-server*))
         (schema-version (restagraph::create-new-schema-version session)))
    ;; Install the core schema
    (restagraph::install-subschema session restagraph::*core-schema* schema-version)
    (let ((schema (restagraph::fetch-current-schema session)))
      ;; Install the default set of resources
      (restagraph::install-default-resources session)
      ;; Confirm the fixtures aren't already present
      (fiveam:is (null (restagraph::get-resources session (format nil "/Organisations/~A" org))))
      ;; Add the fixtures
      (restagraph::store-resource session schema "Organisations" `(("uid" . ,org)) *admin-user*)
      ;; Add a top-level subnet; this should return NIL.
      (fiveam:is (restagraph::insert-subnet session org "" subnet1 schema policy))
      ;; Confirm the subnet is there
      (fiveam:is (restagraph::find-subnet session org "" subnet1))
      ;; Remove the subnet
      (fiveam:is (not (restagraph::delete-subnet session org "" subnet1 schema)))
      ;; Confirm the subnet is gone
      (fiveam:is (not (restagraph::find-subnet session org "" subnet1)))
      ;; Remove the fixtures
      (restagraph::delete-resource-by-path session
                                           (format nil "/Organisations/~A" org)
                                           schema
                                           :recursive t)
      (restagraph::delete-schema-version session schema-version))
    (neo4cl:disconnect session)))

(fiveam:test
  ipam-ipv4-subnets-one-vrf
  "Basic create/read/delete test on IPv4 subnets. Depth of 1, one VRF"
  (let*
    ((org "internet")
     (vrf "red")
     (subnet1 (ipaddress:make-ipv4-subnet "172.16.0.0/12"))
     (policy :ALLOW-ALL)
     (session (neo4cl:establish-bolt-session *bolt-server*))
     (schema-version (restagraph::create-new-schema-version session)))
    ;; Install the core schema
    (restagraph::install-subschema session restagraph::*core-schema* schema-version)
    ;; Now fetch the schema
    (let ((schema (restagraph::fetch-current-schema session)))
      ;; Install the default set of resources
      (restagraph::install-default-resources session)
      ;; Confirm the fixtures aren't already present
      (fiveam:is (null (restagraph::get-resources session (format nil "/Organisations/~A" org))))
      ;; Add the fixtures
      (restagraph::store-resource session schema "Organisations" `(("uid" . ,org)) *admin-user*)
      (restagraph::store-dependent-resource
        session
        schema
        (format nil "/Organisations/~A/VRF_GROUPS/VrfGroups" org)
        `(("uid" . ,vrf))
        *admin-user*)
      ;; Add a top-level subnet; this should return NIL.
      (fiveam:is (restagraph::insert-subnet session org vrf subnet1 schema policy))
      ;; Confirm the subnet is there
      (fiveam:is (restagraph::find-subnet session org vrf subnet1))
      ;; Remove the subnet
      (fiveam:is (null (restagraph::delete-subnet session org vrf subnet1 schema)))
      ;; Confirm the subnet is gone
      (fiveam:is (null (restagraph::find-subnet session org vrf subnet1)))
      ;; Remove the fixtures
      (restagraph::delete-resource-by-path session
                                           (format nil "/Organisations/~A" org)
                                           schema
                                           :recursive t))
    ;; Clean up the mess
    (restagraph::delete-schema-version session schema-version)
    (neo4cl:disconnect session)))

(fiveam:test
  ipam-ipv6-subnets-one-vrf
  "Basic create/read/delete test on IPv6 subnets. Depth of 1, one VRF"
  (let*
    ((org "internet")
     (vrf "red")
     (subnet1 (ipaddress:make-ipv6-subnet "2001:db8::/32"))
     (policy :ALLOW-ALL)
     (session (neo4cl:establish-bolt-session *bolt-server*))
     (schema-version (restagraph::create-new-schema-version session)))
    ;; Install the core schema
    (restagraph::install-subschema session restagraph::*core-schema* schema-version)
    ;; Now fetch the schema
    (let ((schema (restagraph::fetch-current-schema session)))
      ;; Install the default set of resources
      (restagraph::install-default-resources session)
      ;; Confirm the fixtures aren't already present
      (fiveam:is (null (restagraph::get-resources session (format nil "/Organisations/~A" org))))
      ;; Add the fixtures
      (restagraph::store-resource session schema "Organisations" `(("uid" . ,org)) *admin-user*)
      (restagraph::store-dependent-resource
        session
        schema
        (format nil "/Organisations/~A/VRF_GROUPS/VrfGroups" org)
        `(("uid" . ,vrf))
        *admin-user*)
      ;; Add a top-level subnet; this should return NIL.
      (fiveam:is (restagraph::insert-subnet session org vrf subnet1 schema policy))
      ;; Confirm the subnet is there
      (fiveam:is (restagraph::find-subnet session org vrf subnet1))
      ;; Remove the subnet
      (fiveam:is (null (restagraph::delete-subnet session org vrf subnet1 schema)))
      ;; Confirm the subnet is gone
      (fiveam:is (null (restagraph::find-subnet session org vrf subnet1)))
      ;; Remove the fixtures
      (restagraph::delete-resource-by-path session
                                           (format nil "/Organisations/~A" org)
                                           schema
                                           :recursive t))
    ;; Clean up the mess
    (restagraph::delete-schema-version session schema-version)
    (neo4cl:disconnect session)))

(fiveam:test
  ipam-ipv4-subnets-2-levels-no-vrf
  "Create/read/delete tests on nested IPv4 subnets directly under an organisation."
  (let* ((org "testco")
         (subnet1 (ipaddress:make-ipv4-subnet "172.16.0.0/12"))
         (subnet2 (ipaddress:make-ipv4-subnet "172.18.0.0/23"))
         (policy :ALLOW-ALL)
         (session (neo4cl:establish-bolt-session *bolt-server*))
         (schema-version (restagraph::create-new-schema-version session)))
    ;; Install the core schema
    (restagraph::install-subschema session restagraph::*core-schema* schema-version)
    ;; Now fetch the schema
    (let ((schema (restagraph::fetch-current-schema session)))
      ;; Install the default set of resources
      (restagraph::install-default-resources session)
      ;; Confirm the fixtures aren't already present
      (fiveam:is (null (restagraph::get-resources session (format nil "/Organisations/~A" org))))
      ;; Add the fixtures
      (restagraph::log-message :debug ";TEST Creating the fixtures.")
      (restagraph::store-resource session schema "Organisations" `(("uid" . ,org)) *admin-user*)
      ;; Add a top-level subnet; this should return NIL.
      (restagraph::log-message :debug ";TEST Add a top-level subnet.")
      (fiveam:is (restagraph::insert-subnet session org "" subnet1 schema policy))
      ;; Confirm the subnet is there
      (restagraph::log-message :debug ";TEST Confirm the top-level subnet is present.")
      (fiveam:is (restagraph::find-subnet session org "" subnet1))
      ;; Add another subnet
      (restagraph::log-message :debug ";TEST Add a second-level subnet.")
      (fiveam:is (restagraph::insert-subnet session org "" subnet2 schema policy))
      ;; Confirm that's also there
      (restagraph::log-message :debug ";TEST Confirm the second-level subnet is present.")
      (fiveam:is (restagraph::find-subnet session org "" subnet2))
      ;; Remove the second subnet
      (restagraph::log-message :debug ";TEST Delete the second-level subnet.")
      (fiveam:is (null(restagraph::delete-subnet session org "" subnet2 schema)))
      ;; Remove the top-level subnet
      (restagraph::log-message :debug ";TEST Delete the top-level subnet.")
      (fiveam:is (null(restagraph::delete-subnet session org "" subnet1 schema)))
      ;; Confirm the top-level subnet is gone
      (fiveam:is (null (restagraph::find-subnet session org "" subnet1)))
      ;; Remove the fixtures
      (restagraph::log-message :debug ";TEST Deleting the fixtures.")
      (restagraph::delete-resource-by-path session
                                           (format nil "/Organisations/~A" org)
                                           schema
                                           :recursive t))
    ;; Clean up the mess
    (restagraph::delete-schema-version session schema-version)
    (neo4cl:disconnect session)))

(fiveam:test
  ipam-ipv6-subnets-2-levels-no-vrf
  "Create/read/delete tests on nested IPv6 subnets directly under an organisation."
  (let* ((org "testco")
         (subnet1 (ipaddress:make-ipv6-subnet "2001:db8::/32"))
         (subnet2 (ipaddress:make-ipv6-subnet "2001:db8:3456::/48"))
         (policy :ALLOW-ALL)
         (session (neo4cl:establish-bolt-session *bolt-server*))
         (schema-version (restagraph::create-new-schema-version session)))
    ;; Install the core schema
    (restagraph::install-subschema session restagraph::*core-schema* schema-version)
    ;; Now fetch the schema
    (let ((schema (restagraph::fetch-current-schema session)))
      ;; Install the default set of resources
      (restagraph::install-default-resources session)
      ;; Confirm the fixtures aren't already present
      (fiveam:is (null (restagraph::get-resources session (format nil "/Organisations/~A" org))))
      ;; Add the fixtures
      (restagraph::log-message :debug ";TEST Creating the fixtures.")
      (restagraph::store-resource session schema "Organisations" `(("uid" . ,org)) *admin-user*)
      ;; Add a top-level subnet; this should return NIL.
      (restagraph::log-message :debug ";TEST Add a top-level subnet.")
      (fiveam:is (restagraph::insert-subnet session org "" subnet1 schema policy))
      ;; Confirm the subnet is there
      (restagraph::log-message :debug ";TEST Confirm the top-level subnet is present.")
      (fiveam:is (restagraph::find-subnet session org "" subnet1))
      ;; Add another subnet
      (restagraph::log-message :debug ";TEST Add a second-level subnet.")
      (fiveam:is (restagraph::insert-subnet session org "" subnet2 schema policy))
      ;; Confirm that's also there
      (restagraph::log-message :debug ";TEST Confirm the second-level subnet is present.")
      (fiveam:is (restagraph::find-subnet session org "" subnet2))
      ;; Remove the second subnet
      (restagraph::log-message :debug ";TEST Delete the second-level subnet.")
      (fiveam:is (null(restagraph::delete-subnet session org "" subnet2 schema)))
      ;; Remove the top-level subnet
      (restagraph::log-message :debug ";TEST Delete the top-level subnet.")
      (fiveam:is (null(restagraph::delete-subnet session org "" subnet1 schema)))
      ;; Confirm the top-level subnet is gone
      (fiveam:is (null (restagraph::find-subnet session org "" subnet1)))
      ;; Remove the fixtures
      (restagraph::log-message :debug ";TEST Deleting the fixtures.")
      (restagraph::delete-resource-by-path session
                                           (format nil "/Organisations/~A" org)
                                           schema
                                           :recursive t))
    ;; Clean up the mess
    (restagraph::delete-schema-version session schema-version)
    (neo4cl:disconnect session)))

(fiveam:test
  ipam-ipv4-subnets-3-levels-no-vrf
  "Create/read/delete tests on nested IPv4 subnets directly under an organisation."
  (let* ((org "testco")
         (subnet1 (ipaddress:make-ipv4-subnet "172.16.0.0/12"))
         (subnet2 (ipaddress:make-ipv4-subnet "172.16.19.0/24"))
         (subnet3 (ipaddress:make-ipv4-subnet "172.16.18.0/23"))
         (policy :ALLOW-ALL)
         (session (neo4cl:establish-bolt-session *bolt-server*))
         (schema-version (restagraph::create-new-schema-version session)))
    ;; Install the core schema
    (restagraph::install-subschema session restagraph::*core-schema* schema-version)
    ;; Now fetch the schema
    (let ((schema (restagraph::fetch-current-schema session)))
      ;; Install the default set of resources
      (restagraph::install-default-resources session)
      ;; Confirm the fixtures aren't already present
      (fiveam:is (null (restagraph::get-resources session (format nil "/Organisations/~A" org))))
      ;; Add the fixtures
      (restagraph::log-message :debug ";TEST Creating the fixtures.")
      (restagraph::store-resource session schema "Organisations" `(("uid" . ,org)) *admin-user*)
      ;; Add a top-level subnet; this should return NIL.
      (restagraph::log-message :debug ";TEST Add a top-level subnet.")
      (fiveam:is (restagraph::insert-subnet session org "" subnet1 schema policy))
      ;; Confirm the subnet is there
      (restagraph::log-message :debug ";TEST Confirm the top-level subnet is present.")
      (fiveam:is (equal (list (restagraph::make-subnet-uid subnet1))
                        (mapcar #'restagraph::make-subnet-uid
                                (restagraph::find-subnet session org "" subnet1))))
      ;; Add a second subnet
      (restagraph::log-message :debug ";TEST Add a second-level subnet.")
      (fiveam:is (restagraph::insert-subnet session org "" subnet2 schema policy))
      ;; Confirm that's also there
      (restagraph::log-message :debug ";TEST Confirm the second-level subnet is present.")
      (fiveam:is (equal
                   (mapcar #'restagraph::make-subnet-uid
                           (list subnet1 subnet2))
                   (mapcar #'restagraph::make-subnet-uid
                           (restagraph::find-subnet session org "" subnet2))))
      ;; Add a third subnet
      (restagraph::log-message :debug ";TEST Add a third subnet between the first two.")
      (fiveam:is (restagraph::insert-subnet session org "" subnet3 schema policy))
      ;; Confirm that's also there
      (restagraph::log-message :debug ";TEST Confirm the new second-level subnet is present.")
      (fiveam:is (restagraph::find-subnet session org "" subnet3))
      ;; Confirm it's correctly moved the second subnet
      (restagraph::log-message :debug ";TEST Confirm the original second-level subnet is now third.")
      (fiveam:is (equal
                   (mapcar #'restagraph::make-subnet-uid
                           (list subnet1 subnet3 subnet2))
                   (mapcar #'restagraph::make-subnet-uid
                           (restagraph::find-subnet session org "" subnet2))))
      ;; Remove the top-level subnet
      (restagraph::log-message :debug ";TEST Delete the top-level subnet.")
      (fiveam:is (null (restagraph::delete-subnet session org "" subnet1 schema)))
      ;; Confirm the top-level subnet is gone
      (fiveam:is (null (restagraph::find-subnet session org "" subnet1)))
      ;; Remove the second subnet
      (restagraph::log-message :debug ";TEST Delete the now third-level subnet.")
      (fiveam:is (null (restagraph::delete-subnet session org "" subnet2 schema)))
      ;; Remove the fixtures
      (restagraph::log-message :debug ";TEST Deleting the fixtures.")
      (restagraph::delete-resource-by-path session (format nil "/Organisations/~A" org) schema :recursive t))
    ;; Clean up the mess
    (restagraph::delete-schema-version session schema-version)
    (neo4cl:disconnect session)))

(fiveam:test
  ipam-ipv6-subnets-3-levels-no-vrf
  "Create/read/delete tests on nested IPv6 subnets directly under an organisation."
  (let* ((org "testco")
         (subnet1 (ipaddress:make-ipv6-subnet "2001:db8::/32"))
         (subnet2 (ipaddress:make-ipv6-subnet "2001:db8:dead:beef::/64"))
         (subnet3 (ipaddress:make-ipv6-subnet "2001:db8:dead::/48"))
         (policy :ALLOW-ALL)
         (session (neo4cl:establish-bolt-session *bolt-server*))
         (schema-version (restagraph::create-new-schema-version session)))
    ;; Install the core schema
    (restagraph::install-subschema session restagraph::*core-schema* schema-version)
    ;; Now fetch the schema
    (let ((schema (restagraph::fetch-current-schema session)))
      ;; Install the default set of resources
      (restagraph::install-default-resources session)
      ;; Confirm the fixtures aren't already present
      (fiveam:is (null (restagraph::get-resources session (format nil "/Organisations/~A" org))))
      ;; Add the fixtures
      (restagraph::log-message :debug ";TEST Creating the fixtures.")
      (restagraph::store-resource session schema "Organisations" `(("uid" . ,org)) *admin-user*)
      ;; Add a top-level subnet; this should return NIL.
      (restagraph::log-message :debug ";TEST Add a top-level subnet.")
      (fiveam:is (restagraph::insert-subnet session org "" subnet1 schema policy))
      ;; Confirm the subnet is there
      (restagraph::log-message :debug ";TEST Confirm the top-level subnet is present.")
      (fiveam:is (equal (list (restagraph::make-subnet-uid subnet1))
                        (mapcar #'restagraph::make-subnet-uid
                                (restagraph::find-subnet session org "" subnet1))))
      ;; Add a second subnet
      (restagraph::log-message :debug ";TEST Add a second-level subnet.")
      (fiveam:is (restagraph::insert-subnet session org "" subnet2 schema policy))
      ;; Confirm that's also there
      (restagraph::log-message :debug ";TEST Confirm the second-level subnet is present.")
      (fiveam:is (equal
                   (mapcar #'restagraph::make-subnet-uid
                           (list subnet1 subnet2))
                   (mapcar #'restagraph::make-subnet-uid
                           (restagraph::find-subnet session org "" subnet2))))
      ;; Add a third subnet
      (restagraph::log-message :debug ";TEST Add a third subnet between the first two.")
      (fiveam:is (restagraph::insert-subnet session org "" subnet3 schema policy))
      ;; Confirm that's also there
      (restagraph::log-message :debug ";TEST Confirm the new second-level subnet is present.")
      (fiveam:is (restagraph::find-subnet session org "" subnet3))
      ;; Confirm it's correctly moved the second subnet
      (restagraph::log-message :debug ";TEST Confirm the original second-level subnet is now third.")
      (fiveam:is (equal
                   (mapcar #'restagraph::make-subnet-uid
                           (list subnet1 subnet3 subnet2))
                   (mapcar #'restagraph::make-subnet-uid
                           (restagraph::find-subnet session org "" subnet2))))
      ;; Remove the top-level subnet
      (restagraph::log-message :debug ";TEST Delete the top-level subnet.")
      (fiveam:is (null (restagraph::delete-subnet session org "" subnet1 schema)))
      ;; Confirm the top-level subnet is gone
      (fiveam:is (null (restagraph::find-subnet session org "" subnet1)))
      ;; Remove the second subnet
      (restagraph::log-message :debug ";TEST Delete the now third-level subnet.")
      (fiveam:is (null (restagraph::delete-subnet session org "" subnet2 schema)))
      ;; Remove the fixtures
      (restagraph::log-message :debug ";TEST Deleting the fixtures.")
      (restagraph::delete-resource-by-path session (format nil "/Organisations/~A" org) schema :recursive t))
    ;; Clean up the mess
    (restagraph::delete-schema-version session schema-version)
    (neo4cl:disconnect session)))

(fiveam:test
  ipv4address-basic
  "Basic create/find/delete operations on an IPv4 address"
  (let* ((org "example")
         (address (ipaddress:make-ipv4-address "172.17.2.3"))
         (vrf "green")
         (subnet (ipaddress:make-ipv4-subnet "172.17.2.0/24"))
         (policy :ALLOW-ALL)
         (session (neo4cl:establish-bolt-session *bolt-server*))
         (schema-version (restagraph::create-new-schema-version session)))
    ;; Install the core schema
    (restagraph::install-subschema session restagraph::*core-schema* schema-version)
    ;; Now fetch the schema
    (let ((schema (restagraph::fetch-current-schema session)))
      ;; Install the default set of resources
      (restagraph::install-default-resources session)
      ;; Ensure we're clear to start
      (fiveam:is (null (restagraph::get-resources session (format nil "/Organisations/~A" org))))
      ;; Create fixtures
      (restagraph::log-message :debug "Creating fixtures with one VRF")
      (restagraph::store-resource session schema "Organisations" `(("uid" . ,org)) *admin-user*)
      (restagraph::store-dependent-resource
        session
        schema
        (format nil "/Organisations/~A/VRF_GROUPS/VrfGroups" org)
        `(("uid" . ,vrf))
        *admin-user*)
      (restagraph::insert-subnet session org vrf subnet schema policy)
      ;; Tests
      (restagraph::log-message :debug ";TEST Address is absent")
      (fiveam:is (null (restagraph::find-ipaddress session address org vrf)))
      (restagraph::log-message :debug ";TEST Insert address")
      (fiveam:is (restagraph::insert-ipaddress session schema address org vrf policy))
      (fiveam:is (equal (ipaddress:as-string address)
                        (car (last (restagraph::find-ipaddress session address org vrf)))))
      (restagraph::log-message :debug ";TEST Delete address")
      (fiveam:is (null (restagraph::delete-ipaddress session schema address org vrf)))
      (fiveam:is (null (restagraph::find-ipaddress session address org vrf)))
      ;; Remove fixtures
      (restagraph::delete-resource-by-path session
                                           (format nil "/Organisations/~A" org)
                                           schema
                                           :recursive t)
      ;; Ensure the fixtures are gone
      (fiveam:is (null (restagraph::get-resources session (format nil "/Organisations/~A" org)))))
    ;; Clean up the mess
    (restagraph::delete-schema-version session schema-version)
    (neo4cl:disconnect session)))

(fiveam:test
  ipv4-subnets-and-addresses-basic
  "Basic tests for adding and removing subnets with addresses"
  (let* ((org "sample")
         (subnet1 (ipaddress:make-ipv4-subnet "192.168.0.0/16"))
         (subnet2 (ipaddress:make-ipv4-subnet "192.168.32.0/23"))
         (address (ipaddress:make-ipv4-address "192.168.32.3"))
         (policy :ALLOW-ALL)
         (session (neo4cl:establish-bolt-session *bolt-server*))
         (schema-version (restagraph::create-new-schema-version session)))
    ;; Install the core schema
    (restagraph::log-message :info ";TEST Set up the schema")
    (restagraph::install-subschema session restagraph::*core-schema* schema-version)
    ;; Now fetch the schema
    (let ((schema (restagraph::fetch-current-schema session)))
      ;; Install the default set of resources
    (restagraph::log-message :info ";TEST Install default resources")
      (restagraph::install-default-resources session)
      (restagraph::log-message :info ";TEST Creating the fixtures.")
      ;; Confirm the fixtures aren't already present
      (fiveam:is (null (restagraph::get-resources session (format nil "/Organisations/~A" org))))
      ;; Add the fixtures
      (restagraph::store-resource session schema "Organisations" `(("uid" . ,org)) *admin-user*)
      (restagraph::insert-subnet session org "" subnet1 schema policy)
      ;; Add the IP address
      (restagraph::log-message :info ";TEST Add the IP address")
      (fiveam:is (restagraph::insert-ipaddress session schema address org "" policy))
      ;; Confirm the address is there
      (restagraph::log-message :info ";TEST Confirm the address is present.")
      (fiveam:is (restagraph::find-ipaddress session address org ""))
      (fiveam:is (equal (ipaddress:as-string address)
                        (car (last (restagraph::find-ipaddress session address org "")))))
      ;; Add another subnet
      (restagraph::log-message :info ";TEST Add a second-level subnet.")
      (fiveam:is (restagraph::insert-subnet session org "" subnet2 schema policy))
      ;; Confirm that's also there
      (restagraph::log-message :info ";TEST Confirm the second-level subnet is present.")
      (fiveam:is (equal (mapcar #'ipaddress:as-cidr (list subnet1 subnet2))
                        (mapcar #'ipaddress:as-cidr (restagraph::find-subnet session org "" subnet2))))
      ;; Confirm the address has the correct new path
      (restagraph::log-message :info ";TEST Confirm the address has been correctly moved.")
      (let ((newpath (restagraph::find-ipaddress session address org "")))
        (fiveam:is (equal (mapcar #'ipaddress:as-cidr (list subnet1 subnet2))
                          (mapcar #'ipaddress:as-cidr (butlast newpath))))
        (fiveam:is (equal (ipaddress:as-string address)
                          (car (last newpath)))))
      ;; Remove the second subnet
      (restagraph::log-message :info ";TEST Delete the second-level subnet.")
      (fiveam:is (null (restagraph::delete-subnet session org "" subnet2 schema)))
      ;; Confirm the address has moved back again
      (restagraph::log-message :info ";TEST Confirm the address is back under the top-level subnet.")
      (fiveam:is (restagraph::find-ipaddress session address org ""))
      (let ((newpath (restagraph::find-ipaddress session address org "")))
        (fiveam:is (equal (list (ipaddress:as-cidr subnet1))
                          (mapcar #'ipaddress:as-cidr (butlast newpath))))
        (fiveam:is (equal (ipaddress:as-string address)
                          (car (last newpath)))))
      ;; Remove the fixtures
      (restagraph::log-message :info ";TEST Deleting the fixtures.")
      (restagraph::delete-resource-by-path session
                                           (format nil "/Organisations/~A" org)
                                           schema
                                           :recursive t))
    ;; Clean up the mess
    (restagraph::delete-schema-version session schema-version)
    (neo4cl:disconnect session)))

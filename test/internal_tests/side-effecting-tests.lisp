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
    ;; Confirm that there are two people (RgAdmin should be the other)
    ;; (Test get-resources with mod/3 == 1)
    (fiveam:is (equal 2 (length (restagraph::get-resources *server* (format nil "/~A" restype)))))
    ;; While we're here, confirm that we get an error for this.
    ;; (Test get-resources with mod/3 == 0)
    (fiveam:signals restagraph::client-error
                    (restagraph::get-resources *server* (format nil "/~A/~A/BLAH" restype uid)))
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

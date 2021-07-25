;   Copyright 2020-2021 James Fleming <james@electronic-quill.net>
;
;   Licensed under the GNU General Public License
;   - for details, see LICENSE.txt in the top-level directory


(in-package #:restagraph)
(declaim (optimize (compilation-speed 0)
                   (speed 2)
                   (safety 3)
                   (debug 3)))

(defun subnet-dispatcher-v1 ()
  "Hunchentoot dispatch function for the IPAM-specific REST API, version 1. Subnet subset."
  (handler-case
    (cond
      ;; Access policies: dispatch "deny" as early as possible
      ((and (equal :GET (tbnl:request-method*))
            (equal :DENY (get-policy (access-policy *restagraph-acceptor*))))
       (forbidden))
      ((and (equal :POST (tbnl:request-method*))
            (equal :DENY (post-policy (access-policy *restagraph-acceptor*))))
       (forbidden))
      ((and (equal :DELETE (tbnl:request-method*))
            (equal :DENY (delete-policy (access-policy *restagraph-acceptor*))))
       (forbidden))
      ;;
      ;; Create a subnet
      ((and (equal (tbnl:request-method*) :POST)
            (tbnl:post-parameter "subnet")
            (tbnl:post-parameter "org"))
       (log-message :debug
                    (format nil "Dispatching POST request for URI ~A"
                            (tbnl:request-uri*)))
       (cond
         ;; Sanity-check: does the organisation exist?
         ((not (get-resources
                 (datastore *restagraph-acceptor*)
                 (concatenate 'string "/Organisations/" (tbnl:post-parameter "org"))))
          (return-client-error
            (format nil "Organisation '~A' does not exist" (tbnl:post-parameter "org"))))
         ;; It's passed all the sanity checks so far; insert it
         (t
           (let ((result
                   (insert-subnet (datastore *restagraph-acceptor*)
                                  (tbnl:post-parameter "org")
                                  (or (tbnl:post-parameter "vrf") "")
                                  (if (ipaddress:ipv4-subnet-p (tbnl:post-parameter "subnet"))
                                    (ipaddress:make-ipv4-subnet (tbnl:post-parameter "subnet"))
                                    (ipaddress:make-ipv6-subnet (tbnl:post-parameter "subnet")))
                                  (schema *restagraph-acceptor*))))
             ;; Return it to the client for confirmation
             (log-message
               :debug
               (format nil
                       (if result
                         "Stored subnet ~A. Now retrieving it for positive confirmation."
                         "Subnet ~A was already present. Retrieving it for positive confirmation.")
                       (tbnl:post-parameter "subnet")))
             (setf (tbnl:content-type*) "application/json")
             (setf (tbnl:return-code*) (if result
                                         tbnl:+http-created+
                                         tbnl:+http-ok+))
             (format-subnet-path
               (tbnl:post-parameter "org")
               (tbnl:post-parameter "vrf")
               (find-subnet (datastore *restagraph-acceptor*)
                            (tbnl:post-parameter "org")
                            (or (tbnl:post-parameter "vrf") "")
                            (if (ipaddress:ipv4-subnet-p (tbnl:post-parameter "subnet"))
                              (ipaddress:make-ipv4-subnet (tbnl:post-parameter "subnet"))
                              (ipaddress:make-ipv6-subnet (tbnl:post-parameter "subnet")))))))))
      ;;
      ;; Search for a subnet
      ((and (equal (tbnl:request-method*) :GET)
            (tbnl:get-parameter "subnet")
            (tbnl:get-parameter "org"))
       (log-message :debug (format nil "Dispatching GET request for URI ~A"
                                   (tbnl:request-uri*)))
       ;; Go look for it
       (handler-case
         (let ((result (find-subnet (datastore *restagraph-acceptor*)
                                    (tbnl:get-parameter "org")
                                    (or (tbnl:get-parameter "vrf") "")
                                    (if (ipaddress:ipv4-subnet-p (tbnl:get-parameter "subnet"))
                                      (ipaddress:make-ipv4-subnet (tbnl:get-parameter "subnet"))
                                      (ipaddress:make-ipv6-subnet (tbnl:get-parameter "subnet"))))))
           ;; Did we find one?
           (if (or (null result)
                   (equal result ""))
             ;; Not found
             (progn
               (setf (tbnl:content-type*) "text/plain")
               (setf (tbnl:return-code*) tbnl:+http-not-found+)
               "No such subnet")
             ;; Found it!
             (progn
               (setf (tbnl:content-type*) "application/json")
               (setf (tbnl:return-code*) tbnl:+http-ok+)
               (let ((output (format-subnet-path
                               (tbnl:get-parameter "org")
                               (tbnl:get-parameter "vrf")
                               result)))
                 (log-message :debug (format nil "Retrieved subnet path ~A" output))
                 ;; Actually return it to the appserver
                 output))))
         ;; Attempted violation of db integrity
         (integrity-error (e) (return-integrity-error (message e)))
         ;; Generic client errors
         (client-error (e) (return-client-error (message e)))))
      ;;
      ;; Delete a subnet
      ((and (equal (tbnl:request-method*) :DELETE)
            (tbnl:post-parameter "subnet")
            (tbnl:post-parameter "org"))
       (log-message :debug (format nil "Dispatching DELETE request for URI ~A"
                                   (tbnl:request-uri*)))
       (delete-subnet (datastore *restagraph-acceptor*)
                      (tbnl:post-parameter "org")
                      (or (tbnl:post-parameter "vrf") "")
                      (if (ipaddress:ipv4-subnet-p (tbnl:post-parameter "subnet"))
                        (ipaddress:make-ipv4-subnet (tbnl:post-parameter "subnet"))
                        (ipaddress:make-ipv6-subnet (tbnl:post-parameter "subnet")))
                      (schema *restagraph-acceptor*))
       (setf (tbnl:content-type*) "text/plain")
       (setf (tbnl:return-code*) tbnl:+http-no-content+)
       "")
      ;; Methods we don't support.
      ;; Take the whitelist approach
      ((not (member (tbnl:request-method*) '(:POST :GET :DELETE)))
       (method-not-allowed))
      ;;
      ;; Handle all other cases
      (t
        (return-client-error "This wasn't a valid request")))
    ;; Handle general errors
    ;;
    ;; Generic client errors
    (client-error (e) (return-client-error (message e)))
    (neo4cl:client-error (e) (return-client-error (neo4cl:message e)))
    ;; Transient error
    (neo4cl:transient-error (e) (return-transient-error e))
    ;; Database error
    (neo4cl:database-error (e) (return-database-error e))))


(defun address-dispatcher-v1 ()
  "Hunchentoot dispatch function for the IPAM-specific REST API, version 1. Address subset."
  (handler-case
    (cond
      ;; Access policies: dispatch "deny" as early as possible
      ((and (equal :GET (tbnl:request-method*))
            (equal :DENY (get-policy (access-policy *restagraph-acceptor*))))
       (forbidden))
      ((and (equal :POST (tbnl:request-method*))
            (equal :DENY (post-policy (access-policy *restagraph-acceptor*))))
       (forbidden))
      ((and (equal :DELETE (tbnl:request-method*))
            (equal :DENY (delete-policy (access-policy *restagraph-acceptor*))))
       (forbidden))
      ;;
      ;; Create an address
      ((and (equal (tbnl:request-method*) :POST)
            (tbnl:post-parameter "address")
            (tbnl:post-parameter "org"))
       (log-message :debug (format nil "Dispatching POST request for URI ~A" (tbnl:request-uri*)))
       (insert-ipaddress (datastore *restagraph-acceptor*)
                         (schema *restagraph-acceptor*)
                         (make-instance (if (ipaddress:ipv4-address-p (tbnl:post-parameter "address"))
                                          'ipaddress:ipv4-address
                                          'ipaddress:ipv6-address)
                                        :address (tbnl:post-parameter "address"))
                         (tbnl:post-parameter "org")
                         (or (tbnl:post-parameter "vrf") ""))
       ;; Return it to the client for confirmation
       (log-message :debug
                    (format nil "Stored address ~A. Now retrieving it for positive confirmation."
                            (tbnl:post-parameter "address")))
       (setf (tbnl:content-type*) "application/json")
       (setf (tbnl:return-code*) tbnl:+http-created+)
       (format-address-path (tbnl:post-parameter "org")
                            (or (tbnl:post-parameter "vrf") "")
                            (find-ipaddress (datastore *restagraph-acceptor*)
                                            (make-instance (if (ipaddress:ipv4-address-p (tbnl:post-parameter "address"))
                                                             'ipaddress:ipv4-address
                                                             'ipaddress:ipv6-address)
                                                           :address (tbnl:post-parameter "address"))
                                            (tbnl:post-parameter "org")
                                            (or (tbnl:post-parameter "vrf") ""))))
      ;;
      ;; Search for an address
      ((and (equal (tbnl:request-method*) :GET)
            (tbnl:get-parameter "address")
            (tbnl:get-parameter "org"))
       (log-message :debug (format nil "Dispatching GET request for URI ~A" (tbnl:request-uri*)))
       ;; Go look for it
       (handler-case
         (let ((result (find-ipaddress
                         (datastore *restagraph-acceptor*)
                         (make-instance (if (ipaddress:ipv4-address-p (tbnl:get-parameter "address"))
                                          'ipaddress:ipv4-address
                                          'ipaddress:ipv6-address)
                                        :address (tbnl:get-parameter "address"))
                         (tbnl:get-parameter "org")
                         (or (tbnl:get-parameter "vrf") ""))))
           ;; Did we find one?
           (if (or (null result)
                   (equal result ""))
             ;; Not found
             (progn
               (setf (tbnl:content-type*) "text/plain")
               (setf (tbnl:return-code*) tbnl:+http-not-found+)
               "No such address")
             ;; Found it!
             (progn
               (setf (tbnl:content-type*) "application/json")
               (setf (tbnl:return-code*) tbnl:+http-ok+)
               (format-address-path (tbnl:get-parameter "org")
                                    (or (tbnl:get-parameter "vrf") "")
                                    result))))
         ;; Attempted violation of db integrity
         (integrity-error (e) (return-integrity-error (message e)))
         ;; Generic client errors
         (client-error (e) (return-client-error (message e)))))
      ;;
      ;; Delete an address
      ((and (equal (tbnl:request-method*) :DELETE)
            (tbnl:post-parameter "address")
            (tbnl:post-parameter "org"))
       (log-message :debug (format nil "Dispatching DELETE request for URI ~A" (tbnl:request-uri*)))
       (delete-ipaddress (datastore *restagraph-acceptor*)
                         (schema *restagraph-acceptor*)
                         (make-instance (if (ipaddress:ipv4-address-p (tbnl:post-parameter "address"))
                                          'ipaddress:ipv4-address
                                          'ipaddress:ipv6-address)
                                        :address (tbnl:post-parameter "address"))
                         (tbnl:post-parameter "org")
                         (or (tbnl:post-parameter "vrf") ""))
       (setf (tbnl:content-type*) "text/plain")
       (setf (tbnl:return-code*) tbnl:+http-no-content+)
       "")
      ;; Reject any methods we don't support.
      ;; Take the whitelist approach
      ((not (member (tbnl:request-method*) '(:POST :GET :DELETE)))
       (method-not-allowed))
      ;;
      ;; Handle all other cases
      (t
        (return-client-error "This wasn't a valid request")))
    ;; Handle general errors
    ;;
    ;; Generic client errors
    (neo4cl:client-error (e) (return-client-error (neo4cl:message e)))
    ;; Transient error
    (neo4cl:transient-error (e) (return-transient-error e))
    ;; Database error
    (neo4cl:database-error (e) (return-database-error e))
    ;; Service errors, e.g. connection refused
    (neo4cl:service-error (e) (return-service-error (neo4cl:message e)))))

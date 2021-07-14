;   Copyright 2021 James Fleming <james@electronic-quill.net>
;
;   Licensed under the GNU General Public License
;   - for details, see LICENSE.txt in the top-level directory


;;;; IPAM functionality

(in-package #:restagraph)

(declaim (optimize (compilation-speed 0)
                   (speed 2)
                   (safety 3)
                   (debug 3)))


(defun format-subnet-path (org vrf subnet-list)
  "Generate a URI for the path to a subnet."
  (format nil
          (if (= 6 (ipaddress:ip-version (car subnet-list)))
              "/Organisations/~A~A~{/SUBNETS/Ipv6Subnets/~A~}"
              "/Organisations/~A~A~{/SUBNETS/Ipv4Subnets/~A~}")
          org
          (if (and vrf (not (equal vrf "")))
              (format nil "/VRF_GROUPS/VrfGroups/~A" vrf)
              "")
          (mapcar #'make-subnet-uid subnet-list)))


(defun format-address-path (org vrf address-path)
  "Generate a URI for the path to an address."
  (format nil
          (if (= 6 (ipaddress:ip-version (car address-path)))
              "/Organisations/~A~A~{/SUBNETS/Ipv6Subnets/~A~}/ADDRESSES/Ipv6Addresses/~A"
              "/Organisations/~A~A~{/SUBNETS/Ipv4Subnets/~A~}/ADDRESSES/Ipv4Addresses/~A")
          org
          (if (and vrf (not (equal vrf "")))
              (format nil "/VRF_GROUPS/VrfGroups/~A" vrf)
              "")
          (mapcar #'make-subnet-uid (butlast address-path))
          (car (last address-path))))


;;;; Methods

(defgeneric make-subnet-uid (subnet)
  (:documentation "Take an ipaddress:ip-subnet object, and return a URL-safe UID"))

(defmethod make-subnet-uid ((subnet ipaddress:ip-address))
  (with-output-to-string (str)
    (write (ipaddress:as-string subnet) :stream str :escape nil)
    (write "_" :stream str :escape nil)
    (write (ipaddress:prefix-length subnet) :stream str)))


(defgeneric find-subnet (db org vrf subnet &optional path)
  (:documentation "Try to locate a subnet, given the organisation and VRF to check under.
                   If VRF is NIL, assume the subnet is in the default VRF and thus directly attached to the organisation."))

(defmethod find-subnet ((db neo4cl:neo4j-rest-server)
                        (org string)
                        (vrf string)
                        (subnet ipaddress:ip-subnet)
                        &optional path)
  (log-message :debug "find-subnet invoked")
  (log-message
    :debug
    (format nil "find-subnet: searching for subnet ~A" (ipaddress:as-cidr subnet)))
  (let ((match
          (get-resources
            db
            (format nil
                    (if (= 6 (ipaddress:ip-version subnet))
                        "/Organisations/~A~A~{/SUBNETS/Ipv6Subnets/~A~}/SUBNETS/Ipv6Subnets/~A"
                        "/Organisations/~A~A~{/SUBNETS/Ipv4Subnets/~A~}/SUBNETS/Ipv4Subnets/~A")
                    org
                    (if (equal vrf "")
                        ""
                        (format nil "/VRF_GROUPS/VrfGroups/~A" vrf))
                    (mapcar #'make-subnet-uid path)
                    (make-subnet-uid subnet)))))
    ;; If we have a match, return the path to it
    (if (and
          match
          (equal (ipaddress:prefix-length subnet)
                 (cdr (assoc :prefixlength match))))
        (progn
          (log-message :debug (format nil "Found match: ~A" match))
          (let ((newpath (append
                           path
                           (list (make-instance (if (= 6 (ipaddress:ip-version subnet))
                                                    'ipaddress:ipv6-subnet
                                                    'ipaddress:ipv4-subnet)
                                                :address (cdr (assoc :netaddress match))
                                                :prefix-length (cdr (assoc :prefixlength match)))))))
            (log-message
              :debug
              (format nil "Found exact match for '~A'. Returning subnet path '~{/~A~}'."
                      (ipaddress:as-cidr subnet) (mapcar #'ipaddress:as-cidr newpath)))
            newpath))
        ;; If not, check whether we have a supernet of the subnet we're looking for
        (let* ((candidates
                 (mapcar #'(lambda (s)
                             (make-instance (if (= 6 (ipaddress:ip-version subnet))
                                                'ipaddress:ipv6-subnet
                                                'ipaddress:ipv4-subnet)
                                            :address (cdr (assoc :netaddress s))
                                            :prefix-length (cdr (assoc :prefixlength s))))
                         (get-resources
                           db
                           (format nil
                                   (if (= 6 (ipaddress:ip-version subnet))
                                       "/Organisations/~A~A~{/SUBNETS/Ipv6Subnets/~A~}/SUBNETS/Ipv6Subnets"
                                       "/Organisations/~A~A~{/SUBNETS/Ipv4Subnets/~A~}/SUBNETS/Ipv4Subnets")
                                   org
                                   (if (equal vrf "") "" (format nil "/VRF_GROUPS/VrfGroups/~A" vrf))
                                   (mapcar #'make-subnet-uid path)))))
               (supernets
                 (remove-if-not
                   #'(lambda (candidate)
                       (log-message :debug (format nil "Testing ~A" (ipaddress:as-cidr candidate)))
                       (when (ipaddress:subnetp subnet candidate)
                         candidate))
                   candidates)))
          (progn
            (log-message
              :debug
              (format nil "Exact match not found. Looking for a supernet in ~A"
                      (mapcar #'make-subnet-uid candidates)))
            ;; Do we have a supernet (and only one) to check under?
            (if (equal (length supernets) 1)
                (progn
                  (log-message
                    :debug
                    (format nil "Supernet found: ~A" (ipaddress:as-cidr (car supernets))))
                  (let ((newpath (append path (list (car supernets)))))
                    (log-message
                      :debug
                      (format nil "Supernet identified. Finding the location of ~A under /~A~A~{/~A~}"
                              (ipaddress:as-cidr subnet)
                              org
                              (if (equal vrf "")
                                  ""
                                  (format nil "/~A" vrf))
                              (mapcar #'make-subnet-uid newpath)))
                    (find-subnet db org vrf subnet newpath)))
                ;; No candidate supernet found
                (log-message :debug "No exact match or supernet found on this path.")))))))


(defgeneric find-parent-subnet (db subnet org vrfgroup path)
  (:documentation "Find the subnet with the longest prefix-length that could plausibly be a parent subnet to the one supplied. Return a URI representing the path.
                   Expects a canonicalised (full-length) subnet descriptor in CIDR format."))

(defmethod find-parent-subnet ((db neo4cl:neo4j-rest-server)
                               (subnet ipaddress:ip-address)
                               (org string)
                               (vrfgroup string)
                               (path list))
  (log-message
    :debug
    (format nil "Searching for parent subnet for ~A under organisation '~A' and VRF '~A'."
            (ipaddress:as-cidr subnet) org vrfgroup))
  ;; Get a list of candidate supernets
  (let* ((candidates
           ;; Remove nulls from the list returned by the enclosed query
           (remove-if #'null
                      (mapcar
                        #'(lambda
                            (c)
                            (log-message
                              :debug (format nil "Testing candidate parent subnet ~A" c))
                            ;; Create an ip-subnet object from the candidate
                            ;; - make the comparison code simpler by a call to ipaddress:subnetp
                            ;; - we now already have a subnet object to return
                            (let ((supernet (make-instance (if (= 6 (ipaddress:ip-version subnet))
                                                               'ipaddress:ipv6-subnet
                                                               'ipaddress:ipv4-subnet)
                                                           :address (cdr (assoc :netaddress c))
                                                           :prefix-length (cdr (assoc :prefixlength c)))))
                              ;; Test whether it's plausibly a supernet of this subnet
                              (when (ipaddress:subnetp subnet supernet)
                                ;; If it is, return the supernet. Otherwise, default to NIL.
                                supernet)))
                        ;; Extract the list of candidate supernets
                        (get-resources
                          db
                          (format nil (if (= 6 (ipaddress:ip-version subnet))
                                          "/Organisations/~A~A~{/SUBNETS/Ipv6Subnets/~A~}/SUBNETS/Ipv6Subnets"
                                          "/Organisations/~A~A~{/SUBNETS/Ipv4Subnets/~A~}/SUBNETS/Ipv4Subnets")
                                  org
                                  (if (equal vrfgroup "")
                                      ""
                                      (format nil "/VRF_GROUPS/VrfGroups/~A" vrfgroup))
                                  (mapcar 'make-subnet-uid path)))))))
    (log-message
      :debug
      (format nil "Candidate supernets for ~A under ~{/~A~}: ~{~A~^, ~}"
      (make-subnet-uid subnet)
      (mapcar #'make-subnet-uid path)
      (mapcar #'make-subnet-uid candidates)))
    (cond
      ;; If there's no candidate, this is where the search stops. Return the path to this point.
      ((null candidates)
       (log-message :debug (format nil "End of the line. Using the parent path ~{/~A~}"
                                   (mapcar #'make-subnet-uid path)))
       path)
      ;; If there's more than one candidate, return an error because this is illegal.
      ;; FIXME: report the path so the user can fix the problem.
      ((> (length candidates) 1)
       (error 'integrity-error
              :message (format nil "More than one candidate supernet; this is not a valid situation.")))
      ;; Remaining option: there's one candidate.
      ;; Test it by recursing through this function.
      (t
       (let ((newpath (append path (list (car candidates)))))
         (log-message
           :debug
           (format nil "Found a candidate supernet for ~A: recursing into ~{/~A~}."
                   (ipaddress:as-cidr subnet) (mapcar #'make-subnet-uid newpath)))
         (find-parent-subnet db subnet org vrfgroup newpath))))))


(defgeneric insert-subnet (db org vrf subnet schema)
  (:documentation "Insert a subnet under the specified organisation and VRF.
                   If the vrf argument is the empty string, create it directly, implying the default VRF.
                   Return t on success, and NIL if the subnet already exists."))

(defmethod insert-subnet ((db neo4cl:neo4j-rest-server)
                          (org string)
                          (vrf string)
                          (subnet ipaddress:ip-subnet)
                          (schema hash-table))
  (log-message
    :debug
    (format nil "Attempting to insert subnet '~A' under organisation '~A' and VRF-group '~A'."
            (ipaddress:as-cidr subnet) org vrf))
  ;; Sanity check: is it already there?
  (log-message
    :debug
    (format nil "Checking whether ~A is already present" (ipaddress:as-cidr subnet)))
  (cond
    ;; Organisation doesn't exist
    ((or (equal org "")
         (not (get-resources
                db
                (concatenate 'string "/Organisations/" org))))
     (return-client-error
       (format nil "Organisation '~A' does not exist" org)))
    ;; Already present: log the fact, and return now.
    ((find-subnet db org vrf subnet)
     (progn
       (log-message
         :warn
         (format nil "Attempting to insert subnet ~A, which is already present"
                 (ipaddress:as-cidr subnet)))
       nil))
    ;; Doesn't already exist; carry on
    (t
      (let* ((subnet-uid (make-subnet-uid subnet))
             (parent-path (find-parent-subnet db subnet org vrf ()))
             ;; Pre-extract the list of candidate subnets for relocation.
             ;; If we do this now, we don't accidentally include the one we just inserted.
             (subnets
               (progn
                 (log-message
                   :debug
                   (format nil "Checking for subnets to relocate under ~A after creating it"
                           (ipaddress:as-cidr subnet)))
                 (remove-if
                   #'null
                   (mapcar #'(lambda (s)
                               ;; Create an ip-subnet object from the candidate
                               ;; - make the comparison code simpler by a call to ipaddress:subnetp
                               ;; - we now already have a subnet object to return
                               (let ((c_subnet (make-instance
                                                 (if (= 6 (ipaddress:ip-version subnet))
                                                   'ipaddress:ipv6-subnet
                                                   'ipaddress:ipv4-subnet)
                                                 :address (cdr (assoc :netaddress s))
                                                 :prefix-length (cdr (assoc :prefixlength s)))))
                                 ;; If it's a subnet of the new one, return the object
                                 (log-message
                                   :debug
                                   (format nil "Checking whether candidate subnet ~A is a subnet of ~A"
                                           (ipaddress:as-cidr c_subnet)
                                           (ipaddress:as-cidr subnet)))
                                 (when (ipaddress:subnetp c_subnet subnet) c_subnet)))
                           (get-resources
                             db
                             (format nil (if (= 6 (ipaddress:ip-version subnet))
                                           "/Organisations/~A~A~{/SUBNETS/Ipv6Subnets/~A~}/SUBNETS/Ipv6Subnets"
                                           "/Organisations/~A~A~{/SUBNETS/Ipv4Subnets/~A~}/SUBNETS/Ipv4Subnets")
                                     org
                                     (if (equal vrf "")
                                       ""
                                       (format nil "/VRF_GROUPS/VrfGroups/~A" vrf))
                                     (mapcar #'make-subnet-uid parent-path)))))))
             ;; Pre-extract the list of candidate addresses for relocation.
             (addresses
               (progn
                 (log-message
                   :debug
                   (format nil "Checking for addresses to relocate under ~A after creating it"
                           (ipaddress:as-cidr subnet)))
                 (remove-if
                   #'null
                   (mapcar #'(lambda (a)
                               (let ((c_addr (make-instance
                                               (if (= 6 (ipaddress:ip-version subnet))
                                                 'ipaddress:ipv6-address
                                                 'ipaddress:ipv4-address)
                                               :address (cdr (assoc :uid a)))))
                                 (log-message
                                   :debug
                                   (format nil "Checking whether address ~A belongs in new subnet ~A"
                                           (ipaddress:as-string c_addr)
                                           (ipaddress:as-cidr subnet)))
                                 (when (ipaddress:subnetp c_addr subnet) c_addr)))
                           (get-resources
                             db
                             (format nil (if (= 6 (ipaddress:ip-version subnet))
                                           "/Organisations/~A~A~{/SUBNETS/Ipv6Subnets/~A~}/ADDRESSES/Ipv6Addresses"
                                           "/Organisations/~A~A~{/SUBNETS/Ipv4Subnets/~A~}/ADDRESSES/Ipv4Addresses")
                                     org
                                     (if (equal vrf "")
                                       ""
                                       (format nil "/VRF_GROUPS/VrfGroups/~A" vrf))
                                     (mapcar #'make-subnet-uid parent-path)))))))
             ;; Build the path to insert the new subnet
             (insert-path
               (format nil (if (= 6 (ipaddress:ip-version subnet))
                             "/Organisations/~A~A~{/SUBNETS/Ipv6Subnets/~A~}/SUBNETS/Ipv6Subnets"
                             "/Organisations/~A~A~{/SUBNETS/Ipv4Subnets/~A~}/SUBNETS/Ipv4Subnets")
                       org
                       (if (equal vrf "")
                         ""
                         (format nil "/VRF_GROUPS/VrfGroups/~A" vrf))
                       (mapcar #'make-subnet-uid parent-path))))
        ;; Finished with the let-section
        ;;
        ;; Now actually insert the subnet
        (log-message
          :debug
          (format nil "Inserting the subnet ~A using the path ~A."
                  (ipaddress:as-cidr subnet) insert-path))
        (store-dependent-resource db
                                             schema
                                             insert-path
                                             `(("uid" . ,subnet-uid)
                                               ("netaddress" . ,(ipaddress:as-string subnet))
                                               ("prefixlength" . ,(ipaddress:prefix-length subnet))))
        ;; Using the list of candidates we retrieved earlier,
        ;; identify subnets of the one we just inserted, and move them under it.
        (if subnets
          (mapcar #'(lambda (s)
                      (log-message
                        :debug
                        (format nil "Relocating subnet ~A under its new parent." (ipaddress:as-cidr s)))
                      ;; Now actually move it
                      (move-dependent-resource
                        db
                        schema
                        ;; Existing path of subnet
                        (format nil (if (= 6 (ipaddress:ip-version subnet))
                                      "/Organisations/~A~A~{/SUBNETS/Ipv6Subnets/~A~}"
                                      "/Organisations/~A~A~{/SUBNETS/Ipv4Subnets/~A~}")
                                org
                                (if (equal vrf "")
                                  ""
                                  (format nil "/VRF_GROUPS/VrfGroups/~A" vrf))
                                (append
                                  (mapcar #'make-subnet-uid parent-path)
                                  (list (make-subnet-uid s))))
                        ;; New parent for subnet
                        (format nil (if (= 6 (ipaddress:ip-version subnet))
                                      "/Organisations/~A~A~{/SUBNETS/Ipv6Subnets/~A~}/SUBNETS"
                                      "/Organisations/~A~A~{/SUBNETS/Ipv4Subnets/~A~}/SUBNETS")
                                org
                                (if (equal vrf "")
                                  ""
                                  (format nil "/VRF_GROUPS/VrfGroups/~A" vrf))
                                (append (mapcar #'make-subnet-uid parent-path)
                                        (list subnet-uid)))))
                  subnets)
          (log-message :debug "No subnets to relocate"))
        ;; Find all IP addresses directly attached to the supernet which fit in this subnet;
        ;; move them under this one.
        (if addresses
          (mapcar #'(lambda (a)
                      (log-message
                        :debug
                        (format nil "Relocating address ~A under its new parent." (ipaddress:as-string a)))
                      ;; Now actually move it
                      (move-dependent-resource
                        db
                        schema
                        ;; Existing path of address
                        (format nil
                                (if (= 6 (ipaddress:ip-version subnet))
                                  "/Organisations/~A~A~{/SUBNETS/Ipv6Subnets/~A~}/ADDRESSES/Ipv6Addresses/~A"
                                  "/Organisations/~A~A~{/SUBNETS/Ipv4Subnets/~A~}/ADDRESSES/Ipv4Addresses/~A")
                                org
                                (if (equal vrf "")
                                  ""
                                  (format nil "/VRF_GROUPS/VrfGroups/~A" vrf))
                                (mapcar #'make-subnet-uid parent-path)
                                (ipaddress:as-string a))
                        ;; New parent for address
                        (format nil
                                (if (= 6 (ipaddress:ip-version subnet))
                                  "/Organisations/~A~A~{/SUBNETS/Ipv6Subnets/~A~}/ADDRESSES"
                                  "/Organisations/~A~A~{/SUBNETS/Ipv4Subnets/~A~}/ADDRESSES")
                                org
                                (if (equal vrf "")
                                  ""
                                  (format nil "/VRF_GROUPS/VrfGroups/~A" vrf))
                                (append
                                  (mapcar #'make-subnet-uid parent-path)
                                  (list subnet-uid)))))
                  addresses)
          (log-message :debug "No addresses to relocate"))
        ;; Return t, to indicate success.
        t))))


(defgeneric delete-subnet (db org vrf subnet schema)
  (:documentation "Remove a subnet from the IPAM section.
                   Merge its subnets and addresses into its subnet.
                   Note: it assumes you got the path right, most likely via find-subnet.
                   Note: it silently deletes and other kinds of relationship between the target subnet and other resources - if you want those reassigned, you need to do it yourself first.
                   If you want to recursively delete a subnet and everything under it, use delete-resource in recursive mode."))

(defmethod delete-subnet ((db neo4cl:neo4j-rest-server)
                          (org string)
                          (vrf string)
                          (subnet ipaddress:ip-subnet)
                          schema)
  (let* ((subnet-path (find-subnet db org vrf subnet))
         (parent-path (butlast subnet-path)))
    ;; Sanity-check
    (unless subnet-path
      (error 'client-error :message "No such subnet."))
    ;; Move subnets to the parent
    (log-message
      :debug
      (format nil "Relocating subnets of ~A before deleting it" (ipaddress:as-cidr subnet)))
    (mapcar #'(lambda (s)
                (log-message :debug (format nil "Relocating subnet ~A"
                                                       (cdr (assoc :uid s))))
                (move-dependent-resource
                  db
                  schema
                  ;; Existing path
                  (format nil "/Organisations/~A~A~{/SUBNETS/Ipv4Subnets/~A~}"
                          org
                          (if (equal vrf "")
                              ""
                              (format nil "/VRF_GROUPS/VrfGroups/~A" vrf))
                          (append (mapcar #'make-subnet-uid subnet-path)
                                  (list (cdr (assoc :uid s)))))
                  ;; New parent path
                  (format nil "/Organisations/~A~A~{/SUBNETS/Ipv4Subnets/~A~}/SUBNETS"
                          org
                          (if (equal vrf "")
                              ""
                              (format nil "/VRF_GROUPS/VrfGroups/~A" vrf))
                          (mapcar #'make-subnet-uid parent-path))))
            ;; Get a list of subnets
            (get-resources
              db
              (format nil
                      (if (= 6 (ipaddress:ip-version subnet))
                          "/Organisations/~A~A~{/SUBNETS/Ipv6Subnets/~A~}/SUBNETS/Ipv6Subnets"
                          "/Organisations/~A~A~{/SUBNETS/Ipv4Subnets/~A~}/SUBNETS/Ipv4Subnets")
                      org
                      (if (equal vrf "")
                          ""
                          (format nil "/VRF_GROUPS/VrfGroups/~A" vrf))
                      (mapcar #'make-subnet-uid subnet-path))))
    ;; Addresses: move them to the parent
    (log-message
      :debug
      (format nil "Relocating addresses under ~A before deleting it" (ipaddress:as-cidr subnet)))
    (mapcar #'(lambda (a)
                (log-message :debug (format nil "Relocating address ~A" a))
                (move-dependent-resource
                  db
                  schema
                  ;; Existing path
                  (format nil
                          (if (= 6 (ipaddress:ip-version subnet))
                              "/Organisations/~A~A~{/SUBNETS/Ipv6Subnets/~A~}/ADDRESSES/Ipv6Addresses/~A"
                              "/Organisations/~A~A~{/SUBNETS/Ipv4Subnets/~A~}/ADDRESSES/Ipv4Addresses/~A")
                          org
                          (if (equal vrf "")
                              ""
                              (format nil "/VRF_GROUPS/VrfGroups/~A" vrf))
                          (mapcar #'make-subnet-uid subnet-path)
                          (cdr (assoc :uid a)))
                  ;; New parent path
                  (format nil
                          (if (= 6 (ipaddress:ip-version subnet))
                              "/Organisations/~A~A~{/SUBNETS/Ipv6Subnets/~A~}/ADDRESSES"
                              "/Organisations/~A~A~{/SUBNETS/Ipv4Subnets/~A~}/ADDRESSES")
                          org
                          (if (equal vrf "")
                              ""
                              (format nil "/VRF_GROUPS/VrfGroups/~A" vrf))
                          (mapcar #'make-subnet-uid parent-path))))
            ;; Get a list of addresses
            (get-resources
              db
              (format nil
                      (if (= 6 (ipaddress:ip-version subnet))
                          "/Organisations/~A~A~{/SUBNETS/Ipv6Subnets/~A~}/ADDRESSES/Ipv6Addresses"
                          "/Organisations/~A~A~{/SUBNETS/Ipv4Subnets/~A~}/ADDRESSES/Ipv4Addresses")
                      org
                      (if (equal vrf "")
                          ""
                          (format nil "/VRF_GROUPS/VrfGroups/~A" vrf))
                      (mapcar #'make-subnet-uid subnet-path))))
    ;; Delete the subnet itself
    (delete-resource-by-path
      db
      (format nil
              (if (= 6 (ipaddress:ip-version subnet))
                  "/Organisations/~A~A~{/SUBNETS/Ipv6Subnets/~A~}"
                  "/Organisations/~A~A~{/SUBNETS/Ipv4Subnets/~A~}")
              org
              (if (equal vrf "")
                  ""
                  (format nil "/VRF_GROUPS/VrfGroups/~A" vrf))
              (mapcar #'make-subnet-uid subnet-path))
      schema)))


(defgeneric find-ipaddress (db address org vrf)
            (:documentation "Find the path to an IPv4 address in the IPAM section"))

(defmethod find-ipaddress ((db neo4cl:neo4j-rest-server)
                           (address ipaddress:ip-address)
                           (org string)
                           (vrf string))
  (let ((parent-path (find-parent-subnet db address org vrf ())))
    (when parent-path
      (let ((result (get-resources
                      db
                      (format nil
                              (if (= 6 (ipaddress:ip-version address))
                                "/Organisations/~A~A~{/SUBNETS/Ipv6Subnets/~A~}/ADDRESSES/Ipv6Addresses/~A"
                                "/Organisations/~A~A~{/SUBNETS/Ipv4Subnets/~A~}/ADDRESSES/Ipv4Addresses/~A")
                              org
                              (if (equal vrf "")
                                ""
                                (format nil "/VRF_GROUPS/VrfGroups/~A" vrf))
                              (mapcar #'make-subnet-uid parent-path)
                              (ipaddress:as-string address)))))
        (when result
          (append parent-path
                  (list
                    (ipaddress:as-string
                      (make-instance (if (= 6 (ipaddress:ip-version address))
                                       'ipaddress:ipv6-address
                                       'ipaddress:ipv4-address)
                                     :address (cdr (assoc :uid result)))))))))))


(defgeneric insert-ipaddress (db schema address org vrf)
  (:documentation "Add an IP address to the IPAM section"))

(defmethod insert-ipaddress ((db neo4cl:neo4j-rest-server)
                             (schema hash-table)
                             (address ipaddress:ip-address)
                             (org string)
                             (vrf string))
  (let ((parent-subnet-path (find-parent-subnet db address org vrf ())))
    (log-message
      :debug
      (format nil "Found parent subnet for ~A: ~{/~A~}"
              (ipaddress:as-string address)
              (mapcar #'make-subnet-uid parent-subnet-path)))
    ;; Sanity-check: is it already there?
    (log-message
      :debug
      (format nil "Checking whether ~A is already present" (ipaddress:as-string address)))
    (if (get-resources
          db
          (format nil
                  (if (= 6 (ipaddress:ip-version address))
                    "/Organisations/~A~A~{/SUBNETS/Ipv6Subnets/~A~}/ADDRESSES/Ipv6Addresses/~A"
                    "/Organisations/~A~A~{/SUBNETS/Ipv4Subnets/~A~}/ADDRESSES/Ipv4Addresses/~A")
                  org
                  (if (equal vrf "")
                    ""
                    (format nil "/VRF_GROUPS/VrfGroups/~A" vrf))
                  (mapcar #'make-subnet-uid parent-subnet-path)
                  (ipaddress:as-string address)))
      ;; It's a duplicate; return nil because there's nothing to do.
      nil
      ;; All is well; carry on
      (progn
        (log-message
          :debug
          (format nil "No duplicate address found; proceeding to store address ~A"
                  (ipaddress:as-string address)))
        (store-dependent-resource
          db
          schema
          (format nil
                  (if (= 6 (ipaddress:ip-version address))
                    "/Organisations/~A~A~{/SUBNETS/Ipv6Subnets/~A~}/ADDRESSES/Ipv6Addresses"
                    "/Organisations/~A~A~{/SUBNETS/Ipv4Subnets/~A~}/ADDRESSES/Ipv4Addresses")
                  org
                  (if (equal vrf "")
                    ""
                    (format nil "/VRF_GROUPS/VrfGroups/~A" vrf))
                  (mapcar #'make-subnet-uid parent-subnet-path))
          `(("uid" . ,(ipaddress:as-string address))))
        ;; Return t to indicate success.
        t))))


(defgeneric delete-ipaddress (db schema address org vrf)
            (:documentation "Remove an IP address from the IPAM section"))

(defmethod delete-ipaddress ((db neo4cl:neo4j-rest-server)
                             (schema hash-table)
                             (address ipaddress:ip-address)
                             (org string)
                             (vrf string))
  (delete-resource-by-path
    db
    (format nil
            (if (= 6 (ipaddress:ip-version address))
              "/Organisations/~A~A~{/SUBNETS/Ipv6Subnets/~A~}/ADDRESSES/Ipv6Addresses/~A"
              "/Organisations/~A~A~{/SUBNETS/Ipv4Subnets/~A~}/ADDRESSES/Ipv4Addresses/~A")
            org
            (if (equal vrf "")
              ""
              (format nil "/VRF_GROUPS/VrfGroups/~A" vrf))
            (mapcar #'make-subnet-uid (find-parent-subnet db address org vrf ()))
            (ipaddress:as-string address))
    schema)
  ;; Return NIL, because there's no earthly reason to return anything else.
  nil)

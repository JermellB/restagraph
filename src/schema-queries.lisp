;   Copyright 2017-2021 James Fleming <james@electronic-quill.net>
;
;   Licensed under the GNU General Public License
;   - for details, see LICENSE.txt in the top-level directory


;;;; Functions and methods for querying the schema

(in-package #:restagraph)

(declaim (optimize (compilation-speed 0)
                   (speed 2)
                   (safety 3)
                   (debug 3)))


;; Helper function
(defun format-post-params-as-properties (params)
  "Take an alist, as returned by (tbnl:post-parameters*), and transform it into the kind of map
  that Neo4J expects in the :PROPERTIES section of a query."
  (declare (type (or null cons) params))
  (log-message :debug "Formatting a set of POST parameters for use as Neo4j properties.")
  (mapcar #'(lambda (param)
              (cons (intern (escape-neo4j (string-downcase (car param))) :keyword)
                    (if (stringp (cdr param))
                      (escape-neo4j (cdr param))
                      (cdr param))))
          params))

(defun validate-attributes (requested defined &key (invalid '()) (badvalue '()))
  "Recursive helper function to validate the requested attributes against those defined for the resourcetype.
  Return a list of three lists:
  - invalid attributes (attributes whose name is not defined for this resourcetype)
  - attributes for which an invalid value was provided"
  (declare (type (or cons null) requested)  ; alist, where the car is the name and the cdr is the value
           (type (or cons null) defined)    ; Should have the outer layer of conses stripped
           (type (or cons null) invalid)
           (type (or cons null) badvalue))
  (log-message :debug (format nil "validate-attributes requested attrs: ~A" requested))
  (log-message :debug (format nil "validate-attributes defined attrs: ~{~A~^, ~}"
                              (mapcar #'a-listify defined)))
  ;; Are we at the end of the list of requested attributes?
  (if (null requested)
    ;; If we are, return what's been accumulated
    (list invalid badvalue)
    ;; If not, check the attribute at the head of the list, then call this function on the rest.
    (validate-attributes
      ;; Rest of the list.
      (cdr requested)
      ;; Definitions, unchanged.
      defined
      ;; If this attribute is invalid, add it to that accumulator.
      ;; Pull the list of attribute-names from the 'defined parameter, and test whether it's a member.
      :invalid (if (not (member (caar requested)
                                (mapcar #'(lambda (attr) (name attr))
                                        defined)
                                :test #'equal))
                 ;; Invalid attribute. Log it and add it to the `invalid` accumulator.
                 (progn
                   (log-message :debug (format nil "Detected invalid attribute name '~A'" (caar requested)))
                   (append invalid (list (car requested))))
                 ;; It's valid; leave the `invalid` list as-is
                 invalid)
      :badvalue (if
                  ;; Is it a valid attribute? (yes, we have to test this again)
                  (if (member (caar requested)
                              ;; Remember this was parsed from JSON
                              (mapcar #'(lambda (attr) (name attr))
                                      defined)
                              :test #'equal)
                    ;; Is this an enum attribute?
                    (let ((enums
                            (attr-values
                              (car (remove-if-not #'(lambda (attr)
                                                      (equal (caar requested)
                                                             (name attr)))
                                                  defined)))))
                      (if (and enums
                               (not (null enums)))
                        ;; If so, is it a valid value?
                        (when
                          (member (cdar requested)
                                  enums
                                  :test #'equal)
                          ;; If it's a valid value for this enum, return True
                          t)
                        ;; If it's not an enum, then we do no other checking.
                        t))
                    ;; If it's not a valid attribute, this isn't relevant.
                    t)
                  ;; If all those tests passed, pass on the value of `badvalue` we received
                  badvalue
                  ;; If any of those failed, add this to `badvalue`
                  (append badvalue (list (car requested)))))))


(defgeneric validate-resource-before-creating (schema resourcetype params)
  (:documentation "Confirm whether the provided data is valid, before attempting to use it to create a resource.
                   If the data is valid, return a list of attributes suitable for feeding to Neo4J. If not, raise a suitable error"))

(defmethod validate-resource-before-creating ((schema hash-table)
                                              (resourcetype string)
                                              ;; params is what Hunchentoot received via POST
                                              (params list))
  (log-message
    :debug
    (format nil "validate-resource-before-creating resourcetype ~A with params ~{~A~^, ~}"
            resourcetype params))
  ;; Does this resource-type exist?
  (if (gethash resourcetype schema)
      ;; Were attributes specified and, if so, are they all valid for this resource-type?
      (let
        ;; Exempt "uid" from validation
        ((requested-attributes
           (remove-if #'(lambda (param) (equal (car param) "uid"))
                      params))
         ;; Get the attributes defined for this resource-type
         ;; Extract the original UID here, to reduce mess later
         (original-uid (or (cdr (assoc "uid" params :test #'string=)) "")))
        ;; Put this log message here to get it inside the let statement,
        ;; and thus avoid a progn.
        (log-message :debug (format nil "Confirmed: resourcetype '~A' exists" resourcetype))
        ;; Now validate the attributes and return the results.
        (log-message :debug "Checking the supplied attributes.")
        (let ((results (validate-attributes requested-attributes (attributes (gethash resourcetype schema)))))
          ;; Were the requested attributes all valid?
          (if (and (null (first results))
                   (null (second results)))
              ;; If so, return the supplied attributes to the caller, properly formatted for Neo4j.
              (let ((formatted-params
                      (format-post-params-as-properties
                        ;requested-attributes
                        (acons "uid" (sanitise-uid original-uid)
                               (acons "original_uid" original-uid
                                      (remove-if #'(lambda (param) (equal (car param) "uid"))
                                                 params))))))
                (log-message :debug (format nil "Returning formatted parameters ~A" formatted-params))
                formatted-params)
              ;; If not, report the problem.
              (progn
                (when (first results)
                  (log-message :debug (format nil "Identified invalid attribute-names: ~{~A~^, ~}"
                                              (first results))))
                (when (second results)
                  (log-message :debug (format nil "Identified invalid values ~{~A~^, ~}"
                                              (second results))))
                (error 'restagraph:client-error :message
                       (format nil "Invalid attributes for ~A resources: ~{~A~^, ~}. Invalid values: ~{~A~^, ~}."
                               resourcetype (first results) (second results)))))))
      ;; No such resourcetype
      (signal 'client-error :message "No such resourcetype")))

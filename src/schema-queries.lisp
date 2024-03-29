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

(defun validate-attributes (requested defined &key (valid '()) (invalid '()) (badvalue '()))
  "Recursive helper function to validate the requested attributes against those defined for the resourcetype.
  Return a list of three lists:
  - valid attributes, converted to the correct types for encoding via Bolt.
  - invalid attributes (attributes whose name is not defined for this resourcetype)
  - attributes for which an invalid value was provided"
  (declare (type (or cons null) requested)  ; alist, where the car is the name and the cdr is the value
           (type (or sequence null) defined)
           (type (or cons null) invalid)
           (type (or cons null) badvalue))
  (log-message :debug (format nil "validate-attributes requested attrs: ~A" requested))
  (log-message :debug (format nil "validate-attributes defined attrs: ~{~A~^, ~}"
                              (map 'list #'a-listify defined)))
  (log-message :debug (format nil "validate-attributes valid attrs: ~A" valid))
  ;; Are we at the end of the list of requested attributes?
  (if (null requested)
    ;; If we are, return what's been accumulated
    (list valid invalid badvalue)
    ;; If not, check the attribute at the head of the list, then call this function on the rest.
    (let ((attrdef (find-if #'(lambda (attr)
                                (equal (caar requested) (name attr)))
                            defined)))
      ;; Is it even a valid attribute, i.e. is there a definition for it?
      (if attrdef
        ;; There is? Excellent. Let's carry on and validate it.
        (let ((validated-value
                ;; We need to return two separate bits of information here:
                ;; - is it valid?
                ;; - if it's valid, what's the validated value?
                ;; Since only atomic values are permitted, `nil` can stand for "failed".
                (cond
                  ;; Boolean
                  ((equal 'schema-rtype-attr-boolean (type-of attrdef))
                   (log-message :debug (format nil "Validating requested value ~A for boolean value ~A"
                                               (cdar requested) (name attrdef)))
                   (cond
                     ((member (cdar requested) (list t "True") :test #'equal) :true)
                     ((member (cdar requested) (list nil "False") :test #'equal) :false)
                     (t nil)))
                  ;; Integer
                  ((equal 'schema-rtype-attr-integer (type-of attrdef))
                   (log-message :debug (format nil "Validating requested value ~A for integer value ~A"
                                               (cdar requested) (name attrdef)))
                   (log-message :debug (format nil "Detected type of value ~A is ~A"
                                               (cdar requested) (type-of (cdar requested))))
                   (handler-case
                     (let ((candidate
                             (if (stringp (cdar requested))
                               (parse-integer (cdar requested))
                               (cdar requested))))
                       (when (and
                               (integerp candidate)
                               ;; If a minimum was set, is it below that?
                               (if (minimum attrdef)
                                 (>= candidate (minimum attrdef))
                                 t)
                               ;; If a maximum was set, is it below that?
                               (if (maximum attrdef)
                                 (<= candidate (maximum attrdef))
                                 t))
                         ;; If all those tests passed, return the integer-ised version
                         candidate))
                     ;; Catch the case where the arg wasn't an integer, and return nil for false.
                     (sb-int:simple-parse-error (e) (declare (ignore e)) nil)))
                  ;; Text
                  ((equal 'schema-rtype-attr-text (type-of attrdef))
                   (when (stringp (cdar requested))
                     (cdar requested)))
                  ;; Default = varchar
                  (t
                    ;; Is this an enum attribute?
                    (let ((enums (when (equal 'schema-rtype-attr-varchar
                                              (type-of attrdef))
                                   (attrvalues attrdef))))
                      (and
                        (stringp (cdar requested))
                        ;; FIXME: this looks like overkill.
                        ;; Add tests to confirm, then hopefully simplify this section.
                        (if (and enums (not (null enums)))
                          ;; If so, is it a valid value?
                          (when (member (cdar requested)
                                        enums
                                        :test #'equal)
                            (cdar requested))
                          ;; If it's not an enum, then we do no other checking.
                          (cdar requested))))))))
          ;; Report on what we found
          (if validated-value
          (log-message :debug (format nil "Valid value for ~A: ~A" (caar requested) validated-value))
          (log-message :debug (format nil "Invalid value for ~A: ~A" (caar requested) (cdar requested))))
          ;; Recurse, using the result of validation
          (validate-attributes
            ;; Rest of the list.
            (cdr requested)
            ;; Definitions, unchanged.
            defined
            ;; If it passed validation, append it to the `valid` list.
            :valid (if validated-value
                     (append valid `((,(caar requested) . ,validated-value)))
                     valid)
            ;; If we got here, it's not invalid.
            ;; Pass on the `invalid` list as we received it.
            :invalid invalid
            ;; If it failed validation, add it to the `badvalue` list.
            :badvalue (if validated-value
                        badvalue
                        (append badvalue (list (car requested))))))
        ;; Requested attribute was invalid, i.e. not defined
        (progn
          (log-message :debug (format nil "Detected invalid attribute name '~A'" (caar requested)))
          (validate-attributes (cdr requested)
                               defined
                               :valid valid
                               :invalid (append invalid (list (car requested)))
                               :badvalue badvalue))))))


(defgeneric validate-resource-before-creating (schema resourcetype params)
            (:documentation "Confirm whether the resourcetype exists and the requested attributes are valid,
                            before attempting to use it to create a resource.
                            If the data is valid, return a list of attributes suitable for feeding to Neo4J.
                            If not, raise a suitable error"))

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
        (let ((results (validate-attributes requested-attributes
                                            (attributes (gethash resourcetype schema)))))
          ;; Were the requested attributes all valid?
          (if (and (null (second results))
                   (null (third results)))
              ;; OLD: If so, return the supplied attributes to the caller, alist-formatted for Neo4cl.
              ;; NEW: If so, return the validated attributes to the caller, alist-formatted for Neo4cl.
              (let ((formatted-params
                      (acons "uid" (sanitise-uid original-uid)
                               (acons "original_uid" original-uid
                                      (remove-if #'(lambda (param) (equal (car param) "uid"))
                                                 (first results))))))
                (log-message :debug (format nil "Returning formatted parameters ~A" formatted-params))
                formatted-params)
              ;; If not, report the problem.
              (progn
                (when (second results)
                  (log-message :debug (format nil "Identified invalid attribute-names: ~{~A~^, ~}"
                                              (first results))))
                (when (third results)
                  (log-message :debug (format nil "Identified invalid values ~{~A~^, ~}"
                                              (second results))))
                (error 'client-error :message
                       (format nil "Invalid attributes for ~A resources: ~{~A~^, ~}. Invalid values: ~{~A~^, ~}."
                               resourcetype (second results) (third results)))))))
      ;; No such resourcetype
      (signal 'client-error :message "No such resourcetype")))

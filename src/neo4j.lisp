;;;; Methods and functions specifically relating to Neo4J

(in-package #:restagraph)


;;;; Schema methods and functions

(defmethod get-resource-names-from-db ((db neo4cl:neo4j-rest-server))
  (mapcar #'car
          (neo4cl::extract-rows-from-get-request
            (neo4cl:neo4j-transaction
              db
              `((:STATEMENTS
                  ((:STATEMENT . "MATCH (c:rgResource) RETURN c.name"))))))))

(defmethod get-resource-attributes-from-db ((db neo4cl:neo4j-rest-server) (resourcetype string))
  (neo4cl::extract-rows-from-get-request 
    (neo4cl:neo4j-transaction
      db
      `((:STATEMENTS
          ((:STATEMENT .
            ,(format nil "MATCH (c:rgResource { name: '~A' })-[:rgHasAttribute]-(a:rgAttribute) RETURN a"
                     resourcetype))))))))

;;; This currently has to be done one query at a time.
;;; This approach will not scale well.
(defmethod create-db-schema ((db neo4cl:neo4j-rest-server))
  (mapcar #'(lambda (resource)
              (let ((statement (format nil "CREATE CONSTRAINT ON (r:~A) ASSERT r.uid IS UNIQUE"
                                       resource)))
                (log-message :debug "Requesting db constraint as follows: '~A'"
                             statement)
                (neo4cl:neo4j-transaction
                  db
                  `((:STATEMENTS ((:STATEMENT . ,statement)))))))
          (get-resource-names-from-db db)))

(defun format-post-params-as-properties (params)
  "Take an alist, as returned by (tbnl:post-parameters*), and transform it into the kind of map that Neo4J expects in the :PROPERTIES section of a query."
  (mapcar #'(lambda (param)
              (cons (intern (car param) :keyword)
                    (cdr param)))
          params))

(defmethod validate-resource-before-creating ((db neo4cl:neo4j-rest-server)
                                              (resourcetype string)
                                              (params list))
  ;; Does this resource-type exist?
  (let ((resource-exists-p
          (neo4cl:extract-data-from-get-request
            (neo4cl:neo4j-transaction
              db
              `((:STATEMENTS
                  ((:STATEMENT .
                    ,(format nil
                             "MATCH (n:rgResource { name: '~A' }) RETURN n"
                             resourcetype)))))))))
    (if resource-exists-p
      ;; Were attributes specified and, if so, are they all valid for this resource-type?
      (let
        ((requested-attributes
           (remove-if #'(lambda (param) (equal (car param) "uid")) params)))
        ;; Note that the resource was found to be valid
        (log-message :debug (format nil "Resource type ~A is valid." resourcetype))
        ;; If any attributes were requested, check each one for relevance to this
        ;; resource-type.
        ;; Add invalid ones to the invalid-attributes list
        (if requested-attributes
          (progn
            (log-message :debug "Checking the supplied attributes.")
            (let* ((valid-attributes
                     (mapcar #'(lambda (row)
                                 (cdr (assoc :name (car row))))
                             (get-resource-attributes-from-db db resourcetype)))
                   (invalid-attributes
                     (remove-if #'null
                                (mapcar #'(lambda (par)
                                            (unless (member (car par) valid-attributes :test 'equal)
                                              (car par)))
                                        requested-attributes))))
              ;; Record the valid ones, if we're debugging
              (if valid-attributes
                (log-message :debug (format nil "Valid attributes for resource-type ~A: ~{~A~^, ~}."
                                            resourcetype valid-attributes))
                (log-message :debug "Resource-type ~A has no valid attributes to set."
                             resourcetype))
              ;; Record the state of the invalid ones
              (if invalid-attributes
                (log-message :debug (format nil "Identified invalid attributes: ~{~A~^, ~}"
                                            invalid-attributes))
                (log-message :debug "No invalid attributes identified."))
              ;; If the invalid-attributes list is non-empty,
              ;; inform the client of the error.
              (if
                invalid-attributes
                (error 'restagraph:client-error :message
                       (format nil "Invalid attributes for ~A resources: ~{~A~^, ~}"
                               resourcetype invalid-attributes))
                ;; Return the valid attributes to the caller
                (format-post-params-as-properties params))))
          ;; If we got this far, it's valid.
          ;; Return positive confirmation to the caller.
          (progn
            (log-message :debug "No attributes supplied. Looks OK.")
            (format-post-params-as-properties params))))
      ;; The first test failed; inform the user
      (progn
        (log-message :debug
                     (format nil "Requested resource-type ~A is not valid."
                             resourcetype))
        (error 'restagraph:integrity-error :message "No such resource type")))))


;;;; Resources

(defmethod store-resource ((db neo4cl:neo4j-rest-server) (resourcetype string) (post-params list))
  (let ((attributes (validate-resource-before-creating db resourcetype post-params)))
    (if attributes
      (progn
        ;; If we got this far, we have a valid resource type and valid attribute names.
        ;; Make it happen
        (log-message :debug (format nil "Creating a ~A resource with attributes ~A"
                                    resourcetype attributes))
        (handler-case
          (neo4cl:neo4j-transaction
            db
            `((:STATEMENTS
                ((:STATEMENT . ,(format nil "CREATE (:~A { properties })" resourcetype))
                 (:PARAMETERS . ((:PROPERTIES . ,attributes)))))))
          ;; Catch selected errors as they come up
          (neo4cl::client-error
            (e)
            (if (and
                  ;; If it's specifically an integrity error, call this out
                  (equal (neo4cl:category e) "Schema")
                  (equal (neo4cl:title e) "ConstraintValidationFailed"))
              (progn
                (log-message :error (format nil "~A.~A: ~A"
                                            (neo4cl:category e)
                                            (neo4cl:title e)
                                            (neo4cl:message e)))
                (error 'restagraph:integrity-error :message (neo4cl:message e)))
              ;; Otherwise, just resignal it
              (let ((text (format nil "Database error ~A.~A: ~A"
                                  (neo4cl:category e)
                                  (neo4cl:title e)
                                  (neo4cl:message e))))
                (log-message :error text)
                (error 'restagraph:client-error :message text))))))
      (error 'restagraph:integrity-error :message "Requested resource type does not exist"))))

(defmethod get-resource-by-uid ((db neo4cl:neo4j-rest-server) (resourcetype string) (uid string))
  (log-message :debug (format nil "Retrieving ~A resource with UID ~A." resourcetype uid))
  (cl-json:encode-json-alist-to-string
    (neo4cl:extract-data-from-get-request
      (neo4cl:neo4j-transaction
        db
        `((:STATEMENTS
            ((:STATEMENT . ,(format nil "MATCH (n:~A { uid: '~A' }) RETURN n" resourcetype uid)))))))))

(defmethod delete-resource-by-uid ((db neo4cl:neo4j-rest-server) (resourcetype string) (uid string))
  (neo4cl:neo4j-transaction
    db
    `((:STATEMENTS
        ((:STATEMENT . ,(format nil "MATCH (n:~A { uid: '~A' }) DETACH DELETE n" resourcetype uid)))))))


;;;; Relationships

(defmethod relationship-valid-p ((db neo4cl:neo4j-rest-server)
                                 (source-type string)
                                 (reltype string)
                                 (dest-type string))
  (neo4cl:extract-rows-from-get-request
    (neo4cl:neo4j-transaction
      db
      `((:STATEMENTS
          ((:STATEMENT .
            ,(format nil "MATCH (a:rgResource {name: '~A'})-[r:~A]->(b:rgResource {name: '~A'}) RETURN a, type(r), b"
                     source-type reltype dest-type))))))))

(defmethod create-relationship ((db neo4cl:neo4j-rest-server)
                                (source-type string)
                                (source-uid string)
                                (reltype string)
                                (dest-type string)
                                (dest-uid string))
  (if (relationship-valid-p db source-type reltype dest-type)
    (neo4cl:neo4j-transaction
      db
      `((:STATEMENTS
          ((:STATEMENT .
            ,(format nil "MATCH (a:~A { uid: '~A' }), (b:~A { uid: '~A' }) MERGE (a)-[:~A]->(b)"
                     source-type source-uid dest-type dest-uid reltype))))))
    (error 'integrity-error
           :message (format nil "Relationship ~A is not permitted from ~A to ~A"
                            reltype source-type dest-type))))

(defmethod get-resources-with-relationship ((db neo4cl:neo4j-rest-server)
                                            (resourcetype string)
                                            (uid string)
                                            (relationship string))
  (mapcar #'(lambda (row)
              `(("resource-type" . ,(caar row)) ("uid" . ,(second row))))
          (neo4cl:extract-rows-from-get-request
            (neo4cl:neo4j-transaction
              db
              `((:STATEMENTS
                  ((:STATEMENT .
                    ,(format nil "MATCH (a:~A {uid: '~A' })-[:~A]->(b) RETURN labels(b), b.uid"
                             resourcetype uid relationship)))))))))

(defmethod delete-relationship ((db neo4cl:neo4j-rest-server)
                                (source-type string)
                                (source-uid string)
                                (reltype string)
                                (dest-type string)
                                (dest-uid string))
  (neo4cl:neo4j-transaction
    db
    `((:STATEMENTS
        ((:STATEMENT .
          ,(format nil "MATCH (a:~A { uid: '~A' })-[r:~A]->(b:~A { uid: '~A' }) DELETE r"
                   source-type source-uid reltype dest-type dest-uid)))))))

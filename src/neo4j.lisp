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

(defmethod get-resource-attributes-from-db ((db neo4cl:neo4j-rest-server))
  (neo4cl::extract-rows-from-get-request 
    (neo4cl:neo4j-transaction
      db
      `((:STATEMENTS
          ((:STATEMENT . "MATCH (c:rgResource)-[:rgHasAttribute]-(a:rgAttribute) RETURN c.name, a")))))))

(defmethod get-resources-from-db ((db neo4cl:neo4j-rest-server))
  (let ((resources (make-hash-table :test 'equal)))
    ;; Initialise the entries in the hash tables,
    ;; in case one or more resource has no attributes.
    (mapcar #'(lambda (name)
                (setf (gethash name resources) (make-hash-table :test 'equal)))
            (get-resource-names-from-db db))
    ;; Now add the attributes
    (mapcar #'(lambda (attribute)
                (let ((resourcetype (first attribute))
                      (attributename (cdr (assoc :name (second attribute))))
                      (attr-attrs (remove-if #'(lambda (pair) (equal (car pair) :name))
                                             (second attribute))))
                  (setf (gethash attributename (gethash resourcetype resources))
                        attr-attrs)))
            (get-resource-attributes-from-db db))
    ;; Now return the thing we built
    resources))

(defmethod get-resource-relationships-from-db ((db neo4cl:neo4j-rest-server))
          (neo4cl::extract-rows-from-get-request
            (neo4cl:neo4j-transaction
              db
              `((:STATEMENTS
                  ((:STATEMENT . "MATCH (c:rgResource)-[r]->(t:rgResource) RETURN c.name, type(r), t.name")))))))


;;;; Resources

(defmethod store-resource ((db neo4cl:neo4j-rest-server) (resourcetype string) (post-params list))
  (let* (;; Local cache of the schema for the requested resource-type
         (typedata (get-resourcetype-from-schema-by-name (getf *config-vars* :schema) resourcetype))
         ;; Attributes that are valid for this resource type
         (valid-attributes
           (loop for key being the hash-keys in (gethash "attributes" typedata)
                 collect key))
         ;; Attributes with which to create the resource
         (attributes ())
         ;; Attributes that were specified but aren't valid for this resource-type
         (invalid-attributes ()))
    ;; Check whether the requested classname is valid
    (log-message :debug (format nil "Checking validity of resource type '~A'." resourcetype))
    (unless resourcetype
      (error (format nil "The resource type ~A is not present in the schema." resourcetype)))
    ;; Check whether a UID has been specified
    (unless (assoc "uid" post-params :test 'equal)
      (log-message :debug "No UID found in the request parameters")
      (error "UID must be supplied"))
    ;; Check for invalid attributes in the request
    (log-message :debug (format nil "Checking validity of supplied parameters ~A." post-params))
    (loop for (name . value) in post-params
          do (if (or (equal name "uid")                ; Mandatory values
                     (member name valid-attributes :test 'equal))    ; User-configured defaults
               (push (cons (intern name :keyword) value) attributes)
               (push name invalid-attributes)))
    ;; Report on the valid attributes we have
    (log-message :debug (format nil "The following attributes will be used when creating the resource: ~{~A~^, ~}." attributes))
    ;; If any requested attributes are invalid, report them as an error
    (when invalid-attributes
      (let ((message (format nil "These requested attributes are invalid for the resource-type ~A: ~{~A~^, ~}. Valid attributes are: ~{~A~^, ~}."
                             resourcetype
                             invalid-attributes
                             valid-attributes)))
        (log-message :debug message)
        (error message)))
    ;; If we got this far, we have a valid resource type and valid attribute names.
    ;; Make it happen
    (neo4cl:neo4j-transaction
      db
      `((:STATEMENTS
          ((:STATEMENT . ,(format nil "CREATE (:~A { properties })" resourcetype))
           (:PARAMETERS .
            ((:PROPERTIES . ,attributes)))))))))

(defmethod get-resource-by-uid ((db neo4cl:neo4j-rest-server) (resourcetype string) (uid string))
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

(defmethod create-relationship ((db neo4cl:neo4j-rest-server)
                                (source-type string)
                                (source-uid string)
                                (reltype string)
                                (dest-type string)
                                (dest-uid string))
  (neo4cl:neo4j-transaction
    db
    `((:STATEMENTS
        ((:STATEMENT .
          ,(format nil "MATCH (a:~A { uid: '~A' }), (b:~A { uid: '~A' }) CREATE (a)-[:~A]->(b)"
                   source-type source-uid dest-type dest-uid reltype)))))))

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

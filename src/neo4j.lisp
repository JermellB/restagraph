;;;; Methods and functions specifically relating to Neo4J

(in-package #:restagraph)


;;;; Utility functions


;; Shamelessly swiped from http://www.gigamonkeys.com/book/files-and-file-io.html
;; and adapted - thanks, Mr Seibel!
(defmethod load-cypher-file ((db neo4cl:neo4j-rest-server) filepath)
  (let ((in (open filepath :if-does-not-exist nil)))
    (when in
      (loop for line = (read-line in nil)
            while line do (when (not (cl-ppcre:all-matches "^//|^$" line))
                            (neo4cl:neo4j-transaction
                              db
                              `((:STATEMENTS ((:STATEMENT .  ,line)))))))
      (close in))))


;;;; Schema methods and functions

(defmethod get-resource-defs-from-db ((db neo4cl:neo4j-rest-server))
  (mapcar #'car
          (neo4cl::extract-rows-from-get-request
            (neo4cl:neo4j-transaction
              db
              `((:STATEMENTS
                  ((:STATEMENT . "MATCH (c:rgResource) RETURN c"))))))))

(defmethod get-resource-attributes-from-db ((db neo4cl:neo4j-rest-server)
                                            (resourcetype string))
  (neo4cl::extract-rows-from-get-request 
    (neo4cl:neo4j-transaction
      db
      `((:STATEMENTS
          ((:STATEMENT .
            ,(format nil "MATCH (c:rgResource { name: '~A' })-[:rgHasAttribute]-(a:rgAttribute) RETURN a"
                     resourcetype))))))))

(defmethod enforce-db-schema ((db neo4cl:neo4j-rest-server))
  (mapcar #'(lambda (resource)
              (let ((statement
                      (format nil "~A CONSTRAINT ON (r:~A) ASSERT r.uid IS UNIQUE"
                              ;; Create or drop the constraint,
                              ;; according to whether this resource is dependent
                              (if (and (assoc :dependent resource)
                                       (equal (cdr (assoc :dependent resource))
                                              "true"))
                                "DROP"
                                "CREATE")
                              (cdr (assoc :name resource)))))
                (log-message :debug
                             (format nil "Requesting db constraint as follows: '~A'"
                                     statement))
                (handler-case
                  (neo4cl:neo4j-transaction
                    db
                    `((:STATEMENTS ((:STATEMENT . ,statement)))))
                  (neo4cl:database-error (e)
                                         (if (equal (neo4cl:title e) "ConstraintDropFailed")
                                           nil   ; This is OK - do nothing
                                           (return-database-error
                                             (format nil "~A.~A: ~A"
                                                     (neo4cl:category e)
                                                     (neo4cl:title e)
                                                     (neo4cl:message e))))))))
          (get-resource-defs-from-db db)))

(defun format-post-params-as-properties (params)
  "Take an alist, as returned by (tbnl:post-parameters*), and transform it into the kind of map that Neo4J expects in the :PROPERTIES section of a query."
  (mapcar #'(lambda (param)
              (cons (intern (string-downcase (car param)) :keyword)
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
        (when requested-attributes
          (progn
            (log-message :debug "Checking the supplied attributes.")
            (let* ((valid-attributes
                     (mapcar #'(lambda (row)
                                 (cdr (assoc :name (car row))))
                             (get-resource-attributes-from-db db resourcetype)))
                   (invalid-attributes
                     (remove-if #'null
                                (mapcar #'(lambda (par)
                                            (unless (member
                                                      (string-downcase (car par))
                                                      valid-attributes
                                                      :test 'equal)
                                              (string-downcase (car par))))
                                        requested-attributes))))
              ;; Record the valid ones, if we're debugging
              (if valid-attributes
                (log-message :debug (format nil "Valid attributes for resource-type ~A: ~{~A~^, ~}."
                                            resourcetype valid-attributes))
                (log-message :debug (format nil "Resource-type ~A has no valid attributes to set."
                                            resourcetype)))
              ;; Record the state of the invalid ones
              (if invalid-attributes
                (progn
                  (log-message :debug (format nil "Identified invalid attributes: ~{~A~^, ~}"
                                              invalid-attributes))
                  (error 'restagraph:client-error :message
                         (format nil "Invalid attributes for ~A resources: ~{~A~^, ~}"
                                 resourcetype invalid-attributes)))
                (log-message :debug "No invalid attributes identified."))
              ;; Return the valid attributes to the caller
              (format-post-params-as-properties
                params))))
        ;; If we got this far, it's valid.
        ;; Return positive confirmation to the caller.
        (format-post-params-as-properties
          (acons "uid" (sanitise-uid (cdr (assoc "uid" params :test #'string=)))
                 (acons "original_uid" (cdr (assoc "uid" params :test #'string=))
                        (remove-if #'(lambda (param) (equal (car param) "uid"))
                                   params)))))
      ;; The first test failed; inform the user
      (progn
        (log-message :debug
                     (format nil "Requested resource-type ~A is not valid."
                             resourcetype))
        (error 'restagraph:integrity-error :message "No such resource type")))))


;;;; Resources

(defmethod dependent-resource-p ((db neo4cl:neo4j-rest-server) (resourcetype string))
  (neo4cl:extract-data-from-get-request
    (neo4cl:neo4j-transaction
      db
      `((:STATEMENTS
          ((:STATEMENT .
            ,(format nil "MATCH (c:rgResource { name: '~A' }) RETURN c.dependent"
                     resourcetype))))))))

(defmethod dependent-relationship-p ((db neo4cl:neo4j-rest-server)
                                     (source-type string)
                                     (relationship string)
                                     (dest-type string))
  (neo4cl:extract-data-from-get-request
    (neo4cl:neo4j-transaction
      db
      `((:STATEMENTS
          ((:STATEMENT .
            ,(format nil "MATCH (a:rgResource { name: '~A' })-[r:~A { dependent: 'true' } ]->(b:rgResource { name: '~A', dependent: 'true' }) WHERE r.dependent = 'true' RETURN type(r)"
                     source-type relationship dest-type))))))))

(defmethod store-resource ((db neo4cl:neo4j-rest-server) (resourcetype string) (post-params list))
  ;; If this is a dependent resource, bail out now
  (if (dependent-resource-p db resourcetype)
    (error 'integrity-error
           :message "This is a dependent resource; it must be created as a sub-resource of an existing resource.")
    ;; Not a dependent resource: carry on
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
        (error 'restagraph:integrity-error :message "Requested resource type does not exist")))))

(defmethod get-resources ((db neo4cl:neo4j-rest-server) (uri string))
  (log-message :debug (format nil "Fetching resources for URI ~A" uri))
  (let ((uri-parts (get-uri-parts uri)))
    (cond
      ;; All resources of a given type
      ((equal (mod (length uri-parts) 3) 1)
       (let ((result
               (neo4cl:extract-rows-from-get-request
                 (neo4cl:neo4j-transaction
                   db
                   `((:STATEMENTS
                       ((:STATEMENT . ,(format nil "MATCH ~A RETURN n"
                                               (uri-node-helper uri-parts))))))))))
         (when result
           (cl-json:encode-json-to-string result))))
      ;; One specific resource
      ((equal (mod (length uri-parts) 3) 2)
       (cl-json:encode-json-alist-to-string
         (neo4cl:extract-data-from-get-request
           (neo4cl:neo4j-transaction
             db
             `((:STATEMENTS
                 ((:STATEMENT . ,(format nil "MATCH ~A RETURN n"
                                         (uri-node-helper uri-parts))))))))))
      ;; All resources with a particular relationship to this one
      (t
       (let ((result (neo4cl:extract-rows-from-get-request
                       (neo4cl:neo4j-transaction
                         db
                         `((:STATEMENTS
                             ((:STATEMENT .
                               ,(format nil "MATCH ~A RETURN labels(n), n.uid"
                                        (uri-node-helper uri-parts))))))))))
         (when result
           (cl-json:encode-json-to-string
             (mapcar #'(lambda (row)
                         `(("resource-type" . ,(caar row)) ("uid" . ,(second row))))
                     result))))))))

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

(defmethod create-relationship-by-path ((db neo4cl:neo4j-rest-server)
                                        (sourcepath string)
                                        (destpath string))
  (log-message :debug (format nil "Attempting to create a relationship from ~A to ~A"
                              sourcepath destpath))
  (let* ((source-part-list (get-uri-parts sourcepath))
         (relationship (car (last source-part-list)))
         (source-parts (butlast source-part-list))
         (source-type (nth (- (length source-parts) 2) source-parts))
         (dest-parts (get-uri-parts destpath))
         (dest-type (nth (- (length dest-parts) 2) dest-parts)))
    (cond
      ((not (equal (mod (length source-part-list) 3) 0))
       (error 'client-error :message "This is not a valid path to a relationship"))
      ((not (equal (mod (length dest-parts) 3) 2))
       (error 'client-error
              :message (format nil "/~{~A~^/~} is not a valid path to a resource"
                               dest-parts)))
      ((dependent-relationship-p db source-type relationship dest-type)
       (error 'client-error :message "Dependent resources must only have one parent."))
      ((not (relationship-valid-p db source-type relationship dest-type))
       (error 'integrity-error :message "This is not a valid relationship between these resource types"))
      ((equal (get-resources db (format nil "/~{~A~^/~}" source-parts)) "{}")
       (error 'client-error
              :message (format nil "The source resource /~{~A~^/~} does not exist\n" source-parts)))
      ((equal (get-resources db destpath) "{}")
       (error 'client-error :message "The destination resource does not exist"))
      ((check-relationship-by-path
         db (format nil "/~{~A~^/~}" source-parts) relationship destpath)
       (error 'integrity-error :message "Relationship already exists"))
      (t
       (neo4cl:neo4j-transaction
         db
         `((:STATEMENTS
             ((:STATEMENT .
               ,(format nil "MATCH ~A, ~A MERGE (a)-[:~A]->(b)"
                        (uri-node-helper source-parts "" "a")
                        (uri-node-helper dest-parts "" "b")
                        relationship))))))))))

(defmethod store-dependent-resource ((db neo4cl:neo4j-rest-server)
                                     (uri string)
                                     (attributes list))
  (log-message :debug (format nil "Attempting to create a dependent resource at path ~A" uri))
  (let* ((uri-parts (get-uri-parts uri))
         (relationship (car (last uri-parts)))
         (parent-parts (butlast uri-parts))
         (parent-type (nth (- (length parent-parts) 2) parent-parts))
         (dest-type (cdr (assoc "type" attributes :test 'equal)))
         (dest-uid (cdr (assoc "uid" attributes :test 'equal))))
    (cond
      ;; Sanity check: required parameters
      ((not (and dest-type dest-uid))
       (error 'client-error :message "Both the 'type' and 'uid' parameters must be supplied"))
      ;; Sanity check: is this a dependent resource type?
      ((not (dependent-resource-p db dest-type))
       (error 'client-error :message "This is not a dependent resource type"))
      ;; Sanity check: existence of parent resource
      ((equal (get-resources db (format nil "/~{~A~^/~}" parent-parts)) "{}")
       (error 'client-error :message "Parent resource does not exist"))
      ;; Sanity check: dependency between parent and child resource types
      ((not (dependent-relationship-p db parent-type relationship dest-type))
       (error 'client-error
              :message (format nil "Target resource-type ~A doesn't depend on the parent type ~A"
                               dest-type parent-type)))
      ;; Passed the sanity-checks. Create it.
      (t
        ;; Validate the supplied attributes
        (neo4cl:neo4j-transaction
          db
          `((:STATEMENTS
              ((:STATEMENT .
                           ,(format nil "MATCH ~A CREATE (n)-[:~A]->(:~A { properties })"
                                    (uri-node-helper parent-parts)
                                    relationship
                                    dest-type))
               (:PARAMETERS . ((:PROPERTIES .
                                            ,(validate-resource-before-creating
                                               db
                                               dest-type
                                               (remove-if #'(lambda (param) (equal (car param) "type"))
                                                          attributes)))))))))))))

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

(defmethod get-dependent-resources ((db neo4cl:neo4j-rest-server)
                                    (sourcepath list))
  (mapcar #'(lambda (row)
              ;; labels(n) returns a list, hence the (car)
              (list (first row) (car (second row)) (third row)))
  (neo4cl:extract-rows-from-get-request
    (neo4cl:neo4j-transaction
      db
      `((:STATEMENTS
          ((:STATEMENT .
            ,(format nil "MATCH ~A-[r]->(b) RETURN type(r), labels(b), b.uid"
                     (uri-node-helper sourcepath))))))))))

(defmethod get-dependent-relationships-for-type ((db neo4cl:neo4j-rest-server)
                                                 (resource-type string))
  ;; Eliminate duplicates
  (let ((result-list ()))
    (mapcar #'(lambda (foo)
                (pushnew (car foo) result-list :test 'equal))
            (neo4cl:extract-rows-from-get-request
              (neo4cl:neo4j-transaction
                db
                `((:STATEMENTS
                    ((:STATEMENT .
                      ,(format nil "MATCH (a)-[r { dependent: 'true' }]->(~A) RETURN type(r)"
                               resource-type))))))))
    ;; Return the de-duplicated list
    result-list))

(defmethod check-relationship-by-path ((db neo4cl:neo4j-rest-server)
                                       (sourcepath string)
                                       (relationship string)
                                       (destpath string))
  (neo4cl:extract-rows-from-get-request
    (neo4cl:neo4j-transaction
      db
      `((:STATEMENTS
          ((:STATEMENT .
            ,(format nil "MATCH ~A-[r:~A]->~A RETURN labels(a), a.uid, r, labels(b), b.uid"
                     (uri-node-helper (get-uri-parts sourcepath) "" "a")
                     relationship
                     (uri-node-helper (get-uri-parts destpath) "" "b")))))))))

(defmethod delete-relationship-by-path ((db neo4cl:neo4j-rest-server)
                                        (relpath list)
                                        (targetpath list))
  (let* ((parts (get-uri-parts relpath))
        (rel-path (butlast parts))
        (relationship (last parts))
        (target-parts (get-uri-parts targetpath)))
    (cond
      ((not (equal (mod (length parts) 3) 0))
       (error 'client-error :message "Relationship path does not specify a relationship"))
      ((not (equal (mod (length target-parts) 3) 2))
       (error 'client-error :message "Target path does not specify a relationship"))
      (t
       (neo4cl:neo4j-transaction
         db
         `((:STATEMENTS
             ((:STATEMENT .
               ,(format nil "MATCH ~A-[r:~A]->~A DELETE r"
                        (uri-node-helper rel-path)
                        relationship
                        (uri-node-helper target-parts)))))))))))

(defmethod delete-resource-by-path ((db neo4cl:neo4j-rest-server) (targetpath string) &key delete-dependent)
  (log-message :debug (format nil "Attempting to delete resource ~A" targetpath))
  (let* ((parts (get-uri-parts targetpath)))
    (cond
      ;; Delete a first-class resource
      ((equal (length parts) 2)
       (let ((dependents (get-dependent-resources db parts)))
         (when dependents
           (progn
             ;; FIXME: recursively delete any dependent resources
             (log-message
               :debug
               (format nil "Found dependent resources ~{~A~^, ~}. Proceeding to delete them."
                       dependents))
             (mapcar #'(lambda (dependent)
                         (delete-resource-by-path
                           db
                           (format nil "~A/~A/~A/~A"
                                   targetpath (first dependent) (second dependent) (third dependent))
                           :delete-dependent "true"))
                     dependents))))
       ;; Now delete the resource
       (neo4cl:neo4j-transaction
         db
         `((:STATEMENTS
             ((:STATEMENT . ,(format nil "MATCH (n:~A { uid: '~A' }) DETACH DELETE n"
                                     (first parts) (second parts))))))))
      ;; Delete a relationship to a resource and/or a dependent resource
      ((equal (mod (length parts) 3) 2)
       (let ((parent-parts (butlast parts 3))
             (relationship (nth (- (length parts) 3) parts))
             (dest-type (nth (- (length parts) 2) parts))
             (dest-uid (car (last parts))))
         ;; Has the client requested a dependent relationship, or a regular one?
         (if (dependent-resource-p db dest-type)
           ;; Dependent relationship. Does the client really mean it?
           (if delete-dependent
             ;; Yes, they're sure. Do it.
             (neo4cl:neo4j-transaction
               db
               `((:STATEMENTS
                   ((:STATEMENT .
                     ,(format nil "MATCH ~A DETACH DELETE n" (uri-node-helper parts)))))))
             ;; Unconfirmed; warn them
             (error 'client-error :message "This is a dependent resource. If you really want to delete it, try again with the 'delete-dependent=true' parameter."))
           ;; Regular relationship
           (neo4cl:neo4j-transaction
             db
             `((:STATEMENTS
                 ((:STATEMENT .
                   ,(format nil "MATCH ~A-[r:~A]->(:~A {uid: '~A'}) DELETE r"
                            (uri-node-helper parent-parts)
                            relationship
                            dest-type
                            dest-uid)))))))))
      (t
        (error 'client-error :message "This is not a valid deletion request")))))

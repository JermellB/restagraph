;;;; Methods and functions specifically relating to Neo4J

(in-package #:restagraph)


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
  (if (neo4cl:extract-data-from-get-request
        (neo4cl:neo4j-transaction
          db
          `((:STATEMENTS
              ((:STATEMENT .
                           ,(format nil
                                    "MATCH (n:rgResource { name: '~A' }) RETURN n"
                                    resourcetype)))))))
    ;; Were attributes specified and, if so, are they all valid for this resource-type?
    (let
      ((requested-attributes
         (remove-if #'(lambda (param) (equal (car param) "uid")) params)))
      (if (or
            ;; If no attributes were specified other than "uid", we're good
            (not requested-attributes)
            ;; If other attributes were specified, check them all for validity
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
              (log-message :debug "Checking the supplied attributes.")
              ;; Record the valid ones, if we're debugging
              (if valid-attributes
                (log-message :debug (format nil "Valid attributes for resource-type ~A: ~{~A~^, ~}."
                                            resourcetype valid-attributes))
                (log-message :debug (format nil "Resource-type ~A has no valid attributes to set."
                                            resourcetype)))
              ;; If any invalid attributes were requested, log this and signal an error
              (if invalid-attributes
                (progn
                  (log-message :debug (format nil "Identified invalid attributes: ~{~A~^, ~}"
                                              invalid-attributes))
                  (error 'restagraph:client-error :message
                         (format nil "Invalid attributes for ~A resources: ~{~A~^, ~}"
                                 resourcetype invalid-attributes)))
                (log-message :debug "No invalid attributes identified."))
              ;; We were given attributes other than "uid" and all of them checked out OK.
              ;; Return the supplied attributes to the caller
              t))
        (format-post-params-as-properties
          (acons "uid" (sanitise-uid (cdr (assoc "uid" params :test #'string=)))
                 (acons "original_uid" (cdr (assoc "uid" params :test #'string=))
                        (remove-if #'(lambda (param) (equal (car param) "uid"))
                                   params))))))))


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
       (log-message :debug (format nil "Fetching all resources of type ~A" uri))
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
       (log-message :debug (format nil "Fetching the resource matching the path ~A" uri))
       (cl-json:encode-json-alist-to-string
         (neo4cl:extract-data-from-get-request
           (neo4cl:neo4j-transaction
             db
             `((:STATEMENTS
                 ((:STATEMENT . ,(format nil "MATCH ~A RETURN n"
                                         (uri-node-helper uri-parts))))))))))
      ;; All resources with a particular relationship to this one
      (t
        (log-message :debug
                     (format nil "Fetching all resources with relationship ~A to resource ~{~A~^/~}"
                             (car (last uri-parts))
                             (butlast uri-parts)))
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


;;;; Relationships

(defmethod relationship-valid-p ((db neo4cl:neo4j-rest-server)
                                 (source-type string)
                                 (reltype string)
                                 (dest-type string))
  (neo4cl:extract-rows-from-get-request
    (neo4cl:neo4j-transaction
      db
      `((:STATEMENTS
          ((:STATEMENT
             . ,(format nil "MATCH (a:rgResource)-[r:~A]->(b:rgResource {name: '~A'}) WHERE a.name IN ['~A', 'any'] RETURN a, type(r), b"
                        reltype dest-type source-type))))))))

;;;; FIXME Move to a more appropriate location
;;;; and create a defgeneric.
(defmethod get-relationship-attrs ((db neo4cl:neo4j-rest-server)
                                   (source-type string)
                                   (relationship string)
                                   (dest-type string))
  (car
    (neo4cl:extract-rows-from-get-request
      (neo4cl:neo4j-transaction
        db
        `((:STATEMENTS
            ((:STATEMENT
               .  ,(format nil "MATCH (:rgResource {name: '~A'})-[r:~A]->(:rgResource {name: '~A'}) RETURN r.dependent, r.cardinality"
                           source-type relationship dest-type)))))))))

(defmethod create-relationship-by-path ((db neo4cl:neo4j-rest-server)
                                        (sourcepath string)
                                        (destpath string))
  (log-message :debug (format nil "Attempting to create a relationship from ~A to ~A"
                              sourcepath destpath))
  ;; Initial sanity-checks
  (let ((source-part-list (get-uri-parts sourcepath))
        (dest-parts (get-uri-parts destpath)))
    (cond
      ((not (equal (mod (length source-part-list) 3) 0))
       (let ((message "This is not a valid path to a relationship"))
         (log-message :debug message)
         (error 'client-error :message message)))
      ((not (equal (mod (length dest-parts) 3) 2))
       (let ((message (format nil "/~{~A~^/~} is not a valid path to a resource"
                              dest-parts)))
         (log-message :debug message)
         (error 'client-error :message message)))
      ;; Having made it that far, make checks that call to the database
      (t
        (let* ((relationship (car (last source-part-list)))
               (source-parts (butlast source-part-list))
               (source-type (nth (- (length source-parts) 2) source-parts))
               (dest-type (nth (- (length dest-parts) 2) dest-parts))
               (relationship-attrs (get-relationship-attrs db source-type relationship dest-type)))
          (cond
            ;; No such relationship
            ((not relationship-attrs)
             (let ((message "This is not a valid relationship between these resource types"))
               (log-message :debug message)
               (error 'integrity-error :message message)))
            ;; 1:1 dependent relationship
            ((and
               (first relationship-attrs)
               (or
                 (equal (second relationship-attrs) "1:1")
                 (equal (second relationship-attrs) "1:many")))
             (let ((message (format nil "~A dependency. Either move the relationship or create a new dependent resource."
                                    (second relationship-attrs))))
               (log-message :debug message)
               (error 'integrity-error :message message)))
            ;; Are we trying to create a duplicate?
            ((check-relationship-by-path
               db (format nil "/~{~A~^/~}" source-parts) relationship destpath)
             (let ((message "Relationship already exists"))
               (log-message :debug message)
               (error 'integrity-error :message message)))
            ;; Do both the source and destination resources actually exist?
            ((equal (get-resources db (format nil "/~{~A~^/~}" source-parts)) "{}")
             (let ((message (format nil "The source resource /~{~A~^/~} does not exist\n" source-parts)))
               (log-message :debug message)
               (error 'client-error :message message)))
            ((equal (get-resources db destpath) "{}")
             (let ((message "The destination resource does not exist"))
               (log-message :debug message)
               (error 'client-error :message message)))
            ;; Many-to-one, and the source already has this relationship with another such target?
            ((and
               (equal (second relationship-attrs) "many:1")
               (>
                 (neo4cl:extract-data-from-get-request
                   (neo4cl:neo4j-transaction
                     db
                     `((:STATEMENTS
                         ((:STATEMENT
                            .  ,(format nil "MATCH ~A-[:~A]->(b:~A) RETURN count(b)"
                                        (uri-node-helper source-parts "" "a")
                                        relationship
                                        dest-type)))))))
                 0))
             (let ((message (format nil"~{~A~^/~} already has a many:1 ~A relationship with a resource of type ~A"
                                    source-parts relationship dest-type)))
               (log-message :debug message)
               (error 'integrity-error :message message)))
            ;; Go ahead and create the relationship
            (t
              (neo4cl:neo4j-transaction
                db
                `((:STATEMENTS
                    ((:STATEMENT .
                                 ,(format nil "MATCH ~A, ~A MERGE (a)-[:~A]->(b)"
                                          (uri-node-helper source-parts "" "a")
                                          (uri-node-helper dest-parts "" "b")
                                          relationship)))))))))))))


(defmethod move-dependent-resource ((db neo4cl:neo4j-rest-server)
                                    (uri list)
                                    (newparent string))
  (log-message :debug
               (format nil "Attempting to move dependent resource ~A to new parent ~A"
                       uri newparent))
  (let* ((current-parent-path (uri-node-helper (butlast uri 3) "" "b"))
         (current-relationship (car (last (butlast uri 2))))
         (target-type (car (last (butlast uri))))
         (target-uid (car (last uri)))
         (dest-parts (get-uri-parts newparent))
         (new-relationship (car (last dest-parts)))
         (new-parent-path (build-cypher-path (butlast dest-parts)))
         (new-parent-type (car (last (butlast dest-parts 2)))))
    (cond
      ;; Sanity-check: does the target resource exist?
      ((equal (get-resources db (format nil "/~{~A~^/~}" uri)) "{}")
       (log-message :debug (format nil "Target resource ~A does not exist"
                                   (format nil "/~{~A~^/~A~}" uri)))
       (error 'client-error :message "Target resource does not exist"))
      ;; Sanity-check: does the new parent exist?
      ((equal (get-resources db (format nil "/~{~A~^/~}" (butlast dest-parts))) "{}")
       (log-message :debug (format nil "Parent resource ~A does not exist"
                                   (format nil "/~{~A~^/~}" (butlast dest-parts))))
       (error 'client-error :message "Parent resource does not exist"))
      ;; Sanity-check: is the new relationship a valid dependent one?
      ((not (dependent-relationship-p db new-parent-type new-relationship target-type))
       (log-message
         :debug
         (format
           nil
           "Target resource ~A does not depend on the new parent-type ~A for relationship ~A"
           target-type
           new-parent-type
           new-relationship))
       (error 'client-error
              :message
              (format
                nil
                "Target resource-type ~A doesn't depend on the parent type ~A"
                target-type new-parent-type)))
      ;; Sanity-checks passed; let's do it
      (t
        (log-message
          :debug
          (format nil "Moving target ~A:~A from parent ~A to new parent ~A"
                  target-type target-uid
                  current-parent-path
                  new-parent-path))
        ;; Create the new relationship
        (neo4cl:neo4j-transaction
          db
          `((:STATEMENTS
              ((:STATEMENT
                 . ,(format nil "MATCH ~A-[r:~A]->(t:~A {uid: '~A'}), ~A CREATE (m)-[:~A]->(t)"
                            current-parent-path
                            current-relationship
                            target-type
                            target-uid
                            new-parent-path
                            new-relationship))))))
        ;; Delete the old relationship, using all but the last two elements of the source path
        (neo4cl:neo4j-transaction
          db
          `((:STATEMENTS
              ((:STATEMENT .
                           ,(format nil "MATCH ~A-[r:~A]->(t:~A {uid: '~A'}) DELETE r"
                                    current-parent-path
                                    current-relationship
                                    target-type
                                    target-uid))))))))))

(defmethod store-dependent-resource ((db neo4cl:neo4j-rest-server)
                                     (uri string)
                                     (attributes list))
  (log-message :debug (format nil "Attempting to create a dependent resource at path ~A" uri))
  (let* ((uri-parts (get-uri-parts uri))
         (relationship (car (last uri-parts)))
         (parent-parts (butlast uri-parts))
         (parent-type (nth (- (length parent-parts) 2) parent-parts))
         (dest-type (cdr (assoc "type" attributes :test 'equal)))
         (dest-uid (cdr (assoc "uid" attributes :test 'equal)))
         (relationship-attrs (get-relationship-attrs db parent-type relationship dest-type)))
    (cond
      ;; Sanity check: required parameters
      ((not (and dest-type dest-uid))
       (let ((message "Both the 'type' and 'uid' parameters must be supplied"))
         (log-message :debug message)
         (error 'client-error :message message)))
      ;; Sanity check: existence of parent resource
      ((equal (get-resources db (format nil "/~{~A~^/~}" parent-parts)) "{}")
       (let ((message "Parent resource does not exist"))
         (log-message :debug message)
         (error 'client-error :message message)))
      ;; Sanity check: dependency between parent and child resource types
      ((null (first relationship-attrs))
       (let ((message
               (format nil "Target resource-type ~A doesn't depend on the parent type ~A"
                       dest-type parent-type)))
         (log-message :debug message)
         (error 'client-error :message message)))
      ;; Sanity check: is this a dependent resource type?
      ((not (dependent-resource-p db dest-type))
       (let ((message "This is not a dependent resource type"))
         (log-message :debug message)
         (error 'client-error :message message)))
      ;; Passed the initial sanity-checks; try to create it.
      (t
        ;; Validate the supplied attributes
        (let* ((validated-attributes (validate-resource-before-creating
                                       db
                                       dest-type
                                       (remove-if #'(lambda (param) (equal (car param) "type"))
                                                  attributes)))
               (resource-path (format nil "/~{~A~^/~}/~A/~A/~A"
                                      parent-parts
                                      relationship
                                      dest-type
                                      (cdr (assoc "uid" validated-attributes :test #'string=)))))
          ;; Report on the attributes for debugging
          (log-message :debug (format nil "Validated attributes: ~A" validated-attributes))
          ;; One more sanity-check: does it already exist?
          (if (equal (get-resources db resource-path) "{}")
            ;; Cardinality checks: would this violate 1:1 or many:1 constraints?
            (if
              (and
                (or
                  (equal (second relationship-attrs) "1:1")
                  (equal (second relationship-attrs) "many:1"))
                ;; Look for this parent having this relationship with any other dependent resource
                (>
                  (neo4cl:extract-data-from-get-request
                    (neo4cl:neo4j-transaction
                      db
                      `((:STATEMENTS
                          ((:STATEMENT .
                                       ,(format nil "MATCH ~A<-[r {dependent: 'true'}]-() RETURN count(r)"
                                                (uri-node-helper parent-parts))))))))
                  0))
              (error 'integrity-error :message
                     (format nil"~{~A~^/~} already has a ~A ~A relationship with a resource of type ~A"
                             parent-parts
                             (second relationship-attrs)
                             relationship
                             dest-type))
              ;; Constraints are fine; create it
              (neo4cl:neo4j-transaction
                db
                `((:STATEMENTS
                    ((:STATEMENT .
                                 ,(format nil "MATCH ~A CREATE (n)-[:~A]->(:~A { properties })"
                                          (uri-node-helper parent-parts)
                                          relationship
                                          dest-type))
                     (:PARAMETERS . ((:PROPERTIES . ,validated-attributes))))))))
            ;; We already have one of these
            (error 'integrity-error :message (format nil "Resource ~A already exists" resource-path))))))))

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
  (let ((parent-type (car (butlast sourcepath)))
        ;; Get all nodes to which this node has outbound relationships
        (candidates
          (mapcar #'(lambda (row)
                      ;; labels(n) returns a list, hence the (car)
                      ;; List elements: relationship, target type, target UID
                      (list (first row) (car (second row)) (third row)))
                  (neo4cl:extract-rows-from-get-request
                    (neo4cl:neo4j-transaction
                      db
                      `((:STATEMENTS
                          ((:STATEMENT .
                                       ,(format nil "MATCH ~A-[r]->(b) RETURN type(r), labels(b), b.uid"
                                                (uri-node-helper sourcepath)))))))))))
    ;; Filter out any candidate nodes that do not have a _dependent_ relationship on the parent
    (remove-if
      #'null
      (mapcar #'(lambda (c)
                  (when (dependent-relationship-p db parent-type (first c) (second c))
                    c))
              candidates))))

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
                                        (relpath string)
                                        (targetpath string))
  (let* ((parts (get-uri-parts relpath))
         (rel-path (butlast parts))
         (relationship (car (last parts)))
         (target-parts (get-uri-parts targetpath)))
    ;; Initial sanity checks
    (cond
      ((not (equal (mod (length parts) 3) 0))
       (error 'client-error :message "Relationship path does not specify a relationship"))
      ((not (equal (mod (length target-parts) 3) 2))
       (error 'client-error :message "Target path does not specify a relationship"))
      (t
       ;; Check also for dependent relationships
       (let ((relationship-attrs
               (get-relationship-attrs
                 db
                 (nth (- (length parts) 2) parts) ; source-type
                 relationship
                 (nth (- (length target-parts) 1) target-parts))))    ; dest-type
         (cond
           ;; 1-parent dependent resource; bounce the client back to delete-resource
           ((and (first relationship-attrs)
                 (or (equal (second relationship-attrs) "1:1")
                     (equal (second relationship-attrs) "1:many")))
            (error 'restagraph:integrity-error
                   :message "This would leave an orphan dependent resource. Delete the dependent resource instead."))
           ;; Multi-parent dependent resource, and this would be the last parent; bounce to delete-resource.
           ((and
              (first relationship-attrs)
              (or (equal (second relationship-attrs) "many:1")
                  (equal (second relationship-attrs) "many:many"))
              ;; Would this be the last parent?
              (> (neo4cl:extract-data-from-get-request
                   (neo4cl:neo4j-transaction
                     db
                     `((:STATEMENTS
                         ((:STATEMENT .
                           ,(format nil "MATCH ~A<-[r {dependent: 'true'}]-() return count(r)"
                                    (uri-node-helper target-parts ""))))))))
                 0))
            (error 'restagraph:integrity-error
                   :message "Removing the last parent would leave an orphan dependent resource. Delete the dependent resource instead."))
           ;; All those checks passed; delete the relationship
           (t
            (neo4cl:neo4j-transaction
              db
              `((:STATEMENTS
                  ((:STATEMENT .
                    ,(format nil "MATCH ~A-[r:~A]->~A DELETE r"
                             (uri-node-helper rel-path)
                             relationship
                             (uri-node-helper target-parts))))))))))))))

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

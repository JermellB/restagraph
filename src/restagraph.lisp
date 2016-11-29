(in-package #:restagraph)

(defgeneric get-classes-from-db (db)
  (:documentation "Extract the class definitions from the database"))

(defmethod get-classes-from-db ((db neo4cl:neo4j-rest-server))
  (mapcar #'car
          (neo4cl::extract-rows-from-get-request
            (neo4cl:neo4j-transaction
              db
              `((:STATEMENTS
                  ((:STATEMENT . "MATCH (c:rgClass) RETURN c.name"))))))))

(defgeneric get-class-relationships-from-db (db)
  (:documentation "Extract the relationships between the classes, from the database"))

(defmethod get-class-relationships-from-db ((db neo4cl:neo4j-rest-server))
          (neo4cl::extract-rows-from-get-request
            (neo4cl:neo4j-transaction
              db
              `((:STATEMENTS
                  ((:STATEMENT . "MATCH (c:rgClass)-[r]->(t:rgClass) RETURN c.name, type(r), t.name")))))))

(defgeneric add-class-to-schema (schema newclass)
  (:documentation "Add a class to a schema, ensuring the internal structure is ready to receive new attributes and relationships."))

(defmethod add-class-to-schema ((schema hash-table) (newclass string))
  (log-message :debug (format nil "Ensuring class name '~A' is present in the schema" newclass))
  (unless (gethash newclass schema)
    (progn
      ;; Add the class to the schema
      (setf (gethash newclass schema)
            (make-hash-table :test 'equal))
      ;; Add the 'attributes' subschema for the class
      (setf (gethash "attributes" (gethash newclass schema))
            (make-hash-table :test 'equal))
      ;; Add the 'relationships' subschema for the class
      (setf (gethash "relationships" (gethash newclass schema))
            (make-hash-table :test 'equal)))))

(defgeneric get-class-from-schema-by-name (schema classname)
  (:documentation "Extract a class' definition from the schema, by name."))

(defmethod get-class-from-schema-by-name ((schema hash-table) (classname string))
  (gethash classname schema))

(defgeneric add-class-relationship-to-schema (schema from-class relationship to-class)
  (:documentation "Update the schema with a directional relationship between two classes, returning an error if either of the classes doesn't exist."))

(defmethod add-class-relationship-to-schema ((schema hash-table) (from-class string) (relationship string) (to-class string))
  ;; Sanity-check: only proceed if both classes are already in the schema
  (log-message :debug "Attempting to add relationship ~A-[~A]->~A to the schema"
               from-class relationship to-class)
  (if (and (get-class-from-schema-by-name schema from-class)
           (get-class-from-schema-by-name schema to-class))
    ;; If there's already an entry for this relationship, add another target for it
    (if (gethash relationship (gethash "relationships" (get-class-from-schema-by-name schema from-class)))
      (progn
        (log-message :debug "Adding target ~A to the existing list for relationship ~A from source ~A"
                     to-class relationship from-class)
        (pushnew to-class (gethash relationship (gethash "relationships" (get-class-from-schema-by-name schema from-class)))))
      ;; If there isn't, initialise that relationship with this target
      (progn
        (log-message :debug "Adding new relationship ~A with target class ~A to source-class ~A"
                     relationship to-class from-class)
        (setf (gethash relationship (gethash "relationships" (get-class-from-schema-by-name schema from-class)))
              (list to-class))))
    ;; If either the to-class or from-class isn't present in the schema,
    ;; report back to the caller with an error.
    (error "Both the from-class and to-class must already be present in the schema. Ensure they both exist, and try again.")))

(defun populate-schema (db &optional (schema (make-hash-table :test 'equal)))
  "Queries the database for the details of the schema,
   and returns it as a set of nested alists.
   If a schema isn't supplied, automatically creates one in the default form of a hash-table."
  #|
  For each class returned by get-classes-from-db
  - ensure there's an entry for the class in the schema
  - iterate through the relationships returned by get-class-relationships-from-db
  - if the first element matches the name of the class
  - ensure there's an entry for that relationship-type under the class in
  the schema, where the relationship type is the second element in the list
  - add the third element of the list (target class) to the list of valid
  targets under the relationship type
  Finally, return the whole thing
  |#
  (log-message :debug "Populating the schema with classes")
  (mapcar #'(lambda (classname)
              (add-class-to-schema schema classname))
          (get-classes-from-db db))
  (log-message :debug "Populating the schema with relationships between the classes")
  (mapcar #'(lambda (reltriple)
              (let ((from-class (first reltriple))
                    (relationship (second reltriple))
                    (to-class (third reltriple)))
                (log-message :debug (format nil "Adding relationship ~A-[~A]->~A to the schema"
                                            from-class relationship to-class))
                (add-class-relationship-to-schema schema from-class relationship to-class)))
          (get-class-relationships-from-db db))
  ;; Explicitly return the schema object
  schema)

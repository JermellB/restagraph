(in-package #:restagraph)

;;;; Schema methods and functions
;;;;
;;;; Pure-functional methods

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

(defmethod get-class-from-schema-by-name ((schema hash-table) (classname string))
  (gethash classname schema))

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


;;;; Non-pure methods

(defun populate-schema (db &optional (schema (make-hash-table :test 'equal)))
  "Queries the database for the details of the schema,
   and returns it as a set of nested alists.
   If a schema isn't supplied, automatically creates one in the default form of a hash-table."
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

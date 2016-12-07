(in-package #:restagraph)

;;;; Schema methods and functions
;;;;
;;;; Pure-functional methods

(defmethod add-resourcetype-to-schema ((schema hash-table) (resourcetype string) (attributes hash-table))
  (log-message :debug (format nil "Ensuring resource type '~A' is present in the schema" resourcetype))
  ;; Add the resource-type to the schema
  (setf (gethash resourcetype schema)
        (make-hash-table :test 'equal))
  ;; Add the 'attributes' subschema for the resource-type
  (setf (gethash "attributes" (gethash resourcetype schema)) attributes)
  ;; Add the 'relationships' subschema for the resource-type
  (setf (gethash "relationships" (gethash resourcetype schema))
        (make-hash-table :test 'equal)))

(defmethod get-resourcetype-from-schema-by-name ((schema hash-table) (resourcename string))
  (gethash resourcename schema))

(defmethod add-resource-relationship-to-schema ((schema hash-table) (from-type string) (relationship string) (to-type string))
  ;; Sanity-check: only proceed if both resource-types are already in the schema
  (log-message :debug "Attempting to add relationship ~A-[~A]->~A to the schema"
               from-type relationship to-type)
  (if (and (get-resourcetype-from-schema-by-name schema from-type)
           (get-resourcetype-from-schema-by-name schema to-type))
    ;; If there's already an entry for this relationship, add another target for it
    (if (gethash relationship (gethash "relationships" (get-resourcetype-from-schema-by-name schema from-type)))
      (progn
        (log-message :debug "Adding target ~A to the existing list for relationship ~A from source ~A"
                     to-type relationship from-type)
        (pushnew to-type (gethash relationship (gethash "relationships" (get-resourcetype-from-schema-by-name schema from-type)))))
      ;; If there isn't, initialise that relationship with this target
      (progn
        (log-message :debug "Adding new relationship ~A with target type ~A to source type ~A"
                     relationship to-type from-type)
        (setf (gethash relationship (gethash "relationships" (get-resourcetype-from-schema-by-name schema from-type)))
              (list to-type))))
    ;; If either the to-type or from-type isn't present in the schema,
    ;; report back to the caller with an error.
    (error "Both the from-type and to-type must already be present in the schema. Ensure they both exist, and try again.")))


;;;; Non-pure methods

(defun populate-schema (db &optional (schema (make-hash-table :test 'equal)))
  "Queries the database for the details of the schema,
   and returns it as a set of nested alists.
   If a schema isn't supplied, automatically creates one in the default form of a hash-table."
  (log-message :debug "Populating the schema with resource types")
  (maphash #'(lambda (typename attributes)
              (add-resourcetype-to-schema schema typename attributes))
          (get-resources-from-db db))
  (log-message :debug "Populating the schema with relationships between the resource types")
  (mapcar #'(lambda (reltriple)
              (let ((from-type (first reltriple))
                    (relationship (second reltriple))
                    (to-type (third reltriple)))
                (log-message :debug (format nil "Adding relationship ~A-[~A]->~A to the schema"
                                            from-type relationship to-type))
                (add-resource-relationship-to-schema schema from-type relationship to-type)))
          (get-resource-relationships-from-db db))
  ;; Explicitly return the schema object
  schema)

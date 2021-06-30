;   Copyright 2021 James Fleming <james@electronic-quill.net>
;
;   Licensed under the GNU General Public License
;   - for details, see LICENSE.txt in the top-level directory


;;;; Schema-related functions, specific to storing the schema in the database.

(in-package #:restagraph)

(declaim (optimize (compilation-speed 0)
                   (speed 2)
                   (safety 3)
                   (debug 3)))


;;; Install a schema version in the database

(defun current-schema-p (db)
  "Test whether there's a current schema in place"
  (declare (type neo4cl:neo4j-rest-server db))
  (caar
    (neo4cl:extract-rows-from-get-request
      (neo4cl:neo4j-transaction
        db
        `((:STATEMENTS
            ((:STATEMENT
               .  "MATCH (c:RgSchema { name: 'root' })-[:CURRENT_VERSION]->(v:RgSchemaVersion) RETURN v.createddate"))))))))

(defun ensure-current-schema (db subschema)
  "Ensure there's a current schema in place, complete with uniqueness constraints.
   Return the timestamp of the current schema's created date, as its version identifier."
  (declare (type neo4cl:neo4j-rest-server db)
           (type incoming-subschema-version subschema))
  ;; Ensure we have a uniqueness constraint on resource-types
  (handler-case
    (ensure-uniqueness-constraint db "RgResource" "name")
    (error (e)
           (log-message :fatal "Failed to ensure uniqueness constraint on RgResource label.")
           (log-message :fatal (message e))
           (sb-ext:exit)))
  (or (current-schema-p db)
      (let ((version
              ;; No current schema found; install one.
              (progn
                ;; Ensure there's a schema root.
                (neo4cl:neo4j-transaction
                  db
                  `((:STATEMENTS
                      ((:STATEMENT
                         . ,(format nil "MERGE (s:RgSchema {name: \"root\"}) ON CREATE SET s.createddate = ~D RETURN s.name"
                                    (get-universal-time)))))))
                ;; Add a current version.
                (caar
                  (neo4cl:extract-rows-from-get-request
                    (neo4cl:neo4j-transaction
                      db
                      `((:STATEMENTS
                          ((:STATEMENT
                             . ,(format nil "MATCH (r:RgSchema {name: \"root\"}) CREATE (r)-[:VERSION]->(v:RgSchemaVersion { createddate: ~D }),
                                             (r)-[:CURRENT_VERSION]->(v)
                                             RETURN v.createddate"
                                        (get-universal-time))))))))))))
              (install-subschema db subschema version))))

;; FIXME: Handle duplicate resourcetypes and relationships
;; New attributes will be added as an augmentation to existing resourcetypes
;; and relationships, but existing definitions will not be changed.
(defun install-subschema (db subschema schema-version)
  (declare (type neo4cl:neo4j-rest-server db)
           (type incoming-subschema-version subschema)
           (type integer schema-version))
  ;; Install the core-schema to the schema root.
  (log-message :info (format nil "Installing subschema '~A' to schema version ~D."
                             (name subschema)
                             schema-version))
  ;; Install the resourcetypes
  (mapcar
    #'(lambda (rtype)
        ;; Install the resourcetype itself
        (let ((query
                (format
                  nil
                  ;; Basic resourcetype definition
                  "MATCH (r:RgSchema {name: \"root\"})-[:VERSION]->(v:RgSchemaVersion { createddate: ~D })
                   CREATE (v)-[:HAS]->(t:RgResourceType {name: \"~A\", dependent: \"~A\", notes: \"~A\"})~A"
                  schema-version
                  (name rtype)
                  (if (dependent rtype) "true" "false")
                  (notes rtype)
                  ;; Enumerate its attributes
                  (format nil "~{,~%~A~}"
                          (mapcar
                            #'(lambda
                                (attr)
                                (format nil "(t)-[:HAS]->(:RgResourceTypeAttribute {name: \"~A\", description: \"~A\", values: \"~A\"})"
                                        (name attr)
                                        (description attr)
                                        (format nil "~{~A~^,~}" (attr-values attr))))
                            (attributes rtype))))))
          (log-message :debug (format nil "Installing resourcetype definition with this query:~%~A"
                                      query))
          (neo4cl:neo4j-transaction db `((:STATEMENTS ((:STATEMENT . ,query)))))))
    (resourcetypes subschema))
  ;; Now install the relationships
  (log-message :info (format nil "Installing relationships for subschema '~A'"
                             (name subschema)))
  (log-message :debug (format nil "Using relationship definitions ~A" (relationships subschema)))
  (mapcar
    #'(lambda (rel)
        (let ((query
                (format nil
                        "MATCH (r:RgSchema {name: \"root\"})-[:VERSION]->(v:RgSchemaVersion { createddate: ~D })-[:HAS]->(s:RgResourceType {name: \"~A\"}),
                         (v)-[:HAS]->(t:RgResourceType {name: \"~A\"})
                         CREATE (s)<-[:SOURCE]-(:RgRelationship {name: \"~A\", dependent: ~A, notes: \"~A\", cardinality: \"~A\"})-[:TARGET]->(t)"
                        schema-version
                        (source-type rel)
                        (target-type rel)
                        (name rel)
                        (if (dependent rel) "true" "false")
                        (notes rel)
                        (cardinality rel))))
          (log-message :debug (format nil "Installing resourcetype definition with this query:~%~A"
                                      query))
          (handler-case
            (neo4cl:neo4j-transaction db `((:STATEMENTS ((:STATEMENT . ,query)))))
            (error (e)
                   (log-message :error (message e))))))
        (relationships subschema)))


;;; Extract a schema from the database

(defun fetch-resourcetype-def-from-db (db resourcetype)
  "Return a resourcetype struct for use in the API schema,
   based on a definition retrieved from the database."
  (declare (type neo4cl:neo4j-rest-server db)
           (type string resourcetype))
  (let
    ((query-rtype (format
                    nil
                    "MATCH (:RgSchema {name: \"root\"})-[:CURRENT_VERSION]->(:RgSchemaVersion)-[:HAS]->(r:RgResourceType {name: \"~A\"})
                     RETURN r.dependent, r.notes;"
                    resourcetype))
     (query-attrs (format
                    nil
                    "MATCH (:RgSchema {name: \"root\"})-[:CURRENT_VERSION]->(:RgSchemaVersion)-[:HAS]->(r:RgResourceType {name: \"~A\"})-[:HAS]->(a:RgResourceTypeAttribute)
                     RETURN a.name, a.description, a.values;"
                    resourcetype)))
    (let ((rtype (car
                   (neo4cl:extract-rows-from-get-request
                     (neo4cl:neo4j-transaction
                       db `((:STATEMENTS ((:STATEMENT . ,query-rtype))))))))
          (attrs (neo4cl:extract-rows-from-get-request
                   (neo4cl:neo4j-transaction
                     db `((:STATEMENTS ((:STATEMENT . ,query-attrs))))))))
      (make-schema-rtypes
        :name resourcetype
        :dependent (when (equal "true" (first rtype)) t)
        :notes (second rtype)
        :attributes (mapcar #'(lambda (attr)
                                (make-schema-rtype-attrs
                                  :name (first attr)
                                  :description (second attr)
                                  :attr-values (when (and (third attr)
                                                     (stringp (third attr)))
                                            (cl-ppcre:split "," (third attr)))))
                            attrs)
        :relationships ()))))

(defun fetch-rels-from-db (db resourcetype schema)
  "Placeholder function to grab the relationships from a given source resourcetype,
   and install them in the schema hash."
  (declare (type neo4cl:neo4j-rest-server db)
           (type string resourcetype)
           (type hash-table schema))
  (let* ((query-rels (format
                       nil
                       "MATCH (:RgSchema {name: \"root\"})-[:CURRENT_VERSION]->(:RgSchemaVersion)-[:HAS]->(:RgResourceType {name: \"~A\"})<-[:SOURCE]->(r:RgRelationship)-[:TARGET]->(t:RgResourceType)
                        RETURN r.name, r.dependent, r.cardinality, r.notes, t.name;"
                       resourcetype)))
    (set-relationships (gethash resourcetype schema)
                       (mapcar #'(lambda (rel)
                                   (make-schema-rels
                                     :name (first rel)
                                     :dependent (second rel)
                                     :cardinality (third rel)
                                     :notes (fourth rel)
                                     :target-type (gethash (fifth rel) schema)))
                               (neo4cl:extract-rows-from-get-request
                                 (neo4cl:neo4j-transaction
                                   db `((:STATEMENTS ((:STATEMENT . ,query-rels))))))))))

(defun fetch-current-schema (db)
  "Return a hash-table representing the current schema version,
   populated by definitions retrieved from the database."
  (declare (type neo4cl:neo4j-rest-server db))
  (log-message :info "Fetching the current schema from the database.")
  ;; Create a schema structure
  (let ((schema (make-schema-hash-table))
        ;; Fetch this once, to use a couple of times
        (resourcetypes (list-resourcetypes-in-db db)))
    ;; Populate the schema with resourcetypes
    (mapcar #'(lambda (rtype)
                (setf (gethash rtype schema)
                      (fetch-resourcetype-def-from-db db rtype)))
            resourcetypes)
    ;; Add the relationships
    (mapcar #'(lambda (rtype)
                (fetch-rels-from-db db rtype schema))
            resourcetypes)
    ;; Return the schema we created
    schema))

(defun list-resourcetypes-in-db (db)
  "Return a list of the names of resourcetypes in the current schema version."
  (declare (type neo4cl:neo4j-rest-server db))
  (mapcar #'car
          (neo4cl:extract-rows-from-get-request
            (neo4cl:neo4j-transaction
              db
              `((:STATEMENTS
                  ((:STATEMENT
                     . "MATCH (:RgSchema {name: \"root\"})-[:CURRENT_VERSION]->(:RgSchemaVersion)-[:HAS]->(r:RgResourceType)
                        RETURN r.name;"))))))))

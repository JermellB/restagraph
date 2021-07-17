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

(defun ensure-uniqueness-constraint (server label attribute)
  "Ensure Neo4j has a uniqueness constraint on the specified attribute/label combination."
  (declare (type neo4cl:neo4j-rest-server server)
           (type string label)
           (type string attribute))
  (handler-case
    (neo4cl:neo4j-transaction
      server
      `((:STATEMENTS
          ((:STATEMENT
             . ,(format nil "CREATE CONSTRAINT ON (r:~A) ASSERT r.~A IS UNIQUE"
                        label attribute))))))
    ;; If this fails because we already did it, that's fine.
    (neo4cl:client-error
      (e)
      ;; If we already have this constraint, catch the error and move on.
      (if (and
            (equal "Schema" (neo4cl:category e))
            (equal "EquivalentSchemaRuleAlreadyExists" (neo4cl:title e)))
          nil
          ;; If anything else went wrong, log it and pass it on up the stack
          (progn
            (log-message :debug (format nil "Received error '~A.~A ~A'"
                                        (neo4cl:category e)
                                        (neo4cl:title e)
                                        (neo4cl:message e)))
            (return-database-error
              (format nil "~A.~A: ~A"
                      (neo4cl:category e)
                      (neo4cl:title e)
                      (neo4cl:message e))))))))

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
              ;; Install the core schema
              (install-subschema db subschema version)
              ;; Install an additional schema, if specified and present
              (install-additional-schema db version))))

(defun install-subschema (db subschema schema-version)
  "New attributes will be added as an augmentation to existing resourcetypes
  and relationships, but existing definitions will not be changed."
  (declare (type neo4cl:neo4j-rest-server db)
           (type incoming-subschema-version subschema)
           (type integer schema-version))
  ;; Install the core-schema to the schema root.
  (log-message :info (format nil "Installing subschema '~A' to schema version ~D."
                             (name subschema)
                             schema-version))
  (let ((extant-resourcetypes (get-resourcetype-names db)))
    ;; Install the resourcetypes
    (mapcar
      #'(lambda (rtype)
          ;; Install the resourcetypes and their attributes
          (let ((query
                  (format
                    nil
                    ;; Basic resourcetype definition
                    "MATCH (r:RgSchema {name: \"root\"})-[:VERSION]->(v:RgSchemaVersion { createddate: ~D })~A~A"
                    schema-version
                    ;; Create the resource iff it doesn't already exist
                    (if (member (name rtype) extant-resourcetypes :test #'equal)
                      (format nil "-[:HAS]->(t:RgResourceType {name: \"~A\"}) CREATE " (name rtype))
                      (format nil
                              " CREATE (v)-[:HAS]->(t:RgResourceType {name: \"~A\", dependent: ~A, description: ~A})~A"
                              (name rtype)
                              (if (dependent rtype) "true" "false")
                              (if (and (description rtype)
                                       (not (equal "" (description rtype))))
                                (format nil "\"~A\"" (description rtype))
                                "null")
                              ;; Insert a comma if needed
                              (if (attributes rtype) ", " " ")))
                    ;; Enumerate its attributes
                    (format nil "~{~%~A~^,~}"
                            (mapcar
                              #'(lambda
                                  (attr)
                                  (format nil "(t)-[:HAS]->(:RgResourceTypeAttribute {name: \"~A\", description: ~A, values: ~A})"
                                          (name attr)
                                          (if (and (description attr)
                                                   (not (equal "" (description attr))))
                                            (format nil "\"~A\"" (description attr))
                                            "null")
                                          (if (attr-values attr)
                                            (format nil "\"~{~A~^,~}\"" (attr-values attr))
                                            "null")))
                              (attributes rtype))))))
            (log-message :debug (format nil "Installing resourcetype definition with this query:~%~A"
                                        query))
            (handler-case
              (neo4cl:neo4j-transaction db `((:STATEMENTS ((:STATEMENT . ,query)))))
              (error (e)
                     (cond
                       ((typep e 'neo4cl:transient-error)
                        (log-message :fatal (format nil "Neo4j error ~A ~A - ~A"
                                                    (neo4cl:category e)
                                                    (neo4cl:title e)
                                                    (neo4cl:message e))))
                       ((typep e 'neo4cl:database-error)
                        (log-message :fatal (format nil "Neo4j error ~A ~A - ~A"
                                                    (neo4cl:category e)
                                                    (neo4cl:title e)
                                                    (neo4cl:message e))))
                       ((typep e 'neo4cl:client-error)
                        (log-message :fatal (format nil "Neo4j error ~A ~A - ~A"
                                                    (neo4cl:category e)
                                                    (neo4cl:title e)
                                                    (neo4cl:message e))))
                       (t
                         (progn
                           (log-message :fatal (format nil "Unhandled error '~A'" e)))))))))
      (resourcetypes subschema)))
  ;; Now install the relationships
  (log-message :info (format nil "Installing relationships for subschema '~A'"
                             (name subschema)))
  (mapcar
    #'(lambda (rel)
        ;; Only create this relationship if it doesn't already exist
        (if (get-relationship db (source-type rel) (name rel) (target-type rel))
          (log-message :debug (format nil "Refusing to create duplicate relationship (:~A)-[:~A]->(~A)"
                                      (source-type rel) (name rel) (target-type rel)))
          (let ((query
                  ;; Special-case code for self-relationships.
                  ;; If the source-type and target-type are the same resource, Cypher will
                  ;; refuse to make two separate references to iitt
                  (if (equal (source-type rel) (target-type rel))
                    (format nil
                            "MATCH (r:RgSchema {name: \"root\"})-[:VERSION]->(v:RgSchemaVersion { createddate: ~D })-[:HAS]->(s:RgResourceType {name: \"~A\"})
                            CREATE (s)<-[:SOURCE]-(:RgRelationship {name: \"~A\", dependent: ~A, description: ~A, cardinality: \"~A\"})-[:TARGET]->(s)"
                            schema-version
                            (source-type rel)
                            (name rel)
                            (if (dependent rel) "true" "false")
                            (if (description rel)
                              (format nil "\"~A\"" (description rel))
                              "null")
                            (cardinality rel))
                            ;; Normal case where the source and target types are different
                            (format nil
                                    "MATCH (r:RgSchema {name: \"root\"})-[:VERSION]->(v:RgSchemaVersion { createddate: ~D })-[:HAS]->(s:RgResourceType {name: \"~A\"}), (v)-[:HAS]->(t:RgResourceType {name: \"~A\"})
                                    CREATE (s)<-[:SOURCE]-(:RgRelationship {name: \"~A\", dependent: ~A, description: ~A, cardinality: \"~A\"})-[:TARGET]->(t)"
                                    schema-version
                                    (source-type rel)
                                    (target-type rel)
                                    (name rel)
                                    (if (dependent rel) "true" "false")
                                    (if (description rel)
                                      (format nil "\"~A\"" (description rel))
                                      "null")
                                    (cardinality rel)))))
                  (log-message :debug (format nil "Installing relationship definition with this query:~%~A"
                                              query))
                  (handler-case
                    (neo4cl:neo4j-transaction db `((:STATEMENTS ((:STATEMENT . ,query)))))
                    (error (e)
                           (cond ((typep e 'neo4cl:client-error)
                                  (log-message
                                    :error
                                    (format nil "Neo4J client error ~A/~A - ~A"
                                            (neo4cl:category e) (neo4cl:title e) (neo4cl:message e))))
                                 ((typep e 'neo4cl:transient-error)
                                  (log-message
                                    :error
                                    (format nil "Neo4J transient error ~A/~A - ~A"
                                            (neo4cl:category e) (neo4cl:title e) (neo4cl:message e))))
                                 ((typep e 'neo4cl:database-error)
                                  (log-message
                                    :error
                                    (format nil "Neo4J database error ~A/~A - ~A"
                                            (neo4cl:category e) (neo4cl:title e) (neo4cl:message e))))
                                 (t
                                   (log-message :error e))))))))
          (relationships subschema)))

(defun install-additional-schema (db schema-version)
  "Install an additional schema from the filesystem."
  (declare (type neo4cl:neo4j-rest-server db)
           (type integer schema-version))
  ;; Check whether the env var was set
  (let ((filevar (sb-posix:getenv "SCHEMAPATH")))
    ;; If it was set, report this and try to act on it.
    (if filevar
        (progn
          (log-message
            :info
            (format nil "Attempting to install additional schema to version ~D from '~A'"
                    schema-version filevar))
          ;; Check whether the file exists
          ;; Use `let` because `probe-file` returns a valid filepath on success, so we get this for free.
          (let ((filepath (probe-file filevar)))
            (if filepath
                ;; If it's there, install it.
                (install-subschema
                  db
                  (parse-schema-from-alist (cl-json:decode-json-from-source filepath))
                  schema-version)
                ;; If it's not there, log the fact and move on.
                (log-message :info (format nil "No file found at path '~A'" filevar)))))
        ;; If the env var wasn't set, log this and move on.
        (log-message :info "Environment variable SCHEMAPATH not set."))))


;;; Extract a schema from the database

(defun make-schema-hash-table ()
  "Convenience function for repeatably creating the kind of hash-table we expect."
  (make-hash-table :test #'equal))

(defun fetch-current-schema (db)
  "Return a hash-table representing the current schema version,
  populated by definitions retrieved from the database."
  (declare (type neo4cl:neo4j-rest-server db))
  (log-message :info "Fetching the current schema from the database.")
  ;; Create a schema structure
  (let ((schema (make-schema-hash-table)))
    ;; Populate the schema with resourcetypes
    (mapcar #'(lambda (rtype)
                (log-message
                  :debug
                  (format nil "Fetching initial details for resourcetype '~A'" rtype))
                ;; Add it to the main schema hash-table
                (setf (gethash rtype schema) (describe-resource-type db rtype))
                ;; Confirm what's now in the schema hash-table
                (log-message :debug (format nil "Added schema entry ~A"
                                            (a-listify (gethash rtype schema))))
                (log-message :debug (format nil "Current state of schema:~%"))
                (maphash #'(lambda (name rtype-obj)
                             (log-message :debug
                                          (format nil "Resourcetype '~A': ~A" name (a-listify rtype-obj))))
                         schema))
            (get-resourcetype-names db))
    ;; Dump the entire schema for a point-in-time reference
    (log-message :debug (format nil "Full state of schema after loading resourcetypes::~%"))
    (maphash #'(lambda (name rtype-obj)
                 (log-message :debug
                              (format nil "Resourcetype '~A': ~A" name (a-listify rtype-obj))))
             schema)
    ;; Inject the relationships
    (log-message :debug "Fetching relationship definitions")
    (maphash #'(lambda (rtype relationships)
                 (log-message
                   :debug
                   (format nil "Setting relationships for resourcetype '~A' to '~A'"
                           rtype (if relationships (mapcar #'a-listify relationships) "NIL")))
                 (set-relationships (gethash rtype schema) relationships)
                 (log-message :debug (format nil "Schema entry is now ~A"
                                             (a-listify (gethash rtype schema)))))
             (get-relationship-definitions db schema))
    ;; Return the schema we created
    schema))

(defgeneric get-relationship-definitions (db schema)
  (:documentation "Fetch the list of relationship definitions from the database.
  Return them as a hash-table:
  key = resourcetype name
  value = list of schema-rels instances."))

(defmethod get-relationship-definitions ((db neo4cl:neo4j-rest-server)
                                         (schema hash-table))
  (let ((rels (make-hash-table :test #'equal)))
    ;; Create an entry in the relationships lookup table
    ;; for each resourcetype
    (mapcar #'(lambda (rtype)
                (setf (gethash rtype rels) ()))
            (get-resourcetype-names db))
    ;; Accumulate the relationships for each resourcetype
    (mapcar #'(lambda (rel)
                (let ((source-type (first rel))
                      (rel-name (second rel))
                      (target-name (third rel))
                      (target-type (gethash (third rel) schema))
                      (rel-cardinality (fourth rel))
                      (rel-dependent (fifth rel))
                      (rel-description (when (and (sixth rel)
                                                  (not (equal "" (sixth rel))))
                                         (sixth rel))))
                  (log-message
                    :debug
                    (format nil "Creating relationship entry for (:~A)-[:~A {cardinality: '~A', dependent: '~A', description: '~A'}]->(~A - ~A)"
                            source-type
                            rel-name rel-cardinality rel-dependent rel-description
                            target-name
                            (name target-type)))
                  (push (make-schema-rels :name rel-name
                                          :target-type target-type
                                          :cardinality rel-cardinality
                                          :dependent rel-dependent
                                          :description rel-description)
                        (gethash source-type rels))))
            (neo4cl:extract-rows-from-get-request
              (neo4cl:neo4j-transaction
                db
                `((:STATEMENTS
                    ((:STATEMENT
                       . "MATCH (:RgSchema {name: \"root\"})-[:CURRENT_VERSION]->(v:RgSchemaVersion)-[:HAS]->(s:RgResourceType)<-[:SOURCE]-(r:RgRelationship)-[:TARGET]->(t:RgResourceType) RETURN s.name, r.name, t.name, r.cardinality, r.dependent, r.description")))))))
    ;; Return the accumulated hash-table
    rels))

(defun parse-schema-from-alist (schema-alist)
  "Take the alist generated by json:decode-json-from-source
   and return an incoming-subschema-version instance."
  (make-incoming-subschema-version
    :name (cdr (assoc :NAME schema-alist))
    :resourcetypes
    (mapcar
      #'(lambda (res)
          (make-incoming-rtypes
            :name (cdr (assoc :NAME res))
            :dependent (let ((dependent (cdr (assoc :DEPENDENT res))))
                         (when (or (equal dependent t)
                                   (equal dependent "true")
                                   (equal dependent "True"))
                           t))
            :description (when (and (cdr (assoc :DESCRIPTION res))
                                    (not (equal "" (cdr (assoc :DESCRIPTION res)))))
                           (cdr (assoc :DESCRIPTION res)))
            :attributes
            (mapcar #'(lambda (attr)
                        (make-incoming-rtype-attrs
                          :name (cdr (assoc :NAME attr))
                          :description (cdr (assoc :DESCRIPTION attr))
                          :attr-values (cdr (assoc :VALUES attr))))
                    (cdr (assoc :ATTRIBUTES res)))))
      (cdr (assoc :RESOURCETYPES schema-alist)))
    :relationships
    (mapcar
      #'(lambda (rel)
          (make-incoming-rels
            :name (cdr (assoc :NAME rel))
            :source-type (cdr (assoc :SOURCE-TYPE rel))
            :target-type (cdr (assoc :TARGET-TYPE rel))
            :cardinality (cdr (assoc :CARDINALITY rel))
            :dependent (let ((dependent (cdr (assoc :DEPENDENT rel))))
                         (when (or (equal dependent t)
                                   (equal dependent "true")
                                   (equal dependent "True"))
                           t))
            :description (cdr (assoc :DESCRIPTION rel))))
      (cdr (assoc :RELATIONSHIPS schema-alist)))))

(defun install-uploaded-schema (schema db)
  (declare (type neo4cl:neo4j-rest-server db))
  "Install a schema uploaded via the API."
  (log-message :info "Processing uploaded schema.")
  (let ((content (cl-json:decode-json-from-source schema))
        (current-version (current-schema-p db)))
    (log-message :info (format nil "Received schema '~A'" (cdr (assoc :NAME content))))
    ;; Attempt to install it
    (when (install-subschema db (parse-schema-from-alist content) current-version)
      ;; Return indication of success
      t)))


(defgeneric get-relationship (db source-type relationship target-type)
  (:documentation "Extract the attributes of interest for a given relationship. Return a 'schema-rels instance.  cardinality defaults to many:many."))

(defmethod get-relationship ((db hash-table)
                             (source-type string)
                             (relationship string)
                             (target-type string))
  (log-message
    :debug
    (format nil "Retrieving the relationship ~A from ~A to ~A."
            relationship source-type target-type))
  (first
    (remove-if-not #'(lambda (rel)
                       (equal relationship (name rel)))
                   (relationships (gethash source-type db)))))

(defmethod get-relationship ((db neo4cl:neo4j-rest-server)
                             (source-type string)
                             (relationship string)
                             (target-type string))
  (log-message
    :debug
    (format nil "Retrieving the relationship ~A from ~A to ~A."
            relationship source-type target-type))
  (let ((query (format nil "MATCH (:RgSchema {name: \"root\"})-[:CURRENT_VERSION]->(v:RgSchemaVersion)-[:HAS]->(s:RgResourceType {name: \"~A\"})<-[:SOURCE]-(r:RgRelationship {name: \"~A\"})-[:TARGET]->(t:RgResourceType {name: \"~A\"}) RETURN r.name, t.name, r.cardinality, r.dependent, r.description"
                       source-type relationship target-type)))
    (log-message :debug (format nil "Checking for relationship (:~A)-[:~A]->(:~A)"
                                source-type relationship target-type))
    (log-message :debug (format nil "Using query-string '~A'" query))
    (handler-case
      ;; Convert to schema-rel instances
      (mapcar #'(lambda (rel)
                  (make-schema-rels :name (first rel)
                                    :target-type (second rel)
                                    :cardinality (third rel)
                                    :dependent (fourth rel)
                                    :description (fifth rel)))
              (neo4cl:extract-data-from-get-request
                (neo4cl:neo4j-transaction db `((:STATEMENTS ((:STATEMENT . ,query)))))))
      (error (e)
             (if (typep e 'neo4cl:client-error)
               (log-message :fatal (format nil "Neo4j error ~A ~A - ~A"
                                           (neo4cl:category e)
                                           (neo4cl:title e)
                                           (neo4cl:message e)))
               (progn
                 (log-message :fatal (format nil "Unhandled error '~A'" e))))))))

(defgeneric describe-resource-type (schema-db resourcetype )
  (:documentation "Return the description of a resource-type, as a schema-rtypes instance."))

(defmethod describe-resource-type ((schema-db neo4cl:neo4j-rest-server)
                                   (resourcetype string))
  (declare (type (or hash-table neo4cl:neo4j-rest-server) schema-db))
  (log-message :debug (format nil "Describing resource-type '~A'" resourcetype))
  ;; Confirm whether this resourcetype exists at all.
  (let ((resource (get-resourcetype-definition schema-db resourcetype)))
    ;; If it doesn't, automatically return NIL.
    (when resource
      (set-attributes resource (get-resourcetype-attributes schema-db resourcetype)))
    ;; Return the resource object itself
    resource))


(defgeneric get-resourcetype-definition (db resourcetype)
  (:documentation "Fetch a resourcetype's structure. Return a stub schema-rtypes instance, with nulls for attributes and relationships."))

(defmethod get-resourcetype-definition ((db neo4cl:neo4j-rest-server)
                                        (resourcetype string))
  (log-message :debug
               (format nil "Checking for existence of resourcetype '~A'" resourcetype))
  (handler-case
    (let ((rtype
            (car
              (neo4cl:extract-rows-from-get-request
                (neo4cl:neo4j-transaction
                  db
                  `((:STATEMENTS
                      ((:STATEMENT
                         . ,(format nil "MATCH (:RgSchema {name: \"root\"})-[:CURRENT_VERSION]->(v:RgSchemaVersion)-[:HAS]->(s:RgResourceType {name: \"~A\"}) RETURN s.dependent AS dependent, s.description AS description"
                                    resourcetype))))))))))
      ;; Convert to schema-rtypes instances
      (make-schema-rtypes :name resourcetype
                          :dependent (first rtype)
                          ;; Convert to boolean:
                          :description (second rtype)
                          :relationships ()
                          :attributes ()))
    (error (e)
           (if (typep e 'neo4cl:client-error)
               (log-message :fatal (format nil "Neo4j error ~A ~A - ~A"
                                           (neo4cl:category e)
                                           (neo4cl:title e)
                                           (neo4cl:message e)))
               (progn
                 (log-message :fatal (format nil "Unhandled error '~A'" e)))))))


(defgeneric get-resourcetype-attributes (db resourcetype)
  (:documentation "Extract the attributes from resource definitions from the database, and return them as a list of schema-rtype-attrs structs."))
(defmethod get-resourcetype-attributes ((db neo4cl:neo4j-rest-server)
                                        (resourcetype string))
  (log-message :debug (format nil "Getting attributes for resourcetype '~A'" resourcetype))
  (mapcar #'(lambda (attr)
              (make-schema-rtype-attrs :name (first attr)
                                       :description (second attr)
                                       :attr-values (cl-ppcre:split "," (third attr))))
          (neo4cl:extract-rows-from-get-request
            (neo4cl:neo4j-transaction
              db
              `((:STATEMENTS
                  ((:STATEMENT
                     . ,(format nil "MATCH (:RgSchema {name: \"root\"})-[:CURRENT_VERSION]->(v:RgSchemaVersion)-[:HAS]->(:RgResourceType {name: \"~A\"})-[:HAS]->(a:RgResourceTypeAttribute) RETURN a.name, a.description, a.values"
                                resourcetype)))))))))

(defmethod get-resourcetype-relationships ((db neo4cl:neo4j-rest-server)
                                           (resourcetype string))
  (mapcar #'(lambda (rel)
              (make-schema-rels :name (first rel)
                                :target-type (second rel)
                                :cardinality (third rel)
                                :dependent (fourth rel)
                                :description (fifth rel)))
          (neo4cl:extract-rows-from-get-request
            (neo4cl:neo4j-transaction
              db
              `((:STATEMENTS
                  ((:STATEMENT
                     . ,(format nil "MATCH (:RgSchema {name: \"root\"})-[:CURRENT_VERSION]->(v:RgSchemaVersion)-[:HAS]->(:RgResourceType {name: \"~A\"})<-[:SOURCE]-(r:RgRelationship)-[:TARGET]->(t:RgResourceType) RETURN r.name, t.name, r.cardinality, t.dependent, t.description"
                                resourcetype)))))))))

(defgeneric get-resourcetype-names (db)
  (:documentation "Return the names of resourcetypes, as a list of strings."))

(defmethod get-resourcetype-names ((db neo4cl:neo4j-rest-server))
  (log-message :debug "Fetching resourcetype names.")
  (mapcar #'car
          (neo4cl:extract-rows-from-get-request
            (neo4cl:neo4j-transaction
              db
              `((:STATEMENTS
                  ((:STATEMENT
                     . ,(format nil "MATCH (:RgSchema {name: \"root\"})-[:CURRENT_VERSION]->(:RgSchemaVersion)-[:HAS]->(r:RgResourceType) RETURN r.name")))))))))

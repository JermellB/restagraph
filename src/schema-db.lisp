;   Copyright 2021-2022 James Fleming <james@electronic-quill.net>
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

(defun current-schema-version (session)
  "Return an integer datestamp, being the ID of the current version.
  Useful for testing whether there's a current schema in place."
  (declare (type neo4cl:bolt-session session))
  (cdr (assoc "createddate"
              (car
                (neo4cl:bolt-transaction-autocommit
                  session
                  "MATCH (c:RgSchema { name: 'root' })-[:CURRENT_VERSION]->(v:RgSchemaVersion) RETURN v.createddate AS createddate"))
              :test #'equal)))

(defun list-schema-versions (session)
  "Fetch a list of schema versions, plus the current version.
  Return an alist, whose keys are keywords, for ease of comparison:
  :VERSIONS -> list of integers, those being datestamp IDs of versions in the database.
  :CURRENT-VERSION -> integer, this being the datestamp ID of the current version."
  (declare (type neo4cl:bolt-session session))
  `((:VERSIONS
      . ,(sort
           (mapcar #'(lambda (row)
                       (cdr (assoc "version" row :test #'equal)))
                   (neo4cl:bolt-transaction-autocommit
                     session
                     "MATCH (c:RgSchema { name: 'root' })-[:VERSION]->(v:RgSchemaVersion) RETURN v.createddate AS version"))
           #'<))
    (:CURRENT-VERSION . ,(current-schema-version session))))

(defun set-current-schema-version (session new-version)
  (declare (type neo4cl:bolt-session session)
           (type integer new-version))
  "Update the current schema version"
  (let ((versions (list-schema-versions session)))
    ;; Do we have that version in the database?
    (if (member new-version (cdr (assoc :VERSIONS versions)))
        ;; It's there. Is it already the current version?
        (if (equal new-version (cdr (assoc :CURRENT-VERSION versions)))
          ;; Yes, it is. Do nothing.
          (log-message :debug "Too lazy to set the current version to itself.")
          ;; Not the current version, but valid. Update it
          ;; FIXME: wrap this in an explicit transaction
          (progn
            ;; Delete the old current-version
            (neo4cl:bolt-transaction-autocommit
              session
              "MATCH (:RgSchema { name: 'root' })-[r:CURRENT_VERSION]->(:RgSchemaVersion) DELETE r")
            ;; Set the new one
            (neo4cl:bolt-transaction-autocommit
              session
              (format nil "MATCH (r:RgSchema { name: 'root' })-[:VERSION]->(v:RgSchemaVersion { createddate: ~D }) CREATE (r)-[:CURRENT_VERSION]->(v)" new-version))))
        ;; No such version
        (error "No such schema version"))))

(defun delete-schema-version (session version)
  "Remove a schema version from the database"
  (declare (type neo4cl:bolt-session session)
           (type integer version))
  (log-message :info (format nil "Attempting to delete schema version ~D from the database" version))
  ;; Pre-flight checks
  (let* ((versions (list-schema-versions session))
         ;; Extract these as a separate variable to reduce tedious repetition
         (current-version (cdr (assoc :CURRENT-VERSION versions)))
         (all-versions (cdr (assoc :VERSIONS versions))))
    ;; Sanity check: do we have that version in the database?
    (log-message :debug (format nil "Checking whether version ~D exists in the database." version))
    (unless (and all-versions
                 (member version all-versions))
      (progn
        (log-message
          :error
          (format nil "There is no schema with that version identifier '~D'." version))
        (error 'client-error :message "There is no schema with that version identifier.")))
    ;; Have we been asked to delete the current version?
    (log-message :debug "Checking whether we're trying to delete the current version.")
    (log-message
      :debug
      (format nil "Current version: ~D. Available versions: ~{~D~^, ~}." current-version all-versions))
    (if (and current-version
             (equal version current-version))
      ;; If so, set the current version to be the newest remaining version
      (let* ((available-versions (remove-if #'(lambda (candidate) (equal version candidate))
                                            all-versions))
             (new-current-version (when (> (length available-versions) 0)
                                    (apply #'max available-versions))))
        (log-message :debug "Requested version is the current one. Determining new current version.")
        (if new-current-version
          (progn
            (log-message
              :debug
              (format nil "Setting current version to ~D."
                      new-current-version))
            (set-current-schema-version session new-current-version))
          (log-message :warning "There is no previous version. Removing the only remaining schema version.")))
      ;; Not trying to delete the current version. Log the fact for operator reassurance, and carry on.
      (log-message
        :debug
        (format nil "Requested version ~D is not current version ~D. Taking no action regarding current-version."
                version current-version))))
  ;; If we've made it this far, we're good to go.
  (log-message :debug (format nil "Deleting relationships in schema version ~A" version))
  ;;FIXME: wrap these in an explicit transaction
  (neo4cl:bolt-transaction-autocommit
    session
    (format
      nil
      "MATCH (:RgSchema { name: 'root' })-[:VERSION]->(:RgSchemaVersion { createddate: ~D })-[:HAS]->(:RgResourceType)<-[:SOURCE]-(r:RgRelationship) DETACH DELETE r"
      version))
  (log-message :debug (format nil "Deleting resourcetype attributes in schema version ~A" version))
  (neo4cl:bolt-transaction-autocommit
    session
    (format
      nil
      "MATCH (:RgSchema { name: 'root' })-[:VERSION]->(:RgSchemaVersion { createddate: ~D })-[:HAS]->(:RgResourceType)-[:HAS]->(a:RgResourceTypeAttribute) DETACH DELETE a"
      version))
  (log-message :debug (format nil "Deleting resourcetypes in schema version ~A" version))
  (neo4cl:bolt-transaction-autocommit
    session
    (format
      nil
      "MATCH (:RgSchema { name: 'root' })-[:VERSION]->(:RgSchemaVersion { createddate: ~D })-[:HAS]->(t:RgResourceType) DETACH DELETE t"
      version))
  (log-message :debug (format nil "Deleting version identifier for schema version ~A" version))
  (neo4cl:bolt-transaction-autocommit
    session
    (format
      nil
      "MATCH (:RgSchema { name: 'root' })-[:VERSION]->(v:RgSchemaVersion { createddate: ~D }) DETACH DELETE v"
      version)))

(defgeneric uniqueness-constraint-query (version label attribute)
  (:documentation "Generate the applicable uniqueness constraint query for this version of Neo4j."))

(defmethod uniqueness-constraint-query ((version neo4cl:neo4j-4-3-9)
                                        (label string)
                                        (attribute string))
  (format nil "CREATE CONSTRAINT unique_~A_~A IF NOT EXISTS ON (r:~A) ASSERT r.~A IS UNIQUE"
          label attribute label attribute))

(defun ensure-uniqueness-constraint (session label attribute)
  "Ensure Neo4j has a uniqueness constraint on the specified attribute/label combination."
  (declare (type neo4cl:bolt-session session)
           (type string label)
           (type string attribute))
  (neo4cl:bolt-transaction-autocommit
    session
    (uniqueness-constraint-query (neo4cl:neo4j-version session) label attribute)))

(defun create-new-schema-version (session)
  "Create a new schema version, and return its createddate as an integer, giving its version identifier."
  (declare (type neo4cl:bolt-session))
  (restagraph::log-message :info "Attempting to create new schema version")
  (restagraph::ensure-schema-root-exists session)
  ;; Create the version.
  ;; Confirm it by grabbing its returned createddate value as the version ID.
  (let ((previous-version (current-schema-version session))
        (version
          (cdr
            (assoc
              "createddate"
              (car
                (neo4cl:bolt-transaction-autocommit
                  session
                  (format
                    nil
                    "MATCH (r:RgSchema {name: 'root'}) CREATE (r)-[:VERSION]->(v:RgSchemaVersion { createddate: ~D }), (r)-[:CURRENT_VERSION]->(v) RETURN v.createddate AS createddate"
                    (get-universal-time))))
              :test #'equal))))
    ;; If there was a previous version, remove the previous CURRENT_VERSION link.
    ;; There won't be one if this is a brand new database.
    (when previous-version
      (neo4cl:bolt-transaction-autocommit
        session
        (format
          nil
          "MATCH (:RgSchema {name: 'root'})-[r:CURRENT_VERSION]->(v:RgSchemaVersion) WHERE v.createddate <> ~A DELETE r"
          version)))
    ;; Return the new version ID
    version))

(defun install-default-resources (session)
  "Install a default set of resources, such as the admin user."
  (declare (type neo4cl:bolt-session session))
  ;; Admin user
  (neo4cl:bolt-transaction-autocommit
    session
    (format
      nil
      "MERGE (a:People {uid: 'RgAdmin', original_uid: 'RgAdmin'}) ON CREATE SET a.createddate = ~D RETURN a.uid"
      (get-universal-time)))
  ;; Default pronouns
  (mapcar #'(lambda (pronounset)
              (neo4cl:bolt-transaction-autocommit
                session
                (format
                  nil
                  "MERGE (p:Pronouns {uid: '~A', text: '~A'}) ON CREATE SET p.createddate = ~D RETURN p.uid"
                  (car pronounset)
                  (cdr pronounset)
                  (get-universal-time))))
          '(("they_them" . "They/them")
            ("she_her" ."She/her")
            ("he_him" . "He/him")))
  ;; Explicitly return something
  t)

(defgeneric ensure-schema-root-exists (session)
  (:documentation "Standalone function to ensure there _is_ a root element for the schema."))

(defmethod ensure-schema-root-exists ((session neo4cl:bolt-session))
  (restagraph::log-message :debug "Ensuring a root node exists for the schema.")
  (neo4cl:bolt-transaction-autocommit
    session
    (format
      nil
      "MERGE (s:RgSchema {name: 'root'}) ON CREATE SET s.createddate = ~D RETURN s.name"
      (get-universal-time))))

(defun ensure-current-schema (session subschema)
  "Ensure there's a current schema in place, complete with uniqueness constraints.
  Return the timestamp of the current schema's created date, as its version identifier."
  (declare (type neo4cl:bolt-session session)
           (type incoming-subschema-version subschema))
  (or (current-schema-version session)
      (let ((version
              ;; No current schema found; install one.
              (progn
                ;; Ensure there's a schema root.
                (ensure-schema-root-exists session)
                ;; Add a current version.
                (create-new-schema-version session))))
        ;; Install the core schema
        (install-subschema session subschema version)
        ;; Install the default resources
        (install-default-resources session)
        ;; Install an additional schema, if specified and present
        (install-additional-schema session version)
        version)))

(defun install-subschema-resourcetype (session rtype schema-version)
  "Install a single resourcetype definition into the database."
  (declare (type neo4cl:bolt-session session)
           (type incoming-rtypes rtype)
           (type integer schema-version))
  (log-message :debug (format nil "Attempting to install schema definition for resourcetype '~A'"
                              (name rtype)))
  (let* ((schema-base (format nil "MATCH (r:RgSchema {name: 'root'})-[:VERSION]->(v:RgSchemaVersion { createddate: ~D })"
                              schema-version))
         (extant-resource (describe-resource-type session (name rtype) schema-version)))
    ;; This resourcetype doesn't already exist.
    ;; Create it, and return nulls for attributes and relationships
    (unless extant-resource
      (let ((query
              (format nil "~A CREATE (v)-[:HAS]->(t:RgResourceType {name: $name, dependent: $dependent, description: $description})"
                      schema-base))
            ;; Set the parameters, if applicable
            (parameters `(("name" . ,(name rtype))
                          ("dependent" . ,(if (dependent rtype) :true :false))
                          ("description" . ,(if (and (description rtype)
                                                     (not (equal "" (description rtype))))
                                                (description rtype)
                                                :null)))))
        (log-message :debug (format nil "Resourcetype doesn't exist. Attempting to create with query: ~A" query))
        (log-message :debug (format nil "Using parameters ~A" parameters))
        ;; Add the resourcetype
        (neo4cl:bolt-transaction-autocommit session query :parameters parameters)
        ;; Ensure we have a uniqueness constraint for that resourcetype
        (handler-case
          (ensure-uniqueness-constraint session (name rtype) "name")
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
                            (neo4cl:message e)))))))
        ;; Return the null list of attributes
        '()))
    ;; Identify any attributes not already present, and add them.
    (log-message :debug "Checking for any new attributes to install")
    ;; If this resourcetype already exists, enumerate the names of its existing attributes
    (let* ((old-attr-names (when extant-resource (map 'list #'name (attributes extant-resource))))
           ;; Now derive the list of attributes to add.
           (new-attrs
             ;; If the resourcetype already exists, disregard any new attributes with the same name
             ;; as any attributes it already has.
             (if extant-resource
                 ;; Bear in mind these are instances of 'incoming-rtype-attrs subclasses
                 (remove-if #'(lambda (attr) (member (name attr) old-attr-names :test #'equal))
                            (attributes rtype))
                 ;; If this is a new resourcetype, _all_ its attributes are new, so the process is simpler.
                 (attributes rtype))))
      (if old-attr-names
          (log-message :debug (format nil "Found existing attributes for ~A: ~{~A~^, ~}"
                                      rtype old-attr-names))
          (log-message :debug (format nil "No existing attributes found for resourcetype ~A." rtype)))
      ;; Install any new attributes that are required
      (if new-attrs
          (progn
            (log-message :debug (format nil "New attributes to install for resourcetype ~A: ~{~A~^, ~}."
                                        rtype (mapcar #'name new-attrs)))
            (mapcar #'(lambda (attr)
                        ;; Fetch any extra parameters for this attribute-type
                        (let* ((extra-attr-params (extra-attr-params attr))
                               (param-names (append '("name" "description" "readonly")
                                                    (mapcar #'car extra-attr-params)))
                               (query (format nil "~A-[:HAS]->(t:RgResourceType {name: '~A'}) CREATE (t)-[:HAS]->(:RgResourceTypeAttribute { ~{~A: $~:*~A~^, ~} })"
                                              schema-base
                                              (name rtype)
                                              param-names))
                               (params (append
                                         `(("name" . ,(name attr))
                                           ("description" . ,(or (description attr) :null))
                                           ("readonly" . ,(if (readonly attr) :true :false)))
                                         extra-attr-params)))
                          (log-message
                            :debug
                            (format nil "Installing resourcetype-attribute definitions with query: '~A' and params ~A"
                                    query params))
                          (handler-case
                            (neo4cl:bolt-transaction-autocommit session query :parameters params)
                            (neo4cl:transient-error (e) (log-message :fatal (format nil "Neo4j error ~A ~A - ~A"
                                                                                    (neo4cl:category e)
                                                                                    (neo4cl:title e)
                                                                                    (neo4cl:message e))))
                            (neo4cl:database-error (e) (log-message :fatal (format nil "Neo4j error ~A ~A - ~A"
                                                                                   (neo4cl:category e)
                                                                                   (neo4cl:title e)
                                                                                   (neo4cl:message e))))
                            (neo4cl:client-error (e) (log-message :fatal (format nil "Neo4j error ~A ~A - ~A"
                                                                                 (neo4cl:category e)
                                                                                 (neo4cl:title e)
                                                                                 (neo4cl:message e))))
                            (error (e) (log-message :fatal (format nil "Unhandled error '~A'" e))))))
                    new-attrs))
          ;; No new attributes to add to this resourcetype
          (log-message :debug (format nil "No new attributes found to add for resourcetype ~A. Moving on."
                                      (name rtype)))))))

(defun install-subschema-relationship (session rel schema-version)
  "Install a single relationship definition into the database."
  (declare (type neo4cl:bolt-session session)
           (type incoming-rels rel)
           (type integer schema-version))
  (log-message :debug (format nil "Attempting to create relationship (:~A)-[:~A]->(~A)"
                              (source-type rel) (name rel) (target-type rel)))
  ;; Only create this relationship if it doesn't already exist
  (if (get-relationship session (source-type rel) (name rel) (target-type rel))
    (log-message :debug (format nil "Refusing to create duplicate relationship (:~A)-[:~A]->(~A)"
                                (source-type rel) (name rel) (target-type rel)))
    (let ((params `(("schemaversion" . ,schema-version)
                    ("sourcetype" . ,(source-type rel))
                    ("targettype" . ,(target-type rel))
                    ("relname" . ,(name rel))
                    ("reltype" . ,(reltype rel))
                    ("description" . ,(or (description rel) :null))
                    ("cardinality" . ,(cardinality rel))))
          (query-string
            ;; Special-case code for self-relationships.
            ;; If the source-type and target-type are the same resource, Cypher will
            ;; refuse to make two separate references to it.
            (if (equal (source-type rel) (target-type rel))
              "MATCH (r:RgSchema {name: 'root'})-[:VERSION]->(v:RgSchemaVersion { createddate: $schemaversion })-[:HAS]->(s:RgResourceType {name: $sourcetype})
              CREATE (s)<-[:SOURCE]-(:RgRelationship {name: $relname, reltype: $reltype, description: $description, cardinality: $cardinality})-[:TARGET]->(s)"
              ;; Normal case where the source and target types are different
              "MATCH (r:RgSchema {name: 'root'})-[:VERSION]->(v:RgSchemaVersion { createddate: $schemaversion })-[:HAS]->(s:RgResourceType {name: $sourcetype}), (v)-[:HAS]->(t:RgResourceType {name: $targettype})
              CREATE (s)<-[:SOURCE]-(:RgRelationship {name: $relname, reltype: $reltype, description: $description, cardinality: $cardinality})-[:TARGET]->(t)")))
              ;; Debug logging, just in case
              (log-message :debug (format nil "Installing relationship definition with this query:~%~A" query-string))
              (log-message :debug (format nil "Using these parameters:~%~A" params))
              ;; Fire ze missiles!
              (handler-case
                (neo4cl:bolt-transaction-autocommit session query-string :parameters params)
                (neo4cl:client-error (e) (format nil "Neo4J client error ~A/~A - ~A"
                                                 (neo4cl:category e) (neo4cl:title e) (neo4cl:message e)))
                (neo4cl:transient-error (e) (log-message
                                              :error
                                              (format nil "Neo4J transient error ~A/~A - ~A"
                                                      (neo4cl:category e) (neo4cl:title e) (neo4cl:message e))))
                (neo4cl:database-error (e) (log-message
                                             :error
                                             (format nil "Neo4J database error ~A/~A - ~A"
                                                     (neo4cl:category e) (neo4cl:title e) (neo4cl:message e))))
                (error (e) (log-message :fatal (format nil "Unhandled error: ~A" e)))))))

(defun install-subschema (session subschema schema-version)
  "New attributes will be added as an augmentation to existing resourcetypes
  and relationships, but existing definitions will not be changed."
  (declare (type neo4cl:bolt-session session)
           (type incoming-subschema-version subschema)
           (type integer schema-version))
  ;; Install the core-schema to the schema root.
  (log-message :info (format nil "Installing subschema '~A' to schema version ~D."
                             (name subschema)
                             schema-version))
  ;; Install the resourcetypes
  (mapcar
    #'(lambda (rtype)
        ;; Install the resourcetypes and their attributes
        ;; Fetch the attributes as a side-effect of ensuring the resourcetype's existence.
        (install-subschema-resourcetype session rtype schema-version))
    (resourcetypes subschema))
  ;; Now install the relationships
  (log-message :info (format nil "Installing relationships for subschema '~A'"
                             (name subschema)))
  (mapcar
    #'(lambda (rel) (install-subschema-relationship session rel schema-version))
    (relationships subschema)))

(defun install-additional-schema (session schema-version)
  "Install an additional schema from the filesystem."
  (declare (type neo4cl:bolt-session session)
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
                  session
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

(defun fetch-current-schema (session)
  "Return a hash-table representing the current schema version,
  populated by definitions retrieved from the database.
  Keys = resourcetype names.
  Values = schema-rtypes instances."
  (declare (type neo4cl:bolt-session session))
  (log-message :info "Fetching the current schema from the database.")
  ;; Create a schema structure
  (let ((schema (make-schema-hash-table))
        (current-version (current-schema-version session)))
    ;; Populate the schema with resourcetypes
    (mapcar #'(lambda (rtype)
                (log-message
                  :debug
                  (format nil "Fetching initial details for resourcetype '~A'" rtype))
                ;; Add it to the main schema hash-table
                (setf (gethash rtype schema) (describe-resource-type session rtype current-version))
                ;; Confirm what's now in the schema hash-table
                (log-message :debug (format nil "Added schema entry ~A"
                                            (a-listify (gethash rtype schema))))
                ;; This logging is disabled by default
                ;; because it drowns out other useful information with noise
                ;(log-message :debug (format nil "Current state of schema:~%"))
                #+(or)
                (maphash #'(lambda (name rtype-obj)
                             (log-message :debug
                                          (format nil "Resourcetype '~A': ~A" name (a-listify rtype-obj))))
                         schema))
            (get-resourcetype-names session current-version))
    ;; Dump the entire schema for a point-in-time reference
                ;; This logging is disabled by default
                ;; because it drowns out other useful information with noise
    ;(log-message :debug (format nil "Full state of schema after loading resourcetypes:~%"))
    #+(or)
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
             (inject-relationship-definitions session schema current-version))
    ;; Return the schema we created
    schema))

;; FIXME: at what point in proceedings should this be used?!
;; We're assuming the `schema` hash-table has a definitive list of resourcetypes,
;; but not of relationships.
(defgeneric inject-relationship-definitions (session schema version)
  (:documentation "Fetch the list of relationship definitions from the database,
                   and inject them into the supplied schema hash-table.
                   Intended to be used in the course of extracing a schema from the database."))

(defmethod inject-relationship-definitions ((session neo4cl:bolt-session)
                                            (schema hash-table)
                                            (version integer))
  (let ((rels (make-hash-table :test #'equal)))
    ;; Create an entry in the relationships lookup table
    ;; for each resourcetype
    (mapcar #'(lambda (rtype)
                (setf (gethash rtype rels) ()))
            (get-resourcetype-names session version))
    ;; Accumulate the relationships for each resourcetype
    (mapcar #'(lambda (rel)
                (log-message :debug (format nil "Accumulating details for this relationship: ~A" rel))
                (let* ((source-type (cdr (assoc "sourcetype" rel :test #'equal)))
                       (rel-name (cdr (assoc "relname" rel :test #'equal)))
                       (target-name (cdr (assoc "targettype" rel :test #'equal)))
                       (target-type (gethash target-name schema))
                       (rel-cardinality (cdr (assoc "cardinality" rel :test #'equal)))
                       (rel-type (cdr (assoc "reltype" rel :test #'equal)))
                       (description-value (cdr (assoc "description" rel :test #'equal)))
                       (rel-description (when (and description-value
                                                   (not (equal "" description-value)))
                                          description-value)))
                  (log-message
                    :debug
                    (format nil "Creating relationship entry for (:~A)-[:~A {cardinality: '~A', reltype: '~A', description: '~A'}]->(:~A)"
                            source-type
                            rel-name
                            rel-cardinality
                            rel-type
                            rel-description
                            target-name))
                  (push (make-schema-rels :name rel-name
                                          :target-type target-type
                                          :cardinality rel-cardinality
                                          :reltype rel-type
                                          :description rel-description)
                        (gethash source-type rels))))
            (neo4cl:bolt-transaction-autocommit
              session
              "MATCH (:RgSchema {name: 'root'})-[:VERSION]->(v:RgSchemaVersion {createddate: $version})-[:HAS]->(s:RgResourceType)<-[:SOURCE]-(r:RgRelationship)-[:TARGET]->(t:RgResourceType) RETURN s.name AS sourcetype, r.name AS relname, t.name AS targettype, r.cardinality AS cardinality, r.reltype AS reltype, r.description AS description"
              :parameters `(("version" . ,version))))
    ;; Return the accumulated hash-table
    rels))

(defun parse-schema-from-alist (schema-alist)
  "Take the alist generated by json:decode-json-from-source
  and return an incoming-subschema-version instance."
  (log-message :debug "Parsing schema from alist")
  (make-incoming-subschema-version
    :name (cdr (assoc :NAME schema-alist))
    :resourcetypes
    (mapcar
      #'(lambda (res)
          (log-message :debug (format nil "Processing resourcetype definition as follows: ~A" res))
          (make-incoming-rtypes
            :name (cdr (assoc :NAME res))
            :dependent (when (member (cdr (assoc :DEPENDENT res))
                                     '(t "true" "True")
                                     :test #'equal)
                         t)
            :description (let ((candidate (cdr (assoc :DESCRIPTION res))))
                           (when (and candidate
                                      (stringp candidate)
                                      (not (equal "" candidate)))
                             candidate))
            :attributes (mapcar #'(lambda (attr) (make-incoming-rtype-attrs attr))
                                (cdr (assoc :ATTRIBUTES res)))))
      (cdr (assoc :RESOURCETYPES schema-alist)))
    :relationships
    (mapcar
      #'(lambda (rel)
          (make-incoming-rels
            :name (cdr (assoc :NAME rel))
            :source-type (cdr (assoc :SOURCE-TYPE rel))
            :target-type (cdr (assoc :TARGET-TYPE rel))
            :cardinality (or (cdr (assoc :CARDINALITY rel))
                             "many:many")
            ;; Default to reltype "any"
            :reltype (or (cdr (assoc :RELTYPE rel))
                         "any")
            :description (cdr (assoc :DESCRIPTION rel))))
      (cdr (assoc :RELATIONSHIPS schema-alist)))))

(defun install-uploaded-schema (schema session)
  (declare (type neo4cl:bolt-session session))
  "Install a schema uploaded via the API."
  (log-message :info "Processing uploaded schema.")
  (log-message :info (format nil "Received schema '~A'" (cdr (assoc :NAME schema))))
  ;; Attempt to install it
  (when (install-subschema session (parse-schema-from-alist schema) (current-schema-version session))
    ;; Return indication of success
    t))


(defgeneric get-relationship (db source-type relationship target-type)
  (:documentation "Extract the attributes of interest for a given relationship.
                  Return a 'schema-rels instance if querying a hash-table,
                  or an `incoming-rels type if querying a database.
                  Cardinality defaults to many:many."))

(defmethod get-relationship ((db hash-table)
                             (source-type string)
                             (relationship string)
                             (target-type string))
  (log-message
    :debug
    (format nil "Retrieving the relationship ~A from ~A to ~A."
            relationship source-type target-type))
  ;; First, try to fetch the definition of the source-type
  (let ((sourcetype-def (gethash source-type db)))
    ;; *If* we have the source-type, search through its relationships for this one.
    (when sourcetype-def
      (find-if #'(lambda (rel)
                   (and (equal relationship (name rel))
                        (equal target-type (name (target-type rel)))))
               (relationships sourcetype-def)))))

(defmethod get-relationship ((db neo4cl:bolt-session)
                             (source-type string)
                             (relationship string)
                             (target-type string))
  (log-message
    :debug
    (format nil "Retrieving the relationship ~A from ~A to ~A."
            relationship source-type target-type))
  (let ((query (format nil "MATCH (:RgSchema {name: 'root'})-[:CURRENT_VERSION]->(v:RgSchemaVersion)-[:HAS]->(s:RgResourceType {name: '~A'})<-[:SOURCE]-(r:RgRelationship {name: '~A'})-[:TARGET]->(t:RgResourceType {name: '~A'}) RETURN r.cardinality AS cardinality, r.reltype AS reltype, r.description AS description"
                       source-type relationship target-type)))
    (log-message :debug (format nil "Checking for relationship (:~A)-[:~A]->(:~A)"
                                source-type relationship target-type))
    (log-message :debug (format nil "Using query-string '~A'" query))
    (handler-case
      ;; Convert to schema-rel instances
      (mapcar #'(lambda (rel)
                  (make-incoming-rels :name relationship
                                      :source-type source-type
                                      :target-type target-type
                                      :cardinality (cdr (assoc "cardinality" rel :test #'equal))
                                      :reltype (cdr (assoc "reltype" rel :test #'equal))
                                      :description (cdr (assoc "description" rel :test #'equal))))
              (neo4cl:bolt-transaction-autocommit db query))
      (error (e)
             (if (typep e 'neo4cl:client-error)
               (log-message :fatal (format nil "Neo4j error ~A ~A - ~A"
                                           (neo4cl:category e)
                                           (neo4cl:title e)
                                           (neo4cl:message e)))
               (progn
                 (log-message :fatal (format nil "Unhandled error '~A'" e))))))))

(defgeneric describe-resource-type (schema-db resourcetype schema-version)
  (:documentation "Return the description of a resource-type, as a schema-rtypes instance."))

(defmethod describe-resource-type ((schema-db neo4cl:bolt-session)
                                   (resourcetype string)
                                   (schema-version integer))
  (log-message :debug (format nil "Describing resource-type '~A'" resourcetype))
  ;; Confirm whether this resourcetype exists at all.
  (let ((resource (get-resourcetype-definition schema-db resourcetype schema-version)))
    ;; If it doesn't, automatically return NIL.
    (when resource
      (set-attributes resource (get-resourcetype-attributes schema-db resourcetype schema-version)))
    ;; Return the resource object itself
    resource))


(defgeneric get-resourcetype-definition (db resourcetype schema-version)
  (:documentation "Fetch a resourcetype's structure. Return a stub schema-rtypes instance, with nulls for attributes and relationships."))

(defmethod get-resourcetype-definition ((db neo4cl:bolt-session)
                                        (resourcetype string)
                                        (schema-version integer))
  (log-message :debug
               (format nil "Checking for existence of resourcetype '~A'" resourcetype))
  (handler-case
    (let ((rtype
            (car
              (neo4cl:bolt-transaction-autocommit
                db
                "MATCH (:RgSchema {name: 'root'})-[:VERSION]->(v:RgSchemaVersion { createddate: $version })-[:HAS]->(s:RgResourceType {name: $rtypename}) RETURN s.dependent AS dependent, s.description AS description"
                :parameters `(("version" . ,schema-version)
                              ("rtypename" . ,resourcetype))))))
      ;; Convert to schema-rtypes instances
      (when rtype
        (make-schema-rtypes :name resourcetype
                            :dependent (cdr (assoc "dependent" rtype :test #'equal))
                            ;; Convert to boolean:
                            :description (cdr (assoc "description" rtype :test #'equal))
                            :relationships ()
                            :attributes ())))
    (error (e)
           (cond
             ((typep e 'neo4cl:client-error)
              (log-message :fatal (format nil "Neo4j client error ~A ~A - ~A"
                                          (neo4cl:category e)
                                          (neo4cl:title e)
                                          (neo4cl:message e))))
             ((typep e 'neo4cl:bolt-error)
              (log-message :fatal (format nil "Neo4j Bolt error ~A - ~A"
                                          (neo4cl:category e)
                                          (neo4cl:message e))))
             (t
              (log-message :fatal (format nil "Unhandled error '~A'" e)))))))


(defgeneric get-resourcetype-attributes (db resourcetype schema-version)
  (:documentation "Extract the attributes from resource definitions from the database, and return them as a list of schema-rtype-attrs structs."))

(defmethod get-resourcetype-attributes ((db neo4cl:bolt-session)
                                        (resourcetype string)
                                        (schema-version integer))
  (log-message :debug (format nil "Getting attributes for resourcetype '~A'" resourcetype))
  ;; Fetch the name and type for each attribute
  (mapcar #'(lambda (attr)
              (make-schema-rtype-attrs
                (cdr (assoc "a" attr :test #'equal))))
          (neo4cl:bolt-transaction-autocommit
            db
            (format
              nil
              "MATCH (:RgSchema {name: 'root'})-[:VERSION]->(v:RgSchemaVersion {createddate: $version})-[:HAS]->(:RgResourceType {name: $rtypename})-[:HAS]->(a:RgResourceTypeAttribute) RETURN a")
            :parameters `(("version" . ,schema-version)
                          ("rtypename" . ,resourcetype)))))


(defgeneric get-resourcetype-relationships (db resourcetype)
  (:documentation "Extract the relationships for a resourcetype from the database, and return them as a list of alists."))

(defmethod get-resourcetype-relationships ((db neo4cl:bolt-session)
                                           (resourcetype string))
  (mapcar #'(lambda (rel)
              (make-schema-rels :name (cdr (assoc "relname" rel :test #'equal))
                                :target-type (cdr (assoc "targettype" rel :test #'equal))
                                :cardinality (cdr (assoc "cardinality" rel :test #'equal))
                                :reltype (cdr (assoc "reltype" rel :test #'equal))
                                :description (cdr (assoc "description" rel :test #'equal))))
          (neo4cl:bolt-transaction-autocommit
            db
            (format
              nil
              "MATCH (:RgSchema {name: 'root'})-[:CURRENT_VERSION]->(v:RgSchemaVersion)-[:HAS]->(:RgResourceType {name: '~A'})<-[:SOURCE]-(r:RgRelationship)-[:TARGET]->(t:RgResourceType) RETURN r.name AS relname, t.name AS targettype, r.cardinality AS cardinality, t.reltype AS reltype, t.description AS description"
              resourcetype))))

(defgeneric get-resourcetype-names (db schema-version)
  (:documentation "Return the names of resourcetypes, as a list of strings."))

(defmethod get-resourcetype-names ((db neo4cl:bolt-session)
                                   (schema-version integer))
  (log-message :debug "Fetching resourcetype names.")
  (mapcar #'(lambda (rtype)
              (cdr (assoc "name" rtype :test #'equal)))
          (neo4cl:bolt-transaction-autocommit
            db
            "MATCH (:RgSchema {name: 'root'})-[:VERSION]->(:RgSchemaVersion {createddate: $version})-[:HAS]->(r:RgResourceType) RETURN r.name AS name"
            :parameters `(("version" . ,schema-version)))))

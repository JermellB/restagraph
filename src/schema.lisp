;   Copyright 2017 James Fleming <james@electronic-quill.net>
;
;   Licensed under the GNU General Public License
;   - for details, see LICENSE.txt in the top-level directory


;;;; Schema creation functions

(in-package #:restagraph)

(defun read-schema-yaml-to-structs (filepath)
  "Digest a single YAML file, and return it as a set of structs.
  for use by inject-schema-from-struct."
  (declare (type pathname filepath))
  (log-message :info "Attempting to digest schema file ~A into structs" filepath)
  (let ((schema (cl-yaml:parse filepath)))
    (make-schema
      :name (gethash "name" schema)
      :version (gethash "version" schema)
      ;; Don't assume there _are_ resourcetypes defined in this schema.
      ;; Also, `maphash` breaks when you feed it '().
      :resourcetypes
      (if (gethash "resourcetypes" schema)
        (loop for resourcename being the hash-keys
              in (gethash "resourcetypes" schema)
              using (hash-value value)
              collect (make-schema-rtypes
                        :name resourcename
                        :dependent (gethash "dependent" value)
                        :notes (or (gethash "notes" value) "")
                        :attributes
                        ;; Don't assume this resourcetype has attributes defined.
                        ;; Again, `maphash` doesn't cope with NIL.
                        (when (gethash "attributes" value)
                          (loop for attrname being the hash-keys
                                in (gethash "attributes" value)
                                using (hash-value attrdetails)
                                collect (make-schema-rtype-attrs
                                          :name attrname
                                          :description (if (and attrdetails
                                                                (hash-table-p attrdetails)
                                                                (gethash "description" attrdetails))
                                                         (gethash "description" attrdetails)
                                                         "")
                                          :values (when (and attrdetails
                                                             (hash-table-p attrdetails)
                                                             (gethash "vals" attrdetails))
                                                    (cl-ppcre:split ","
                                                                    (gethash "vals" attrdetails))))))))
        (log-message :info "No resourcetypes found in schema '~A'"
                     (gethash "name" schema)))
      ;; Don't assume there _are_ relationships defined in this schema.
      :relationships (if (gethash "relationships" schema)
                       (remove-if
                         #'null
                         (mapcar
                           #'(lambda (rel)
                               (log-message :info "Attempting to create a schema-rel struct with URI '~A" (gethash "uri" rel))
                               ;; Sanity checks are required here
                               (cond
                                 ;; It doesn't have a URI
                                 ((not (gethash "uri" rel))
                                  (log-message :warn "No URI in this entry. Skipping.")
                                  '())
                                 ;; The happy path: it's OK
                                 (t (make-schema-rels
                                      :uri (gethash "uri" rel)
                                      :cardinality (or (gethash "cardinality" rel) "many:many")
                                      :dependent (gethash "dependent" rel)
                                      :notes (gethash "notes" rel)))))
                           (gethash "relationships" schema)))
                       (log-message "No relationships found in schema '~A'"
                                    (gethash "name" schema))))))

(defun enumerate-schemas-in-dir (schemadir)
  "Return a list of pathnames for .yaml files in the specified directory."
  (log-message :info (format nil "Attempting to read schemas in directory ~A" schemadir))
  ;; Safety first: is the directory even there?
  (let ((schemapath (directory-namestring schemadir)))
    (if (probe-file schemapath)
      ;; This is _really_ ugly, but guarantees alphabetical order.
      (mapcar #'pathname
              (sort (mapcar #'namestring
                            (directory (make-pathname
                                         :name :wild
                                         :type "yaml"
                                         :directory schemapath)))
                    #'string<))
      ;; Safety-check failed. Complain loudly.
      (let ((message (format nil "Schema directory ~A doesn't exist!" schemapath)))
        (log-message :fatal message)
        (error message)))))

(defun read-schemas (parent-dir)
  "Parse the .yaml files in the specified directory, in alphabetical order.
  Return the result as a list of objects output by cl-yaml:parse,
  expected to be hash objects."
  (declare (type (string) parent-dir))
  (log-message :info (format nil "Attempting to read schemas in directory ~A" parent-dir))
  ;; Safety first: is the directory even there?
  (if (probe-file parent-dir)
    ;; This is _really_ ugly, but guarantees alphabetical order.
    (mapcar #'cl-yaml:parse
            (mapcar #'pathname
                    (sort
                      (mapcar #'namestring
                              (directory (make-pathname
                                           :name :wild
                                           :type "yaml"
                                           :directory parent-dir)))
                      #'string<)))
    ;; Safety-check failed. Complain loudly.
    (let ((message (format nil "Schema directory ~A doesn't exist!" parent-dir)))
      (log-message :fatal message)
      (error message))))

(defun ensure-schema-schema (db)
  "Bootstrap function to ensure the database contains the schema-related schema.
  Must be handled separately from the schema we're trying to inject."
  (log-message :info "Ensuring the schema schema is present.")
  ;; Schema name.
  ;; Enables us to combine multiple schemas in a single system.
  (unless (resourcetype-exists-p db "rgSchemas")
    (progn
      (log-message :info "Attempting to add resourcetype rgSchemas")
      (add-resourcetype
        db
        "rgSchemas"
        :notes "Schema-management schema.")))
  ;; Schema version object.
  ;; Allows us to track the history of schema updates in this installation.
  (unless (resourcetype-exists-p db "rgSchemaVersions")
    (progn
      (log-message :info "Attempting to add resourcetype rgSchemaVersions")
      (add-resourcetype
        db
        "rgSchemaVersions"
        :dependent t
        :notes "Schema version-management atom.")))
  ;; Define the relationship between schemas and their versions
  (unless (get-relationship-attrs db "rgSchemas" "Versions" "rgSchemaVersions")
    (log-message :info "Attempting to link rgSchemas with rgSchemaVersions")
    (add-resource-relationship
      db
      "rgSchemas"
      "Versions"
      "rgSchemaVersions"
      :dependent t
      :cardinality "1:many")))

(defun get-schema-version (db name &key all-versions)
  "Extract the highest version number in the database for the named schema.
   If no such schema is present, return NIL."
  (declare (type (string) name)
           (type (boolean) all-versions))
  (let ((rawdata (get-resources
                   db
                   (format nil "/rgSchemas/~A/Versions/rgSchemaVersions" name))))
    (log-message :debug "Versions retrieved")
    (when rawdata
      (let ((versions
              (mapcar
                #'(lambda (row)
                    (realpart
                      (parse-integer
                        (cdr (assoc :uid row :test #'equal)))))
                rawdata)))
        (if all-versions
            versions
            (apply 'max versions))))))

(defun set-schema-version (db name version)
  "Set the version for the named schema.
   If there's no record of a schema by this name, create that first.
   version should be an integer."
  (declare (type (string) name)
           (type (integer) version))
  ;; Ensure the schema itself is represented,
  ;; and that we're attempting to store a newer version than already exists.
  (if (get-resources db (format nil "/rgSchemas/~A" name))
      ;; Schema is already represented; sanity-check the version
      (let ((existing-version (get-schema-version db name)))
        (when (and existing-version
                   (not (> version existing-version)))
          (error "Attempting to store a version that is not greater than the existing highest.")))
      ;; Not already there; create it.
      (store-resource db "rgSchemas" `(("uid" . ,name))))
  ;; Now set the version
  (store-dependent-resource
    db
    (format nil "/rgSchemas/~A/Versions/rgSchemaVersions" name)
    `(("uid" . ,(format nil "~A" version)))))

(defun inject-schema-from-struct (db schema)
  "Apply the supplied schema, if it's a newer version than the one already present,
   or if there isn't one already there.
   `db` must be of type `neo4cl:neo4j-rest-server`
   `schemas` is a `schema` struct."
   (declare (type neo4cl:neo4j-rest-server db)
            (type schema schema))
  (log-message :info (format nil "Attempting to apply version ~A of core schema ~A."
                             (schema-version schema) (schema-name schema)))
  ;; Test whether the db is already up to date with this schema
  (let ((current-version (get-schema-version db (schema-name schema))))
    ;; If it is, take no action.
    (if (and current-version
             (>= current-version (schema-version schema)))
        (log-message
          :info
          (format nil "Core schema ~A is at version ~A. Not attempting to replace it with version ~A."
                  (schema-name schema) current-version (schema-version schema)))
        ;; DB schema is not up to date. Make it so.
        (progn
          (log-message :info "Superseding existing schema version ~A with version ~A."
                       current-version (schema-version schema))
          ;; Ensure the resourcetypes are present
          (log-message :info "Adding resources")
          (mapcar #'(lambda (rtype)
                      (log-message :info "Adding attribute '~A'" (schema-rtypes-name rtype))
                      (add-resourcetype db
                                        (schema-rtypes-name rtype)
                                        :dependent (schema-rtypes-dependent rtype)
                                        :notes (schema-rtypes-notes rtype))
                      ;; Ensure the attributes for each resourcetype
                      (mapcar #'(lambda (attribute)
                                  (log-message :info "Adding attribute '~A' to resourcetype '~A'"
                                               (schema-rtype-attrs-name attribute)
                                               (schema-rtypes-name rtype))
                                  (set-resourcetype-attribute
                                    db
                                    (schema-rtypes-name rtype) ; resource-type
                                    :name (schema-rtype-attrs-name attribute)
                                    :description (schema-rtype-attrs-description attribute)
                                    ;; Condense the vals back into a comma-separated string
                                    :vals (format nil "~{~A~^,~}"
                                                  (schema-rtype-attrs-values attribute))))
                              (schema-rtypes-attributes rtype)))
                  (schema-resourcetypes schema))))))

(defun inject-all-schemas (db parent-dir)
  "Read all .yaml files in parent-dir in alphabetical order,
  and inject the schema described in each one, in turn."
  (declare (type (or null string) parent-dir))
  ;; Ensure the schema-schema is in place
  (ensure-schema-schema db)
  ;; Now ensure the core schemas are present and up to date
  (log-message :info "Ensuring core schemas are present and up to date")
  (log-message :info
               (format nil "Attempting to apply any/all schemas specified in directory '~A'" parent-dir))
  (mapcar #'(lambda (schema) (inject-schema-from-struct db schema))
          restagraph::*core-schemas*)
  ;; Lastly, if there were any user-defined schemas, apply those as well.
  ;; The point of digesting all the schema files before beginning to inject them,
  ;; instead of simply doing it as a single loop, is to ensure that we don't get
  ;; partway through injecting the schemas before discovering a problem in the files.
  (when parent-dir
    (mapcar #'(lambda (schemafile)
                (inject-schema-from-struct db schemafile))
            (mapcar #'read-schema-yaml-to-structs
                    (enumerate-schemas-in-dir parent-dir)))))

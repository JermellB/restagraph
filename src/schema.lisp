;;;; Schema creation functions

(in-package #:restagraph)

(defun ensure-schema-schema (db)
  "Bootstrap function to ensure the database contains the schema-related schema.
  Must be handled separately from the schema we're trying to inject."
  (log-message :info "Attempting to apply schema")
  ;; Schema name.
  ;; Enables us to combine multiple schemas in a single system.
  (log-message :info "Attempting to add resourcetype rgSchemas")
  (handler-case
    (add-resourcetype
      db
      "rgSchemas"
      :notes "Schema-management schema.")
    ;; If it's an integrity error, i.e. it already exists, carry on.
    (neo4cl:client-error
      (e)
      (when (and (equal (neo4cl:title e) "Schema")
                 (equal (neo4cl:message e) "ConstraintValidationFailed"))
        (log-message :warn "Failed to create rgSchemas resource: ConstraintValidationFailed")
        nil)))
  ;; Schema version object.
  ;; Allows us to track the history of schema updates in this installation.
  (log-message :info "Attempting to add resourcetype rgSchemaVersions")
  (handler-case
    (add-resourcetype
      db
      "rgSchemaVersions"
      :dependent t
      :notes "Schema version-management atom.")
    ;; If it's an integrity error, i.e. it already exists, carry on.
    (neo4cl:client-error
      (e)
      (when (and (equal (neo4cl:title e) "Schema")
                 (equal (neo4cl:message e) "ConstraintValidationFailed"))
        (log-message :warn "Failed to create rgSchemaVersions resource: ConstraintValidationFailed")
        nil)))
  ;; Define the relationship between schemas and their versions
  (log-message :info "Attempting to link rgSchemas with rgSchemaVersions")
  (add-resource-relationship
    db
    "rgSchemas"
    "Versions"
    "rgSchemaVersions"
    :dependent t
    :cardinality "1:many"))

(defun get-schema-version (db name &key all-versions)
  "Extract the highest version number in the database for the named schema.
   If no such schema is present, return NIL."
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
  ;; Ensure the schema itself is represented,
  ;; and that we're attempting to store a newer version than already exists.
  (if (get-resources db (format nil "/rgSchemas/~A" name))
      ;; Schema is already represented; sanity-check the version
      (let ((existing-version (get-schema-version db name)))
        (when (not (> version existing-version))
          (error "Attempting to store a version that is not greater than the existing highest.")))
      ;; Not already there; create it.
      (store-resource db "rgSchemas" `(("uid" . ,name))))
  ;; Now set the version
  (store-dependent-resource
    db
    (format nil "/rgSchemas/~A/Versions/rgSchemaVersions" name)
    `(("uid" . ,(format nil "~A" version)))))

(defun inject-schema (db schema)
  "Apply the supplied schema, if it's a newer version than the one already present,
   or if there isn't one already there.
   schema is expected to be the output of cl-yaml:parse."
  ;; Ensure the schema-schema is in place
  (ensure-schema-schema db)
  ;; Now do the actual thing
  ;; Get these values now because current-version is used repeatedly
  (let* ((current-version (get-schema-version db (gethash "name" schema))))
    ;; Sanity-check: is there already a schema in place,
    ;; of a version equal to or greater than the one we've read in?
    (if (and current-version
             (>= current-version (gethash "version" schema)))
        ;; Schema is already in place, and this one isn't newer.
        (log-message
          :info
          "Schema version ~a is present. Not attempting to supersede it with version ~A."
          current-version (gethash "version" schema))
        ;; This schema is newer than the existing one. Carry on.
        (progn
          (log-message
            :info
            "Superseding existing schema version ~A with version ~A."
            current-version (gethash "version" schema))
          ;; Update resourcetypes
          (log-message :info "Adding resources")
          (maphash
            #'(lambda (resourcename value)
                (add-resourcetype
                  db
                  resourcename
                  :dependent (gethash "dependent" value)
                  :notes (gethash "notes" value)))
            (gethash "resourcetypes" schema))
          ;; Update relationships between resourcetypes
          (log-message :info "Adding relationships between resources")
          (mapcar
            #'(lambda (rel)
                (let ((relparts (cl-ppcre:split "/" (gethash "uri" rel))))
                  (add-resource-relationship
                    db
                    (second relparts)   ; parent-type
                    (third relparts)    ; relationship
                    (fourth relparts)   ; dependent-type
                    :dependent (gethash "dependent" rel)
                    :cardinality (gethash "cardinality" rel)
                    :notes (gethash "notes" rel))))
            (gethash "relationships" schema))
          ;; Record the current version of the schema
          (log-message :info "Update schema version in database")
          (set-schema-version db (gethash "name" schema) (gethash "version" schema))))))

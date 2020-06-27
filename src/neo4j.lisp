;   Copyright 2017 James Fleming <james@electronic-quill.net>
;
;   Licensed under the GNU General Public License
;   - for details, see LICENSE.txt in the top-level directory


;;;; Methods and functions specifically relating to Neo4J

(in-package #:restagraph)


;;;; Utility functions

(defun escape-neo4j (str)
  "Escape any undesirable characters in a string, e.g. the single-quote."
  (declare (type (string) str))
  (cl-ppcre:regex-replace-all
    "'"
    str
    "´"))


;;;; Schema methods and functions

(defmethod resourcetype-exists-p ((db neo4cl:neo4j-rest-server)
                                  (resourcetype string))
  (log-message :debug
               (format nil "Checking for existence of resourcetype '~A'"
                       resourcetype))
  (neo4cl:extract-data-from-get-request
    (neo4cl:neo4j-transaction
      db
      `((:STATEMENTS
          ((:STATEMENT
             . ,(format nil "MATCH (n:rgResource {name: '~A'}) RETURN n"
             (sanitise-uid resourcetype)))))))))

(defmethod resourcetype-relationship-exists-p ((db neo4cl:neo4j-rest-server)
                                  (source string)
                                  (relationship string)
                                  (dest string))
  (log-message :debug
               (format nil "Checking for relationship '~A'-'~A'->'~A'" source relationship dest))
  (when
    (neo4cl:extract-data-from-get-request
      (neo4cl:neo4j-transaction
        db
        `((:STATEMENTS
            ((:STATEMENT
               . ,(format nil "MATCH (:rgResource {name: '~A'})-[r:~A]->(:rgResource {name: '~A'}) RETURN r"
                          (sanitise-uid source)
                          (sanitise-uid relationship)
                          (sanitise-uid dest))))))))
    t))

(defmethod add-resourcetype ((db neo4cl:neo4j-rest-server)
                             (resourcetype string)
                             &key dependent notes)
  (log-message :info
               (format nil "Attempting to create resourcetype '~A'" resourcetype))
  ;; Do we already have one of these?
  (if (resourcetype-exists-p db resourcetype)
    ;; Already have one; return early
    (progn
      (log-message
        :info
        (format nil "Resourcetype '~A' already exists" resourcetype))
      ;; FIXME: return 200, not 201
      t)
    ;; Not already present; create it
    (progn
      (neo4cl:neo4j-transaction
        db
        `((:STATEMENTS
            ((:STATEMENT
               . ,(format nil "CREATE (r:rgResource {name: '~A'~A~A});"
                          (sanitise-uid resourcetype)
                          (if dependent
                            ", dependent: 'true'"
                            "")
                          (if notes
                            (format nil ", notes: '~A'" (escape-neo4j notes))
                            "")))))))
      (log-message :debug "Add a uniqueness constraint in the database, but only if it's a primary resource.")
      (unless dependent
        (handler-case
          (neo4cl:neo4j-transaction
            db
            `((:STATEMENTS
                ((:STATEMENT
                   . ,(format nil "CREATE CONSTRAINT ON (r:~A) ASSERT r.uid IS UNIQUE"
                   (sanitise-uid resourcetype)))))))
          (neo4cl:database-error (e)
                                 (if (equal (neo4cl:title e) "ConstraintCreateFailed")
                                   nil   ; This is OK - do nothing
                                   (return-database-error
                                     (format nil "~A.~A: ~A"
                                             (neo4cl:category e)
                                             (neo4cl:title e)
                                             (neo4cl:message e)))))))
      ;; Return a uniform response, either way.
      t)))

(defmethod add-resourcetype-attribute ((db neo4cl:neo4j-rest-server)
                                       (resourcetype string)
                                       &key name description)
  ;; Perform some sanity checks before proceeding.
  (cond
    ;; Does the specified resourcetype exist?
    ((not (resourcetype-exists-p db (sanitise-uid resourcetype)))
     (signal 'client-error :message "Resourcetype '~A' does not exist."))
    ;; Was a name supplied for the attribute? As a string?
    ((not (and name
               (stringp name)))
     (signal 'client-error :message "The 'name' attribute is mandatory."))
    ;; Has this attribute already been added?
    ((resourcetype-attribute-exists-p db (sanitise-uid resourcetype) (sanitise-uid name))
     t)
    ;; If a description was supplied, is it a string?
    ((and description
          (not (stringp description)))
     (signal 'client-error :message "The 'description' attribute must be a string."))
    ;; All sanity-checks have passed; carry on.
    (t
     (log-message :debug (format nil "Attempting to add to resourcetype '~A' an attribute called '~A', with description '~A'."
                                 resourcetype name (if description description "")))
     ;; Now that we have all the attributes, proceed with creation
     (neo4cl:neo4j-transaction
       db
       `((:STATEMENTS
           ((:STATEMENT
              . ,(format nil "MATCH (r:rgResource {name: '~A'}) CREATE (r)-[:rgHasAttribute]->(:rgAttribute {~{~{~A: '~A'~}~^, ~}});"
                         (sanitise-uid resourcetype)
                         ;; Handle the optional attribute-attributes with an accumulator
                         (append `(("name" ,(sanitise-uid name)))
                                 (when description
                                   `(("description" ,(escape-neo4j description))))))))))))))

(defmethod get-resource-attributes-from-db ((db neo4cl:neo4j-rest-server)
                                            (resourcetype string))
  (log-message :debug "Getting attributes for resourcetype '~A'" resourcetype)
  (neo4cl::extract-rows-from-get-request 
    (neo4cl:neo4j-transaction
      db
      `((:STATEMENTS
          ((:STATEMENT .
            ,(format nil "MATCH (c:rgResource { name: '~A' })-[:rgHasAttribute]-(a:rgAttribute) RETURN a"
                     (sanitise-uid resourcetype)))))))))

(defmethod update-resourcetype-attribute ((db neo4cl:neo4j-rest-server)
                                          (resourcetype string)
                                          (attribute string)
                                          &key description)
  ;; Sanity checks
  (cond
    ;; Is the description a string?
    ((or (not description) ; Client could have explicitly passed a null
         (not (stringp description)))
     (signal 'client-error :message "Description attribute must be in the form of a string"))
    ;; Looks good; proceed.
    (t
     (log-message :debug (format nil "Updating attribute '~A' of resourcetype '~A' with description '~A'"
                                 attribute resourcetype description))
     (neo4cl:neo4j-transaction
       db
       `((:STATEMENTS
           ((:STATEMENT
              . ,(format nil "MATCH (r:rgResource {name: '~A'})-[:rgHasAttribute]->(a:rgAttribute {name: '~A'}) SET n.description = '~A';"
                         (sanitise-uid resourcetype)
                         (sanitise-uid attribute)
                         (sanitise-uid description))))))))))

(defmethod resourcetype-attribute-exists-p ((db neo4cl:neo4j-rest-server)
                                            (resourcetype string)
                                            (attribute string))
  (member (escape-neo4j attribute)
          (mapcar #'(lambda (attr) (cdr (assoc :name (car attr))))
                  (get-resource-attributes-from-db db (sanitise-uid resourcetype)))
          :test #'equal))

(defmethod delete-resourcetype-attribute ((db neo4cl:neo4j-rest-server)
                                          (resourcetype string)
                                          (name string))
  (log-message :debug "Was requested to delete attribute '~A' from resourcetype '~A'"
               name resourcetype)
  ;; Sanity checks
  (cond
    ;; Resourcetype doesn't exist
    ((not (resourcetype-exists-p db resourcetype))
     (log-message :debug (format nil "Can't delete attributes from nonexistent resourcetype ~A"
                                 resourcetype))
     (signal 'client-error :message "No such resourcetype"))
    ;; Attribute doesn't exist
    ((not (resourcetype-attribute-exists-p db resourcetype name))
     (log-message :debug
                  (format nil "Can't delete nonexistent attribute '~A' from resourcetype '~A'"
                          name resourcetype))
     (signal 'client-error (format nil "Resourcetype '~A' has no attribute with name '~A'"
                                   resourcetype name)))
    ;; Looks OK; go ahead.
    (t
     (log-message :debug "Was requested to delete attribute '~A' from resourcetype '~A'"
                  name resourcetype)
     (neo4cl:neo4j-transaction
       db
       `((:STATEMENTS
           ((:STATEMENT
              . ,(format nil "MATCH (:rgResource {name: '~A'})-[:rgHasAttribute]->(a:rgAttribute {name: '~A'}) DETACH DELETE a;"
                         (sanitise-uid resourcetype)
                         (escape-neo4j name))))))))))

(defmethod delete-resourcetype ((db neo4cl:neo4j-rest-server)
                                (resourcetype string))
  ;; If it's not a dependent type, delete its uniqueness constraint.
  (when (not (dependent-resource-p db resourcetype))
    (log-message
      :debug
      (format nil "Dropping uniqueness constraint for resource-type ~A" resourcetype))
    (handler-case
      (neo4cl:neo4j-transaction
        db
        `((:STATEMENTS
            ((:STATEMENT
               .  ,(format nil "DROP CONSTRAINT ON (r:~A) ASSERT r.uid IS UNIQUE"
               (sanitise-uid resourcetype)))))))
      (neo4cl:database-error (e)
                             (if (equal (neo4cl:title e) "ConstraintDropFailed")
                                 nil   ; This is OK - do nothing
                                 (return-database-error
                                   (format nil "~A.~A: ~A"
                                           (neo4cl:category e)
                                           (neo4cl:title e)
                                           (neo4cl:message e)))))))
  (log-message :debug "Delete all instances of this resource-type.")
  (neo4cl:neo4j-transaction
    db
    `((:STATEMENTS
        ((:STATEMENT
           . ,(format nil "MATCH (n:~A) DETACH DELETE n;" (sanitise-uid resourcetype)))))))
  (log-message :debug "Delete this type, along with any relationships it has to other types.")
  (neo4cl:neo4j-transaction
    db
    `((:STATEMENTS
        ((:STATEMENT
           . ,(format nil "MATCH (:rgResource {name: '~A'})-[:rgHasAttribute]->(r) DETACH DELETE r;"
           (sanitise-uid resourcetype)))))))
  (neo4cl:neo4j-transaction
    db
    `((:STATEMENTS
        ((:STATEMENT
           . ,(format nil "MATCH (r:rgResource {name: '~A'}) DETACH DELETE r;"
           (sanitise-uid resourcetype))))))))

(defmethod get-resource-types ((db neo4cl:neo4j-rest-server))
  (log-message :debug "Getting all resource-types")
  (mapcar #'car
          (neo4cl::extract-rows-from-get-request
            (neo4cl:neo4j-transaction
              db
              `((:STATEMENTS
                  ((:STATEMENT . "MATCH (c:rgResource) RETURN c"))))))))

(defmethod describe-resource-type ((db neo4cl:neo4j-rest-server)
                                   (resourcetype string)
                                   &key resources-seen)
  (log-message :debug (format nil "Describing resource-type '~A'" resourcetype))
  ;; Confirm whether this resourcetype exists at all.
  ;; If it doesn't, automatically return NIL.
  (let ((node))
    (when (resourcetype-exists-p db resourcetype)
      ;; Construct the return values
      `((:NAME . ,(sanitise-uid resourcetype))
        (:ATTRIBUTES
          . ,(sort
               (mapcar
                 #'(lambda (s) (cdr (assoc :name (car s))))
                 (neo4cl:extract-rows-from-get-request
                   (neo4cl:neo4j-transaction
                     db
                     `((:STATEMENTS
                         ((:STATEMENT
                            . ,(format nil "MATCH (:rgResource {name: '~A'})-[:rgHasAttribute]->(n:rgAttribute) RETURN n"
                                       (sanitise-uid resourcetype)))))))))
               #'string-lessp))
        (:DEPENDENT . ,(if (assoc :DEPENDENT node)
                           "true"
                           "false"))
        (:NOTES . ,(if (cdr (assoc :NOTES node))
                       (cdr (assoc :NOTES node))
                       ""))
        (:RELATIONSHIPS . ,(mapcar #'(lambda (rel)
                                       (log-message :debug "Retrieving description for linked resourcetype '~A'" (cdr rel))
                                       ;; Return an alist of the values, ready for rendering into Javascript
                                       `((:RELATIONSHIP . ,(relationship-attrs-name (car rel)))
                                         (:DEPENDENT . ,(if (relationship-attrs-dependent (car rel)) "true" "false"))
                                         (:CARDINALITY . ,(relationship-attrs-cardinality (car rel)))
                                         (:NOTES . ,(relationship-attrs-notes (car rel)))
                                         (:resourcetype ,(cdr rel))))
                                   (describe-dependent-resources
                                     db
                                     (sanitise-uid resourcetype)
                                     :resources-seen resources-seen)))))))

(defmethod describe-resource-type-for-graphql ((db neo4cl:neo4j-rest-server)
                                               (resourcetype string)
                                               (all-resourcetype-names list)
                                               (rels-from-any list)
                                               &key resources-seen)
  (log-message :debug (format nil "Describing resource-type '~A'" resourcetype))
  ;; Confirm whether this resourcetype exists at all.
  ;; If it doesn't, automatically return NIL.
  ;; Construct the return values
  (let* ((name (sanitise-uid resourcetype))
         (attributes (sort
                       (mapcar
                         #'(lambda (s) (cdr (assoc :name (car s))))
                         (neo4cl:extract-rows-from-get-request
                           (neo4cl:neo4j-transaction
                             db
                             `((:STATEMENTS
                                 ((:STATEMENT
                                    . ,(format nil "MATCH (:rgResource {name: '~A'})-[:rgHasAttribute]->(n:rgAttribute) RETURN n"
                                               (sanitise-uid resourcetype)))))))))
                       #'string-lessp))
         (dependent-resources (describe-dependent-resources
                                db
                                (sanitise-uid resourcetype)
                                :resources-seen resources-seen))
         ;; Build lists of '(relationship target-resourcetype)
         ;; FIXME
         (rels-to-any
           (apply #'append
                  (mapcar #'(lambda (any-rel)
                                  ;; For each relationship of this type, return '(rel-type . target)
                                  ;; for each resourcetype in the schema, as supplied here via
                                  ;; all-resourcetype-names
                                  (mapcar #'(lambda (name)
                                              (cons (relationship-attrs-name (car any-rel)) name))
                                          all-resourcetype-names))
                              ;; List of all relationships this type has to "any"
                              (remove-if-not #'(lambda (rel)
                                                 (equal (cdr rel) "any"))
                                             dependent-resources))))
         (specific-rels
           ;; Build list of '(relationship . target-resourcetype) to all specific types
           ;; with which this one has a relationship
           (mapcar #'(lambda (rel)
                       (cons (relationship-attrs-name (car rel)) (cdr rel)))
                   ;; Generate a list of relationships this type has to non-any resourcetypes
                   (remove-if #'(lambda (res)
                                  (equal (cdr res) "any"))
                              dependent-resources)))
         (processed-rels-from-any
           ;; Generate '(rel . target) conses for all in rels-from-any argument
           (mapcar #'(lambda (rel)
                       (cons (relationship-attrs-name (car rel)) (cdr rel)))
                   rels-from-any))
         ;; Operates on rels-from-any, rels-to-any and specific-rels
         (relationships
           (mapcar #'(lambda (rel)
                       (log-message :debug (format nil "Disassembling relationship ~A for output" rel))
                       (format nil "~A_~A: [~A] @relation(name: \"~A\", direction: \"OUT\")"
                               ; Downcased relationship-name and target resourcetype.
                               ; It's clumsy, but that's a consequence of Node's approach.
                               (string-downcase (car rel))
                               (string-downcase (cdr rel))
                               ; Target resourcetype
                               (cdr rel)
                               ; Relationship-name (correct case)
                               (car rel)))
                   (append
                     processed-rels-from-any
                     rels-to-any
                     specific-rels))))
    (log-message :debug "Assembled relationships ~A" relationships)
    (with-output-to-string (outstr)
      (format outstr "type ~A { ~{
              ~A: String~}~{
              ~A~}
              }"
              name
              (append '("uid" "createddate") attributes)
              relationships))))

(defmethod describe-dependent-resources ((db neo4cl:neo4j-rest-server)
                                         (resourcetype string)
                                         &key resources-seen)
  (log-message :debug (format nil "Describing resources linked from '~A'" resourcetype))
  ;; Skip any resources we've already seen, to break loops
  (remove-if #'null
             (mapcar
               #'(lambda (row)
                   (when (not (member (fourth row) resources-seen :test #'equal))
                     ;; If you think this looks cumbersome, you should have tried to reason through
                     ;; this code _without_ using the struct to keep things clear.
                     (cons
                       (make-relationship-attrs
                         :name (second row)
                         :dependent (when (and (third row)
                                               (equal (third row) "true"))
                                      t)
                         :cardinality (or (fourth row) "many:many")
                         :notes (if (and (fifth row)
                                         (stringp (fifth row)))
                                    (fifth row)
                                    ""))
                       (first row))))
               (neo4cl:extract-rows-from-get-request
                 (neo4cl:neo4j-transaction
                   db
                   `((:STATEMENTS
                       ((:STATEMENT
                          .  ,(format nil "MATCH (:rgResource {name: '~A'})-[r]->(n:rgResource) WHERE type(r) <> 'rgHasAttribute' RETURN n.name, type(r), r.dependent, r.cardinality, r.notes"
                                      (sanitise-uid resourcetype)))))))))))

(defmethod add-resource-relationship ((db neo4cl:neo4j-rest-server)
                                      (parent-type string)
                                      (relationship string)
                                      (dependent-type string)
                                      &key dependent cardinality notes)
  (log-message :info
               (format nil "Attempting to create relationship '~A' from '~A' to '~A'"
                       relationship parent-type dependent-type))
  (cond
    ;; Sanity checks
    ((not (describe-resource-type db parent-type))
     (error 'restagraph:integrity-error :message
            (format nil "Parent resource type ~A does not exist" parent-type)))
    ((not (describe-resource-type db dependent-type))
     (error 'restagraph:integrity-error :message
            (format nil "Dependent resource type ~A does not exist" dependent-type)))
    ;; catch the case where this relationship already exists
    ((resourcetype-relationship-exists-p db parent-type relationship dependent-type)
     t)
    ;; FIXME catch attempts to create dependent relationships to non-dependent types
    ;; All sanity checks have passed. Create it.
    (t
     (let ((attrs (list
                    (format nil "dependent: '~A'" (if dependent "true" "false"))
                    (format nil "cardinality: '~A'"
                            (if (member cardinality '("1:1" "many:1" "1:many" "many:many") :test #'equal)
                                cardinality
                                "many:many")))))
       ;; Add notes if they were supplied
       (when notes
         (pushnew (format nil "notes: '~A'" (escape-neo4j notes)) attrs))
       ;; Create it
       (neo4cl:neo4j-transaction
         db
         `((:STATEMENTS
             ((:STATEMENT .
               ,(format
                  nil
                  "MATCH (s:rgResource { name: '~A' }), (d:rgResource { name: '~A'}) CREATE (s)-[:~A {~{~A~^, ~}}]->(d)"
                  (sanitise-uid parent-type)
                  (sanitise-uid dependent-type)
                  (sanitise-uid relationship)
                  attrs))))))))))

(defmethod delete-resource-relationship ((db neo4cl:neo4j-rest-server)
                                         (parent-type string)
                                         (relationship string)
                                         (dependent-type string))
  (neo4cl:neo4j-transaction
    db
    `((:STATEMENTS
        ((:STATEMENT .
          ,(format
             nil
             "MATCH (s:rgResource { name: '~A' })-[r:~A]->(d:rgResource { name: '~A'}) DELETE r"
             (sanitise-uid parent-type)
             (sanitise-uid relationship)
             (sanitise-uid dependent-type))))))))

(defun format-post-params-as-properties (params)
  "Take an alist, as returned by (tbnl:post-parameters*), and transform it into the kind of map that Neo4J expects in the :PROPERTIES section of a query."
  (declare (type (or null cons) params))
  (log-message :debug "Formatting a set of POST parameters for use as Neo4j properties.")
  (mapcar #'(lambda (param)
              (cons (intern (escape-neo4j (string-downcase (car param))) :keyword)
                    (if (stringp (cdr param))
                      (escape-neo4j (cdr param))
                      (cdr param))))
          params))

(defmethod validate-resource-before-creating ((db neo4cl:neo4j-rest-server)
                                              (resourcetype string)
                                              (params list))
  (log-message
    :debug
    (format nil "validate-resource-before-creating resourcetype ~A with params ~{~A~^, ~}"
            resourcetype params))
  ;; Does this resource-type exist?
  (if (resourcetype-exists-p db resourcetype)
    ;; Were attributes specified and, if so, are they all valid for this resource-type?
    (let
      ((requested-attributes
         (remove-if #'(lambda (param) (equal (car param) "uid"))
                    params)))
      (log-message :debug "Confirmed: resourcetype '~A' exists" resourcetype)
      ;; Implicit logic for deciding what to return:
      ;; - if this conditional sequence passes, format the validated parameters.
      ;; - if it doesn't, return nil.
      (log-message :debug "Checking the supplied attributes.")
      (if (or
            ;; If no attributes were specified other than "uid", we're good
            (not requested-attributes)
            ;; If other attributes were specified, check them all for validity.
            ;; First, get a list of valid attributes for this resourcetype.
            (let* ((valid-attributes
                     (mapcar #'(lambda (row)
                                 (cdr (assoc :name (car row))))
                             (get-resource-attributes-from-db db resourcetype)))
                   ;; Now check the requested set of parameters against the list of valid ones.
                   (invalid-attributes
                     (remove-if #'null
                                (mapcar #'(lambda (par)
                                            (unless (member
                                                      (string-downcase (car par))
                                                      valid-attributes
                                                      :test 'equal)
                                              (escape-neo4j (string-downcase (car par)))))
                                        requested-attributes))))
              ;; Record the valid possibilities, if we're debugging.
              (if valid-attributes
                  (log-message :debug (format nil "Valid attributes for resource-type ~A: ~{~A~^, ~}."
                                              resourcetype valid-attributes))
                  (log-message :debug (format nil "Resource-type ~A has no valid attributes to set."
                                              resourcetype)))
              ;; If any invalid attributes were requested, log this and signal an error.
              (if invalid-attributes
                  (progn
                    (log-message :debug (format nil "Identified invalid attributes: ~{~A~^, ~}"
                                                invalid-attributes))
                    (error 'restagraph:client-error :message
                           (format nil "Invalid attributes for ~A resources: ~{~A~^, ~}"
                                   resourcetype invalid-attributes)))
                  (log-message :debug "No invalid attributes identified. Proceeding."))
              ;; We were given attributes other than "uid" and all of them checked out OK.
              ;; Explicitly return something positive from this clause of the if statement.
              t))
          ;; Return the supplied attributes to the caller, properly formatted for Neo4j.
          (let* ((original-uid (or (cdr (assoc "uid" params :test #'string=)) ""))
                (formatted-params
                  (format-post-params-as-properties
                    ;requested-attributes
                    (acons "uid" (sanitise-uid original-uid)
                           (acons "original_uid" original-uid
                                  (remove-if #'(lambda (param) (equal (car param) "uid"))
                                             params))))))
            (log-message :debug "Returning formatted parameters ~A" formatted-params)
            formatted-params)))
    ;; No such resourcetype
    (signal 'client-error :message "No such resourcetype")))


;;;; Resources

(defmethod dependent-resource-p ((db neo4cl:neo4j-rest-server) (resourcetype string))
  (neo4cl:extract-data-from-get-request
    (neo4cl:neo4j-transaction
      db
      `((:STATEMENTS
          ((:STATEMENT .
            ,(format nil "MATCH (c:rgResource { name: '~A' }) RETURN c.dependent"
                     (sanitise-uid resourcetype)))))))))

(defmethod dependent-relationship-p ((db neo4cl:neo4j-rest-server)
                                     (source-type string)
                                     (relationship string)
                                     (dest-type string))
  (log-message :debug
               (format nil "Checking whether ~A is a valid dependent relationship from ~A to ~A"
                       relationship source-type dest-type))
  (neo4cl:extract-data-from-get-request
    (neo4cl:neo4j-transaction
      db
      `((:STATEMENTS
          ((:STATEMENT
             . ,(format nil "MATCH (a:rgResource { name: '~A' })-[r:~A { dependent: 'true' } ]->(b:rgResource { name: '~A', dependent: 'true' }) WHERE r.dependent = 'true' RETURN type(r)"
                        (sanitise-uid source-type)
                        (sanitise-uid relationship)
                        (sanitise-uid dest-type)))))))))

(defmethod store-resource ((db neo4cl:neo4j-rest-server)
                           (resourcetype string)
                           (attributes list))
  (cond
    ;; Catch any critical deficiencies in the definition asap
    ((or (null (assoc "uid" attributes :test 'equal))
         (equal (cdr (assoc "uid" attributes :test 'equal)) ""))
     (error 'client-error
            :message "The UID must be a non-empty string"))
    ;; If this is a dependent resource, bail out now
    ((dependent-resource-p db resourcetype)
     (error 'integrity-error
            :message "This is a dependent resource; it must be created as a sub-resource of an existing resource."))
    ;; OK so far: carry on
    (t
     (handler-case
       (let ((attributes (validate-resource-before-creating db resourcetype attributes)))
         (progn
           ;; If we got this far, we have a valid resource type and valid attribute names.
           ;; Make it happen
           (log-message :debug (format nil "Creating a ~A resource with attributes ~A"
                                       resourcetype attributes))
           (handler-case
             (neo4cl:neo4j-transaction
               db
               `((:STATEMENTS
                   ((:STATEMENT . ,(format nil "CREATE (:~A $properties)"
                                           (sanitise-uid resourcetype)))
                    (:PARAMETERS . ((:PROPERTIES
                                      . ,(append attributes
                                                 `(("createddate" . ,(get-universal-time)))))))))))
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
                     (error 'restagraph:client-error :message text)))))))
       (client-error
         (e)
         (if (equal (message e) "No such resourcetype")
             (error 'restagraph:integrity-error
                    :message "Requested resource type does not exist")
             (error 'restagraph:integrity-error
                    :message (message e))))))))

(defmethod update-resource-attributes ((db neo4cl:neo4j-rest-server)
                                       (path list)
                                       (attributes list))
  (log-message :debug (format nil "Updating attributes for resource ~{/~A~}" path))
  (let ((attrs
          (append
            (remove-if #'(lambda (f)
                           (or (equal (car f) :|uid|)
                               (equal (car f) :|original_uid|)))
                       (validate-resource-before-creating
                         db
                         (car (last (butlast path)))
                         attributes))
            `(("updateddate" . ,(get-universal-time))))))
    (when attrs
      (log-message
        :debug
        (format nil "Applying the attributes ~{~A~^, ~} to resource ~{/~A~}" attrs path))
      (let ((query (format nil "MATCH ~A SET ~{~A~^, ~}"
                           (uri-node-helper path :path "" :marker "n" :directional t)
                           (mapcar #'(lambda (a)
                                       (let ((attrname (car a))
                                             (attrvalue (cdr a)))
                                         (if (null attrvalue)
                                             (format nil "n.~A = NULL" attrname)
                                             (format nil
                                                     (if (numberp attrvalue)
                                                         "n.~A = ~A"
                                                         "n.~A = '~A'")
                                                     attrname attrvalue))))
                                   attrs))))
        (log-message
          :debug
          (format nil "Applying statement ~A" query))
        (neo4cl:neo4j-transaction db `((:STATEMENTS ((:STATEMENT .  ,query)))))))))

;;; FIXME: validate the attrs, at least to ensure they're escaped
(defmethod delete-resource-attributes ((db neo4cl:neo4j-rest-server)
                                       (path list)
                                       (attributes list))
  (log-message
    :debug
    "Attempting to delete attributes '~{~A~^, ~}' from the resource at path '~{~A~^/~}'"
    attributes path)
  (neo4cl:neo4j-transaction
    db
    `((:STATEMENTS
        ((:STATEMENT
           . ,(format nil "MATCH ~A REMOVE ~{n.~A~^, ~};"
                      (uri-node-helper path :path "" :marker "n" :directional t)
                      attributes)))))))

(defun process-filter (filter)
  "Process a single filter from a GET parameter.
  Assumes uri-node-helper was called with its default marker, which is 'n'.
  Returns NIL when the cdr of the filter is NIL."
  (log-message :debug "Attempting to process filter ~A" filter)
  ;; Sanity-check: is this an empty filter expression?
  ;; These can legitimately be sent via badly-written search pages, for example.
  (cond
    ;; Empty filter
    ((and (listp filter)
          (or (null (cdr filter))
              (equal (cdr filter) "")))
     (log-message :debug (format nil "Empty filter ~A; ignoring" (car filter)))
     nil)
    ;; The filter's non-empty; carry on
    ((and (listp filter)
          (cdr filter)
          (stringp (cdr filter)))
     (log-message :debug (format nil "Filter ~A looks OK; attempting to process it" (car filter)))
     (let ((name (car filter))
           ;; Does the value start with "!" to indicate negation?
           (negationp (string= "!" (cdr filter) :end2 1)))
       ;; Log whether negation was detected
       (if negationp
         (log-message :debug "Negation detected. negationp = ~A" negationp)
         (log-message :debug "Negation not detected. Double-negative in progress."))
       ;; Prepend negation if applicable
       (let ((value (escape-neo4j (if negationp
                                    (subseq (cdr filter) 1)
                                    (cdr filter)))))
         (format
           nil
           "~A~A"
           ;; Are we negating it?
           (if negationp "NOT " "")
           ;; Infer the operator
           (cond
             ;; Outbound links
             ;; Simple format: relationship/path/to/target
             ((equal name "outbound")
              (let* ((parts (remove-if #'(lambda (n)
                                           (or (null n)
                                               (equal n "")))
                                       (cl-ppcre:split "/" value)))
                     (relationship (sanitise-uid (first parts)))
                     (target-type (sanitise-uid (second parts)))
                     (target-uid (sanitise-uid (third parts))))
                (log-message :debug "Outbound link detected: ~A" value)
                (format nil "(n)-[:~A]-(:~A {uid: '~A'})" relationship target-type target-uid)))
             ;; Regex match
             ;; Full reference: https://docs.oracle.com/javase/7/docs/api/java/util/regex/Pattern.html
             ((cl-ppcre:all-matches "[\\.\\*\\+[]" value)
              (let ((offset (if negationp 1 0)))
                (log-message :debug
                             "Regex detected. Extracting the value from a starting offset of ~d."
                             offset)
                (format
                  nil "n.~A =~~ '~A'"
                  (escape-neo4j name)
                  ;; Drop the first character if we're negating the match,
                  ;; otherwise use the whole string.
                  (escape-neo4j (subseq value offset)))))
             ;;
             ;; Simple existence check
             ((string= "exists" (escape-neo4j value))
              (format nil "exists(n.~A)" (escape-neo4j name)))
             ;;
             ;; Default case: exact text match
             (t
               (format nil "n.~A = '~A'" (escape-neo4j name) (escape-neo4j value))))))))
    (t
      (log-message :warn "Invalid filter")
      nil)))

(defun process-filters (filters)
  "Take GET parameters, and turn them into a string of Neo4j WHERE clauses."
  (log-message :debug (format nil "Attempting to process filters ~A" filters))
  (let ((result (remove-if #'null (mapcar #'process-filter filters))))
    (log-message :debug "Result of filter processing: ~A" result)
    (if result
        (let ((response (format nil " WHERE ~{ ~A~^ AND~}" result)))
          (log-message :debug "Output from process-filters: ~A." response)
          response)
        "")))

(defmethod get-resources ((db neo4cl:neo4j-rest-server)
                          (uri string)
                          &key filters
                          directional)
  (log-message :debug (format nil "Fetching resources for URI ~A" uri))
  (let ((uri-parts (get-uri-parts uri)))
    (cond
      ;; All resources of a given type
      ((equal (mod (length uri-parts) 3) 1)
       (log-message :debug (format nil "Fetching all resources of type ~A" uri))
       (let ((query (format nil "MATCH ~A~A RETURN n"
                            (uri-node-helper uri-parts
                                             :path ""
                                             :marker "n"
                                             :directional directional)
                            (process-filters filters))))
         (log-message :debug (concatenate 'string "Querying database: "
                                          (cl-ppcre:regex-replace "\~" query "~~")))
         (mapcar #'car
                 (neo4cl:extract-rows-from-get-request
                   (neo4cl:neo4j-transaction
                     db
                     `((:STATEMENTS
                         ((:STATEMENT . ,query)))))))))
      ;; One specific resource
      ((equal (mod (length uri-parts) 3) 2)
       (log-message :debug (format nil "Fetching the resource matching the path ~A" uri))
       (let ((query (format nil "MATCH ~A RETURN n"
                            (uri-node-helper uri-parts
                                             :path ""
                                             :marker"n"
                                             :directional directional))))
         (log-message :debug (concatenate 'string "Querying database: "
                                          (cl-ppcre:regex-replace "\~" query "~~")))
         (neo4cl:extract-data-from-get-request
           (neo4cl:neo4j-transaction
             db
             `((:STATEMENTS
                 ((:STATEMENT . ,query))))))))
      ;; All resources with a particular relationship to this one
      (t
       (log-message
         :debug
         (format nil "Fetching all resources with relationship ~A to resource ~{/~A~}"
                 (car (last uri-parts))
                 (butlast uri-parts)))
       ;; Get the raw data
       (let ((query (format nil "MATCH ~A~A RETURN labels(n), n"
                            (uri-node-helper uri-parts
                                             :path ""
                                             :marker "n"
                                             :directional directional)
                            (process-filters filters))))
         (log-message :debug (concatenate 'string "Querying database: "
                                          (cl-ppcre:regex-replace "\~" query "~~")))
         (let ((response
                 (neo4cl:extract-rows-from-get-request
                   (neo4cl:neo4j-transaction
                     db
                     `((:STATEMENTS
                         ((:STATEMENT . ,query))))))))
           (log-message
             :debug
             (format nil "Retrieved results: ~A" response))
           ;; Reformat it so that (:type <type>) appears at the start of the list
           (mapcar (lambda (r) (cons (cons :type (caar r)) (cadr r)))
                   response)))))))


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
                        (sanitise-uid reltype)
                        (sanitise-uid dest-type)
                        (sanitise-uid source-type)))))))))

(defstruct relationship-attrs
  "Describes the attributes of a relationship:
  relationship-attrs-dependent = boolean, indication whether this is a dependent relationship
  relationship-attrs-cardinality = string, returning the cardinality of the relationship"
  (name nil :type string :read-only t)
  (dependent nil :type boolean :read-only t)
  (cardinality "many:many" :type string :read-only t)
  (notes "" :type string :read-only t))

(defmethod get-relationship-attrs ((db neo4cl:neo4j-rest-server)
                                   (source-type string)
                                   (relationship string)
                                   (dest-type string))
  (log-message
    :debug
    (format nil "Retrieving the dependency and cardinality attributes of relationship ~A from ~A to ~A"
            relationship source-type dest-type))
  (let ((result
          (car
            (neo4cl:extract-rows-from-get-request
              (neo4cl:neo4j-transaction
                db
                `((:STATEMENTS
                    ((:STATEMENT
                       .  ,(format nil "MATCH (:rgResource {name: '~A'})-[r:~A]->(:rgResource {name: '~A'}) RETURN r.dependent, r.cardinality, r.notes"
                                   (sanitise-uid source-type)
                                   (sanitise-uid relationship)
                                   (sanitise-uid dest-type)))))))))))
    (when result
      ;; Sanity-check: is this relationship properly defined?
      (progn
        (log-message :debug "Got a result. Making a relationship object now.")
        (log-message :debug "Result structure: ~A" result)
        (make-relationship-attrs
          ;; The relationship name we return will be used in a URL.
          ;; Sanitise it for safety, just in case an unsafe version slipped through.
          :name (sanitise-uid relationship)
          ;; Avoid false positives for :dependent
          :dependent (when (equal (first result) "true") t)
          ;; Apply a sane default to cardinality (many:many)
          :cardinality (or (second result) "many:many")
          ;; Cautious approach: ensure we set :notes to a string.
          :notes (if (and (third result) (stringp (third result)))
                   (third result)
                   ""))))))

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
       (let ((message (format nil "~A is not a valid path to a relationship" sourcepath)))
         (log-message :debug message)
         (error 'client-error :message message)))
      ((not (equal (mod (length dest-parts) 3) 2))
       (let ((message (format nil "~A is not a valid path to a resource" destpath)))
         (log-message :debug message)
         (error 'client-error :message message)))
      ;; Having made it that far, make checks that call to the database
      (t
       (let* ((relationship (car (last source-part-list)))
              (source-parts (butlast source-part-list)) ; Path to the source resource
              (source-type (nth (- (length source-parts) 2) source-parts))
              (dest-type (nth (- (length dest-parts) 2) dest-parts))
              (relationship-attrs
                (or
                  (get-relationship-attrs db source-type relationship dest-type)
                  (get-relationship-attrs db "any" relationship dest-type))))
         (cond
           ;; No such relationship
           ((not relationship-attrs)
            (let ((message
                    (format nil "'~A' is not a valid relationship from type '~A' to type '~A'"
                            relationship source-type dest-type)))
              (log-message :debug message)
              (error 'integrity-error :message message)))
           ;; 1:1 dependent relationship
           ((and
              (relationship-attrs-dependent relationship-attrs)
              (or
                (equal (relationship-attrs-cardinality relationship-attrs) "1:1")
                (equal (relationship-attrs-cardinality relationship-attrs) "1:many")))
            (let ((message (format nil "~A dependency. Either move the relationship or create a new dependent resource."
                                   (relationship-attrs-cardinality relationship-attrs))))
              (log-message :debug message)
              (error 'integrity-error :message message)))
           ;; Are we trying to create a duplicate?
           ((check-relationship-by-path
              db (format nil "~{/~A~}" source-parts) relationship destpath)
            (let ((message "Relationship already exists"))
              (log-message :debug message)
              (error 'integrity-error :message message)))
           ;; Do both the source and destination resources actually exist?
           ((null (get-resources db (format nil "/~{~A~^/~}" source-parts)))
            (let ((message (format nil "The source resource /~{~A~^/~} does not exist" source-parts)))
              (log-message :debug message)
              (error 'client-error :message message)))
           ((null (get-resources db destpath))
            (let ((message "The destination resource does not exist"))
              (log-message :debug message)
              (error 'client-error :message message)))
           ;; Many-to-one, and the source already has this relationship with another such target?
           ((and
              (equal (relationship-attrs-cardinality relationship-attrs) "many:1")
              (>
                (neo4cl:extract-data-from-get-request
                  (neo4cl:neo4j-transaction
                    db
                    `((:STATEMENTS
                        ((:STATEMENT
                           .  ,(format nil "MATCH ~A-[:~A]->(b:~A) RETURN count(b)"
                                       (uri-node-helper source-parts
                                                        :path ""
                                                        :marker "a"
                                                        :directional t)
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
                   ((:STATEMENT
                      .  ,(format nil "MATCH ~A, ~A MERGE (a)-[:~A]->(b)"
                                  (uri-node-helper source-parts
                                                   :path ""
                                                   :marker "a"
                                                   :directional t)
                                  (uri-node-helper dest-parts
                                                   :path ""
                                                   :marker "b"
                                                   :directional t)
                                  relationship)))))))))))))

(defmethod move-dependent-resource ((db neo4cl:neo4j-rest-server)
                                    (uri string)
                                    (newparent string))
  (log-message :debug
               (format nil "Attempting to move dependent resource ~A to new parent ~A"
                       uri newparent))
  (let* ((uri-parts (get-uri-parts uri))
         (current-parent-path (uri-node-helper (butlast uri-parts 3)
                                               :path ""
                                               :marker "b"
                                               :directional t))
         ;; It's a dependent resource, so the length of this path
         ;; will always be longer than 2:
         (current-relationship (car (last (butlast uri-parts 2))))
         (target-type (car (last (butlast uri-parts))))
         (target-uid (car (last uri-parts)))
         (dest-parts (get-uri-parts newparent))
         (new-relationship (car (last dest-parts)))
         ;; The new parent may have a 2-element path,
         ;; in which case we don't need to extract the last 2 elements:
         (new-parent-type (car (if (> (length dest-parts) 2)
                                   (last (butlast dest-parts 2))
                                   dest-parts)))
         ;; Define this here because we use it at both the start and the end
         (new-path (format nil "~{/~A~}/~A/~A" dest-parts target-type target-uid)))
    (cond
      ;; Sanity-check: does this path already exist?
      ((get-resources db new-path)
       (log-message :warn "This path already exists")
       (error 'integrity-error :message "Path already exists; refusing to create a duplicate."))
      ;; Sanity-check: does the target resource exist?
      ((null (get-resources db uri))
       (log-message :debug (format nil "Target resource ~A does not exist" uri))
       (error 'client-error :message "Target resource does not exist"))
      ;; Sanity-check: does the new parent exist?
      ((null (get-resources db (format nil "~{/~A~}" (butlast dest-parts))))
       (progn
         (log-message :debug (format nil "Parent resource ~{/~A~} does not exist"
                                     (butlast dest-parts)))
         (error 'client-error :message "Parent resource does not exist")))
      ;; Sanity-check: is the new relationship a valid dependent one?
      ((not (dependent-relationship-p db new-parent-type new-relationship target-type))
       (progn
         (log-message
           :debug
           (format
             nil
             "Target resource ~A does not depend on the new parent-type ~A for relationship ~A"
             target-type new-parent-type new-relationship))
         (error 'client-error
                :message
                (format
                  nil
                  "Target resource-type ~A doesn't depend on the parent type ~A"
                  target-type new-parent-type))))
      ;; Sanity-checks passed; let's do it
      (t
       (log-message :debug "Sanity-checks have passed. Attempting to move the resource.")
       (let* ((new-parent-path (build-cypher-path (butlast dest-parts)))
              (sourcepath
                (uri-node-helper (append
                                   (butlast uri-parts 3)
                                   (list current-relationship
                                         target-type
                                         target-uid))
                                 :path ""
                                 :marker "t"
                                 :directional t))
              (destpath (format nil "~A-[:~A]->(:~A {uid: '~A'})"
                                new-parent-path
                                new-relationship
                                target-type
                                target-uid)))
         (log-message
           :debug
           (format nil "Moving target ~A to new parent ~A" sourcepath destpath))
         ;; Create the new relationship
         (neo4cl:neo4j-transaction
           db
           `((:STATEMENTS
               ((:STATEMENT
                  . ,(format nil "MATCH ~A MATCH ~A CREATE (m)-[:~A]->(t)"
                             new-parent-path
                             sourcepath
                             new-relationship)))))))
       ;; Confirm that the new relationship is actually present.
       ;; If the MATCH clause matched nothing, it'll return OK.
       ;; We want to check this every time, and bail out if we detect that it failed.
       (unless (get-resources db new-path)
         (error 'integrity-error :message (format nil "New path ~A was not created." new-path)))
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
         (relationship (car (last (butlast uri-parts))))
         (parent-parts (butlast uri-parts 2))
         (parent-type (nth (- (length parent-parts) 2) parent-parts))
         (dest-type (car (last uri-parts)))
         (dest-uid (sanitise-uid (cdr (assoc "uid" attributes :test 'equal))))
         (relationship-attrs (get-relationship-attrs db parent-type relationship dest-type)))
    (cond
      ;; Sanity check: required parameters
      ((not dest-uid)
       (let ((message "The 'uid' parameter must be supplied"))
         (log-message :debug message)
         (error 'client-error :message message)))
      ;; Sanity check: existence of parent resource
      ((null (get-resources db (format nil "~{/~A~}" parent-parts)))
       (let ((message "Parent resource does not exist"))
         (log-message :debug message)
         (error 'client-error :message message)))
      ;; Sanity-check: is there a relationship between the parent and child resource types?
      ((null relationship-attrs)
       (error 'client-error
              :message
              (format nil "There is no relationship ~A from ~A to ~A"
                      relationship parent-type dest-type)))
      ;; Sanity check: dependency between parent and child resource types
      ((null (relationship-attrs-dependent relationship-attrs))
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
               (resource-path (format nil "~{/~A~}/~A/~A/~A"
                                      parent-parts
                                      relationship
                                      dest-type
                                      (cdr (assoc "uid" validated-attributes :test #'string=)))))
          ;; Report on the attributes for debugging
          (log-message :debug (format nil "Validated attributes: ~A" validated-attributes))
          ;; One more sanity-check: does it already exist?
          (if (null (get-resources db resource-path))
            ;; Cardinality checks: would this violate 1:1 or many:1 constraints?
            (if
              (and
                (or
                  (equal (relationship-attrs-cardinality relationship-attrs) "1:1")
                  (equal (relationship-attrs-cardinality relationship-attrs) "many:1"))
                ;; Look for this parent having this relationship with any other dependent resource
                (>
                  (neo4cl:extract-data-from-get-request
                    (neo4cl:neo4j-transaction
                      db
                      `((:STATEMENTS
                          ((:STATEMENT
                             .  ,(format nil "MATCH ~A<-[r {dependent: 'true'}]-() RETURN count(r)"
                                         (uri-node-helper parent-parts
                                                          :path ""
                                                          :marker "n"
                                                          :directional t))))))))
                  0))
              (error 'integrity-error :message
                     (format nil"~{~A~^/~} already has a ~A ~A relationship with a resource of type ~A"
                             parent-parts
                             (relationship-attrs-cardinality relationship-attrs)
                             relationship
                             dest-type))
              ;; Constraints are fine; create it
              (neo4cl:neo4j-transaction
                db
                `((:STATEMENTS
                    ((:STATEMENT .
                                 ,(format nil "MATCH ~A CREATE (n)-[:~A]->(:~A $properties)"
                                          (uri-node-helper parent-parts
                                                           :path ""
                                                           :marker "n"
                                                           :directional t)
                                          relationship
                                          dest-type))
                     (:PARAMETERS . ((:PROPERTIES
                     . ,(append validated-attributes
                     `(("createddate" . ,(get-universal-time))))))))))))
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
                             (sanitise-uid resourcetype)
                             (sanitise-uid uid)
                             (sanitise-uid relationship))))))))))

(defmethod get-dependent-resources ((db neo4cl:neo4j-rest-server)
                                    (sourcepath list))
  ;; Filter out any candidate nodes that do not have a _dependent_ relationship on the parent
  (remove-if
            #'null
            (mapcar #'(lambda (c)
                        (when (dependent-relationship-p
                                db
                                (car (butlast sourcepath))    ; Type of target-resource
                                (first c)                     ; Relationship to candidate
                                (second c))                   ; Type of candidate
                          c))
                    ;; Get all nodes to which this node has outbound relationships
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
                                               (uri-node-helper sourcepath
                                                                :path ""
                                                                :marker "n"
                                                                :directional t))))))))))))

(defmethod critical-dependency-p ((db neo4cl:neo4j-rest-server)
                                  (path list))
  ;; The path must end in a resource UID and be long enough to contain a dependency
  (log-message
    :debug
    (format nil "Checking whether ~{/~A~} ends with a critical dependency." path))
  (if (and
        (> (length path) 2)
        (= (mod (length path) 3) 2))
      ;; Path is valid.
      ;; Does the target even depend on this relationship?
      (if (dependent-relationship-p
            db
            (sanitise-uid (nth (- (length path) 5) path))    ; source-type
            (sanitise-uid (nth (- (length path) 3) path))    ; relationship
            (sanitise-uid (nth (- (length path) 2) path)))   ; target-type
          ;; Extract all _other_ links to the target resource,
          ;; and determine whether at least one is a dependent relationship.
          (let ((target-type (sanitise-uid (nth (- (length path) 2) path)))
                (candidates
                  (neo4cl:extract-rows-from-get-request
                    (neo4cl:neo4j-transaction
                      db
                      `((:STATEMENTS
                          ((:STATEMENT .
                            ,(format nil "MATCH ~A<-[r]-(n) RETURN type(r), labels(n)"
                                     (uri-node-helper path
                                                      :path ""
                                                      :marker "n"
                                                      :directional t))))))))))
            (log-message
              :debug
              (format nil "Extracted the following list of relationships to check for dependency: ~A"
                      candidates))
            ;; Now check for any dependent relationships in the returned list.
            ;; Invert the result of this test, because "no other dependent relationships"
            ;; is a positive answer to our question.
            (not
              (remove-if #'null
                         (mapcar #'(lambda (c)
                                     (dependent-relationship-p
                                       db
                                       target-type
                                       (first c)
                                       (car (second c))))
                                 candidates))))
          ;; The target does not depend on this relationship
          (log-message :debug "This is not a dependent path."))
      ;; Invalid path
      (error 'client-error :message "Path must end with a resource UID")))

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
                     (uri-node-helper (get-uri-parts sourcepath)
                                      :path ""
                                      :marker "a"
                                      :directional t)
                     relationship
                     (uri-node-helper (get-uri-parts destpath)
                                      :path ""
                                      :marker "b"
                                      :directional nil)))))))))

(defmethod delete-relationship-by-path ((db neo4cl:neo4j-rest-server)
                                        (relationship-uri string)
                                        (target-resource string))
  (log-message :debug (format nil "Attempting to delete the relationship ~A to ~A"
                              relationship-uri target-resource))
  (let* ((rel-parts (get-uri-parts relationship-uri))
         (source-type (car (last (butlast rel-parts 2))))
         (relationship (car (last rel-parts)))
         (dest-parts (get-uri-parts target-resource))
         (dest-type (first dest-parts))
         (dest-uid (second dest-parts)))
    (log-message :debug (format nil "Source type: ~A" source-type))
    (log-message :debug (format nil "Relationship: ~A" relationship))
    (log-message :debug (format nil "Dest type: ~A" dest-type))
    (log-message :debug (format nil "Dest UID: ~A" dest-uid))
    ;; Sanity checks
    (cond
      ;; Is the relationship URI valid?
      ((not (equal (mod (length rel-parts) 3) 0))
       (error 'client-error :message "This URI does not specify a relationship."))
      ;; Is the target URI valid?
      ((not (equal (mod (length dest-parts) 3) 2))
       (error 'client-error :message "Target path does not specify a resource."))
       ;; Is there a relationship defined between these types?
       ((not (or (get-relationship-attrs db source-type relationship dest-type)
                 (get-relationship-attrs db "any" relationship dest-type)))
        (error
          'client-error
          :message "There is no relationship between these resource-types. Are you sure there's something here to delete?"))
      ;; Would this orphan a dependent resource at the end of the relationship,
      ;; by removing its last parent?
      ((and
         ;; The first element in the list returned by get-relationship-attrs
         ;; is a boolean indicating whether it's a dependent relationship
         (relationship-attrs-dependent
           (get-relationship-attrs
             db
             ;; Be smart about which relationship we're checking here
             (if (get-relationship-attrs db source-type relationship dest-type)
               source-type
               "any")
             relationship dest-type))
         ;; Would this be the last parent?
         ;; Test by checking for other incoming dependent relationships.
         ;; If there's one or more, we're good to go.
         (let ((others (neo4cl:extract-rows-from-get-request
                         (neo4cl:neo4j-transaction
                           db
                           `((:STATEMENTS
                               ((:STATEMENT .
                                 ,(format nil "MATCH ~A<-[r]-(n) RETURN type(r), labels(n);"
                                          (uri-node-helper
                                            (append rel-parts dest-parts)
                                            :path ""
                                            :marker "n"
                                            :directional t))))))))))
           (log-message :debug (format nil "Found ~D other incoming relationships to the target resource"
                                       (length others)))
           ;; Either there are no others to check...
           (or (null others)
               ;; ...or there is at least one, but none of them is a dependent type
               (not (some #'(lambda (inc)
                              ;; Is there a :dependent value of 't in this incoming relationship?
                              (log-message
                                :debug
                                (format nil "Checking for dependencies in incoming relationship ~A from type ~A"
                                        (car inc) (car (second inc))))
                              (relationship-attrs-dependent
                                (get-relationship-attrs db dest-type (car inc) (car (second inc)))))
                          others)))))
       (error 'restagraph:integrity-error
              :message "This would leave an orphan dependent resource. Delete the dependent resource instead."))
      ;; Sanity-checks passed; let's try to make it happen
      (t
       (neo4cl:neo4j-transaction
         db
         `((:STATEMENTS
             ((:STATEMENT
                .
                ,(format nil "MATCH ~A-[r:~A]->(:~A {uid: '~A'}) DELETE r"
                         (uri-node-helper (butlast rel-parts) :path "" :marker "n" :directional t)
                         relationship
                         dest-type
                         dest-uid))))))))))

(defmethod delete-resource-by-path ((db neo4cl:neo4j-rest-server)
                                    (targetpath string)
                                    &key recursive)
  (log-message :debug (format nil "Attempting to delete resource ~A" targetpath))
  (let ((parts (get-uri-parts targetpath)))
    ;; The special case turns out to be a link to another resource
    ;; that does _not_ depend on this one.
    ;; Both other cases involve deleting the resource and then potentially
    ;; all resources depending on it.
    ;; If feasible, refactor this to remove the significant amount of duplication.
    ;;
    ;; Expected to use critical-dependency-p to answer some of these questions
    (if (equal (mod (length parts) 3) 2)
      ;; Do any other resources depend critically on this one?
      (let ((dependents (get-dependent-resources db parts)))
        (when dependents
          ;; Yes: it's a first-class resource with dependents.
          ;; Was the recursive argument supplied?
          (if recursive
            ;; Yes. Delete the dependents, passing the value of the recursive argument
            (progn
              (mapcar
                #'(lambda (d)
                    (let ((newpath (format nil "~{/~A~}" (append parts d))))
                      (log-message
                        :debug
                        (format nil "Recursing through delete-resource-by-path with new path ~A"
                                newpath))
                      (delete-resource-by-path db newpath :recursive t)))
                dependents)
              ;; Having deleted the dependents, delete the resource itself
              (neo4cl:neo4j-transaction
                db
                `((:STATEMENTS
                    ((:STATEMENT . ,(format nil "MATCH (n:~A { uid: '~A' }) DETACH DELETE n"
                                            (first parts) (second parts))))))))
            ;; Dependents, but no recursive argument. Bail out.
            ;; FIXME: return a list of the dependents, to help the client.
            (error 'integrity-error
                   :message
                   "Other resources depend critically on this one, and recursive was not specified.")))
        ;; First-class resource with no dependents: remove it.
        (neo4cl:neo4j-transaction
          db
          `((:STATEMENTS
              ((:STATEMENT . ,(format nil "MATCH ~A DETACH DELETE n" (uri-node-helper parts))))))))
      (error 'client-error :message "This is not a valid deletion request"))))

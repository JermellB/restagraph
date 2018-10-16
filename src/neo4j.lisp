;   Copyright 2017 James Fleming <james@electronic-quill.net>
;
;   Licensed under the Apache License, Version 2.0 (the "License");
;   you may not use this file except in compliance with the License.
;   You may obtain a copy of the License at
;
;       http://www.apache.org/licenses/LICENSE-2.0
;
;   Unless required by applicable law or agreed to in writing, software
;   distributed under the License is distributed on an "AS IS" BASIS,
;   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;   See the License for the specific language governing permissions and
;   limitations under the License.


;;;; Methods and functions specifically relating to Neo4J

(in-package #:restagraph)


;;;; Utility functions

(defun escape-neo4j (str)
  "Escape any undesirable characters in a string, e.g. the single-quote."
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
  (mapcar #'car
          (neo4cl::extract-rows-from-get-request
            (neo4cl:neo4j-transaction
              db
              `((:STATEMENTS
                  ((:STATEMENT . "MATCH (c:rgResource) RETURN c"))))))))

(defmethod describe-resource-type ((db neo4cl:neo4j-rest-server)
                                   (resourcetype string)
                                   &key recursive
                                   resources-seen)
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
        (:RELATIONSHIPS . ,(describe-dependent-resources
                             db
                             (sanitise-uid resourcetype)
                             :recursive recursive
                             :resources-seen resources-seen))))))

(defmethod describe-dependent-resources ((db neo4cl:neo4j-rest-server)
                                         (resourcetype string)
                                         &key recursive
                                         resources-seen)
  (log-message :debug (format nil "Describing resources linked from '~A'" resourcetype))
  (mapcar #'(lambda (row)
              (log-message :debug "Retrieving description for linked resourcetype '~A'" (fourth row))
              ;; Return an alist of the values, ready for rendering into Javascript
              `((:relationship . ,(first row))
                (:dependent . ,(if (second row) "true" "false"))
                (:cardinality . ,(third row))
                (:resourcetype
                  . ,(if recursive
                         (progn
                           ;; Add this resource-type to the list of those seen
                           (log-message :debug (format nil "Adding '~A' to the list of resources already seen" (fourth row)))
                           (pushnew (fourth row) resources-seen)
                           ;; Now recurse through this process
                           (describe-resource-type db
                                                   (fourth row)
                                                   :recursive t
                                                   :resources-seen resources-seen))
                         (fourth row)))))
          ;; Skip any resources we've already seen, to break loops
          (remove-if #'(lambda (row)
                         (member (fourth row) resources-seen :test #'equal))
                     (neo4cl:extract-rows-from-get-request
                       (neo4cl:neo4j-transaction
                         db
                         `((:STATEMENTS
                             ((:STATEMENT .
                               ,(format nil "MATCH (:rgResource {name: '~A'})-[r]->(n:rgResource) WHERE type(r) <> 'rgHasAttribute' RETURN type(r), r.dependent, r.cardinality, n.name"
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
  (mapcar #'(lambda (param)
              (cons (intern (escape-neo4j (string-downcase (car param))) :keyword)
                    (escape-neo4j (cdr param))))
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
      (if (or
            ;; If no attributes were specified other than "uid", we're good
            (not requested-attributes)
            ;; If other attributes were specified, check them all for validity.
            (log-message :debug "Checking the supplied attributes.")
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
        (format-post-params-as-properties
          (acons "uid" (sanitise-uid (cdr (assoc "uid" params :test #'string=)))
                 (acons "original_uid" (cdr (assoc "uid" params :test #'string=))
                        (remove-if #'(lambda (param) (equal (car param) "uid"))
                                   params))))))
    ;; No such resourcetype
    (signal 'client-error
            :message
            (format nil "There is no resourcetype called '~A'" resourcetype))))


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
      (let ((attributes (validate-resource-before-creating db resourcetype attributes)))
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
                    ((:STATEMENT . ,(format nil "CREATE (:~A { properties })"
                                            (sanitise-uid resourcetype)))
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
          (error 'restagraph:integrity-error :message "Requested resource type does not exist"))))))

(defmethod update-resource-attributes ((db neo4cl:neo4j-rest-server)
                                       (path list)
                                       (attributes list))
  (log-message :debug (format nil "Updating attributes for resource ~{/~A~}" path))
  (let ((attrs
          (remove-if #'(lambda (f)
                         (or (equal (car f) :|uid|)
                             (equal (car f) :|original_uid|)))
                     (validate-resource-before-creating
                       db
                       (car (last (butlast path)))
                       attributes))))
    (when attrs
      (log-message
        :debug
        (format nil "Applying the attributes ~{~A~^, ~} to resource ~{/~A~}" attrs path))
      (let ((query (format nil "MATCH ~A SET ~{~A~^, ~}"
                           (uri-node-helper path :path "" :marker "n" :directional t)
                           (mapcar #'(lambda (a)
                                       (if (null (cdr a))
                                           (format nil "n.~A = NULL" (car a))
                                           (format nil "n.~A = '~A'" (car a) (cdr a))))
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
   Assumes uri-node-helper was called with its default marker, which is 'n'."
  (log-message :debug "Attempting to process filter ~A" filter)
  (let* ((name (car filter))
         (value (cdr filter))
         ;; Does the value start with "!" to indicate negation?
         (negationp (string= "!" value :end2 1)))
    (if negationp
        (log-message :debug "Negation detected. negationp = ~A" negationp)
        (log-message :debug "Negation not detected. Double-negative in progress."))
    ;; Prepend negation if applicable
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
         (format nil "n.~A = '~A'" (escape-neo4j name) (escape-neo4j value)))))))

(defun process-filters (filters)
  "Take GET parameters, and turn them into a string of Neo4j WHERE clauses."
  (log-message :debug (format nil "Attempting to process filters ~A" filters))
  (let ((result (mapcar #'process-filter filters)))
    (log-message :debug "Result of filter processing: ~A" result)
    (if result
        (let ((response (format nil " WHERE ~{ ~A~^ AND~}" result)))
          (log-message :debug "Output from process-filters: ~A." response)
          response)
        "")))

;;; FIXME: turn &optional into &key, and add :directional
;;; so GET requests can accept a directional parameter from the client
(defmethod get-resources ((db neo4cl:neo4j-rest-server)
                          (uri string)
                          &optional filters)
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
                                             :directional nil)
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
                                             :directional nil))))
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
                                             :directional nil)
                            (process-filters filters))))
         (log-message :debug (concatenate 'string "Querying database: "
                                          (cl-ppcre:regex-replace "\~" query "~~")))
         (let* ((response
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

(defmethod get-relationship-attrs ((db neo4cl:neo4j-rest-server)
                                   (source-type string)
                                   (relationship string)
                                   (dest-type string))
  (log-message
    :debug
    (format nil "Retrieving the dependency and cardinality attributes of relationship ~A from ~A to ~A"
            relationship source-type dest-type))
  (car
    (neo4cl:extract-rows-from-get-request
      (neo4cl:neo4j-transaction
        db
        `((:STATEMENTS
            ((:STATEMENT
               .  ,(format nil "MATCH (:rgResource {name: '~A'})-[r:~A]->(:rgResource {name: '~A'}) RETURN r.dependent, r.cardinality"
                           (sanitise-uid source-type)
                           (sanitise-uid relationship)
                           (sanitise-uid dest-type))))))))))

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
              (relationship-attrs
                (or
                  (get-relationship-attrs db source-type relationship dest-type)
                  (get-relationship-attrs db "any" relationship dest-type))))
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
              db (format nil "~{/~A~}" source-parts) relationship destpath)
            (let ((message "Relationship already exists"))
              (log-message :debug message)
              (error 'integrity-error :message message)))
           ;; Do both the source and destination resources actually exist?
           ((null (get-resources db (format nil "/~{~A~^/~}" source-parts)))
            (let ((message (format nil "The source resource /~{~A~^/~} does not exist\n" source-parts)))
              (log-message :debug message)
              (error 'client-error :message message)))
           ((null (get-resources db destpath))
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
                  (equal (second relationship-attrs) "1:1")
                  (equal (second relationship-attrs) "many:1"))
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
                             (second relationship-attrs)
                             relationship
                             dest-type))
              ;; Constraints are fine; create it
              (neo4cl:neo4j-transaction
                db
                `((:STATEMENTS
                    ((:STATEMENT .
                                 ,(format nil "MATCH ~A CREATE (n)-[:~A]->(:~A { properties })"
                                          (uri-node-helper parent-parts
                                                           :path ""
                                                           :marker "n"
                                                           :directional t)
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
                                        (relpath string)
                                        (targetpath string))
  (let* ((parts (get-uri-parts relpath))
         (rel-path (butlast parts))
         (relationship (car (last parts)))
         (target-parts (get-uri-parts targetpath))
         (source-type (nth (- (length parts) 2) parts))
         (dest-type (nth (- (length target-parts) 1) target-parts)))
    ;; Initial sanity checks
    (cond
      ((not (equal (mod (length parts) 3) 0))
       (error 'client-error :message "Relationship path does not specify a relationship"))
      ((not (equal (mod (length target-parts) 3) 2))
       (error 'client-error :message "Target path does not specify a relationship"))
      (t
       ;; Check also for dependent relationships
       (let ((relationship-attrs
               (get-relationship-attrs db source-type relationship dest-type)))
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
                                    (uri-node-helper target-parts
                                                     :path ""
                                                     :marker "n"
                                                     :directional t))))))))
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
                             (uri-node-helper rel-path
                                              :path ""
                                              :marker "n"
                                              :directional t)
                             relationship
                             (uri-node-helper target-parts
                                              :path ""
                                              :marker "n"
                                              :directional t))))))))))))))

(defmethod delete-resource-by-path ((db neo4cl:neo4j-rest-server)
                                    (targetpath string)
                                    &key delete-dependent
                                    recursive)
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
        ;; Is this a first-class resource?
        (if (= (length parts) 2)
            ;; First-class resource
            ;; Do any other resources depend critically on this one?
            (let ((dependents (get-dependent-resources db parts)))
              (if dependents
                  ;; Yes: it's a first-class resource with dependents.
                  ;; Was the recursive argument supplied?
                  (if recursive
                      ;; Yes. Delete the dependents, passing both the delete-dependent
                      ;; and recursive arguments.
                      (progn
                        (mapcar
                          #'(lambda (d)
                              (let ((newpath (format nil "/~{~A~^/~}" (append parts d))))
                                (log-message
                                  :debug
                                  (format nil "Recursing through delete-resource-by-path with new path ~A"
                                          newpath))
                                (delete-resource-by-path
                                  db
                                  newpath
                                  :delete-dependent t
                                  :recursive t)))
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
                             "Other resources depend critically on this one, and recursive was not specified."))
                  ;; First-class resource with no dependents: remove it.
                  (neo4cl:neo4j-transaction
                    db
                    `((:STATEMENTS
                        ((:STATEMENT . ,(format nil "MATCH (n:~A { uid: '~A' }) DETACH DELETE n"
                                                (first parts) (second parts)))))))))
            ;; Either a dependent resource, or a link to another resource.
            ;; Does this resource's existence depend on the presence of this link?
            (if (critical-dependency-p db parts)
                ;; Yes: this is a critical dependency for this resource.
                ;; Did the client expect that?
                (if delete-dependent
                    ;; Client wants to delete a dependent resource.
                    ;; Do any other resources depend on this one?
                    (let ((dependents (get-dependent-resources db parts)))
                      (if dependents
                          ;; There are dependent resources.
                          ;; Was recursive specified?
                          (if recursive
                              ;; Yes. Delete the dependents,
                              ;; passing the delete-dependent and recursive arguments
                              (progn
                                (mapcar #'(lambda (d)
                                            (delete-resource-by-path
                                              db
                                              (format nil "~{/~A~}"
                                                      (append parts d))
                                              :delete-dependent t
                                              :recursive t))
                                        dependents)
                                ;; Having deleted the dependents, delete the resources itself
                                (let ((delpath (format nil "MATCH ~A DETACH DELETE n"
                                                       (uri-node-helper parts
                                                                        :path ""
                                                                        :marker "n"
                                                                        :directional t))))
                                  (neo4cl:neo4j-transaction
                                    db
                                    `((:STATEMENTS ((:STATEMENT .  ,delpath)))))))
                              ;; Dependents, but no recursive argument. Bail out.
                              ;; FIXME: return a list of the depdendents, to help the client.
                              (error 'integrity-error
                                     :message
                                     "Other resources depend critically on this one, and recursive was not specified."))
                          ;; Dependent resource with no dependents of its own. Delete it.
                          (let ((delpath (format nil "MATCH ~A DETACH DELETE n"
                                                 (uri-node-helper parts
                                                                  :path ""
                                                                  :marker "n"
                                                                  :directional t))))
                            (neo4cl:neo4j-transaction
                              db
                              `((:STATEMENTS ((:STATEMENT .  ,delpath))))))))
                    ;; This is a critical dependency, and delete-dependent is absent
                    (error 'integrity-error :message "This would delete the resource itself, and delete-dependent was not specified."))
                ;; No critical dependency; we're just deleting a relationship. Do it.
                (let* ((parent-path (uri-rel-helper (butlast parts 2)
                                                    :path ""
                                                    :marker "n"
                                                    :directional t))
                       (restype (car (last parts 2)))
                       (uid (car (last parts)))
                       (query (format nil "MATCH ~A->(:~A {uid: '~A'}) DELETE n"
                                      parent-path restype uid)))
                  (log-message
                    :debug
                    (format nil "Attempting to delete relationship with query '~A'" query))
                  (neo4cl:neo4j-transaction
                    db `((:STATEMENTS ((:STATEMENT .  ,query))))))))
        (error 'client-error :message "This is not a valid deletion request"))))

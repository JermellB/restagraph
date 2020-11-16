;   Copyright 2020 James Fleming <james@electronic-quill.net>
;
;   Licensed under the GNU General Public License
;   - for details, see LICENSE.txt in the top-level directory

;;;; Resource-related methods

(in-package #:restagraph)


(defgeneric store-resource (db resourcetype attributes schema)
  (:documentation "Store a resource in the database. Attributes argument is expected in the form of an alist.
Return an error if
- the resource type is not present in the schema
- the client attempts to set attributes that aren't defined for this resourcetype."))

(defmethod store-resource ((db neo4cl:neo4j-rest-server)
                           (resourcetype string)
                           ;; `attributes` is an alist, where the car is the name
                           ;; and the cdr is the value
                           (attributes list)
                           schema)
  (cond
    ;; Catch any critical deficiencies in the definition asap
    ((or (null (assoc "uid" attributes :test 'equal))
         (equal (cdr (assoc "uid" attributes :test 'equal)) ""))
     (error 'client-error
            :message "The UID must be a non-empty string"))
    ;; If this is a dependent resource, bail out now
    ((dependent-resource-p schema resourcetype)
     (let ((message "This is a dependent resource; it must be created as a sub-resource of an existing resource."))
       (log-message :warn message)
       (error 'integrity-error :message message)))
    ;; OK so far: carry on
    (t
      (handler-case
        (let ((attributes (validate-resource-before-creating schema
                                                             resourcetype
                                                             attributes)))
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
            (error 'restagraph:client-error
                   :message (message e))))))))


(defgeneric store-dependent-resource (db uri attributes schema)
  (:documentation "Create a dependent resource, at the end of the path given by URI. Its parent resource must exist, and the relationship must be a valid dependent relationship."))

(defmethod store-dependent-resource ((db neo4cl:neo4j-rest-server)
                                     (uri string)
                                     (attributes list)
                                     schema)
  (log-message :debug (format nil "Attempting to create a dependent resource at path ~A" uri))
  (let* ((uri-parts (get-uri-parts uri))
         (relationship (car (last (butlast uri-parts))))
         (parent-parts (butlast uri-parts 2))
         (parent-type (nth (- (length parent-parts) 2) parent-parts))
         (dest-type (car (last uri-parts)))
         (dest-uid (sanitise-uid (cdr (assoc "uid" attributes :test 'equal))))
         (relationship-attrs (car (get-relationship-attrs schema
                                                          parent-type
                                                          relationship
                                                          dest-type))))
    (log-message :debug "Relationship attributes found: ~A" relationship-attrs)
    (log-message :debug "Beginning sanity checks")
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
       (let ((message (format nil "There is no relationship ~A from ~A to ~A"
                              relationship parent-type dest-type)))
         (log-message :error message)
         (error 'client-error :message message)))
      ;; Sanity check: dependency between parent and child resource types
      ((null (schema-rels-dependent relationship-attrs))
       (let ((message
               (format nil "Target resource-type ~A doesn't depend on the parent type ~A"
                       dest-type parent-type)))
         (log-message :debug message)
         (error 'client-error :message message)))
      ;; Sanity check: is this a dependent resource type?
      ((not (dependent-resource-p schema dest-type))
       (let ((message "This is not a dependent resource type"))
         (log-message :debug message)
         (error 'client-error :message message)))
      ;; Passed the initial sanity-checks; try to create it.
      (t
        (log-message :debug "Sanity checks passed. Attempting to create the resource.")
       ;; Validate the supplied attributes
       (let* ((validated-attributes (validate-resource-before-creating
                                      schema
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
                    (equal (schema-rels-cardinality relationship-attrs) "1:1")
                    (equal (schema-rels-cardinality relationship-attrs) "many:1"))
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
                               (schema-rels-cardinality relationship-attrs)
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


(defgeneric move-dependent-resource (db uri newparent schema)
  (:documentation "Take an existing dependent resource, and give it a new parent, where both are identified by their URI paths."))

(defmethod move-dependent-resource ((db neo4cl:neo4j-rest-server)
                                    (uri string)
                                    (newparent string)
                                    schema)
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
      ((not (dependent-relationship-p schema new-parent-type new-relationship target-type))
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


(defgeneric get-resources (db uri &key filters directional)
  (:documentation "Adaptable method to search for resources in a manner deterined by the modulo-3 length of the URI.
The optional 'filters parameter is for refining the search results."))

;; Helper function
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

;; Helper function
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
                                             :marker "n"
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


(defgeneric get-dependent-resources (db sourcepath schema)
  (:documentation "Return a list of the resources that depend critically on this one.
The returned list contains 3-element lists of relationship, type and UID."))

(defmethod get-dependent-resources ((db neo4cl:neo4j-rest-server)
                                    (sourcepath list)
                                    schema)
  (log-message :debug "Searching for resources dependent on parent ~{/~A~}" sourcepath)
  ;; Get all nodes to which this node has outbound relationships
  (let ((candidates
          (mapcar
            #'(lambda (row)
                ;; List elements: relationship, target type, target UID
                ;; The Neo4j operator `labels(n)` returns a list, hence the (car (second row)).
                (list (first row) (car (second row)) (third row)))
            (neo4cl:extract-rows-from-get-request
              (neo4cl:neo4j-transaction
                db
                `((:STATEMENTS
                    ((:STATEMENT
                       .  ,(format nil "MATCH ~A-[r]->(b) RETURN type(r), labels(b), b.uid"
                                   (uri-node-helper sourcepath
                                                    :path ""
                                                    :marker "n"
                                                    :directional t)))))))))))
    (log-message :debug "Retrieved candidate links ~A" candidates)
    ;; Filter out any candidate nodes that do not have a _dependent_ relationship on the parent
    (remove-if
      #'null
      (mapcar #'(lambda (c)
                  (when (dependent-relationship-p
                          schema
                          (car (butlast sourcepath))    ; Type of target-resource
                          (first c)                     ; Relationship to candidate
                          (second c))                   ; Type of candidate
                    c))
              candidates))))



;; Suspected to be unused
#+(or)
(defgeneric critical-dependency-p (db path)
  (:documentation "Determine whether the resource identified by this path depends solely on its relationship to its immediate parent on that path.
Return a boolean."))

;; Suspected to be unused
#+(or)
(defmethod critical-dependency-p ((db neo4cl:neo4j-rest-server)
                                  (path list)
                                  schema)
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
            schema
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
                                       schema
                                       target-type
                                       (first c)
                                       (car (second c))))
                                 candidates))))
          ;; The target does not depend on this relationship
          (log-message :debug "This is not a dependent path."))
      ;; Invalid path
      (error 'client-error :message "Path must end with a resource UID")))


(defgeneric update-resource-attributes (db path attributes schema)
  (:documentation "Add, update or delete a set of attributes of a given resource."))

(defmethod update-resource-attributes ((db neo4cl:neo4j-rest-server)
                                       (path list)
                                       (attributes list)
                                       schema)
  (log-message :debug (format nil "Updating attributes for resource ~{/~A~}" path))
  (let ((attrs
          (append
            (remove-if #'(lambda (f)
                           (or (equal (car f) :|uid|)
                               (equal (car f) :|original_uid|)))
                       (validate-resource-before-creating
                         schema
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


(defgeneric delete-resource-by-path (db targetpath schema &key recursive)
  (:documentation "Delete a relationship or resource according to the URI supplied.
  :recursive confirms that you intend to delete all resources depending on the one identified in the path."))

(defmethod delete-resource-by-path ((db neo4cl:neo4j-rest-server)
                                    (targetpath string)
                                    schema
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
      (let ((dependents (get-dependent-resources db parts schema)))
        (if dependents
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
                      (delete-resource-by-path db newpath schema :recursive t)))
                dependents)
              ;; Having deleted the dependents, delete the resource itself
              (log-message :debug "Deleting target resource ~A" targetpath)
              (neo4cl:neo4j-transaction
                db
                `((:STATEMENTS
                    ((:STATEMENT . ,(format nil "MATCH (n:~A { uid: '~A' }) DETACH DELETE n"
                                            (first parts) (second parts))))))))
            ;; Dependents, but no recursive argument. Bail out.
            (error 'integrity-error
                   :message
                   "Other resources depend critically on this one, and recursive was not specified."))
          ;; First-class resource with no dependents: remove it.
          (let ((query (format nil "MATCH ~A DETACH DELETE n" (uri-node-helper parts))))
            (log-message :debug "No dependents. Deleting resource ~A with query '~A'"
                         targetpath query)
            (neo4cl:neo4j-transaction
              db
              `((:STATEMENTS
                  ((:STATEMENT . ,query))))))))
      (error 'client-error :message "This is not a valid deletion request"))))


(defgeneric delete-resource-attributes (db path attributes)
  (:documentation "Delete attributes from a resource."))

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

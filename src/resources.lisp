;   Copyright 2020-2022 James Fleming <james@electronic-quill.net>
;
;   Licensed under the GNU General Public License
;   - for details, see LICENSE.txt in the top-level directory

;;;; Resource-related methods

(in-package #:restagraph)

(declaim (optimize (compilation-speed 0)
                   (speed 2)
                   (safety 3)
                   (debug 3)))


(defgeneric store-resource (db schema resourcetype attributes creator-uid)
  (:documentation "Store a resource in the database. Attributes argument is expected in the form of an alist.
Return the UID on success.
Return an error if
- the resource type is not present in the schema
- the client attempts to set attributes that aren't defined for this resourcetype."))

(defmethod store-resource ((db neo4cl:bolt-session)
                           (schema hash-table)
                           (resourcetype string)
                           ;; `attributes` is an alist, where the car is the name
                           ;; and the cdr is the value
                           (attributes list)
                           (creator-uid string))
  (log-message :debug (format nil "Attempting to store a resource of type '~A' with attributes ~{~A~^, ~}"
                              resourcetype attributes))
  ;; Initial sanity-check
  (if (null (assoc "uid" attributes :test #'equal))
    (error 'client-error :message "UID must be specified")
    ;; More sanity-checks.
    ;; Also derive the requested UID once, to save lots of repeated code.
    (let* ((uid (cdr (assoc "uid" attributes :test #'equal)))
           (sanitised-uid (sanitise-uid uid)))
      (cond
        ;; Do we even have this resourcetype?
        ((null (gethash resourcetype schema))
         (error 'client-error :message "No such resourcetype."))
        ;; Did the client provide a non-empty UID?
        ((equal "" uid)
         (error 'client-error :message "The UID must be a non-empty string"))
        ;; If this is a dependent resource, bail out now
        ((dependent (gethash resourcetype schema))
         (let ((message "This is a dependent resource; it must be created as a sub-resource of an existing resource."))
           (log-message :warn message)
           (error 'integrity-error :message message)))
        ;; Do we already have one of these?
        ((get-resources db (format nil "/~A/~A" resourcetype sanitised-uid))
         (let ((message (format nil "/~A/~A already exists; refusing to create a duplicate."
                                resourcetype sanitised-uid)))
           (log-message :warn message)
           (error 'integrity-error :message message)))
        ;; OK so far: carry on
        (t
          (let* ((attributes (append (validate-resource-before-creating
                                       schema
                                       resourcetype
                                       attributes)
                                     `(("createddate" . ,(get-universal-time)))))
                 (query (format
                          nil
                          "MATCH (c:People {uid: '~A'}) CREATE (:~A { ~A })-[:RG_CREATOR]->(c)"
                          creator-uid
                          (sanitise-uid resourcetype)
                          ;; This odd-looking chunk of code autogenerates a set of references
                          ;; to the query-parameter map.
                          (format nil "~{~A: $~A~^, ~}"
                                  (let ((acc '()))
                                    (mapcar #'(lambda (param)
                                                (push (car param) acc) (push (car param) acc))
                                            attributes)
                                    acc)))))
            ;; If we got this far, we have a valid resource type and valid attribute names.
            ;; Make it happen
            (log-message :debug (format nil "Creating a '~A' resource with query '~A' and attributes ~A"
                                        resourcetype query attributes))
            (handler-case
              (progn
                (neo4cl:bolt-transaction-autocommit db query :parameters attributes)
                ;; Return the new UID
                sanitised-uid)
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
                    (error 'integrity-error :message (neo4cl:message e)))
                  ;; Otherwise, just resignal it
                  (let ((text (format nil "Database error ~A.~A: ~A"
                                      (neo4cl:category e)
                                      (neo4cl:title e)
                                      (neo4cl:message e))))
                    (log-message :error text)
                    (error 'client-error :message text)))))))))))


(defun canonicalise-path (schema path target &optional acc)
  "Check the supplied path against the schema, to find the subset of it that represents a valid
   canonical path to a dependent resource.
   - `target` is a 2-element list of strings, representing its resourcetype and UID, in that order.
   - `path` is a list of strings representing the `/type/uid/relationship` path to the target,
   and it must be 0 modulo 3 in order to be valid."
  (declare (type hash-table schema)
           (type list path target acc))
  (log-message
    :debug
    (format nil "Checking canonical path for target /~A/~A, with path ~{/~A~} and accumulator ~{/~A~}."
            (first target) (second target) path acc))
  ;; Extract the type, UID and relationship for the immediate parent
  (let* ((parts (last path 3))
         (parent-rtype (first parts))
         (parent-uid (second parts))
         (relationship (third parts))
         ;; Also pull the resourcetype definition for the parent, from the in-memory schema.
         (parent-typedef (gethash parent-rtype schema)))
      (log-message :debug (format nil "Checking relationship /~A/~A/~A."
                                  parent-rtype relationship (first target)))
    (cond
      ;; Lead with the sanity checks
      ((not (= 2 (length target)))
       (error "Invalid target; its length should be 2."))
      ((not (= 0 (mod (length path) 3)))
       (error "Invalid path; its length should be an integer multiple of 3."))
      ;; Is this a primary resource?
      ((and (null parts)
            (not (dependent (gethash (first target) schema))))
       (log-message :debug "Primary resource identified.")
       target)
      ;; Is this a direct path to a dependent resource?
      ((null parts)
       (error "Direct path to a dependent resource. This is not valid."))
      ;; Is this relationship dependent?
      ;; If not, this isn't a viable candidate, and we can stop here.
      ((not (equal "dependent"
                   (reltype (get-relationship schema
                                              parent-rtype
                                              relationship
                                              (first target)))))
       ;; Explicitly return `nil
       nil)
      ;; Have we reached the root?
      ((not (dependent parent-typedef))
       ;; If so, assemble the path and return it.
       (append parts target acc))
      ;; If we got here, it's a dependent relationship from a dependent resourcetype.
      ;; Thus, it's another element in the path.
      ;; It's recursin' time!
      (t
       (canonicalise-path
         schema
         (butlast path 3)
         (list parent-rtype parent-uid)
         (append (list relationship) target acc))))))


(defgeneric get-canonical-path (db schema path)
  (:documentation "Get the canonical URI path(s) to a resource.
                   Canonical means that it starts with a primary resource, and follows only dependent
                   relationships to the target, thus representing its identity.
                   Returns a list of strings, which should have a maximum length of 1."))

(defmethod get-canonical-path ((db neo4cl:bolt-session)
                               (schema hash-table)
                               (path string))
  (let* ((uri-parts (get-uri-parts path))
         (target-type (gethash (car (last uri-parts 2)) schema)))
    ;; Is it a primary or dependent type?
    (cond
      ;; Is it a valid path to a resource?
      ((not (= 2 (mod (length uri-parts) 3)))
       (error 'client-error :message "This is not a valid path to a single resource."))
      ;; Does it even exist?
      ((not (get-resources db path))
       (error 'client-error :message "The requested resource does not exist."))
      ;;
      ;; It's a primary resourcetype.
      ((and (not (dependent target-type))
            (canonicalise-path schema
                              '()
                              (list (car (last uri-parts 2)) (car (last uri-parts)))))
       ;; Return the resource's path, discarding any indirection that precedes it
       ;; in the supplied path.
       ;; I.e, if the path follows several relationships to eventually reach a primary resource,
       ;; just return the resource at the end of that path.
       (list (format nil "/~A/~A" (name target-type) (car (last uri-parts)))))
      ;;
      ;; If we got here, it's a valid dependent type.
      ;; Now we go forensic on its ass.
      (t
        ;; First, trace backwards along the supplied path, and check whether it includes
        ;; a canonical path.
        (let ((acc (canonicalise-path
                     schema
                     (butlast uri-parts 2)
                     (last uri-parts 2))))
          ;; This strategy takes into account the fact that Neo4j will exclude the MATCH path
          ;; from the list of inbound connections to the target resource, when assembling its
          ;; return value:
          ;; The obvious approach is to use a single Cypher query, but that won't work because we
          ;; need to check whether each resource and relationship on the path is a dependent one.
          ;; Second, trace backwards along the supplied path, checking for other branches
          ;; that produce canonical paths.
          (list (format nil "~{/~A~}" acc)))))))


(defgeneric store-dependent-resource (db schema uri attributes creator-uid)
  (:documentation "Create a dependent resource, at the end of the path given by URI. Its parent resource must exist, and the relationship must be a valid dependent relationship."))

(defmethod store-dependent-resource ((db neo4cl:bolt-session)
                                     (schema hash-table)
                                     (uri string)
                                     (attributes list)
                                     (creator-uid string))
  (log-message :debug (format nil "Attempting to create a dependent resource at path ~A" uri))
  (let* ((uri-parts (get-uri-parts uri))
         (relationship (car (last (butlast uri-parts))))
         (parent-parts (butlast uri-parts 2))
         (parent-type (nth (- (length parent-parts) 2) parent-parts))
         (dest-type (car (last uri-parts)))
         (dest-uid (sanitise-uid (cdr (assoc "uid" attributes :test 'equal))))
         (relationship-attrs (get-relationship schema parent-type relationship dest-type))
         (validated-attributes
           (validate-resource-before-creating
             schema
             dest-type
             (remove-if #'(lambda (param) (equal (car param) "type"))
                        attributes)))
         (resource-path
           (format nil "~{/~A~}/~A/~A/~A"
                   parent-parts
                   relationship
                   dest-type
                   (cdr (assoc "uid" validated-attributes :test #'string=)))))
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
      ((not (equal "dependent" (reltype relationship-attrs)))
       (let ((message
               (format nil "Target resource-type '~A' doesn't depend on the parent type '~A' for relationship '~A': ~A"
                       dest-type parent-type relationship (a-listify relationship-attrs))))
         (log-message :debug message)
         (error 'client-error :message message)))
      ;; Sanity check: is this a dependent resource type?
      ((not (dependent (gethash dest-type schema)))
       (let ((message "This is not a dependent resource type"))
         (log-message :debug message)
         (error 'client-error :message message)))
      ;; Sanity check: if it's a 1:1 relationship,
      ;; does the parent already have one with an instance of this type?
      ((and (equal (cardinality relationship-attrs) "1:1")
            ;; Look for this parent having this relationship with any other dependent resource
            (get-resources db (format nil "~{/~A~}" (butlast uri-parts))))
       #+(or)
       ;; FIXME: this produces a Bolt deserialisation error under some circumstances.
       (> (length
            (cdr (assoc "count"
                        (car (neo4cl:bolt-transaction-autocommit
                               db
                               (format nil "MATCH ~A<-[r:~A]-() RETURN count(r)"
                                       (uri-node-helper parent-parts
                                                        :path ""
                                                        :marker "n")
                                       relationship)))
                        :test #'equal))
            0))
       (error 'integrity-error :message
              (format nil"~{~A~^/~} already has a ~A ~A relationship with a resource of type ~A"
                      parent-parts
                      (cardinality relationship-attrs)
                      relationship
                      dest-type)))
          ;; Duplicate prevention: does the target resource already exist?
          ((get-resources db resource-path)
           (error 'integrity-error :message (format nil "Resource ~A already exists" resource-path)))
          ;; Passed the initial sanity-checks; try to create it.
          (t
            (log-message :debug "Sanity checks passed. Attempting to create the resource.")
            ;; Report on the attributes for debugging
            (log-message :debug (format nil "Validated attributes: ~A" validated-attributes))
            ;; Constraints are fine; create it
            (let ((params (append validated-attributes
                                  `(("createddate" . ,(get-universal-time))
                                    ("RGcreator_uid" . ,creator-uid)))))
              (neo4cl:bolt-transaction-autocommit
                db
                (format
                  nil
                  "MATCH ~A, (c:People {uid: $RGcreator_uid}) CREATE (n)-[:~A]->(:~A { ~A })-[:RG_CREATOR]->(c)"
                  (uri-node-helper parent-parts
                                   :path ""
                                   :marker "n")
                  relationship
                  dest-type
                  (format nil "~{~A: $~A~^, ~}"
                          (let ((acc '()))
                            (mapcar #'(lambda (param)
                                        (push (car param) acc) (push (car param) acc))
                                    params)
                            acc)))
                :parameters params))))))


(defgeneric move-dependent-resource (db schema uri newparent)
  (:documentation "Take an existing dependent resource, and give it a new parent, where both are identified by their URI paths."))

(defmethod move-dependent-resource ((db neo4cl:bolt-session)
                                    (schema hash-table)
                                    (uri string)
                                    (newparent string))
  (log-message :debug
               (format nil "Attempting to move dependent resource ~A to new parent ~A"
                       uri newparent))
  (let* ((uri-parts (get-uri-parts uri))
         (dest-parts (get-uri-parts newparent))
         ;; Reassemble the parent path by removing the dependent resource from the source path.
         ;; This is so we can delete this relationship after creating the new one.
         (current-parent-path (uri-node-helper (butlast uri-parts 3)
                                               :path ""
                                               :marker "b"))
         ;; It's a dependent resource, so the length of this path
         ;; will always be longer than 2:
         (current-relationship (car (last (butlast uri-parts 2))))
         (target-type (car (last (butlast uri-parts)))) ; Dependent resourcetype
         (target-uid (car (last uri-parts)))  ; Dependent UID
         (new-relationship-type (car (last dest-parts)))
         ;; The new parent may have a 2-element path,
         ;; in which case we don't need to extract the last 2 elements:
         (new-parent-type (car (if (> (length dest-parts) 2)
                                 (last (butlast dest-parts 2))
                                 dest-parts)))
         (new-relationship-details
           (get-relationship schema new-parent-type new-relationship-type target-type))
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
      ;; Sanity-check: is the new relationship a valid one?
      ((not new-relationship-details)
       (let ((message
               (format nil "New parent-type ~A does not have relationship ~A to target resource ~A"
                       new-parent-type new-relationship-type target-type)))
         (log-message :debug message)
         (error 'client-error :message message)))
      ;; Sanity-check: is the new relationship dependent?
      ((not (equal "dependent" (reltype new-relationship-details)))
       (progn
         (log-message
           :debug
           (format
             nil
             "Target resource ~A does not depend on the new parent-type ~A for relationship ~A"
             target-type new-parent-type new-relationship-type))
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
                                  :marker "t"))
               (destpath (format nil "~A-[:~A]->(:~A {uid: '~A'})"
                                 new-parent-path
                                 new-relationship-type
                                 target-type
                                 target-uid)))
          (log-message
            :debug
            (format nil "Moving target ~A to new parent ~A" sourcepath destpath))
          ;; Create the new relationship
          (neo4cl:bolt-transaction-autocommit
            db
            (format nil "MATCH ~A MATCH ~A CREATE (m)-[:~A]->(t)"
                    new-parent-path
                    sourcepath
                    new-relationship-type)))
        ;; Confirm that the new relationship is actually present.
        ;; If the MATCH clause matched nothing, it'll return OK.
        ;; We want to check this every time, and bail out if we detect that it failed.
        (unless (get-resources db new-path)
          (error 'integrity-error :message (format nil "New path ~A was not created." new-path)))
        ;; Delete the old relationship, using all but the last two elements of the source path
        (neo4cl:bolt-transaction-autocommit
          db
          (format nil "MATCH ~A-[r:~A]->(t:~A {uid: '~A'}) DELETE r"
                  current-parent-path
                  current-relationship
                  target-type
                  target-uid))))))

(defun process-filter (filter schema rtype)
  "Process a single filter from a GET parameter, expecting a dotted cons.
  Assumes uri-node-helper was called with its default marker, which is 'n'.
  Returns NIL when the cdr of the filter is NIL.
  Helper-function for `process-filters`."
  (declare (type list filter)
           (type hash-table schema) ; Specific to checking enums
           (type string rtype))     ; Specific to checking enums
  (log-message :debug (format nil "Attempting to process filter ~A for resourcetype ~A" filter rtype))
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
     (let* ((name (car filter))
            ;; Note that the attribute is an object,
            ;; so we need to inspect its class for type-matching.
            (attribute (get-attribute (gethash rtype schema) name))
            ;; Does the value start with "!" to indicate negation?
            (negationp (string= "!" (cdr filter) :end2 1))
            ;; Get the value of the expression.
            ;; If it's negated, drop the leading `!`.
            (value (if negationp
                     (subseq (cdr filter) 1)
                     (cdr filter))))
       (log-message :debug (format nil "De-negated value: '~A'" value))
       ;; Log whether negation was detected
       (if negationp
         (log-message :debug (format nil "Negation detected. negationp = ~A" negationp))
         (log-message :debug "Negation not detected. Proceeding in the affirmative."))
       (if attribute
           (log-message :debug (format nil "Fetched attribute with name ~A." (name attribute)))
           (log-message :debug (format nil "Didn't find an attribute with name ~A." name)))
       (format
         nil
         "~A~A"
         ;; Are we negating it?
         (if negationp "NOT " "")
         ;; Infer the operator
         (cond
           ;; Outbound links
           ;; Simple format: relationship/path/to/target
           ((equal name "RGoutbound")
            (let* ((parts (get-uri-parts value))
                   (relationship (first parts)))
              (log-message :debug (format nil "Outbound link detected: ~A" value))
              ;; FIXME: needs sanity-checking that (not (equal (mod (length parts) 3) 1))
              ;; because that would be an outbound relationship to a relationship,
              ;; which makes no sense on the face of it.
              ;; Strictly, we could allow this by appending `->()` to it, for a test that it has
              ;; that kind of relationship to _anything_, which _does_ make sense.
              ;; In fact, `uri-node-helper` may well do  this automatically for us, so there may be
              ;; nothing to do beyond documenting the behaviour.
              (format nil "(n)-[:~A]->~A" relationship (uri-node-helper (cdr parts) :marker ""))))
           ;; Inbound links
           ;; Simple format: /path/to/source/relationship
           ((equal name "RGinbound")
            (log-message :debug (format nil "Inbound link detected: ~A" value))
            ;; Do basically the same thing as get-uri-parts, *except*
            ;; don't sanitise the wildcard #\* character.
            (uri-node-helper (loop for val in (cdr (cl-ppcre:split "/" value))
                                   for i from 1
                                   collecting (if (and (equal 2 (mod i 3))
                                                       (string= "*" val))
                                                val
                                                (sanitise-uid val)))
                             :marker "n"))
           ;; Regex match for string-types
           ;; Full regex reference: https://docs.oracle.com/javase/7/docs/api/java/util/regex/Pattern.html
           ((and (or
                   ;; Remember that "uid" doesn't show up as a defined attribute
                   (member name '("uid") :test #'equal)
                   (and attribute
                        (member (type-of attribute) '(schema-rtype-attr-varchar schema-rtype-attr-text))))
                 (regex-p value))
            (format nil "n.~A =~~ '~A'" name value))
           ;;
           ;; Simple existence check
           ((and attribute
                 (string= "exists" value))
            (format nil "exists(n.~A)" name))
           ;;
           ;; Enum attribute
           ((and
              attribute
              (eq (type-of attribute) 'schema-rtype-attr-varchar)
              (attrvalues (get-attribute (gethash rtype schema) name)))
            (format nil "n.~A IN [~{'~A'~^, ~}]" name (cl-ppcre:split "," value)))
           ;;
           ;; Default case: exact text match
           (t
             (format nil "n.~A = '~A'" name value))))))
    (t
      (log-message :warn "Invalid filter")
      nil)))

(defun process-filters (filters schema rtype)
  "Take GET parameters, and turn them into a string of Neo4j WHERE clauses.
  Expects an alist, as returned by `tbnl:get-parameters*`"
  (declare (type list filters)
           (type hash-table schema)
           (type string rtype))
  (log-message :debug (format nil "Attempting to process filters ~A" filters))
  (let ((result (remove-if #'null (mapcar #'(lambda (filter)
                                              (process-filter filter schema rtype))
                                          filters))))
    (log-message :debug (format nil "Result of filter processing: ~A" result))
    (if result
      (let ((response (format nil " WHERE ~{ ~A~^ AND~}" result)))
        (log-message :debug (format nil "Output from process-filters: ~A." response))
        response)
      "")))


(defgeneric get-resources (db uri &key filters)
  (:documentation "Adaptable method to search for resources in a manner deterined by the modulo-3 length of the URI.
                   The optional 'filters' parameter is for refining the search results. Its expected value is a string, as returned by `process-filters`.
                   Return value is either a hash-table containing the node's properties, or a list of them."))

(defmethod get-resources ((db neo4cl:bolt-session)
                          (uri string)
                          &key filters)
  (declare (type (or null string) filters))
  (log-message :debug (format nil "Fetching resources for URI ~A" uri))
  (let ((uri-parts (get-uri-parts uri)))
    (cond
      ;; All resources of a given type
      ((equal (mod (length uri-parts) 3) 1)
       (log-message :debug (format nil "Fetching all resources of type ~A" uri))
       (let ((query (format nil "MATCH ~A~A RETURN n"
                            (uri-node-helper uri-parts
                                             :path ""
                                             :marker "n")
                            (or filters ""))))
         (log-message :debug (concatenate 'string "Querying database: "
                                          (cl-ppcre:regex-replace "\~" query "~~")))
         (mapcar #'(lambda (foo)
                     (neo4cl:node-properties
                       (cdr (assoc "n" foo :test #'equal))))
                 (neo4cl:bolt-transaction-autocommit db query))))
      ;; One specific resource
      ((equal (mod (length uri-parts) 3) 2)
       (log-message :debug (format nil "Fetching the resource matching the path ~A" uri))
       (let ((query (format nil "MATCH ~A RETURN n"
                            (uri-node-helper uri-parts
                                             :path ""
                                             :marker "n"))))
         (log-message :debug (concatenate 'string "Using query-string: " query))
         (let ((result (neo4cl:bolt-transaction-autocommit db query)))
           (when result (neo4cl:node-properties
                          (cdr (assoc "n"
                                      (car result)
                                      :test #'equal)))))))
      ;; All resources with a particular relationship to this one
      (t
        (log-message :debug (format nil "Fetching all resources on the path '~A'" uri))
        ;; Get the raw data
        (let ((query (format nil "MATCH ~A~A RETURN labels(n) AS labels, n AS node"
                             (uri-node-helper uri-parts
                                              :path ""
                                              :marker "n")
                             (or filters ""))))
          (log-message :debug (concatenate 'string "Using query-string: "
                                           (cl-ppcre:regex-replace "\~" query "~~")))
          ;; Extract each node, and insert "type" into its property list
          (mapcar #'(lambda (foo)
                      (let ((properties
                              (neo4cl:node-properties
                                (cdr (assoc "node" foo :test #'equal)))))
                        ;; Set the "type" attribute to be the first-returned label
                        (setf (gethash "type" properties)
                              (cdr (assoc "labels" foo :test #'equal)))
                        ;; Return the updated hash-table
                        properties))
                  (neo4cl:bolt-transaction-autocommit db query)))))))


(defgeneric get-dependent-resources (db schema sourcepath)
  (:documentation "Return a list of the resources that depend critically on this one.
The returned list contains 3-element lists of relationship, type and UID."))

(defmethod get-dependent-resources ((db neo4cl:bolt-session)
                                    (schema hash-table)
                                    (sourcepath list))
  (let ((rtypename (car (last sourcepath 2))))
    (log-message :debug (format nil "Searching for resources dependent on parent ~{/~A~}" sourcepath))
    (log-message :debug (format nil "Fetching dependent relationships from resourcetype ~A" rtypename))
    ;; Attempt to fetch the resourcetype
    (let ((rtypedef (gethash rtypename schema)))
      (if rtypedef
        ;; Get all dependent relationships outbound from this resourcetype
        ;; Get all nodes to which this node has outbound relationships of those types
        (let ((dependent-rels (map 'list
                                   #'name
                                   (remove-if-not
                                     #'(lambda (foo)
                                         (equal "dependent" (reltype foo)))
                                     (relationships rtypedef)))))
          (log-message :debug (format nil "Got list of dependent types: ~A" dependent-rels))
          (when dependent-rels
            (let ((query-string (format nil "MATCH ~A-[r]->(b) WHERE type(r) IN [~{'~A'~^, ~}] RETURN type(r) AS type, labels(b) AS labels, b.uid AS uid"
                                        (uri-node-helper sourcepath
                                                         :path ""
                                                         :marker "n")
                                        dependent-rels)))
              (log-message :debug (format nil "Generated query-string '~A'" query-string))
              ;; We should probably return the result
              (mapcar
                #'(lambda (row)
                    ;; List elements: relationship, target type, target UID
                    (list (cdr (assoc "type" row :test #'equal))
                          (cdr (assoc "labels" row :test #'equal))
                          (cdr (assoc "uid" row :test #'equal))))
                (neo4cl:bolt-transaction-autocommit db query-string)))))
        (error 'client-error :message (format nil "No such resourcetype ~A in this schema" rtypename))))))


(defgeneric update-resource-attributes (db schema path attributes)
  (:documentation "Add, update or delete a set of attributes of a given resource."))

(defmethod update-resource-attributes ((db neo4cl:bolt-session)
                                       (schema hash-table)
                                       (path list)
                                       (attributes list))
  (log-message :debug (format nil "Updating attributes for resource ~{/~A~}" path))
  (let ((attrs
          (append
            (remove-if #'(lambda (f)
                           (or (equal (car f) "uid")
                               (equal (car f) "original_uid")))
                       (validate-resource-before-creating
                         schema
                         (car (last (butlast path)))
                         attributes))
            `(("lastmodified" . ,(get-universal-time))))))
    (when attrs
      (log-message
        :debug
        (format nil "Applying the attributes ~{~A~^, ~} to resource ~{/~A~}" attrs path))
      (let ((query (format nil "MATCH ~A SET ~A"
                           (uri-node-helper path :path "" :marker "n")
                           (format nil "~{n.~A = $~A~^, ~}"
                                   (let ((acc '()))
                                     (mapcar #'(lambda (param)
                                                 (push (car param) acc) (push (car param) acc))
                                             attrs)
                                     acc)))))
        (log-message :debug (format nil "Applying statement ~A" query))
        (neo4cl:bolt-transaction-autocommit db query :parameters attrs)))))


(defgeneric delete-resource-by-path (db targetpath schema &key recursive)
  (:documentation "Delete a relationship or resource according to the URI supplied.
  :recursive confirms that you intend to delete all resources depending on the one identified in the path."))

(defmethod delete-resource-by-path ((db neo4cl:bolt-session)
                                    (targetpath string)
                                    (schema hash-table)
                                    &key recursive)
  (log-message :debug (format nil "Attempting to delete resource ~A" targetpath))
  (log-message :debug (format nil "The recursive flag was~A set" (if recursive "" " not")))
  (let ((parts (get-uri-parts targetpath)))
    (if (equal (mod (length parts) 3) 2)
      ;; Do any other resources depend critically on this one?
      (let ((dependents (get-dependent-resources db schema parts)))
        (if dependents
          ;; Yes: it's a first-class resource with dependents.
          ;; Was the recursive argument supplied?
          (if recursive
            ;; Yes. Delete the dependents, passing the value of the recursive argument
            (progn
              (log-message :debug "Dependent resources are present, and recursive deletion was requested.")
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
              (let ((querystring (format nil "MATCH ~A DETACH DELETE n" (uri-node-helper parts))))
                (log-message :debug (format nil "Deleting target resource '~A' with query '~A'"
                                            targetpath querystring))
                (neo4cl:bolt-transaction-autocommit db querystring)))
            ;; Dependents, but no recursive argument. Bail out.
            (error 'integrity-error
                   :message
                   "Other resources depend critically on this one, and recursive was not specified."))
          ;; First-class resource with no dependents: remove it.
          (let ((query (format nil "MATCH ~A DETACH DELETE n" (uri-node-helper parts))))
            (log-message
              :debug
              (format nil "No dependents. Deleting resource ~A with query '~A'" targetpath query))
            (neo4cl:bolt-transaction-autocommit db query))))
      (error 'client-error :message "This is not a valid deletion request"))))


(defgeneric delete-resource-attributes (db path attributes)
  (:documentation "Delete attributes from a resource."))

;;; FIXME: validate the attrs, at least to ensure they're escaped
(defmethod delete-resource-attributes ((db neo4cl:bolt-session)
                                       (path list)
                                       (attributes list))
  (log-message
    :debug
    (format nil "Attempting to delete attributes '~{~A~^, ~}' from the resource at path '~{~A~^/~}'"
            attributes path))
  (neo4cl:bolt-transaction-autocommit
    db
    (format nil "MATCH ~A REMOVE ~{n.~A~^, ~};"
            (uri-node-helper path :path "" :marker "n")
            attributes)))

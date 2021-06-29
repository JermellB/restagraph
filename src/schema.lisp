;   Copyright 2017 James Fleming <james@electronic-quill.net>
;
;   Licensed under the GNU General Public License
;   - for details, see LICENSE.txt in the top-level directory


;;;; Schema creation functions

(in-package #:restagraph)

(declaim (optimize (compilation-speed 0)
                   (speed 2)
                   (safety 3)
                   (debug 3)))


;;; Structures and their methods

(defstruct schema
  "Defines a schema."
  (name nil :type string :read-only t)
  (version 0 :type integer :read-only t)
  ;; List of schema-rtypes structs:
  (resourcetypes nil :type list :read-only t))


;; Data _in_ a schema

(defstruct schema-rtypes
  "Resource-type definition, for use in schema definitions."
  (name nil :type string :read-only t)
  (dependent nil :type boolean :read-only t)
  (notes nil :type (or null string) :read-only t)
  ;; List of `schema-rtype-attrs` structs.
  ;; Read-write to enable user-augmentation of existing resourcetypes.
  (attributes nil :type list :read-only nil)
  ;; List of `schema-rels` structs, which is expected to be updated repeatedly
  ;; as new schemas back-reference existing types.
  (relationships nil :type list :read-only nil))

(defstruct schema-rtype-attrs
  "Attributes of resource-types"
  (name nil :type string :read-only t)
  (description "" :type string :read-only t)
  (values nil :type list :read-only t))

(defstruct schema-rels
  "Relationships between resourcetypes, for use in schema definitions."
  (relationship nil :type string :read-only t)
  (target-type nil :type schema-rtypes :read-only t)
  (cardinality nil :type string :read-only t)
  (dependent nil :type boolean :read-only t)
  (notes "" :type (or null string) :read-only t))

(defstruct relationship-attrs
  "Describes the attributes of a relationship:
  relationship-attrs-dependent = boolean, indication whether this is a dependent relationship
  relationship-attrs-cardinality = string, returning the cardinality of the relationship"
  (name nil :type string :read-only t)
  (dependent nil :type boolean :read-only t)
  (cardinality "many:many" :type string :read-only t)
  (notes "" :type string :read-only t))


;; Incoming data, describing things _to be added to_ a schema

(defstruct incoming-subschema-version
  "Parent structure for a subschema definition.
  Used for installing a schema."
  (name nil :type string :read-only t)
  (version nil :type integer :read-only t)
  (apply-order nil :type integer :read-only t)
  (resourcetypes nil :type list :read-only nil)
  (relationships nil :type list :read-only nil))

(defstruct incoming-rtypes
  "Resource-type definition, normally generated by digesting a YAML file."
  (name nil :type string :read-only t)
  (dependent nil :type boolean :read-only t)
  (notes nil :type (or null string) :read-only t)
  ;; List of `incoming-rtype-attrs` structs.
  ;; Read-only because this is only expected to be used when ingesting new definitions.
  (attributes nil :type list :read-only t)
  ;; List of `incoming-rels` structs.
  ;; Read-only because this is only expected to be used when ingesting new definitions.
  (relationships nil :type list :read-only t))

(defstruct incoming-rtype-attrs
  "Attributes of _incoming_ resource-types."
  (name nil :type string :read-only t)
  (description "" :type string :read-only t)
  (values nil :type list :read-only t))

(defstruct incoming-rels
  "Relationships between resourcetypes, for use in updating schema definitions."
  (relationship nil :type string :read-only t)
  (target-type nil :type string :read-only t)
  (cardinality "many:many" :type string :read-only t)
  (dependent nil :type boolean :read-only t)
  (notes "" :type (or null string) :read-only t))


;;; Structure methods

(defgeneric get-relationship-attrs (schema source-type relationship dest-type)
  (:documentation "Extract the attributes of interest for a given relationship.
                  Return a 'relationship-attrs struct.
                  cardinality defaults to many:many."))

(defmethod get-relationship-attrs ((schema hash-table)
                                   (source-type string)
                                   (relationship string)
                                   (dest-type string))
  (log-message
    :debug
    (format nil "Retrieving the relationship ~A from ~A to ~A, from a hash-table schema."
            relationship source-type dest-type))
  (relationship-in-struct-p
    (resourcetype-exists-p schema source-type)
    relationship
    dest-type))


(defgeneric relationship-in-struct-p (rtype rel-type target-rtype)
  (:documentation "Check for a named type of relationship from a resourcetype to a target resourcetype.
  Return a schema-rels struct if present, otherwise nil.
  Note that it returns a list, which will have more than one element if we screwed up and allowed two
  relationships of the same type to the same target."))

(defmethod relationship-in-struct-p ((rtype schema-rtypes)
                                     (rel-type string)
                                     (target-rtype string))
  (log-message :debug (format nil "Checking resourcetype ~A for relationship ~A to type ~A (string)"
                              (schema-rtypes-name rtype) rel-type target-rtype))
  (remove-if-not #'(lambda (rel)
                     (and
                       (equal rel-type (schema-rels-relationship rel))
                       (equal target-rtype (schema-rtypes-name
                                             (schema-rels-target-type rel)))))
                 (schema-rtypes-relationships rtype)))

(defmethod relationship-in-struct-p ((rtype schema-rtypes)
                                     (rel-type string)
                                     (target-rtype schema-rtypes))
  (log-message :debug (format nil "Checking resourcetype ~A for relationship ~A to type ~A (rtype)"
                              (schema-rtypes-name rtype) rel-type (schema-rtypes-name target-rtype)))
  (remove-if-not #'(lambda (rel)
                     (and
                       (equal rel-type (schema-rels-relationship rel))
                       (equal (schema-rtypes-name target-rtype)
                              (schema-rtypes-name (schema-rels-target-type rel)))))
                 (schema-rtypes-relationships rtype)))


;;; Functions - schema creation

(defun make-schema-hash-table ()
  "Convenience function for repeatably creating the kind of hash-table we expect."
  (make-hash-table :test #'equal))

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



;;; Methods for querying the schema

(defgeneric dependent-resource-p (db resourcetype)
  (:documentation "Determine whether a resource-type is dependent or first-class"))

(defmethod dependent-resource-p ((db hash-table) (resourcetype string))
  (let ((rtype (gethash resourcetype db)))
    (when rtype
      (schema-rtypes-dependent rtype))))

(defmethod dependent-resource-p ((db neo4cl:neo4j-rest-server) (resourcetype string))
  (neo4cl:extract-data-from-get-request
    (neo4cl:neo4j-transaction
      db
      `((:STATEMENTS
          ((:STATEMENT .
            ,(format nil "MATCH (c:rgResource { name: '~A' }) RETURN c.dependent"
                     (sanitise-uid resourcetype)))))))))


(defgeneric dependent-relationship-p (db source-type relationship dest-type)
  (:documentation "Determine whether this relationship is a dependent one between these two resource-types."))

(defmethod dependent-relationship-p ((db hash-table)
                                     (source-type string)
                                     (relationship string)
                                     (dest-type string))
  (log-message :debug
               (format nil "Checking whether ~A is a valid dependent relationship from ~A to ~A"
                       relationship source-type dest-type))
  (let ((stype (gethash source-type db)))
    (log-message :debug (format nil "Fetched value ~A" (schema-rtypes-name stype)))
    (when source-type
      ;; Fetch _all_ the relationships by this name, to that target-type
      (let ((candidates (remove-if-not
                          #'(lambda (rel)
                              (and (equal relationship (schema-rels-relationship rel))
                                   (equal dest-type
                                          (schema-rtypes-name (schema-rels-target-type rel)))))
                          (schema-rtypes-relationships stype))))
        ;; If a non-null list resulted,
        ;; return the boolean indicating whether the first (and theoretically only) item
        ;; in that list is a dependent relationship.
        (when candidates
          (schema-rels-dependent (car candidates)))))))


(defgeneric resourcetype-exists-p (db resourcetype)
  (:documentation "Verify whether we have a definition for a resourcetype by this name.
  Return a schema-rtypes struct."))

;; FIXME Return value needs updating
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


(defgeneric get-resource-attributes-from-db (db resourcetype)
  (:documentation "Extract the attributes from resource definitions from the database,
  and return them as a list of schema-rtype-attrs structs."))

(defmethod get-resource-attributes-from-db ((db hash-table)
                                            (resourcetype string))
  (log-message :debug (format nil "Getting attributes for resourcetype '~A'" resourcetype))
  (let ((struct (gethash resourcetype db)))
    (when struct
      (schema-rtypes-attributes struct))))


(defgeneric get-resourcetype-names (db)
  (:documentation "Return the names of resourcetypes, as a list of strings."))

(defmethod get-resourcetype-names ((db hash-table))
  (log-message :debug "Fetching resourcetype names.")
  (loop for key being the hash-keys in db collecting key))


(defgeneric describe-resource-type (db resourcetype)
  (:documentation "Return the description of a resource-type, as an alist.
                   Entries include :name, :attributes and :dependent.
                   The :recursive key is a boolean indicating whether to recursively traverse all the
                   relationships from this resource-type.
                   The :resources-seen key is used internally to break loops when recursing."))

(defmethod describe-resource-type ((db hash-table)
                                   (resourcetype string))
  (log-message :debug (format nil "Describing resource-type '~A'" resourcetype))
  ;; Confirm whether this resourcetype exists at all.
  ;; If it doesn't, automatically return NIL.
  (let ((node (resourcetype-exists-p db resourcetype)))
    (when node
      ;; Construct the return values
      `((:NAME . ,(schema-rtypes-name node))
        (:ATTRIBUTES . ,(sort
                          (mapcar
                            #'(lambda (s) `(("name" . ,(schema-rtype-attrs-name s))
                                            ("description" . ,(schema-rtype-attrs-description s))
                                            ("vals" . ,(schema-rtype-attrs-values s))))
                            (schema-rtypes-attributes node))
                          #'string-lessp
                          :key #'caar))
        (:DEPENDENT . ,(if (schema-rtypes-dependent node) "true" "false"))
        (:NOTES . ,(or (schema-rtypes-notes node) ""))
        (:RELATIONSHIPS . ,(mapcar #'(lambda (rel)
                                       ;; Return an alist of the values, ready for rendering into Javascript
                                       `((:RELATIONSHIP . ,(schema-rels-relationship rel))
                                         (:DEPENDENT . ,(if (schema-rels-dependent rel) "true" "false"))
                                         (:CARDINALITY . ,(schema-rels-cardinality rel))
                                         (:NOTES . ,(or (schema-rels-notes rel) ""))
                                         (:RESOURCETYPE ,(schema-rtypes-name
                                                           (schema-rels-target-type rel)))))
                                   (schema-rtypes-relationships node)))))))


;; Helper function
(defun format-post-params-as-properties (params)
  "Take an alist, as returned by (tbnl:post-parameters*), and transform it into the kind of map
  that Neo4J expects in the :PROPERTIES section of a query."
  (declare (type (or null cons) params))
  (log-message :debug "Formatting a set of POST parameters for use as Neo4j properties.")
  (mapcar #'(lambda (param)
              (cons (intern (escape-neo4j (string-downcase (car param))) :keyword)
                    (if (stringp (cdr param))
                      (escape-neo4j (cdr param))
                      (cdr param))))
          params))

;; Helper function
(defun validate-attributes (requested defined &key (invalid '()) (badvalue '()))
  "Recursive helper function to validate the requested attributes against those defined for the resourcetype.
   Return a list of three lists:
   - invalid attributes (attributes whose name is not defined for this resourcetype)
   - attributes for which an invalid value was provided"
  (declare (type (or cons null) requested)  ; alist, where the car is the name and the cdr is the value
           (type (or cons null) defined)    ; Should have the outer layer of conses stripped
           (type (or cons null) invalid)
           (type (or cons null) badvalue))
  (log-message :debug (format nil "validate-attributes requested attrs: ~A" requested))
  (log-message :debug (format nil "validate-attributes defined attrs: ~A" defined))
  ;; Are we at the end of the list of requested attributes?
  (if (null requested)
      ;; If we are, return what's been accumulated
      (list invalid badvalue)
      ;; If not, check the attribute at the head of the list, then call this function on the rest.
      (validate-attributes
        ;; Rest of the list.
        (cdr requested)
        ;; Definitions, unchanged.
        defined
        ;; If this attribute is invalid, add it to that accumulator.
        ;; Pull the list of attribute-names from the 'defined parameter, and test whether it's a member.
        :invalid (if (not (member (caar requested)
                                  (mapcar #'(lambda (attr) (schema-rtype-attrs-name attr))
                                          defined)
                                  :test #'equal))
                   ;; Invalid attribute. Log it and add it to the `invalid` accumulator.
                   (progn
                     (log-message :debug (format nil "Detected invalid attribute name '~A'" (caar requested)))
                     (append invalid (list (car requested))))
                   ;; It's valid; leave the `invalid` list as-is
                   invalid)
        :badvalue (if
                     ;; Is it a valid attribute? (yes, we have to test this again)
                     (if (member (caar requested)
                                 ;; Remember this was parsed from JSON
                                 (mapcar #'(lambda (attr) (schema-rtype-attrs-name attr))
                                         defined)
                                 :test #'equal)
                         ;; Is this an enum attribute?
                         (let ((enums
                                 (schema-rtype-attrs-values
                                   (car (remove-if-not #'(lambda (attr)
                                                           (equal (caar requested)
                                                                  (schema-rtype-attrs-name attr)))
                                                       defined)))))
                           (if (and enums
                                    (not (null enums)))
                               ;; If so, is it a valid value?
                               (when
                                 (member (cdar requested)
                                         enums
                                         :test #'equal)
                                 ;; If it's a valid value for this enum, return True
                                 t)
                               ;; If it's not an enum, then we do no other checking.
                               t))
                         ;; If it's not a valid attribute, this isn't relevant.
                         t)
                     ;; If all those tests passed, pass on the value of `badvalue` we received
                     badvalue
                     ;; If any of those failed, add this to `badvalue`
                     (append badvalue (list (car requested)))))))


(defgeneric validate-resource-before-creating (db resourcetype params)
  (:documentation "Confirm whether the provided data is valid, before attempting to use it to create a resource.
  If the data is valid, return a list of attributes suitable for feeding to Neo4J.
  If not, raise a suitable error"))

;; This actually works as-is for both types, hence the funky approach of
;; not-specialising and then manually declaring types.
(defmethod validate-resource-before-creating (db
                                               resourcetype
                                               ;; params is what Hunchentoot received via POST
                                               params)
  (declare (type (or neo4cl:neo4j-rest-server hash-table))
           (type string resourcetype)
           (type list params))
  (log-message
    :debug
    (format nil "validate-resource-before-creating resourcetype ~A with params ~{~A~^, ~}"
            resourcetype params))
  ;; Does this resource-type exist?
  (if (resourcetype-exists-p db resourcetype)
    ;; Were attributes specified and, if so, are they all valid for this resource-type?
    (let
      ;; Exempt "uid" from validation
      ((requested-attributes
         (remove-if #'(lambda (param) (equal (car param) "uid"))
                    params))
       ;; Get the attributes defined for this resource-type
       (defined-attributes (get-resource-attributes-from-db db resourcetype))
       ;; Extract the original UID here, to reduce mess later
       (original-uid (or (cdr (assoc "uid" params :test #'string=)) "")))
      ;; Put this log message here to get it inside the let statement,
      ;; and thus avoid a progn.
      (log-message :debug (format nil "Confirmed: resourcetype '~A' exists" resourcetype))
      ;; Now validate the attributes and return the results.
      (log-message :debug "Checking the supplied attributes.")
      (let ((results (validate-attributes requested-attributes defined-attributes)))
        ;; Were the requested attributes all valid?
        (if (and (null (first results))
                 (null (second results)))
          ;; If so, return the supplied attributes to the caller, properly formatted for Neo4j.
          (let ((formatted-params
                  (format-post-params-as-properties
                    ;requested-attributes
                    (acons "uid" (sanitise-uid original-uid)
                           (acons "original_uid" original-uid
                                  (remove-if #'(lambda (param) (equal (car param) "uid"))
                                             params))))))
            (log-message :debug (format nil "Returning formatted parameters ~A" formatted-params))
            formatted-params)
          ;; If not, report the problem.
          (progn
            (when (first results)
              (log-message :debug (format nil "Identified invalid attribute-names: ~{~A~^, ~}"
                                          (first results))))
            (when (second results)
              (log-message :debug (format nil "Identified invalid values ~{~A~^, ~}"
                                          (second results))))
            (error 'restagraph:client-error :message
                   (format nil "Invalid attributes for ~A resources: ~{~A~^, ~}. Invalid values: ~{~A~^, ~}."
                           resourcetype (first results) (second results)))))))
    ;; No such resourcetype
    (signal 'client-error :message "No such resourcetype")))

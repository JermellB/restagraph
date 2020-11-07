;   Copyright 2017 James Fleming <james@electronic-quill.net>
;
;   Licensed under the GNU General Public License
;   - for details, see LICENSE.txt in the top-level directory


;;;; Schema creation functions

(in-package #:restagraph)


;;; Structures and their methods

(defstruct schema
  "Defines a schema."
  (name nil :type string :read-only t)
  (version 0 :type integer :read-only t)
  ;; List of schema-rtypes structs:
  (resourcetypes nil :type list :read-only t))

(defstruct schema-rtypes
  "Resource-type definition, for use in schema definitions."
  (name nil :type string :read-only t)
  (dependent nil :type boolean :read-only t)
  (notes nil :type string :read-only t)
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
  (target-type nil :type string :read-only t)
  (cardinality "many:many" :type string :read-only t)
  (dependent nil :type boolean :read-only t)
  (notes "" :type (or null string) :read-only t))


;;; Structure methods

(defgeneric relationship-in-struct-p (rtype rel-type target-rtype)
  (:documentation "Check for a named type of relationship from a resourcetype to a target resourcetype. Return a schema-rels struct if present, otherwise nil.
  Note that it returns a list, which will have more than one element if we screwed up and allowed two relationships of the same type to the same target."))

(defmethod relationship-in-struct-p ((rtype schema-rtypes)
                                     (rel-type string)
                                     (target-rtype string))
  (remove-if-not #'(lambda (rel)
                     (and
                       (equal rel-type (schema-rels-relationship rel))
                       (equal target-rtype (schema-rels-target-type rel))))
                 (schema-rtypes-relationships rtype)))


(defgeneric add-rel-to-schema-rtype (rtype new-rel)
  (:documentation "Add a relationship to a schema-rtypes struct. If it already has a relationship of the same type to the same target-type, the one being added replaces the old one."))

(defmethod add-rel-to-schema-rtype ((rtype schema-rtypes)
                                    (new-rel schema-rels))
  ;; Replace the existing list of relationships with an updated list featuring the new relationship.
  (log-message :debug "Attempting to add relationship '~A' from source-type '~A' to target-type '~A'"
               (schema-rels-relationship new-rel)
               (schema-rtypes-name rtype)
               (schema-rels-target-type new-rel))
  (setf (schema-rtypes-relationships rtype)
        (append
          ;; Is there already a relationship of this type?
          (if (relationship-in-struct-p rtype
                                        (schema-rels-relationship new-rel)
                                        (schema-rels-target-type new-rel))
              ;; If there is, filter it out.
              ;; We could have done this more elegantly by matching the relationship we already found,
              ;; but it's possible that more than one is already there.
              ;; This way, we remove any duplicates.
              (remove-if #'(lambda (rel)
                             (and
                               (equal (schema-rels-relationship new-rel) (schema-rels-relationship rel))
                               (equal (schema-rels-target-type new-rel) (schema-rels-target-type rel))))
                         (schema-rtypes-relationships rtype))
              ;; If not, use the list as-is.
              (schema-rtypes-relationships rtype))
          ;; Either way, we're adding the new relationship.
          (list new-rel))))


;;; Functions - schema creation

(defun digest-schema-yaml (filepath)
  "Digest a single YAML file, and return it as a plist with the following keys:
   :name = string. Schema name, as reported in the YAML file.
   :version = string. Schema version, as reported in the YAML file.
   :resourcetypes = list of schema-rtypes structs.
   :relationships = list of dotted-lists: (<source-type name> . <schema-rels struct>)"
  (declare (type pathname filepath))
  (log-message :info "Attempting to digest schema file ~A" filepath)
  (let ((schema (cl-yaml:parse filepath)))
    (list
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
      :relationships
      (if (gethash "relationships" schema)
          (remove-if
                    #'null
                    (mapcar
                      #'(lambda (rel)
                          (log-message :info "Attempting to create a schema-rel struct with URI '~A" (gethash "uri" rel))
                          (let* ((rel-parts
                                   (remove-if #'(lambda (part) (equal "" part)) 
                                              ;; If there's a URI attribute, split it on forward-slash and remove
                                              ;; any remaining empty strings resulting from a leading slash.
                                              ;; If there isn't, do that to an empty string. It's simpler this way.
                                              (cl-ppcre:split "/" (gethash "uri" rel ""))))
                                 ;; Enforce a valid cardinality.
                                 ;; If a valid one was not supplied, make it the default.
                                 (raw-cardinality (gethash "cardinality" rel "many:many"))
                                 (cardinality (if (member raw-cardinality
                                                          '("many:many" "1:many" "many:1" "1:1")
                                                          :test #'equal)
                                                  raw-cardinality
                                                  "many:many")))
                            ;; Warn the admin if the original cardinality was not valid.
                            (when (not (equal raw-cardinality cardinality))
                              (log-message :error "Relationship /~A/~A/~A had invalid cardinality '~A'. Forcing to default many:many."
                                           (first rel-parts)
                                           (second rel-parts)
                                           (third rel-parts)
                                           raw-cardinality))
                            ;; If there was a valid path, generate the expected plist.
                            (if (equal (length rel-parts) 3)
                                ;; Create a duple: name of the source-type, and a schema-rels struct
                                (cons
                                  (first rel-parts)
                                  (make-schema-rels
                                    :relationship (second rel-parts)
                                    :target-type (third rel-parts)
                                    :cardinality cardinality
                                    :dependent (when (gethash "dependent" rel) t)
                                    :notes (gethash "notes" rel)))
                                ;; If not, log the fact and return a null value for filtering out.
                                (progn
                                  (log-message :warn "No URI in this entry. Skipping.")
                                  '()))))
                      (gethash "relationships" schema)))
          (log-message "No relationships found in schema '~A'"
                       (gethash "name" schema))))))


(defgeneric add-resource-to-schema (schema resourcetype)
  (:documentation "Add a new resourcetype to the schema. In the event of a collision, merge the new definition into the existing one."))

;; WARNING: Mutates existing state
(defmethod add-resource-to-schema ((schema hash-table)
                                   (resourcetype schema-rtypes))
  (let ((existing (resourcetype-exists-p schema (schema-rtypes-name resourcetype))))
    (if existing
        ;; If it already exists, try to merge them
        (progn
          (log-message :info "Definition already exists for resourcetype '~A'. Attempting to merge them."
                       (schema-rtypes-name resourcetype))
          (mapcar #'(lambda (newattr)
                      ;; Is there already an attribute by this name?
                      (if (remove-if-not
                            #'(lambda (existingattr)
                                (equal (schema-rtype-attrs-name newattr)
                                       (schema-rtype-attrs-name existingattr)))
                            (schema-rtypes-attributes existing))
                          ;; There is. Log it and move on.
                          (log-message :error "Resourcetype '~A' already has an attribute named '~A'. Skipping this one.")
                          ;; Not already there. Add it.
                          (setf (schema-rtypes-attributes existing)
                                (append (list newattr) (schema-rtypes-attributes existing)))))
                  (schema-rtypes-attributes resourcetype)))
        ;; Not a dupe; add it to the schema as-is, by prepending it to the existing list of attributes.
        (progn
          (log-message :debug "Adding resourcetype '~A' to the schema."
                       (schema-rtypes-name resourcetype))
          (setf (gethash (schema-rtypes-name resourcetype) schema) resourcetype)))))


(defun update-hash-from-digest (hash digest)
  "Update the contents of a hash-table (with test #'equal),
   from the output of digest-schema-yaml."
  (declare (type hash-table hash)
           (list digest))
  (log-message :debug "Attempting to update a hash-table with the contents of digest ~A"
               (getf digest :name))
  ;; Add the resourcetypes to the hash
  (mapcar #'(lambda (rtype) (add-resource-to-schema hash rtype))
          (getf digest :resourcetypes))
  ;; Update the relationships between resourcetypes.
  ;; Do this as a separate step, to allow for back-references.
  (mapcar #'(lambda (rel)
              (log-message
                :debug
                "Attempting to update resourcetype '~A' with relationship '~A' to resourcetype '~A'"
                (car rel)
                (schema-rels-relationship (cdr rel))
                (schema-rels-target-type (cdr rel)))
              ;; Is the source-type in there?
              (let ((sourcetype (gethash (car rel) hash)))
                (if sourcetype
                    ;; If so, go ahead.
                    (add-rel-to-schema-rtype sourcetype (cdr rel))
                    ;; If it's not there, complain and move on.
                    (log-message :error "Doomed attempt to update nonexistent resourcetype '~A' with relationship '~A' to resourcetype '~A'"
                                 (car rel)
                                 (schema-rels-relationship (cdr rel))
                                 (schema-rels-target-type (cdr rel))))))
          (getf digest :relationships)))

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

(defun inject-all-schemas (hash schemadir)
  "Update the supplied hash-table with the digested contents of the schema directory."
  (declare (type hash-table hash)
           (type (or null pathname) schemadir))
  ;; Ensure the core schemas are present and up to date
  (log-message :info "Ensuring core schemas are present and up to date")
  (mapcar #'(lambda (schema)
              (update-hash-from-digest hash schema))
          restagraph::*core-schemas*)
  ;; Lastly, if there were any user-defined schemas, apply those as well.
  ;; The point of digesting all the schema files before beginning to inject them,
  ;; instead of simply doing it as a single loop, is to ensure that we don't get
  ;; partway through injecting the schemas before discovering a problem in the files.
  (when schemadir
    (mapcar #'(lambda (schemafile)
                (update-hash-from-digest hash (digest-schema-yaml schemafile)))
            (enumerate-schemas-in-dir schemadir))))


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
    (when source-type
      (schema-rels-dependent
        (car
          (remove-if-not #'(lambda (rel)
                             (and (equal relationship (schema-rels-relationship rel))
                                  (equal dest-type (schema-rels-target-type rel))))
                         (schema-rtypes-relationships stype)))))))

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


(defgeneric resourcetype-exists-p (db resourcetype)
  (:documentation "Verify whether we have a definition for a resourcetype by this name."))

(defmethod resourcetype-exists-p ((db hash-table)
                                  (resourcetype string))
  (log-message :debug (format nil "Checking for existence of resourcetype '~A'" resourcetype))
  (gethash resourcetype db))

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
  (:documentation "Extract the attributes from resource definitions from the database, and return them as a list of schema-rtype-attrs structs."))

(defmethod get-resource-attributes-from-db ((db hash-table)
                                            (resourcetype string))
  (log-message :debug "Getting attributes for resourcetype '~A'" resourcetype)
  (let ((struct (gethash resourcetype db)))
    (when struct
      (schema-rtypes-attributes struct))))

(defmethod get-resource-attributes-from-db ((db neo4cl:neo4j-rest-server)
                                            (resourcetype string))
  (log-message :debug "Getting attributes for resourcetype '~A'" resourcetype)
  (mapcar #'(lambda (attr)
              (make-schema-rtype-attrs :name (cdr (assoc :NAME (car attr)))
                                       :description (cdr (assoc :DESCRIPTION (car attr)))
                                       :values (cl-ppcre:split "," (cdr (assoc :VALS (car attr))))))
          (neo4cl::extract-rows-from-get-request
            (neo4cl:neo4j-transaction
              db
              `((:STATEMENTS
                  ((:STATEMENT .
                    ,(format nil "MATCH (c:rgResource { name: '~A' })-[:rgHasAttribute]-(a:rgAttribute) RETURN a"
                             (sanitise-uid resourcetype))))))))))


(defgeneric get-resourcetype-names (db)
  (:documentation "Return the names of resourcetypes, as a list of strings."))

(defmethod get-resourcetype-names ((db neo4cl:neo4j-rest-server))
  (log-message :debug "Fetching resourcetype names.")
  (mapcar #'(lambda (foo)
              (cdr (assoc :NAME (car foo))))
          (neo4cl::extract-rows-from-get-request
            (neo4cl:neo4j-transaction
              db
              `((:STATEMENTS
                  ((:STATEMENT . "MATCH (c:rgResource) RETURN c"))))))))

(defmethod get-resourcetype-names ((db hash-table))
  (log-message :debug "Fetching resourcetype names.")
  (loop for key being the hash-keys in db collecting key))



(defgeneric describe-resource-type (db resourcetype &key resources-seen)
  (:documentation "Return the description of a resource-type, as an alist.
                   Entries include :name, :attributes and :dependent.
                   The :recursive key is a boolean indicating whether to recursively traverse all the relationships
                   from this resource-type.
                   The :resources-seen key is used internally to break loops when recursing."))

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
                 #'(lambda (s)
                     `(("name" . ,(cdr (assoc :name (car s))))
                       ("description" . ,(cdr (assoc :description (car s))))
                       ("vals" . ,(cdr (assoc :vals (car s))))))
                 (neo4cl:extract-rows-from-get-request
                   (neo4cl:neo4j-transaction
                     db
                     `((:STATEMENTS
                         ((:STATEMENT
                            . ,(format nil "MATCH (:rgResource {name: '~A'})-[:rgHasAttribute]->(n:rgAttribute) RETURN n"
                                       (sanitise-uid resourcetype)))))))))
               #'string-lessp
               :key #'caar))
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

(defmethod describe-resource-type ((db hash-table)
                                   (resourcetype string)
                                   &key resources-seen)
  (declare (ignore resources-seen))
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
                                         (:RESOURCETYPE ,(schema-rels-target-type rel))))
                                   (schema-rtypes-relationships node)))))))


(defgeneric describe-dependent-resources (db resourcetype &key resources-seen)
  (:documentation "Return a list of descriptions of all the dependent resourcetypes for this resource.
                   The :resources-seen key is used internally to break loops when recursing.
                   Return format is a list of conses, whose car is a relationship-attrs struct,
                   and whose cdr is the name of the target resourcetype."))

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


;; Helper function
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
                                  ;; Remember this was parsed from JSON
                                  (mapcar #'(lambda (attr) (schema-rtype-attrs-name attr))
                                          defined)
                                  :test #'equal))
                     (append invalid (list (car requested)))
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
         (defined-attributes (mapcar #'car (get-resource-attributes-from-db db resourcetype)))
         ;; Extract the original UID here, to reduce mess later
         (original-uid (or (cdr (assoc "uid" params :test #'string=)) "")))
        ;; Put this log message here to get it inside the let statement,
        ;; and thus avoid a progn.
        (log-message :debug "Confirmed: resourcetype '~A' exists" resourcetype)
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
                (log-message :debug "Returning formatted parameters ~A" formatted-params)
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


;;; GraphQL methods

(defgeneric describe-resource-type-for-graphql (db
                                                 resourcetype
                                                 all-resourcetype-names
                                                 rels-from-any
                                                 &key resources-seen)
  (:documentation "Return the description of a resource-type, as an alist.
Entries include :name, :attributes and :dependent.
The :recursive key is a boolean indicating whether to recursively traverse all the relationships
from this resource-type.
The :resources-seen key is used internally to break loops when recursing."))

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

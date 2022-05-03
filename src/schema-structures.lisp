;   Copyright 2017-21 James Fleming <james@electronic-quill.net>
;
;   Licensed under the GNU General Public License
;   - for details, see LICENSE.txt in the top-level directory

;;;; Classes and other structures used for interacting with the schema
;;;; along with generic functions specifically for interacting with them.


(in-package #:restagraph)
(declaim (optimize (compilation-speed 0)
                   (speed 2)
                   (safety 3)
                   (debug 3)))


(defgeneric set-relationships (obj rels)
  (:documentation "Set the value of the relationships slot in an incoming-subschema-version instance.
                   The value must be a list of incoming-rels instances."))

(defgeneric set-attributes (rtype attributes)
  (:documentation "Set the value of the attributes slot in an incoming-rtypes instance.
                   The value must be a list of incoming-rtype-attrs instances."))

(defgeneric set-resourcetypes (subschema rtypes)
  (:documentation "Set the value of the resourcetypes slot in an incoming-subschema-version instance.
                   The value must be a list of incoming-rtypes instances."))

(defclass abstract-rtypes ()
  ((name :initarg :name
         :reader name
         :type string)
   (dependent :initarg :dependent
              :reader dependent
              :type boolean)
   (description :initarg :description
          :reader description
          :type (or null string))
   (attributes :reader attributes
               :type (or null sequence)
               :initform nil))
  (:documentation "Parent class for resourcetype subclasses. Not to be instantiated directly."))


(defclass schema-rtypes (abstract-rtypes)
  ((relationships
     :initarg relationships
     :reader relationships
     :type sequence
     :initform nil))
  (:documentation "Resource-type definition, normally generated by digesting a JSON document."))


(defgeneric a-listify (obj)
  (:documentation "Serialise an object to a-list format, for further serialisation to JSON."))

(defmethod a-listify ((obj schema-rtypes))
  `((:name . ,(name obj))
    (:dependent . ,(dependent obj))
    (:description . ,(description obj))
    (:attributes .  ,(map 'list #'a-listify (attributes obj)))
    (:relationships . ,(map 'list #'a-listify (relationships obj)))))


(defgeneric p-listify (obj)
  (:documentation "Serialise an object to p-list format, for further serialisation to, e.g, HTML."))

(defmethod p-listify ((obj schema-rtypes))
  `(:name ,(name obj)
    :dependent ,(if (dependent obj) "true" "false")
    :description ,(description obj)
    :attributes ,(map 'list #'p-listify (attributes obj))
    :relationships ,(map 'list #'p-listify (relationships obj))))

(defmethod set-attributes ((rtype schema-rtypes) (attributes list))
  (if (every #'(lambda (attr) (typep attr 'schema-rtype-attrs))
             attributes)
    (setf (slot-value rtype 'attributes)
          (make-array
            (length attributes) ; 1-dimensional array, i.e. a vector
            :element-type 'schema-rtype-attrs
            :initial-contents   ; Supply the data
            (sort attributes #'string< :key #'name)))
    (error "Invalid type for schema-rtypes attributes slot.")))

(defmethod set-relationships ((rtype schema-rtypes) (rels list))
  (if (every #'(lambda (rel) (typep rel 'schema-rels))
             rels)
      (setf (slot-value rtype 'relationships)
            (make-array
              (length rels) ; 1-dimensional array, i.e. a vector
              :element-type 'schema-rels
              :initial-contents  ; Supply the data
              ;; Sort them by relationship name, then by target-type name
              (stable-sort
                (stable-sort rels
                             #'string<
                             :key #'(lambda (rel) (name (target-type rel))))
                #'string<
                :key #'name)))
      (error "Invalid type for schema-rtypes relationships slot.")))


(defun make-schema-rtypes (&key name dependent description attributes relationships)
  (declare (type string name)
           (type boolean dependent)
           (type (or null string) description)
           (type (or null list) attributes relationships))
  "Constructor for schema-rtypes instances."
  (let ((rtype (make-instance 'schema-rtypes :name name
                              :dependent dependent
                              :description description)))
    (when attributes (set-attributes rtype attributes))
    (when relationships (set-relationships rtype relationships))
    ;; Return the instance we created
    rtype))


(defclass schema-rtype-attrs ()
  ((name :initarg :name
         :reader name
         :type string)
   (description :initarg :description
                :reader description
                :type (or null string))
   (readonly :initarg :readonly
              :reader readonly
              :type boolean
              :initform nil
              :documentation "Whether this attribute can be updated via client request. Default is nil, i.e. it's writeable. Maps to 'readonly' within Neo4j."))
  (:documentation "Parent class for resource-types. Should not be instantiated directly; instead, one of its subclasses (or their subclasses) should be used."))

(defun make-schema-rtype-attrs (attribute)
  "Take a neo4cl:node object, and return an instance of the appropriate subclass of schema-rtype-attrs."
  (declare (type neo4cl:node attribute))
  (let ((properties (neo4cl:node-properties attribute)))
    (cond
      ((equal "varchar" (gethash "type" properties))
       (make-instance 'schema-rtype-attr-varchar
                      :name (gethash "name" properties)
                      :description (gethash "description" properties)
                      :readonly (gethash "readonly" properties)
                      :maxlength (gethash "maxlength" properties)
                      :attrvalues (gethash "attrvalues" properties)))
      ((equal "text" (gethash "type" properties))
       (make-instance 'schema-rtype-attr-text
                      :name (gethash "name" properties)
                      :description (gethash "description" properties)
                      :readonly (gethash "readonly" properties)))
      ((equal "integer" (gethash "type" properties))
       (make-instance 'schema-rtype-attr-integer
                      :name (gethash "name" properties)
                      :description (gethash "description" properties)
                      :readonly (gethash "readonly" properties)))
      ((equal "boolean" (gethash "type" properties))
       (make-instance 'schema-rtype-attr-boolean
                      :name (gethash "name" properties)
                      :description (gethash "description" properties)
                      :readonly (gethash "readonly" properties)))
      ;; Provide a sensible fallback,
      ;; if only so it doesn't crash on startup and prevent uploading a type-annotated schema.
      (t
       (make-instance 'schema-rtype-attr-varchar
                      :name (gethash "name" properties)
                      :description (gethash "description" properties)
                      :readonly (gethash "readonly" properties)
                      :maxlength nil
                      :attrvalues nil)))))

(defclass schema-rtype-attr-varchar (schema-rtype-attrs)
  ((maxlength :initarg :maxlength
               :reader maxlength
               :type (or null integer)
               :initform nil  ; Default is unlimited
               :documentation "Maximum acceptable length of the string.")
   (attrvalues :initarg :attrvalues
                :reader attrvalues
                :type (or null list)
                :initform nil ; Default is not to restrict input to a set of values
                :documentation "When not null, this is a list of acceptable values for this attribute."))
  (:documentation "Basic variable-length string attribute. Use this for one-liners, like titles and summary descriptions; use schema-rtype-attr-text for bulk text, such as Wikipages content."))

(defmethod a-listify ((obj schema-rtype-attr-varchar))
  `((:name . ,(name obj))
    (:type . "varchar")
    (:description . ,(description obj))
    (:readonly . ,(readonly obj))
    (:maxlength . ,(maxlength obj))
    (:values . ,(attrvalues obj))))

(defclass schema-rtype-attr-text (schema-rtype-attrs)
  ()
  (:documentation "Bulk text field, e.g. Wikipages content. For smaller one-liner string attributes, use schema-rtype-attr-string."))

(defmethod a-listify ((obj schema-rtype-attr-text))
  `((:name . ,(name obj))
    (:type . "text")
    (:description . ,(description obj))
    (:readonly . ,(readonly obj))))

(defclass schema-rtype-attr-integer (schema-rtype-attrs)
  ((minimum :initarg :minimum
            :reader minimum
            :type (or null integer)
            :initform nil
            :documentation "Optional minimum value.")
   (maximum :initarg :maximum
            :reader maximum
            :type (or null integer)
            :initform nil
            :documentation "Optional maximum value."))
  (:documentation "Basic integer attribute."))

(defmethod a-listify ((obj schema-rtype-attr-integer))
  `((:name . ,(name obj))
    (:type . "integer")
    (:description . ,(description obj))
    (:readonly . ,(readonly obj))
    (:minimum . ,(minimum obj))
    (:maximum . ,(maximum obj))))

(defclass schema-rtype-attr-boolean (schema-rtype-attrs)
  ()
  (:documentation "Simple boolean type."))

(defmethod a-listify ((obj schema-rtype-attr-boolean))
  `((:name . ,(name obj))
    (:type . "boolean")
    (:description . ,(description obj))
    (:readonly . ,(readonly obj))))


(defgeneric get-attribute (attr attr-name)
  (:documentation "Fetch the attribute with a given name, from a schema-rtypes instance. Return a `schema-rtype-attrs` instance if it's present, or NIL otherwise."))

(defmethod get-attribute ((attr schema-rtypes) (attr-name string))
  (find-if #'(lambda (att) (equal attr-name (name att)))
           (attributes attr)))

(defmethod a-listify ((obj schema-rtype-attrs))
  `((:name . ,(name obj))
    (:description . ,(description obj))
    (:readonly . ,(readonly obj))
    (:values . ,(attrvalues obj))))

(defmethod p-listify ((obj schema-rtype-attrs))
  `(:name ,(name obj)
    :description ,(description obj)
    :readonly ,(readonly obj)
    :values ,(attrvalues obj)))


(defclass schema-rels ()
  ((name :initarg :name
         :reader name
         :type string)
   (target-type :initarg :target-type
                :reader target-type
                :type schema-rtypes)
   (cardinality :initarg :cardinality
                :reader cardinality
                :type string)
   (dependent :initarg :dependent
              :reader dependent
              :type boolean
              :initform nil)
   (description :initarg :description
          :reader description
          :type (or null string)
          :initform nil))
  (:documentation "Relationships between resourcetypes, for use in schema definitions."))

(defmethod a-listify ((obj schema-rels))
  `((:name . ,(name obj))
    (:target-type . ,(name (target-type obj)))
    (:cardinality . ,(cardinality obj))
    (:dependent . ,(dependent obj))
    (:description . ,(description obj))))

(defmethod p-listify ((obj schema-rels))
  `(:name ,(name obj)
    :target-type ,(name (target-type obj))
    :cardinality ,(cardinality obj)
    :dependent ,(if (dependent obj) "true" "false")
    :description ,(description obj)))

(defmethod make-schema-rels (&key name target-type cardinality dependent description)
  (declare (type string name cardinality)
           (type schema-rtypes target-type)
           (type boolean dependent)
           (type (or null string) description))
  "Constructor for schema-rels instances"
  (make-instance 'schema-rels :name name
                 :target-type target-type
                 :cardinality cardinality
                 :dependent dependent
                 :description description))


;; Incoming data, describing things _to be added to_ a schema

(defclass incoming-subschema-version ()
  ((name :initarg :name
         :reader name
         :type string
         :initform (error ":name parameter must be specified."))
   (resourcetypes :initarg :resourcetypes
                  :reader resourcetypes
                  :type (or null list)
                  :initform nil)
   (relationships :initarg :relationships
                  :reader relationships
                  :type (or null list)
                  :initform nil))
  (:documentation "Parent structure for a subschema definition. Used for installing a schema."))

(defmethod set-resourcetypes ((subschema incoming-subschema-version)
                              (rtypes list))
  (if (every #'(lambda (rtype) (typep rtype 'incoming-rtypes))
             rtypes)
      (setf (slot-value subschema 'resourcetypes) rtypes)))

(defmethod set-relationships ((obj incoming-subschema-version)
                              (rels list))
  (if (every #'(lambda (rtype) (typep rtype 'incoming-rels))
             rels)
      (setf (slot-value obj 'relationships) rels)))

(defun make-incoming-subschema-version (&key name resourcetypes relationships)
  (declare (type string name)
           (type list resourcetypes))
  "Constructor for incoming-subschema-version instances."
  (let ((subschema (make-instance 'incoming-subschema-version :name name)))
    (when resourcetypes
      (set-resourcetypes subschema resourcetypes))
    (when relationships
      (set-relationships subschema relationships))
    ;; Return the instance that we created and modified
    subschema))


(defclass incoming-rtypes (abstract-rtypes)
  ()
  (:documentation "Resource-type definition, normally generated by digesting a JSON document."))

(defmethod set-attributes ((rtype incoming-rtypes) (attributes list))
  (if (every #'(lambda (attr) (typep attr 'incoming-rtype-attrs))
             attributes)
      (setf (slot-value rtype 'attributes) attributes)))


(defun make-incoming-rtypes (&key name dependent description attributes)
  "Constructor for incoming-rtypes class."
  (declare (type string name)
           (type boolean dependent)
           (type (or null string) description)
           (type list attributes))
  (log-message :debug (format nil "Creating an incoming-rtypes instance for ~A"
                              name))
  (let ((instance (make-instance 'incoming-rtypes
                                 :name name
                                 :dependent dependent
                                 :description description)))
    (when attributes (set-attributes instance attributes))
    ;; Return the instance that we created and modified
    instance))


(defclass incoming-rtype-attrs ()
  ((name :initarg :name
         :reader name
         :type string
         :initform (error ":name parameter must be specified."))
   (description :initarg :description
                :reader description
                :type (or null string))
   (readonly :initarg :readonly
              :reader readonly
              :type boolean
              :initform nil
              :documentation "Whether this attribute can be updated via client request. Default is nil, i.e. it's writeable."))
  (:documentation "Parent class for attributes of _incoming_ resource-types. Not to be instantiated directly; use one of its subclasses, or one of _their_ subclasses."))

(defgeneric extra-attr-params (attribute)
  (:documentation "Return any extra parameters relevant to this subclass of incoming-rtype-attrs."))

(defmethod extra-attr-params ((attribute incoming-rtype-attrs))
  "Default implementation. Returns an empty list."
  '())


(defclass incoming-rtype-attr-varchar (incoming-rtype-attrs)
  ((maxlength :initarg :maxlength
               :reader maxlength
               :type (or null integer)
               :initform nil  ; Default is unlimited
               :documentation "Maximum acceptable length of the string.")
   (attrvalues :initarg :attrvalues
                :reader attrvalues
                :type (or null list)
                :initform nil ; Default is not to restrict input to a set of values
                :documentation "When not null, this is a list of acceptable values for this attribute."))
  (:documentation "Basic variable-length string attribute. Use this for one-liners, like titles and summary descriptions; use incoming-rtype-attr-text for bulk text, such as Wikipages content."))

(defmethod extra-attr-params ((attribute incoming-rtype-attr-varchar))
  `(("type" . "varchar")
    ("maxlength" . ,(or (maxlength attribute) :null))
    ("attrvalues" . ,(or (attrvalues attribute) :null))))

(defclass incoming-rtype-attr-text (incoming-rtype-attrs)
  ()
  (:documentation "Bulk text field, e.g. Wikipages content. For smaller one-liner string attributes, use incoming-rtype-attr-string."))

(defmethod extra-attr-params ((attribute incoming-rtype-attr-text))
  `(("type" . "text")))

(defclass incoming-rtype-attr-integer (incoming-rtype-attrs)
  ((minimum :initarg :minimum
            :reader minimum
            :type (or null integer)
            :initform nil
            :documentation "Optional minimum value.")
   (maximum :initarg :maximum
            :reader maximum
            :type (or null integer)
            :initform nil
            :documentation "Optional maximum value."))
  (:documentation "Basic integer attribute, with optional minimum and maximum values."))

(defmethod extra-attr-params ((attribute incoming-rtype-attr-integer))
  `(("type" . "integer")
    ("minimum" . ,(or (minimum attribute) :null))
    ("maximum" . ,(or (maximum attribute) :null))))

(defclass incoming-rtype-attr-boolean (incoming-rtype-attrs)
  ()
  (:documentation "Basic boolean type."))

(defmethod extra-attr-params ((attribute incoming-rtype-attr-boolean))
  `(("type" . "boolean")))


(defun make-incoming-rtype-attrs (data)
  "Dispatching function that instantiates a suitable subclass of incoming-rtype-attrs."
  (let* ((alistp (consp (car data)))
         (name (if alistp
                 (cdr (assoc :NAME data))
                 (getf data :NAME)))
         (attribute-type (if alistp
                           (cdr (assoc :TYPE data))
                           (getf data :TYPE)))
         (description (if alistp
                        (cdr (assoc :DESCRIPTION data))
                        (getf data :DESCRIPTION)))
         ;; Force readonly to be a boolean
         (readonly (when (if alistp
                           (cdr (assoc :READONLY data))
                           (getf data :READONLY)) t)))
    (cond
      ;; varchar was specified
      ((equal "varchar" attribute-type)
       (make-instance 'incoming-rtype-attr-varchar
                      :name name
                      :description description
                      :readonly readonly
                      :maxlength (if alistp
                                   (cdr (assoc :MAXLENGTH data))
                                   (getf data :MAXLENGTH))
                      :attrvalues (if alistp
                                    (cdr (assoc :VALUES data))
                                    (getf data :VALUES))))
      ;; text was specified
      ((equal "text" attribute-type)
       (make-instance 'incoming-rtype-attr-text
                      :name name
                      :description description
                      :readonly readonly))
      ;; integer was specified
      ((equal "integer" attribute-type)
       (make-instance 'incoming-rtype-attr-integer
                      :name name
                      :description description
                      :readonly readonly
                      :minimum (if alistp
                                 (cdr (assoc :MINIMUM data))
                                 (getf data :MINIMUM))
                      :maximum (if alistp
                                 (cdr (assoc :MAXIMUM data))
                                 (getf data :MAXIMUM))))
      ;; boolean was specified
      ((equal "boolean" attribute-type)
       (make-instance 'incoming-rtype-attr-boolean
                      :name name
                      :description description
                      :readonly readonly))
      ;; No type was specified.
      ;; Fall back to varchar, with default values
      (t
        (make-instance 'incoming-rtype-attr-varchar
                       :name name
                       :description description
                       :readonly readonly)))))


(defclass incoming-rels ()
  ((name :initarg :name
         :reader name
         :type string
         :initform (error ":name parameter must be specified."))
   (source-type :initarg :source-type
                :reader source-type
                :type string
                :initform (error ":source-type parameter must be specified."))
   (target-type :initarg :target-type
                :reader target-type
                :type string
                :initform (error ":target-type parameter must be specified"))
   (cardinality :initarg :cardinality
                :reader cardinality
                :type string)
   (dependent :initarg :dependent
              :reader dependent
              :type boolean)
   (description :initarg :description
          :reader description
          :type (or null string)))
  (:default-initargs :cardinality "many:many")
  (:documentation "Relationships between resourcetypes, for use in updating schema definitions."))

(defun make-incoming-rels (&key name
                                source-type
                                target-type
                                (cardinality "many:many")
                                (dependent nil)
                                description)
  "Constructor function for incoming-rels"
  (declare (type string name source-type target-type cardinality)
           (type (or null string) description)
           (type boolean dependent))
  (log-message :debug (format nil "Creating an incoming-rels instance for ~A from ~A to ~A"
                              name source-type target-type))
  (unless (member cardinality '("many:many" "many:1" "1:many" "1:1") :test #'equal)
    (error "Cardinality argument is not valid."))
  (make-instance 'incoming-rels :name name
                 :source-type source-type
                 :target-type target-type
                 :cardinality cardinality
                 :dependent dependent
                 :description description))

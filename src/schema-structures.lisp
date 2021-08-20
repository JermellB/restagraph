;   Copyright 2017 James Fleming <james@electronic-quill.net>
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
               :type (or null list)
               :initform nil))
  (:documentation "Parent class for resourcetype subclasses. Not to be instantiated directly."))


(defclass schema-rtypes (abstract-rtypes)
  ((relationships
     :initarg relationships
     :reader relationships
     :type list
     :initform nil))
  (:documentation "Resource-type definition, normally generated by digesting a JSON document."))

(defgeneric a-listify (obj)
  (:documentation "Serialise an object to a-list format, for further serialisation to JSON."))

(defmethod a-listify ((obj schema-rtypes))
  `((:name . ,(name obj))
    (:dependent . ,(dependent obj))
    (:description . ,(description obj))
    (:attributes
      . ,(mapcar #'a-listify
                 (sort (attributes obj)
                       #'string<
                       :key #'name)))
    (:relationships
      . ,(mapcar #'a-listify
                 (sort
                   (sort
                     (relationships obj)
                     #'string<
                     :key #'(lambda (rel) (name (target-type rel))))
                   #'string<
                   :key #'name)))))

(defmethod set-attributes ((rtype schema-rtypes) (attributes list))
  (if (every #'(lambda (attr) (typep attr 'schema-rtype-attrs))
             attributes)
      (setf (slot-value rtype 'attributes) attributes)))

(defmethod set-relationships ((obj schema-rtypes) (rels list))
  (if (every #'(lambda (rel) (typep rel 'schema-rels))
             rels)
      (setf (slot-value obj 'relationships) rels)
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
   (attr-values :initarg :attr-values
                :reader attr-values
                :type (or null list)))
  (:documentation "Attributes of resource-types"))


(defgeneric get-attribute (attr attr-name)
  (:documentation "Fetch the attribute with a given name, from a schema-rtypes instance."))

(defmethod get-attribute ((attr schema-rtypes) (attr-name string))
  (car
    (remove-if-not #'(lambda (att) (equal attr-name (name att)))
                   (attributes attr))))

(defmethod a-listify ((obj schema-rtype-attrs))
  `((:name . ,(name obj))
    (:description . ,(description obj))
    (:values . ,(attr-values obj))))


(defun make-schema-rtype-attrs (&key name description attr-values)
  (declare (type string name)
           (type (or null string) description)
           (type list attr-values))
  "Constructor for schema-rtype-attrs instances."
  (if (or (null attr-values)
          (and (listp attr-values)
               (every #'stringp attr-values)))
      (make-instance 'schema-rtype-attrs :name name
                     :description description
                     :attr-values attr-values)
      (error ":values arg must be a list of strings.")))


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
   (attr-values :initarg :attr-values
           :reader attr-values
           :type (or null list)))
  (:documentation "Attributes of _incoming_ resource-types."))

(defun make-incoming-rtype-attrs (&key name description attr-values)
  (declare (type string name)
           (type (or null string) description)
           (type (or null list) attr-values))
  (if (or (null attr-values)
          (and (listp attr-values)
               (every #'stringp attr-values)))
      (make-instance 'incoming-rtype-attrs
                     :name name
                     :description description
                     :attr-values attr-values)
      (error ":values arg must be a list of strings.")))


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
  (unless (member cardinality '("many:many" "many:1" "1:many" "1:1") :test #'equal)
    (error "Cardinality argument is not valid."))
  (make-instance 'incoming-rels :name name
                 :source-type source-type
                 :target-type target-type
                 :cardinality cardinality
                 :dependent dependent
                 :description description))

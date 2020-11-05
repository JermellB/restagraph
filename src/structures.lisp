(in-package #:restagraph)

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
  ;; list of `schema-rtype-attrs` structs
  (attributes nil :type list :read-only t)
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


(defclass restagraph-acceptor (tbnl:easy-acceptor)
  ;; Class attributes
  ((datastore :initarg :datastore
              :reader datastore
              :initform (error "Datastore object must be supplied.")
              :documentation "An object representing the datastore, on which the generic functions will be dispatched.")
   (uri-base-api :initarg :uri-base-api
                 :reader uri-base-api
                 :initform "/raw/v1"
                 :documentation "Base URI on which the raw API is to be presented.")
   (uri-base-schema :initarg :uri-base-schema
                    :reader uri-base-schema
                    :initform "/schema/v1"
                    :documentation "Base URI on which the schema API is to be presented.")
   (uri-base-files :initarg :uri-base-files
                   :reader uri-base-files
                   :initform "/files/v1"
                   :documentation "Base URI on which the files API is to be presented.")
   (files-location :initarg :files-location
                   :reader files-location
                   :initform (error "files-location is required"))
   (schema :accessor schema
           :initform (make-hash-table :test #'equal)))
  ;; Class defaults
  (:default-initargs :address "127.0.0.1")
  (:documentation "Customised Hunchentoot acceptor, subclassed from tbnl:easy-acceptor. Carries additional configuration data for the site."))


;;; Methods

(defmethod get-relationship ((rtype schema-rtypes)
                             (rel-type string)
                             (target-rtype string))
  (remove-if-not #'(lambda (rel)
                     (and
                       (equal rel-type (schema-rels-relationship rel))
                       (equal target-rtype (schema-rels-target-type rel))))
                 (schema-rtypes-relationships rtype)))

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
          (if (get-relationship rtype (schema-rels-relationship new-rel) (schema-rels-target-type new-rel))
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

(in-package #:restagraph)

(defstruct schema
  "Defines a schema."
  (name nil :type string :read-only t)
  (version 0 :type integer :read-only t)
  (resourcetypes nil :type list :read-only t)   ; List of schema-rtypes structs
  (relationships nil :type list :read-only t))  ; List of schema-rels structs

(defstruct schema-rtypes
  "Resource-type definition, for use in schema definitions."
  (name nil :type string :read-only t)
  (dependent nil :type boolean :read-only t)
  (notes nil :type string :read-only t)
  (attributes nil :type list :read-only t))

(defstruct schema-rtype-attrs
  "Attributes of resource-types"
  (name nil :type string :read-only t)
  (description "" :type string :read-only t)
  (values nil :type list :read-only t))

(defstruct schema-rels
  "Relationships between resourcetypes, for use in schema definitions."
  (uri nil :type string :read-only t)
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

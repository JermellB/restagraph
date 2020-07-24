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
  (dependent nil :type boolean :read-only t))

;;; Customised Hunchentoot acceptor.
;;; Carries information about the datastore being used.
(defclass restagraph-acceptor (tbnl:easy-acceptor)
  ;; Class attributes
  ((datastore :initarg :datastore
              :reader datastore
              :initform (error "Datastore object must be supplied.")
              :documentation "An object representing the datastore, on which the generic functions will be dispatched.")
   (uri-base-api :initarg :uri-base-api
                 :reader uri-base-api
                 :initform (error "uri-base-api is required"))
   (uri-base-schema :initarg :uri-base-schema
                    :reader uri-base-schema
                    :initform (error "uri-base-schema is required"))
   (uri-base-files :initarg :uri-base-files
                   :reader uri-base-files
                   :initform (error "uri-base-files is required"))
   (files-location :initarg :files-location
                   :reader files-location
                   :initform (error "files-location is required")))
  ;; Class defaults
  (:default-initargs :address "127.0.0.1")
  (:documentation "vhost object, subclassed from tbnl:easy-acceptor"))

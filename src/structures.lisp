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

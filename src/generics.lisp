(in-package #:restagraph)


;;;; Schema

(defgeneric get-resources-from-db (db)
  (:documentation "Extract the resource definitions from the database"))

(defgeneric get-resource-relationships-from-db (db)
  (:documentation "Extract the relationships between the resource types, from the database"))

(defgeneric add-resourcetype-to-schema (schema resourcetype)
  (:documentation "Add a resource-type to a schema, ensuring the internal structure is ready to receive new attributes and relationships."))

(defgeneric get-resourcetype-from-schema-by-name (schema resourcename)
  (:documentation "Extract a resource' definition from the schema, by name."))

(defgeneric add-resource-relationship-to-schema (schema from-resource relationship to-resource)
  (:documentation "Update the schema with a directional relationship between two resource types, returning an error if either of the resource types doesn't exist."))


;;;; Resource instances

(defgeneric store-resource (db resourcetype attributes)
  (:documentation "Store a resource in the database.
Return an error if
- the resource type is not present in the schema
- any of the attributes is not defined in the schema for this resource type."))

(defgeneric get-resource-by-uid (db resourcetype uid)
  (:documentation "Retrieve a representation of a resource from the database."))

(defgeneric delete-resource-by-uid (db resourcetype uid)
  (:documentation "Delete a resource from the database."))

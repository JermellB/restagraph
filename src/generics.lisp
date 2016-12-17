(in-package #:restagraph)


;;;; Schema

(defgeneric get-resource-names-from-db (db)
  (:documentation "Extract the names of resource definitions from the database"))

(defgeneric get-resource-attributes-from-db (db resourcetype)
  (:documentation "Extract the attributes from resource definitions from the database"))

(defgeneric relationship-valid-p (db from-resource relationship to-resource)
  (:documentation "Checks whether this type of relationship is permitted between these types of resources. Returns a boolean."))

(defgeneric create-db-schema (db)
  (:documentation "Creates whatever schema is most appropriate to the DB engine in use"))

(defgeneric validate-resource-before-creating (db resourcetype params)
  (:documentation "Confirm whether the provided data is valid, before attempting to use it to create a resource.
  If the data is valid, return a list of attributes suitable for feeding to Neo4J.
  If not, raise a restagraph:integrity-error"))


;;;; Resource instances

(defgeneric store-resource (db resourcetype attributes)
  (:documentation "Store a resource in the database.
Return an error if
- the resource type is not present in the schema
- any of the attributes is not defined in the schema for this resource type."))

(defgeneric get-resource-by-uid (db resourcetype uid)
  (:documentation "Retrieve a representation of a resource from the database."))

(defgeneric delete-resource-by-uid (db resourcetype uid)
  (:documentation "Delete a resource from the database. Automatically remove all relationships to other nodes."))

;;;; Relationships

(defgeneric create-relationship (db source-type source-uid reltype dest-type dest-uid)
  (:documentation "Create a relationship between two resources"))

(defgeneric get-resources-with-relationship (db resourcetype uid reltype)
  (:documentation "Retrieve a summary of all resources with a given relationship to this one.
  Return a list of two-element lists, where the first element is the resource-type and the second is the UID."))

(defgeneric delete-relationship (db source-type source-uid reltype dest-type dest-uid)
  (:documentation "Delete a relationship between two resources"))

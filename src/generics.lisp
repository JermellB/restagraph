(in-package #:restagraph)

;;;; Utility methods

(defgeneric load-cypher-file (db filepath)
  (:documentation "Read a file and execute each non-comment, non-blank line in turn"))


;;;; Schema

(defgeneric get-resource-defs-from-db (db)
  (:documentation "Extract resource definitions from the database"))

(defgeneric get-resource-attributes-from-db (db resourcetype)
  (:documentation "Extract the attributes from resource definitions from the database"))

(defgeneric relationship-valid-p (db from-resource relationship to-resource)
  (:documentation "Checks whether this type of relationship is permitted between these types of resources. Returns a boolean."))

(defgeneric enforce-db-schema (db)
  (:documentation "Creates whatever schema is most appropriate to the DB engine in use. Currently enforces the presence/absence of uniqueness constraints according to whether each resource is dependent."))

(defgeneric validate-resource-before-creating (db resourcetype params)
  (:documentation "Confirm whether the provided data is valid, before attempting to use it to create a resource.
  If the data is valid, return a list of attributes suitable for feeding to Neo4J.
  If not, raise a restagraph:integrity-error"))


;;;; Resource instances

(defgeneric dependent-resource-p (db resourcetype)
  (:documentation "Determine whether a resource-type is dependent or first-class"))

(defgeneric store-resource (db resourcetype attributes)
  (:documentation "Store a resource in the database.
Return an error if
- the resource type is not present in the schema
- any of the attributes is not defined in the schema for this resource type."))

(defgeneric store-dependent-resource (db uri attributes)
  (:documentation "Create a dependent resource, at the end of the path given by URI. Its parent resource must exist, and the relationship must be a valid dependent relationship."))

(defgeneric move-dependent-resource (db uri newparent)
  (:documentation "Take an existing dependent resource, and give it a new parent, where both are identified by their URI paths."))

(defgeneric get-resources (db uri)
  (:documentation "Adaptable method to search for resources in a manner deterined by the modulo-3 length of the URI."))

(defgeneric get-dependent-resources (db sourcepath)
  (:documentation "Return a list of the resources that depend on this one, where the list contains 3-element lists of relationship, type and UID."))

(defgeneric get-dependent-relationships-for-type (db resource-type)
  (:documentation "Return a list of relationships for which the given resource is dependent."))


;;;; Relationships

(defgeneric dependent-relationship-p (db source-type relationship dest-type)
  (:documentation "Determine whether this relationship is a dependent one between these two resource-types."))

(defgeneric create-relationship-by-path (db sourcepath destpath)
  (:documentation "Create a relationship between two arbitrary, pre-existing resources. The last element of the sourcepath must be the relationship type."))

(defgeneric get-resources-with-relationship (db resourcetype uid reltype)
  (:documentation "Retrieve a summary of all resources with a given relationship to this one.
  Return a list of two-element lists, where the first element is the resource-type and the second is the UID."))

(defgeneric check-relationship-by-path (db sourcepath relationship destpath)
  (:documentation "Confirm whether this relationship exists between these resources."))

(defgeneric delete-relationship-by-path (db relpath targetpath)
  (:documentation "Delete a relationship based on its path, and that of its target. More precise than delete-path, especially when a node has the same relationship to 2+ identical dependent resources."))


;;;; Both

(defgeneric delete-resource-by-path (db path &key delete-dependent)
  (:documentation "Delete a relationship or resource according to the URI supplied"))

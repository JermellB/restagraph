(in-package #:restagraph)


;;;; Schema

(defgeneric get-classes-from-db (db)
  (:documentation "Extract the class definitions from the database"))

(defgeneric get-class-relationships-from-db (db)
  (:documentation "Extract the relationships between the classes, from the database"))

(defgeneric add-class-to-schema (schema newclass)
  (:documentation "Add a class to a schema, ensuring the internal structure is ready to receive new attributes and relationships."))

(defgeneric get-class-from-schema-by-name (schema classname)
  (:documentation "Extract a class' definition from the schema, by name."))

(defgeneric add-class-relationship-to-schema (schema from-class relationship to-class)
  (:documentation "Update the schema with a directional relationship between two classes, returning an error if either of the classes doesn't exist."))


;;;; Class instances

(defgeneric store-class-instance (db classname attributes)
  (:documentation "Store an instance of a class in the database.
Return an error if
- the classname is not present in the schema
- any of the attributes is not defined in the schema for this class"))

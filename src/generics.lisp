;   Copyright 2017 James Fleming <james@electronic-quill.net>
;
;   Licensed under the GNU General Public License
;   - for details, see LICENSE.txt in the top-level directory

(in-package #:restagraph)


;;;; Schema

(defgeneric add-resourcetype (db resourcetype &key dependent notes)
  (:documentation "Create a resource, add its attributes, and update the database's uniqueness constraints. Attributes are supplied as a list of strings, naming them. If :dependent evaluates to True, it will be created as a dependent type. Notes provide the opportunity to explain the intent of each resourcetype."))

(defgeneric resourcetype-exists-p (db resourcetype)
  (:documentation "Verify whether we have a definition for a resourcetype by this name."))

(defgeneric resourcetype-relationship-exists-p (db source relationship dest)
  (:documentation "Verify whether a specific relationship is present. Return a boolean."))

(defgeneric add-resourcetype-attribute (db resourcetype &key name description)
  (:documentation "Add an attribute to an existing resourcetype"))

(defgeneric resourcetype-attribute-exists-p (db resourcetype attribute)
  (:documentation "Verify whether we have a definition for a resourcetype attribute by this name"))

(defgeneric get-resource-attributes-from-db (db resourcetype)
  (:documentation "Extract the attributes from resource definitions from the database"))

(defgeneric update-resourcetype-attribute (db resourcetype name &key description)
  (:documentation "Update the attributes of a resourcetype's attributes."))

(defgeneric delete-resourcetype-attribute (db resourcetype name)
  (:documentation "Remove an attribute from a resourcetype. Don't delete existing data; leave it in place bu inaccessible via this API."))

(defgeneric delete-resourcetype (db resourcetype)
  (:documentation "Delete a resource-type, and all its instances along with any relationships to other types."))

(defgeneric add-resource-relationship (db parent-type relationship dependent-type &key dependent cardinality notes)
  (:documentation "Create a relationship between two resource types. Notes provide the opportunity to explain the intent of each resourcetype."))

(defgeneric delete-resource-relationship (db parent-type relationship dependent-type)
  (:documentation "Delete a dependency between two resource types. If this removes the last relationship on which a dependent resource-type depends, that type and all its instances will also be deleted."))

(defgeneric get-resource-types (db)
  (:documentation "Extract resource definitions from the database"))

(defgeneric describe-resource-type (db resourcetype &key resources-seen)
  (:documentation "Return the description of a resource-type, as an alist.
                   Entries include :name, :attributes and :dependent.
                   The :recursive key is a boolean indicating whether to recursively traverse all the relationships
                   from this resource-type.
                   The :resources-seen key is used internally to break loops when recursing."))

(defgeneric describe-dependent-resources (db resourcetype &key resources-seen)
  (:documentation "Return a list of descriptions of all the dependent resourcetypes for this resource.
                   Entries include :name :attributes and :dependent.
                   The :recursive key is a boolean indicating whether to recursively traverse all the relationships
                   The :resources-seen key is used internally to break loops when recursing."))

(defgeneric relationship-valid-p (db from-resource relationship to-resource)
  (:documentation "Checks whether this type of relationship is permitted between these types of resources. Returns a boolean."))

(defgeneric validate-resource-before-creating (db resourcetype params)
  (:documentation "Confirm whether the provided data is valid, before attempting to use it to create a resource.
  If the data is valid, return a list of attributes suitable for feeding to Neo4J.
  If not, raise a suitable error"))


;;;; Resource instances

(defgeneric dependent-resource-p (db resourcetype)
  (:documentation "Determine whether a resource-type is dependent or first-class"))

(defgeneric store-resource (db resourcetype attributes)
  (:documentation "Store a resource in the database. Attributes argument is expected in the form of an alist.
Return an error if
- the resource type is not present in the schema
- the client attempts to set attributes that aren't defined for this resourcetype."))

(defgeneric delete-resource-by-path (db path &key recursive)
  (:documentation "Delete a relationship or resource according to the URI supplied.
  :recursive confirms that you intend to delete all resources depending on the one identified in the path."))

(defgeneric update-resource-attributes (db path attributes)
  (:documentation "Add, update or delete a set of attributes of a given resource."))

(defgeneric delete-resource-attributes (db path attributes)
  (:documentation "Delete attributes from a resource."))

(defgeneric store-dependent-resource (db uri attributes)
  (:documentation "Create a dependent resource, at the end of the path given by URI. Its parent resource must exist, and the relationship must be a valid dependent relationship."))

(defgeneric move-dependent-resource (db uri newparent)
  (:documentation "Take an existing dependent resource, and give it a new parent, where both are identified by their URI paths."))

(defgeneric get-resources (db uri &key filters directional)
  (:documentation "Adaptable method to search for resources in a manner deterined by the modulo-3 length of the URI.
                  The optional 'filters parameter is for refining the search results."))

(defgeneric get-dependent-resources (db sourcepath)
  (:documentation "Return a list of the resources that depend critically on this one.
                   The returned list contains 3-element lists of relationship, type and UID."))

(defgeneric critical-dependency-p (db path)
  (:documentation "Determine whether the resource identified by this path depends solely on its relationship to its immediate parent on that path.
                   Return a boolean."))

(defgeneric update-resource-attributes (db path attributes)
            (:documentation "Add, update or delete a set of attributes of a given resource."))


;;;; Relationships

(defgeneric dependent-relationship-p (db source-type relationship dest-type)
  (:documentation "Determine whether this relationship is a dependent one between these two resource-types."))

(defgeneric create-relationship-by-path (db sourcepath destpath)
  (:documentation "Create a relationship between two arbitrary, pre-existing resources. The last element of the sourcepath must be the relationship type."))

(defgeneric get-resources-with-relationship (db resourcetype uid reltype)
  (:documentation "Retrieve a summary of all resources with a given relationship to this one.
  Return a list of two-element lists, where the first element is the resource-type and the second is the UID."))

(defgeneric check-relationship-by-path (db sourcepath relationship destpath)
  (:documentation "Confirm whether this relationship exists between these resources.
                   A special-case method for avoiding the ambiguity that can catch out get-resources."))

(defgeneric delete-relationship-by-path (db relationship-uri target-resource)
  (:documentation "Delete a relationship based on its path, and that of its target.
                  Arguments:
                  - relationship-uri = URI of the relationship itself
                  - target-resource = /<type>/<uid> of the resource at the end of the relationship.
                  This form is required to distinguish between deleting the relationship, and the resource itself."))

(defgeneric get-relationship-attrs (db source-type relationship dest-type)
  (:documentation "Extract the attributes of interest for a given relationship.
                  Return a 'relationship-attrs struct.
                  cardinality defaults to many:many."))

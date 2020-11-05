;   Copyright 2017 James Fleming <james@electronic-quill.net>
;
;   Licensed under the GNU General Public License
;   - for details, see LICENSE.txt in the top-level directory

(in-package #:restagraph)


;;;; Schema

(defgeneric resourcetype-exists-p (db resourcetype)
  (:documentation "Verify whether we have a definition for a resourcetype by this name."))

(defgeneric resourcetype-relationship-exists-p (db source relationship dest)
  (:documentation "Verify whether a specific relationship is present. Return a boolean."))

(defgeneric get-resource-attributes-from-db (db resourcetype)
  (:documentation "Extract the attributes from resource definitions from the database"))

(defgeneric get-resourcetype-names (db)
  (:documentation "Return the names of resourcetypes, as a list of strings."))

(defgeneric describe-resource-type (db resourcetype &key resources-seen)
  (:documentation "Return the description of a resource-type, as an alist.
                   Entries include :name, :attributes and :dependent.
                   The :recursive key is a boolean indicating whether to recursively traverse all the relationships
                   from this resource-type.
                   The :resources-seen key is used internally to break loops when recursing."))

(defgeneric describe-resource-type-for-graphql (db
                                                resourcetype
                                                all-resourcetype-names
                                                rels-from-any
                                                &key resources-seen)
  (:documentation "Return the description of a resource-type, as an alist.
                   Entries include :name, :attributes and :dependent.
                   The :recursive key is a boolean indicating whether to recursively traverse all the relationships
                   from this resource-type.
                   The :resources-seen key is used internally to break loops when recursing."))

(defgeneric describe-dependent-resources (db resourcetype &key resources-seen)
  (:documentation "Return a list of descriptions of all the dependent resourcetypes for this resource.
                   The :resources-seen key is used internally to break loops when recursing.
                   Return format is a list of conses, whose car is a relationship-attrs struct,
                   and whose cdr is the name of the target resourcetype."))

(defgeneric validate-resource-before-creating (db resourcetype params)
  (:documentation "Confirm whether the provided data is valid, before attempting to use it to create a resource.
  If the data is valid, return a list of attributes suitable for feeding to Neo4J.
  If not, raise a suitable error"))

(defgeneric add-rel-to-schema-rtype (rtype new-rel)
  (:documentation "Add a relationship to a schema-rtypes struct. If it already has a relationship of the same type to the same target-type, the one being added replaces the old one."))

(defgeneric get-relationship (rtype rel-type target-rtype)
  (:documentation "Check for a named type of relationship from a resourcetype to a target resourcetype. Return a schema-rels struct if present, otherwise nil.
  Note that it returns a list, which will have more than one element if we screwed up and allowed two relationships of the same type to the same target."))


;;;; Resource instances

(defgeneric dependent-resource-p (db resourcetype)
  (:documentation "Determine whether a resource-type is dependent or first-class"))

(defgeneric store-resource (db resourcetype attributes)
  (:documentation "Store a resource in the database. Attributes argument is expected in the form of an alist.
Return an error if
- the resource type is not present in the schema
- the client attempts to set attributes that aren't defined for this resourcetype."))

(defgeneric delete-resource-by-path (db targetpath &key recursive)
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

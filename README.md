# Restagraph

# What it is, what it does and what it's for*

A framework of sorts for producing a REST API for a Neo4J graph database, from a schema defined within that database.

* OK, what it _will_ be, once I've implemented it. This is the spec I'm working from.


# What goes in the database

Objects/resources are defined with the label `rgClass`; their name becomes the label used to create their nodes in the database.

Their attributes are created as objects with the label `rgAttribute`, linked via the `hasAttribute` relationship. This is partly because it's the best fit with the graph model, and partly because it enables me to add attributes to the attributes later, such as `MIMEtype` or `mandatory`.

The third element is relationships between `rgClass` objects. These are implemented as regular Neo4J relationships, and define the relationships that can be created from one class instance to another.

# What API you get

For each `rgClass` object, the following patterns are recognised by the application server:

Create/retrieve/delete an object of type `<class-name>`:
```
/api/v1/<class-name>/<unique ID>/
```

Create/retrieve/delete an attribute for an object of type `<class-name>`:
```
/api/v1/<class-name>/<unique ID>/<attribute-name>
```

Create/delete a relationship to another object:
```
/api/v1/<class-name>/<unique ID>/<relationship>/<unique ID of target node>
```

Search for objects of type `<class-name>`, matching a set of attribute/value pairs:
```
/api/v1/<class-name>/?<attribute-name>=<value>
```

Search for objects to which this one has a particular kind of relationship, optionally matching a set of attribute/value pairs:
```
/api/v1/<class-name>/<unique ID>/<relationship>/?<attribute-name>=<value>
```

Per the usual REST pattern, the HTTP methods POST, GET and DELETE are used for create, retrieve and delete operations, respectively.

# Stuff I'm currently thinking of adding once that's working

- discovery, because it'd be nice to get a list of classes/resources and their attributes
- hypermedia links, to help with on-the-fly discovery
- regex searching for text content in attributes

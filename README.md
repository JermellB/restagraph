# Restagraph

Restagraph is an application that dynamically generates a REST API in front of a [Neo4j](https://neo4j.com/) graph database, based on a schema defined within that same database. It basically takes the relational database model and enables you to join one table to lots of others without drowning in many-to-many intermediate tables, and adds a REST API on top.

This includes features such as:

- constraints on the relationships that can be created between two types of resource
    - this includes cardinality constraints, i.e. 1:1, 1:many, many:1 and many:many relationships
- resources which only make sense in the context of other resources, e.g. interfaces on computers.

Thus, it gives you a schema and constraints similar in spirit to a relational database, but without grinding to a halt on lots of many-to-many table joins.

All of this works without any _need_ for a regular user to know about its internals. You define the schema and it generates the API and applies the constraints.

There is explicit support for dependent resources, i.e. resources that only make sense in the context of another.


## Benefits, a.k.a. the point of this thing

- data integrity: it ensures that the data that goes _in_ to a Neo4j database has a consistent structure
- language independence: the REST API means that any language can be used to build applications on top of this structure

It is _not_ intended to be the sole interface for _querying_ the database. You _can_ use it to do that in a structured way, and it's useful when constructing the data input portions of an application, but the more complex the question you want to ask of the database, the more likely it is that you'll want to go straight to [Cypher](https://neo4j.com/developer/cypher-basics-i/), Neo4j’s graph query language.


## Why use a graph database?

Referential flexibility, in short: this is built for a type of problem for which graph databases are perfectly suited, and for which relational databases are not well suited at all.

The author is very fond of relational databases, but found one of their natural limitations while developing [Syscat](https://github.com/equill/syscat): if you want to be able to link one type of thing to any number of other types of things without giving up referential integrity, you start drowning in many-to-many tables and the DBMS starts grinding to a halt.


## Defining the schema

The schema is defined in YAML files.

It can be subdivided into multiple files; back-references are permitted (and expected) but forward references are not. That is, you can add relationships and attributes to resourcetypes that have already been defined, but you can't create relationships to resourcetypes that haven't yet been defined.

These files can be in any directory, but must all be in the same one. They're read in alphanumeric order, so you can prepend serial numbers to control the sequence in which they're applied, enabling later schemas to depend on resources defined in previous ones. These are also version-controlled, and version updates will be applied on startup.

Their contents must be in the form of a dict with the following entries:
- `name`
    - value must be a string
    - this is the name that will be recorded in the database for version-control purposes; the filename will be ignored.
- `version`
    - value should be a number, preferably an integer. The code has only been used with integers; behavious with other types of value is undefined and unsupported.
    - If this is greater than the existing version for the schema with this name, or there's no record of a schema with that name, the schema will be applied, and then this number will be set as a version. The "current" version is assumed to be the highest number of a version linked to the schema name, so there's no "currentVersion" link or attribute to manage.
    - `resourcetypes`
        - a list of dicts
    - `relationships`
        - a list of sets

Examples of valid schemas are found in the [Syscat sources](https://bitbucket.org/equill/syscat/src/schemas/master/). These include examples of backward references.


## Elements of the schema

### Resource-types

The types of things you can create via the API.

The UID is a required attribute for all resourcetypes; you can't create a resource without one, so it isn't explicitly mentioned in the schema.

Attributes you can define:

- whether it's a dependent type.
- notes about the resource-type, i.e. what kind of thing it represents, and how it's intended to be used.
- a list of attributes
    - each attribute can have attributes of its own, such as `comments` or `vals`
        - `vals` is a reserved attribute-name. It's a comma-separated list of values, which identifies the resource-type as an enum. If defined, the API will only accept values in this list when setting the value of such an attribute. Note that it's only enforced at this time; changing the list of values will not cause any changes to existing values.


### Relationships between resource-types

These define the relationships that the API will allow you to create between a pair of resource-types.

These are directional, and encode whether they can be used to connect a dependent type to its parent type, but don't allow for storing attributes in the relationship between two resources.


## Test suite

Two test suites are included:
- `test/test-rest-api.py` for confirming its operation from a client's perspective
- `restagraph-test` package for confirming internal correctness, using [FiveAM](https://common-lisp.net/project/fiveam/).


# Docker image

It's available from Dockerhub as `equill/restagraph`

Wants a volume mounted at `/files` for storing uploaded files.

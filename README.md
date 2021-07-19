# Restagraph - what is it?

Restagraph is an application that dynamically generates a REST-ish API in front of a [Neo4j](https://neo4j.com/) graph database, based on a schema defined within that same database. The auto-generated API is regular and consistent, making it easy to build automation against.

This includes features such as:

- constraints on the relationships that can be created between two types of resource
    - this includes cardinality constraints, i.e. 1:1, 1:many, many:1 and many:many relationships
- resources which only make sense in the context of other resources, e.g. interfaces on computers.

Thus, it gives you a schema and constraints similar in spirit to a relational database, but with much more flexibility.

There is explicit support for dependent resources, i.e. resources that only make sense in the context of another.


## Benefits, a.k.a. the point of this thing

- Data integrity: it ensures that the data that goes _in_ to a Neo4j database has a consistent structure.
- Language independence: the REST API means that any language can be used to build applications on top of this structure.


## Why use a graph database?

Referential flexibility, in short: this is built for a type of problem for which graph databases are perfectly suited, and for which relational databases are not well suited at all.

The author is very fond of relational databases, but found one of their natural limitations while developing [Syscat](https://github.com/equill/syscat): if you want to be able to link one type of thing to any number of other types of things without giving up referential integrity, you start drowning in many-to-many tables and the DBMS starts grinding to a halt.


# Quick start

It's a Docker system, comprising two images: the database and the application server.

- Download `https://github.com/equill/restagraph/blob/develop/scripts/docker/docker-compose.yml` (this will soon be merged into the `main` branch)
- Create two local Docker volumes:
    - `docker volume create -d local rgtestfiles`
    - `docker volume create -d local rgtestdata`
- If your Docker tooling doesn't do this for you, pre-download the latest docker images for `equill/restagraph` (the application itself) and `neo4j` (the database server)
- Start it up with this command: `docker stack deploy -c docker-compose.yml rgtest`

After short delay, it should be available on `http://10.255.0.1:4965/` (depending on your computer's Docker network configs).

To inspect the built-in schema (installing this is part of the start-up delay): `curl http://10.255.0.1:4965/schema/v1`. It's output is not human-friendly, so you'll probably want to install the wonderful JSON-querying utility `jq`, and pipe the output through that for pretty-printing: `curl http://10.255.0.1:4965/schema/v1 | jq .` (don't forget the trailing dot, which tells `jq` to print everything from the document-root down). All the documentation for Restagraph assumes that you've installed `jq`, but it's not _necessary_, just exceedingly helpful.

When you look at the schema, pay attention to the `any` resourcetype. It's a special case: relationships from the `any` type can be created with, well, _any_ resourcetype as the source. E.g, you can link any resource in the database to a tag or a group.

You can change most of the details in `docker-compose.yml` to suit your own needs. There's nothing significant about the port numbers, volume names or network name, Restagraph doesn't _need_ to listen on all possible addresses, and it's always good practice to change the database password.


## Demo session

The following sequence gives you an overview of what you can do with Restagraph. The one shortcoming is that it sticks within the core schema, which doesn't include any dependent resources.

Create a person: `curl -X POST -d 'uid=James' http://10.255.0.1:4965/raw/v1/People`

Look at the person you created: `curl http://10.255.0.1:4965/raw/v1/People/James`

Check all the characteristics of a `People` resource: `curl http://10.255.0.1:4965/raw/v1/People | jq .`

Create a tag: `curl -X POST -d 'uid=developer' http://10.255.0.1:4965/raw/v1/Tags`

Create another tag: `curl -X POST -d 'uid=sysadmin' http://10.255.0.1:4965/raw/v1/Tags`

List all the tags in the database: `curl http://10.255.0.1:4965/raw/v1/Tags`

Tag the person: `curl -X POST -d 'target=/Tags/developer` http://10.255.0.1:4965/raw/v1/People/James/TAGS`

Take another look at the person you created: `curl http://10.255.0.1:4965/raw/v1/People/James | jq.`

Give him another tag: `curl -X POST -d 'target=/Tags/sysadmin` http://10.255.0.1:4965/raw/v1/People/James/TAGS`

List his tags: `curl http://10.255.0.1:4965/raw/v1/People/James/TAGS/Tags | jq.`

List anything to which he has a TAGS relationship: `curl http://10.255.0.1:4965/raw/v1/People/James/TAGS | jq.` - note that each object in the result now shows its type. This may seem like a subtle distinction at first glance, but it demonstrates something fundamental to the way Restagraph works: you can define the same relationship from the same resourcetype to any number of target resourcetypes. As a trivial example, an organisation can have a CUSTOMERS relationship to people _and_ to other organisations.


## Defining the schema

The schema is defined in JSON files, which are uploaded via a separate API.

There is a core schema, containing the essential resources and relationships on which the API itself relies.


## Elements of the schema

### Resource-types

The types of things you can create via the API.

The UID is a required attribute for all resourcetypes; you can't create a resource without one, so it isn't explicitly mentioned in the schema.

Attributes built into every resource:

- the UID (Unique IDentifier).
- whether it's a dependent type.
- notes about the resource-type, i.e. what kind of thing it represents, and how it's intended to be used.

You can also define attributes of your own

Each attribute has a couple of attributes of its own, such as `comments` or `vals`
- `name`
- `description`
- `values` is a reserved attribute-name. It's a comma-separated list of values, which identifies the resource-type as an enum. If defined, the API will only accept values in this list when setting the value of such an attribute. Note that it's only enforced at this time; changing the list of values will not cause any changes to existing values.


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

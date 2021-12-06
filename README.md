# Restagraph - what is it?

Restagraph is an application that dynamically generates an HTTP API in front of a [Neo4j](https://neo4j.com/) graph database, based on a schema defined within that same database. The auto-generated API is regular and consistent, making it easy to build automation against it.

This includes features such as:

- constraints on the relationships that can be created between two types of resource
    - this includes cardinality constraints, i.e. 1:1, 1:many, many:1 and many:many relationships
- resources which only make sense in the context of other resources, e.g. interfaces on computers.

Thus, it gives you a schema and constraints similar in spirit to a relational database, but with much more flexibility.

There is explicit support for dependent resources, i.e. resources that only make sense in the context of another.


## Benefits, a.k.a. the point of this thing

- Consistent data structure: it ensures that the data that goes _in_ to a Neo4j database follows a consistent and predictable structure.
- Language independence: the REST API means that any language can be used to build applications on top of this structure.
- Development speed: with a single JSON file, you define both the database schema and the API.


## Why use a graph database?

Referential flexibility, in short: this is built for a type of problem for which graph databases are perfectly suited, and for which relational databases are not well suited at all.

Relational databases are a great fit for many use-cases, but I found one of their natural limitations while developing [Syscat](https://github.com/equill/syscat): if you want to be able to link one type of thing to any number of other types of things without giving up referential integrity, you start drowning in many-to-many tables and the DBMS starts grinding to a halt.


## Where to get it

Source code is in [equill/restagraph on GitHub](https://github.com/equill/restagraph).

Docker images are [Docker hub](https://hub.docker.com/repository/docker/equill/restagraph).


## License

GPLv3.

My intention: you're free to use it, and you're free to build on it. If you extend the code itself, I expect you to share your changes.


# Quick start

It's a Docker system, comprising two images: the database and the application server.

- Download `https://github.com/equill/restagraph/blob/develop/scripts/docker/docker-compose.yml`
- Check what address you've configured Docker to listen on. This documentation asssumes 192.0.2.1, so if your system uses a different address, remember to make that change as you follow along.
    - To make sure of it, create a file at `/etc/docker/daemon.json` with the contents `{ "bip": "192.0.2.1/24" }`
    - Why this subnet? It's reserved for testing and documentation, per [RFC5727](https://datatracker.ietf.org/doc/html/rfc5737) so it's unlikely to collide with anything you're actually using. It actually specifies three subnets for this purpose, so you can take your pick:
        - 192.0.2.0/24 (TEST-NET-1)
        - 198.51.100.0/24 (TEST-NET-2)
        - 203.0.113.0/24 (TEST-NET-3)
- Create two local Docker volumes, one for the database and one for storing uploaded files:
    - `docker volume create -d local rgtestfiles`
    - `docker volume create -d local rgtestdata`
- If your Docker tooling doesn't do this for you, pre-download the latest docker images for `equill/restagraph` (the application itself) and `neo4j` (the database server). Use the versions referrred to in `docker-compose.yml`, because that's the definitive reference.
- Start it up with this command: `docker stack deploy -c docker-compose.yml rgtest`

After short delay, it should be available on `http://192.0.2.1:4952/` (depending on your computer's Docker network configs).

To inspect the built-in schema: `curl http://192.0.2.1:4950/schema/v1`. It's output is not human-friendly, so you'll probably want to install the wonderful JSON-querying utility [jq](https://stedolan.github.io/jq/), and pipe the output through that for pretty-printing: `curl http://192.0.2.1:4950/schema/v1 | jq .` (don't forget the trailing dot, which tells `jq` to print everything from the document-root down). All the documentation for Restagraph assumes that you've installed `jq`, but it's not _necessary_ - it's just exceedingly helpful.

When you look at the schema, pay attention to the `any` resourcetype. It's a special case: relationships from the `any` type can be created with, well, _any_ resourcetype as the source. E.g, you can link any resource in the database to a tag or a group. It's useful to note that the server only checks for a relationship from the source type to the target type _after_ checking for one between those two resourcetypes, which means, you can override or pre-empt the relationship from `any` and use it as a catch-all fallback.

You can change most of the details in `docker-compose.yml` to suit your own needs. There's nothing significant about the port numbers, volume names or network name, Restagraph doesn't _need_ to listen on all possible addresses, and it's always good practice to change the database password.


## Demo session

A detailed, fairly comprehensive walkthrough is at [docs/Demo_session.md](docs/Demo_session.md)


# The schema - how it works

## Short version

The schema is stored in the database. Restagraph checks for it on startup; if one is present, it loads that into memory. If it doesn't find one, it installs the default (core) schema, then an additional schema if one is included, and loads the result into memory.

The additional schema is defined in a JSON file; you can upload any number of them via a separate API, and in fact this is the process for updating/upgrading a built-in one.

The core schema contains the essential resources and relationships on which the API itself relies, hence the name.


## Elements of the schema

### Resource-types

The types of things you can create via the API. If you're familiar with Object-Oriented Programming, resourcetypes correspond to classes and resources equate to instances.

The UID is a required attribute for all resourcetypes; you can't create a resource without one, so it isn't explicitly mentioned in the schema.

Attributes built into every resourcetype:

- The name
    - This is how the resourcetype is addressed via the API, so it needs to be UID-safe.
    - Following the [Neo4j naming conventions](https://neo4j.com/docs/cypher-manual/current/syntax/naming/), resourcetype names should be in PascalCase.
- Whether it's a dependent type
    - Dependent types only exist in relation to another resource. E.g, a room only exists in the context of a building.
- Notes about the resource-type, i.e. what kind of thing it represents, and how it's intended to be used.

Resourcetypes can have any number of user-defined attributes. Each of these is defined with three characteristics:
- `name`
    - These are not specified in the Neo4j conventions, so I've gone with lowercase.
- `description`
    - This only appears in the schema. It's for clarifying what the attribute is for, or how it's to be used.
- `values` is an optional list of acceptable values for an attribute. If it's not set, it has no effect.
    - It's a comma-separated list of values, which turns the attribute into a kind of enum. If it's defined, the API will only accept values in this list when setting the attribute's value.


### Relationships between resource-types

These define the relationships that the API will allow you to create from one resourcetype to another. Note that they're directional.

Mandatory attributes, which must be specified when defining one of these:

- `name` = the name used when referring to this relationship in a URI.
    - Following the [Neo4j naming conventions](https://neo4j.com/docs/cypher-manual/current/syntax/naming/), relationship names should be in `SCREAMING_SNAKE_CASE`.
        - This has nothing to do with it being the coolest case-name in the history of case-names, but it's a pleasing coincidence.
- `sourcetype` = the resourcetype that the relationship is _from_.
- `target-type` = the resourcetype it connects _to_.
- `cardinality` = how many relationships of this kind are to be permitted from an instance of the `sourcetype`, and how many to an instance of the `target-type`. Valid options are
    - `many:many`
    - `1:many`
    - `many:1`
    - `1:1`

Optional attributes:

- `dependent` = whether this relationship is from a parent resource to a dependent one.
    - default is `false`
- `notes` = descriptive text, clarifying the intended meaning/purpose of this relationship.
    - default is `null`


### Resources

That is, instances of a resourcetype. Their attributes are:

- The UID (Unique IDentifier)
    - This is how the resource is addressed via the API, so it needs to be UID-safe. Restagraph automatically sanitises these on the way in.
    - This is the only attribute you're _required_ to specify, when you create a resource.
- Original UID
    - The un-sanitised version of the requested UID, regardless of whether it's different from the sanitised version.
    - This is autogenerated, so you don't need to (or get to) specify it.
- Creation date/time
    - A datestamp in Unix epoch time, i.e. seconds since midnight at the start of January 01 1970, recording the time at which this resource was created.
    - Another autogenerated attribute.
- Last-modified date/time
    - Also a datestamp, in the same format as `createddate`. This records the last time this resource was changed.
- User-defined attributes
    - Whatever attributes are defined in the schema.
    - These can be set when you create the resource with a POST request, or via PUT at any time after that.


### Relationships between resources

The simplest of the lot, because they have no user-serviceable attributes inside.

Created via POST, as long as they meet the constraints defined in the schema _at that moment in time_.


### Inspect the schema

For a graphic view of the current schema in the Neo4j browser, use this query:
```
MATCH (:RgSchema {name: "root"})-[:CURRENT_VERSION]->(:RgSchemaVersion)-[:HAS]->(s:RgResourceType)<-[:SOURCE]-(r:RgRelationship)-[:TARGET]->(t:RgResourceType) RETURN s, t, r;
```


# Test suite

Two test suites are included:
- `test/test-rest-api.py` for confirming its operation from a client's perspective
- `restagraph-test` package for confirming internal correctness, using [FiveAM](https://common-lisp.net/project/fiveam/).


# More information

There's more in the `docs` folder in this repo.

- For more detail about the HTTP API, read [docs/HTTP_API.md](docs/HTTP_API.md).
- The IPAM API (IP Address Management) gets a separate file: [docs/IPAM_API.md](docs/IPAM_API.md).
- For information aobut the access-control mechanism, read [docs/Access-control.md](docs/Access-control.md).

You may also want to look over the core schema (`src/core-schema.lisp`) to see what's installed by default before you add anything else.

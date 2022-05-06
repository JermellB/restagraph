# Restagraph - what is it?

Restagraph is an application that dynamically generates an HTTP API in front of a [Neo4j](https://neo4j.com/) graph database, based on a schema defined inside that database. The auto-generated API is regular and consistent, making it easy to build automation and GUIs on top of it.

This includes features such as:

- Declaring the type of data that can be stored in each attribute of a resource.
- Constraints on the relationships that can be created between two types of resource.
    - This includes cardinality constraints, i.e. 1:1, 1:many, many:1 and many:many relationships.
- Resources which only make sense in the context of other resources, e.g. interfaces on computers.

Thus, it gives you a schema and constraints similar in spirit to a relational database, but with the flexibility that only a graph database can provide.

For convenient reference, [the docs are on Github Pages](https://equill.github.io/restagraph/).


## Benefits, a.k.a. the point of this thing

- Predictable, consistent data structure: you know for sure what kinds of data are stored, and how they can be connected to each other.
- Language independence: the REST API means that any language can be used to build applications on top of this structure.
- Development speed: with a single JSON file, you define both the database schema and the API.
- GUI independence: the rules that define and validate the data structures are not mixed in with the code that generates a particular GUI for the app.
    - You can build multiple GUIs, including mobile ones, that all automatically conform to the same rules.
    - You can build automation against the application, still with all the same guarantees about the data.


## What does it look like?

Screenshots don't help much, because it's an HTTP API, so here's a quick demo of the very heart of it:

Create a JSON file with the following contents:

    {
      "name": "Movies",
      "resourcetypes": [
        { "name": "Movie" }
      ],
      "relationships": [
        {
          "source-type": "People",
          "name": "ACTED_IN",
          "target-type": "Movie",
          "cardinality": "many:many"
        }
      ]
    }

This defines a resourcetype called `Movie`, and a relationship from `People` to `Movie` (and only in that direction) called `ACTED_IN`. For this relationship, any number of people can be recorded to have acted in a movie, and a person can have acted in many movies. You don't need to define the "People" resourcetype, because that's predefined in Restagraph.

Upload that file to the server, via a command like this:

    curl -X POST --data-urlencode schema@core_demo.json http://192.0.2.1:4950/schema/v1

Now create a person and a movie, and link them:

    curl -X POST -d 'uid=Keanu Reeves' http://192.0.2.1:4950/raw/v1/People
    curl -X POST -d 'uid=The Matrix' http://192.0.2.1:4950/raw/v1/Movie
    curl -X POST -d 'target=/Movie/The_Matrix' http://192.0.2.1:4950/raw/v1/People/Keanu_Reeves/ACTED_IN

Ask the server what movies Keanu has acted in:

    curl -s http://192.0.2.1:4950/raw/v1/People/Keanu_Reeves/ACTED_IN
    [{"type":"Movie","uid":"The_Matrix","createddate":3851927697,"original_uid":"The Matrix"}]

For full detail about what it's capable of, read the [HTTP API docs](docs/HTTP_API.md); for a detailed walkthrough of how to use it with the Neo4j movies dataset, read the [demo session](docs/Demo_session.md).


## Where to get it

Source code is in [equill/restagraph on GitHub](https://github.com/equill/restagraph).

Docker images are on [Docker hub](https://hub.docker.com/repository/docker/equill/restagraph).


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
- Whether its value is read-only.
    - This is really only useful for built-in attributes, e.g. the checksum that the server computes for each file that's uploaded.


### User-defined resource attributes

Resourcetypes can have any number of user-defined attributes. Each of these is defined with three main characteristics:

- `name`
    - These are not specified in the Neo4j conventions, so I've gone with lowercase.
    - Don't name them with a leading `RG`. Technically you _can_, but if it collides with a term used by the API for some other kind of filtering, the reserved term takes precedence. Filtered terms currently include:
        - `RGoutbound`
        - `RGinbound`
- `description`
    - This only appears in the schema. It's for clarifying what the attribute is for, or how it's to be used.
- `type`
    - What kind of data can be stored in this attribute. Available options include
        - `varchar` (the default)
        - `text` (for large stretches of text)
        - `integer` and `boolean`.
    - You can define further constraints on some types:
        - maximum length for a varchar
        - a list of acceptable values for a varchar (turning it into an enum type)
        - maximum and/or minimum values for an integer.


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


**Important note:** The `any` resourcetype can be used to define relationships from "any" other resourcetype to this one, or from this one to "any" other. So it's important to remember that when you query the Schema API about a resourcetype, the `relationships` section _combines_ outbound relationships from "any" with those from the specific type you're querying it about. The same applies when it's validating a request to create a relationship.

That's the `any` resourcetype's reason for existence; the server won't allow you to create an instance, or to query one; it's only there to make relationship definitions manageable.


# The API

That is, how you actually put data into the system, and get it back out again.

## Resources

That is, instances of a resourcetype. Their attributes are:

- The UID (Unique IDentifier)
    - This is how the resource is addressed via the API, so it needs to be UID-safe. Restagraph automatically sanitises these on the way in.
        - The list of acceptable characters is the unaccented Latin alphabet, digits, plus four non-alphanumeric characters (`-`, `_`, `.` and `~`). This is the set of "unreserved characters" from section 2.3 of [RFC 3986](https://www.ietf.org/rfc/rfc3986.txt).
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
    - These can be included when you create the resource with a POST request, or set/updated via PUT at any time after that.


## Relationships between resources

The simplest of the lot, because they have no user-serviceable attributes inside.

Created via POST, as long as they meet the constraints defined in the schema _at that moment in time_.


# Test suite

Two test suites are included:
- Client-side python tests, using `pytest`.
- Internal tests of the implementation itself.


# More information

There's more in the `docs` folder in this repo:

- For more detail about the HTTP API, read [docs/HTTP_API.md](docs/HTTP_API.md).
- For more about defining your own subschemas, see [docs/Defining_a_schema.md](docs/Defining_a_schema.md)
- The IPAM API (IP Address Management) gets a separate file: [docs/IPAM_API.md](docs/IPAM_API.md).
- For information about the access-control mechanism, read [docs/Access-control.md](docs/Access-control.md).
- The core schema is documented in [docs/schema.html](docs/schema.html).

You may also want to look over the core schema to see what's installed by default before you add anything else. It's summarised in [Core_schema.md](docs/Core_schema.md), and the definitive reference is always in the source code: `src/core-schema.lisp`.

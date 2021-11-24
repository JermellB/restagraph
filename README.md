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

The following sequence gives you an overview of what you can do with Restagraph, using the familiar arena of movies, actors and directors.

This uses two command-line tools:

- `curl` - the canonical tool for using HTTP from the CLI.
- `jq` - an incredibly useful tool for filtering JSON and presenting it in human-readable form. You'll see the output of each HTTP GET request piped through `jq` in this example, because it's just so much more readable.
    - It's _useful_, not _required_, so you'll probably want to leave it out when you're piping the output into some other utility, unless you're using it for filtering.
    - It's the reason for using `curl -s`, where the `-s` flag means "don't print out extra information about the HTTP request.
    - The trailing dot in `jq .` _is_ significant: it tells `jq` to print out everything it receives, without filtering its input.

A core schema is automatically installed, which includes people. You'll need to install the movies demo schema, though. Assuming it's listening on port 4950 on address 192.0.2.1, you can `cd` to the top of the repo, and upload it with the following command:

    curl -X POST --data-urlencode schema@schemas/movies_demo.json http://192.0.2.1:4950/schema/v1

This demonstrates one of Restagraph's features: you can augment the schema and API at any time, by uploading more definitions.

Create a person:

    curl -X POST -d 'uid=Keanu Reeves' http://192.0.2.1:4950/raw/v1/People

Look at the person you created:

    curl -s http://192.0.2.1:4950/raw/v1/People/Keanu_Reeves | jq .

Note that the space in his UID has become an underscore. Restagraph does this automatically, so that everything can be referred to by its URL, with as few issues as possible. You'll also see the UID that was originally requested, and the date and time at which this entry was created in seconds since 01 January 1970.

Check all the characteristics of a `People` resource:

    curl http://192.0.2.1:4950/schema/v1/People | jq .

We can add a display-name and a note to a person. We use PUT for this, because we're updating an attribute on an existing resource:

    curl -X PUT -d 'displayname=Keanu Reeves' -d 'notes=May or may not be married to Winona Ryder.' http://192.0.2.1:4950/raw/v1/People/Keanu_Reeves

Look at him again, and now we see the extra details we just added:

    curl -s http://192.0.2.1:4950/raw/v1/People/Keanu_Reeves | jq .

Now there's also an `updateddate` timestamp, so you can see when a resource was last changed, separately from when it was created.

Add a movie he acted in:

    curl -X POST -d 'uid=Dracula' -d 'year_released=1992' http://192.0.2.1:4950/raw/v1/Movies

Now link them together, in both directions. Why _both_ directions? Because Restagraph enables you to trace paths through the graph with URLs, and I didn't see a practical way of embedding "follow this link backwards" in a URL.

    curl -X POST -d 'target=/Movies/Dracula' http://192.0.2.1:4950/raw/v1/People/Keanu_Reeves/ACTED_IN
    curl -X POST -d 'target=/People/Keanu_Reeves' http://192.0.2.1:4950/raw/v1/Movies/Dracula/ACTORS

OK, so what can we find out about movies? What does the schema say we can record about them?

    curl -s http://192.0.2.1:4950/schema/v1/Movies | jq .

That's a little more information than I really wanted. Let's use `jq` to filter out only the information about what relationships you can create from movies to other things:

    curl -s http://192.0.2.1:4950/schema/v1/Movies | jq .relationships

Actors and directors. OK, let's see who we already know acted in that movie?

    curl -s http://192.0.2.1:4950/raw/v1/Movies/Dracula/ACTORS | jq .

Keanu - what a surprise. But we already know he's a person (or a people), and right now we're only interested in the human actors in that movie anyway, so we have two reasons to ask for just the _people_ who acted in it:

    curl -s http://192.0.2.1:4950/raw/v1/Movies/Dracula/ACTORS/People | jq .

Ah, that's a little more compact. This seems kinda verbose either way, though, compared to a REST query. It's because Restagraph allows you to create the same kind of relationship to different things (a person might direct a movie, a TV show, or a music video). So it tells you explicitly about the type of each thing at the far end of the link, and you can ask for all the things of one particular type that this thing has some relationship to. It makes for some extra typing when you're doing this by hand, but who the heck does this kind of thing by hand when you can put a web page in front of it, or write a script to do it?

Let's add another actor for that movie. This time, we'll include all the details while creating it, instead of adding them afterward:

    curl -X POST -d 'uid=Winona Ryder' -d 'displayname=Winona Ryder' -d 'notes=May or may not be married to Keanu Reeves' http://192.0.2.1:4950/raw/v1/People
    curl -X POST -d 'target=/Movies/Dracula' http://192.0.2.1:4950/raw/v1/People/Winona_Ryder/ACTED_IN
    curl -X POST -d 'target=/People/Winona_Ryder' http://192.0.2.1:4950/raw/v1/Movies/Dracula/ACTORS

Now she's in that movie, right?

    curl -s http://192.0.2.1:4950/raw/v1/Movies/Dracula/ACTORS/People | jq .

OK, we're looking good.

But that's not all Winona's done, is it? She's been in other movies, and on TV as well:

    curl -X POST -d 'uid=Beetlejuice' http://192.0.2.1:4950/raw/v1/Movies
    curl -X POST -d 'target=/Movies/Beetlejuice' http://192.0.2.1:4950/raw/v1/People/Winona_Ryder/ACTED_IN
    curl -X POST -d 'uid=Stranger Things' http://192.0.2.1:4950/raw/v1/TvSeries
    curl -X POST -d 'target=/TvSeries/Stranger_Things' http://192.0.2.1:4950/raw/v1/People/Winona_Ryder/ACTED_IN

OK, now let's look at what she's acted in:

    curl -s http://192.0.2.1:4950/raw/v1/People/Winona_Ryder/ACTED_IN | jq .

We see both movies and TV series, and now it becomes a little more clear just why these URLs include both the relationship _and_ the type of thing there's a relationship to. Let's see just the movies she's been in:

    curl -s http://192.0.2.1:4950/raw/v1/People/Winona_Ryder/ACTED_IN/Movies | jq .

Now just the TV series:

    curl -s http://192.0.2.1:4950/raw/v1/People/Winona_Ryder/ACTED_IN/TvSeries | jq .


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

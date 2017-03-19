# Restagraph

## What it is, what it does and what it's for

A framework of sorts for producing a REST API for a Neo4J graph database, from a schema defined within that database.

The aim is a black box that automagically converts a schema into an API, without any _need_ for a regular user to know about its internals.

There is explicit support for dependent resources, i.e. resources that only make sense in the context of another.

### Where it came from

Impetus 1: I wanted something that would enforce a schema for Neo4j, which I gather a few other people would also like. It would also be nice to have a REST API whose shape matches that of the schema, to make it easier to think in terms of the problem domain, and to save application users having to learn Cypher before being able to do anything with it.

Impetus 2: I needed to prototype schemas for [Syscat](https://github.com/equill/syscat), and writing everything by hand was just tedious.

it certainly doesn't solve all cases, but it's working nicely for its niche.

Note that I'm still primarily focused on Syscat, so development of Restagraph will be driven by that project's needs until it has matured enough that I can divide my attention again. Or unless I find out somebody's using this and needs me to fix/add something.

### Current state

The good: it does its primary job well enough for Syscat development to be ticking along. It's usable _if_ you're familar with running Common Lisp applications that are based on Hunchentoot. If that sentence made no sense to you, you want to wait until I've Dockerised this, which is ticket #18.

The less good: it still needs a lot of polish, hardening, documentation, and general stuff to make it operations-ready.


## What goes in the database

Objects/resources are defined with the label `rgResource`; their name becomes the label used to create their nodes in the database.

Their attributes are defined as objects with the label `rgAttribute`, linked to the `rgResource` node via the `hasAttribute` relationship. This is partly because it's the best fit with the graph model, and partly because it enables me to add attributes to the attributes later, such as `MIMEtype` or `mandatory`.

*Note:* attribute names must be in lowercase, due to the way this system handles them. I hope to lift this restriction in future.

The third element is relationships between `rgResource` objects. These are implemented as regular Neo4J relationships, and define the relationships that can be created from one resource instance to another.


## The API it generates

HTTP return codes are used to indicate success or error, and the Content-type header is set according to whether text or JSON is being returned. As a rule, JSON will be returned on success, and plain text for anything else. The one salient exception is when deleting a resource or relationship, where the MIME-type is "text/plain" and the return code is `NO CONTENT`.

The definitive API reference is in `test/test-rest-api.py`

For each `rgResource` object, the following patterns are recognised by the application server:


### Create a resource
```
POST /api/v1/<resource-type>/
```

With payload of `uid=<uid>`, plus optionally `<attribute-name>=<value>` pairs for any subset of the attributes defined for this resource type.

On success, returns a code of 201 and a JSON representation of the newly-created resource.

The UID must actually be unique for each resource-type. That is, if you define a `routers` resource and a `switches` resource, no two routers can have the same UID, but a router and a switch can. Bear this in mind when designing your schema.


### Retrieve a resource
```
GET /api/v1/<resource-type>/<uid>
```

Returns a JSON representation of the resource.


### Retrieve all resources of a given type
```
GET /api/v1/<resource-type>/
```

Returns a JSON representation of all resources of that type, or 404 if there aren't any.


### Delete a resource
```
DELETE /api/v1/<resource-type>
```
Requires a payload of `'uid=<uid>'`, and any other parameters are ignored.

Returns `204 (NO CONTENT)` on success.


### Create a relationship from one resource to another
Note that, due to the way Neo4J works, these are always directional.

```
POST /api/v1/<resource-type>/<Unique ID>/<relationship>
with parameter: 'target' = '/type/uid'
```

Parameter _must_ include `type` and `uid`, and _may_ also include `attributes`.

If the destination resource doesn't already exist, it will be automatically created first. This has to be done as a separate transaction; beware race-conditions where two clients try to create the same thing at the same time.


### Retrieve the type and UID of all resources to which this one has a specific relationship
```
GET /api/v1/<resource-type>/<Unique ID>/<relationship>
```


### Delete a relationship to another object
```
DELETE /api/v1/<resource-type>/<Unique ID>/<relationship>/<Unique ID>
```


### Search for objects to which this one has a particular kind of relationship, optionally matching a set of attribute/value pairs
```
GET /api/v1/<resource-type>/<Unique ID>/<relationship>/
```

### Create a resource that depends on another for its context
This is defined in the schema by adding the attribute `dependent=true` to the dependent `rgResource` definition, and by then adding the same attribute to the relationships to that resource-type from resource-types that are valid parents.
It's valid to create resources that depend on other dependent resources, with no limit to the depth of these chains.
```
POST /api/v1/<parent-type>/<parent-uid>/<relationship-type>
with parameters: 'type=<child-type>' and 'uid=<child-uid>' (both are required)
```

### Delete a dependent resource
Either use the `DELETE` method on the full path to the resource in question to remove it specifically, or pass the `delete-dependent=true` parameter to the API call to one of its parents further up the chain.
The `delete-dependent` parameter acts recursively downward from whatever resource is being deleted.

### Move a dependent resource from one parent to another
Note that the new parent must be a valid parent for the child resource, and the new relationship must also be a valid dependent relationship.
```
POST /api/v1/path/to/dependent/resource
with parameter: 'target=/uri/path/to/new/parent/and/relationship'
```

## Working example

Using the following Cyper to define the schema:
```
CREATE (:rgResource {name: 'routers'});
CREATE (:rgResource {name: 'interfaces'});
CREATE (:rgResource {name: 'ipv4Addresses'});
MATCH (r:rgResource {name: 'routers'}), (i:rgResource {name: 'interfaces'}) CREATE (r)-[:Interfaces]->(i);
MATCH (i:rgResource {name: 'interfaces'}), (a:rgResource {name: 'ipv4Addresses'}) CREATE (i)-[:Ipv4Addresses]->(a);
```

Create a router:
```
prompt> curl -i -X POST -d 'uid=amchitka' -d 'comment=Router 1' http://localhost:4950/api/v1/routers
HTTP/1.1 201 Created
Content-Length: 11
Date: Tue, 06 Dec 2016 19:47:57 GMT
Server: Hunchentoot 1.2.35
Content-Type: text/plain; charset=utf-8

201 CREATED
```

Retrieve its details:
```
prompt> curl -i http://localhost:4950/api/v1/routers/amchitka
HTTP/1.1 200 OK
Content-Length: 39
Date: Tue, 06 Dec 2016 19:48:08 GMT
Server: Hunchentoot 1.2.35
Content-Type: application/json

{"uid":"amchitka","comment":"Router 1"}
```

Delete it:
```
prompt> curl -i -X DELETE -d 'uid=amchitka' -d 'comment=Router 1' http://localhost:4950/api/v1/routers
HTTP/1.1 200 OK
Content-Length: 2
Date: Tue, 06 Dec 2016 19:48:15 GMT
Server: Hunchentoot 1.2.35
Content-Type: text/plain; charset=utf-8

OK
```

Confirm that it's gone:
```
prompt> curl -i http://localhost:4950/api/v1/routers/amchitka
HTTP/1.1 404 Not Found
Content-Length: 40
Date: Tue, 06 Dec 2016 19:48:18 GMT
Server: Hunchentoot 1.2.35
Content-Type: text/plain; charset=utf-8

No routers found with a UID of amchitka.
```

## Test suite

Two test suites are included:
- `test/test-rest-api.py` for confirming its operation from a client's perspective
- `restagraph-test` package for confirming internal correctness, using [FiveAM](https://common-lisp.net/project/fiveam/).

# Restagraph

## What it is, what it does and what it's for*

A framework of sorts for producing a REST API for a Neo4J graph database, from a schema defined within that database.

* OK, what it _will_ be, once I've implemented it. This is the spec I'm working from.


## What goes in the database

Objects/resources are defined with the label `rgResource`; their name becomes the label used to create their nodes in the database.

Their attributes are created as objects with the label `rgAttribute`, linked via the `hasAttribute` relationship. This is partly because it's the best fit with the graph model, and partly because it enables me to add attributes to the attributes later, such as `MIMEtype` or `mandatory`.

The third element is relationships between `rgResource` objects. These are implemented as regular Neo4J relationships, and define the relationships that can be created from one resource instance to another.


## The API it generates

For each `rgResource` object, the following patterns are recognised by the application server:

Create a resource:
```
POST /api/v1/<resource-type>
```

With payload of `'uid=<uid>'`, plus optionally `'<attribute>=attribute'` pairs for any subset of the attributes defined for this resource type.

Returns 201 CREATED if it succeeded.


Retrieve a resource:
```
GET /api/v1/<resource-type>/<uid>
```

Returns a JSON representation of the resource, with status code 200.


Delete a resource:
```
DELETE /api/v1/<resource-type>
```
Requires a payload of `'uid=<uid>'`, and any other parameters are ignored.

Returns 200 if it succeeded.


## The API it _will_ generate in the future

Create/delete a relationship to another object:
```
POST|GET|DELETE /api/v1/<resource-name>/<unique ID>/<relationship>
```


Create/retrieve/delete an attribute for an object of type `<resource-name>`:
```
POST|GET|DELETE /api/v1/<resource-name>/<unique ID>/<attribute-name>
```


Search for objects of type `<resource-name>`, matching a set of attribute/value pairs:
```
GET /api/v1/<resource-name>/?<attribute-name>=<value>
```


Search for objects to which this one has a particular kind of relationship, optionally matching a set of attribute/value pairs:
```
GET /api/v1/<resource-name>/<unique ID>/<relationship>/?<attribute-name>=<value>
```


## Working example

Using the following Cyper to define the schema
```
CREATE (:rgResource {name: 'routers'});
CREATE (:rgResource {name: 'interfaces'});
CREATE (:rgResource {name: 'ipv4Addresses'});
MATCH (r:rgResource {name: 'routers'}), (i:rgResource {name: 'interfaces'}) CREATE (r)-[:Interfaces]->(i);
MATCH (i:rgResource {name: 'interfaces'}), (a:rgResource {name: 'ipv4Addresses'}) CREATE (i)-[:Ipv4Addresses]->(a);
```

Create a router
```
prompt> curl -i -X POST -d 'uid=amchitka' -d 'comment=Router 1' http://localhost:4950/api/v1/routers
HTTP/1.1 201 Created
Content-Length: 11
Date: Tue, 06 Dec 2016 19:47:57 GMT
Server: Hunchentoot 1.2.35
Content-Type: text/plain; charset=utf-8

201 CREATED
```

Retrieve its details
```
prompt> curl -i http://localhost:4950/api/v1/routers/amchitka
HTTP/1.1 200 OK
Content-Length: 39
Date: Tue, 06 Dec 2016 19:48:08 GMT
Server: Hunchentoot 1.2.35
Content-Type: application/json

{"uid":"amchitka","comment":"Router 1"}
```

Delete it
```
prompt> curl -i -X DELETE -d 'uid=amchitka' -d 'comment=Router 1' http://localhost:4950/api/v1/routers
HTTP/1.1 200 OK
Content-Length: 2
Date: Tue, 06 Dec 2016 19:48:15 GMT
Server: Hunchentoot 1.2.35
Content-Type: text/plain; charset=utf-8

OK
```

Confirm that it's gone
```
prompt> curl -i http://localhost:4950/api/v1/routers/amchitka
HTTP/1.1 404 Not Found
Content-Length: 40
Date: Tue, 06 Dec 2016 19:48:18 GMT
Server: Hunchentoot 1.2.35
Content-Type: text/plain; charset=utf-8

No routers found with a UID of amchitka.
```

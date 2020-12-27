# API reference

## Supported HTTP methods

Restagraph makes use of several of the HTTP methods, using their standard meanings according to [RFC 7231](https://tools.ietf.org/html/rfc7231).


### GET

Retrieve the content of one or more resources.

HTTP status code on success is 200 (OK).


### POST

Create a new resource.

HTTP status code on success is 201 (created).


### PUT

Update one or more attributes of a resource.

HTTP status code on success is 204 (no content).


### DELETE

Ensure that a resource is not present.

HTTP status code on success is 204 (no content).



# The Schema API

This describes the types of resources that can be created on "this" instance, and what optional attributes they can have.

It´s the "other half" of the main (raw) API, and is read-only - there is no provision for updating the schema via this API.

Use this to get a description of the available resource-types, in JSON format.


## GET - retrieve the schema

The endpoint base URI is `/schema/v1`:

`GET /schema/v1/` will return a description of all resources.

`GET /schema/v1/<resourcetype>` will return the description of a single resourcetype.


# The Raw API (resources)

This is where you perform most CRUD operations on resources in the database. It´s called "raw" because it doesn´t have any domain-specific logic, like you find in the `files` API (described in the next section).

HTTP return codes are used according to RFC7231, and the Content-type header is set according to whether text or JSON is being returned. As a rule, JSON will be returned on success, and plain text for anything else. The one salient exception is when deleting a resource or relationship, where the MIME-type is "text/plain" and the return code is `NO CONTENT`.

The definitive API reference is in `test/test-rest-api.py`, along with usage examples.

The following patterns are recognised by the application server:


## Create a resource

```
POST /api/v1/<resource-type>/
```

With payload of `uid=<uid>`, plus optionally `<attribute-name>=<value>` pairs for any subset of the attributes defined for this resource type.

On success, returns a status code of 201 and the URI for the newly-created resource. That URI is returned both in the body of the HTTP response, and in the `Location` header.

The UID must actually be unique for each resource-type. That is, if you define a `routers` resource and a `switches` resource, no two routers can have the same UID, but a router and a switch can. Bear this in mind when designing your schema.


## Retrieve a resource

```
GET /api/v1/<resource-type>/<uid>
```

Returns a JSON representation of the resource. On success, returns a status code of 200.


## Retrieve all resources of a given type

```
GET /api/v1/<resource-type>/
```

Returns a JSON representation of all resources of that type, with a status code of 200, or 404 if there aren´t any.


## Update one or more attributes of a resource

```
PUT /api/v1/<resource-type>/<resource UID>
```

The payload should be supplied in the request body, POST-style. Among other considerations, this avoids the 1024-character limit for GET-style parameters.

This always returns a status code of 204 (no content) on success.

Although [section 4.3.4 of RFC 7231](https://tools.ietf.org/html/rfc7231#section-4.3.4) states that 201 must be returned "[i]f the target resource does not have a current representation and the PUT successfully creates one," this API provides for updating multiple resources in a single request, making it entirely possible to create, update _and_ delete attributes in a single transaction. It seems like a backward step to restrict clients to updating a single attribute per request, so I´m making the counter-argument that unpopulated attributes have the de facto representation of `Null`, so _technically_ there aren´t any valid resources lacking representation in the context of this method.


## Delete a resource

```
DELETE /api/v1/<resource-type>
```
Requires a payload of `'uid=<uid>'`, and any other parameters are ignored.

Returns 204 (no content) on success.


## Create a relationship from one resource to another
Note that, due to the way Neo4J works, these are always directional.

```
POST /api/v1/<resource-type>/<Unique ID>/<relationship>
with parameter: 'target' = '/type/uid'
```

Parameter _must_ include `type` and `uid`, and _may_ also include `attributes`.

If the destination resource doesn´t already exist, it will be automatically created first. This has to be done as a separate transaction; beware race-conditions where two clients try to create the same thing at the same time.

Returns the URI of the newly-created path through this relationship.


## Retrieve the type and UID of all resources to which this one has a specific relationship
```
GET /api/v1/<resource-type>/<Unique ID>/<relationship>
```


## Delete a relationship to another object
```
DELETE /api/v1/<resource-type>/<Unique ID>/<relationship>/<Unique ID>
```


## Search for objects to which this one has a particular kind of relationship, optionally matching a set of attribute/value pairs
```
GET /api/v1/<resource-type>/<Unique ID>/<relationship>/?<attribute>=<value>
```

Regular expressions based on [Java regexes](https://docs.oracle.com/javase/7/docs/api/java/util/regex/Pattern.html) can be used. Negation can be effected by putting `!` at the start of the regex.


## Search for objects with a set relationship to another resource

This is currently limited to one hop.
```
GET /api/v1/<resource-type>?outbound=<relationship>/<resource-type>/<resource-uid>
```

E.g, `GET /api/v1/devices?outbound=BusinessOwner/organisations/Sales`


## Create a resource that depends on another for its context
This is defined in the schema by adding the attribute `dependent=true` to the dependent `rgResource` definition, and by then adding the same attribute to the relationships to that resource-type from resource-types that are valid parents.
It´s valid to create resources that depend on other dependent resources, with no limit to the depth of these chains.
```
POST /api/v1/<parent-type>/<parent-uid>/<relationship-type>
with parameters: 'type=<child-type>' and 'uid=<child-uid>' (both are required)
```


## Delete a dependent resource
Either use the `DELETE` method on the full path to the resource in question to remove it specifically, or pass the `delete-dependent=true` parameter to the API call to one of its parents further up the chain.
The `delete-dependent` parameter acts recursively downward from whatever resource is being deleted.


## Move a dependent resource from one parent to another
Note that the new parent must be a valid parent for the child resource, and the new relationship must also be a valid dependent relationship.
```
POST /api/v1/path/to/dependent/resource
with parameter: 'target=/uri/path/to/new/parent/and/relationship'
```


# Working example

FIXME: `schema goes here`

Create a router:
```
prompt> curl -i -X POST -d 'uid=amchitka' -d 'comment=Router 1' http://localhost:4950/api/v1/routers
HTTP/1.1 201 Created
Content-Length: 11
Date: Tue, 06 Dec 2016 19:47:57 GMT
Server: Hunchentoot 1.2.35
Content-Type: text/plain; charset=utf-8

201 /routers/amchitka
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
```

Confirm that it´s gone:
```
prompt> curl -i http://localhost:4950/api/v1/routers/amchitka
HTTP/1.1 404 Not Found
Content-Length: 40
Date: Tue, 06 Dec 2016 19:48:18 GMT
Server: Hunchentoot 1.2.35
Content-Type: text/plain; charset=utf-8

No routers found with a UID of amchitka.
```

# Files API

Endpoint is `/files/v1`

Three methods are supported:
- POST
- GET
- DELETE

To use them via `curl`:
```
curl -F "file=@/path/to/file.jpg" -F "name=NameOfMyFile" http://localhost:4950/files/v1/
```




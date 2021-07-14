# API reference

## General overview

The purpose of the API is to ensure that data going _into_ the database fits a schema. It's arranged around two key things:

- resources, or the "things" represented in the database
- relationships between the resources.

The rest is details, around things like:

- What types of resource can be represented in the database.
- What attributes a resourcetype can have.
- Whether a resource can exist in its own right (primary resource) or whether it only makes sense in the context of another resource (dependent resource).
    - E.g, a floor of a building.
- What relationships are permitted from one resourcetype to another.
- The cardinality of a relationship: one-to-one, one-to-many, many-to-one, or many-to-many.
- Whether a relationship is a parent-child one between a primary resource and a dependent one.


### What makes this API different from the others

"Why don't you tell her... why you are the way you are."

Restagraph is designed explicitly around the property-graph model, as you find in Neo4j, rather than presenting a graph db in relational terms.

The most significant part of this is that one resourcetype can have the same type of relationship to _multiple_ other resourcetypes. That is, a task can relate to a wikipage, a person _and_ another task, all in the same schema at the same time. In the same way, a resourcetype can have several different relationships to another resourcetype: one organisation can be a vendor to another, _and_ be a customer of it, both at the same time. Thus, it can represent the real world as faithfully as possible.

Thus, we separate what a thing is, from its role in the scheme of things. There are no `customer` resourcetypes, just `people` or `organisations` that have customer or vendor relationships to other people or organisations.

The next important element is that you can trace the path of relationships from one resource to another, starting from any resource, and following any number of relationships. Among other things, this means that the hierarchy of dependent resources can go just as deep as necessary to represent your subject area.

Combining these two things, you get the `resourcetype/UID/relationship/resourcetype/UID` pattern. If you have two people who are friends, and one has a dog, you could trace the path to the dog via this URI: `/People/PersonOne/FRIEND/People/PersonTwo/PETS/dog/Fifi`

Why is the resourcetype _always_ in the path, even if that relationship could only lead to one resourcetype?

- Predictability, for one thing. As a sysadmin and network automation engineer, I've learned that a predictable, self-consistent API is the best kind.
- Another reason is preparing for the future: however unexpected, you may wind up defining another resourcetype as a possible target for that relationship, and this approach means you won't have to change any of your existing links or definitions.
- Lastly, the extra code to handle these special cases is another source of potential bugs. The simpler the code, the more reliable the server can be.



## Supported HTTP methods

Restagraph makes use of several of the HTTP methods, using their standard meanings according to [RFC 7231](https://tools.ietf.org/html/rfc7231). Those methods are:

- GET
- POST
- PUT
- DELETE


# The Schema API

This describes the types of resources that can be created, and what optional attributes they can have. It´s the "other half" of the main (raw) API.

Use this with GET to get a description of the available resource-types, and with POST to upload new resourcetypes and relationships. There is no inherent limit to the number of times you can add new resourcetypes or relationships.


## GET - retrieve the schema

The endpoint base URI is `/schema/v1`, and it always returns the schema in JSON format.

`GET /schema/v1/` will return a description of all resources.

`GET /schema/v1/<resourcetype>` will return the description of a single resourcetype.

Note that this format is _not_ the same as the format required for uploading a subschema, as it's intended for clients to use in interactions with the server.


## POST - upload a new schema

`POST /schema/v1/` with a payload of a JSON-formatted file.

Expected format of the file

    {
      "name": "<name of the schema>",
      "resourcetypes": [
        {
          "name": "NameOfResourceType",
          "dependent": <boolean>,
          "notes": "<Text describing what this resourcetype represents, and optionally how it's expected to be used.>",
          "attributes": [
            {
              "name": "nameofattribute",
              "description": "<Text describing what this attribute represents/means.>",
              "values": [ "optional", "list", "of", "permissible", "values" ]
            },
            ...
          ]
        },
        ...
      ],
      "relationships": [
        {
          "name": "NameOfRelationship",
          "source-type": "ResourcetypeTheRelationshipComesFrom",
          "target-type": "ResourcetypeTheRelationshipGoesTo",
          "cardinality": One of "many:many", "1:many", "many:1" or "1:1" (default is "many:many"),
          "dependent": boolean,
          "notes": "<Text clarifying what this relationship is intended to mean.>"
        },
        ...
      ],
    }

### Notes about the format and naming conventions

- It's recommended that you follow [Neo4j naming conventions](https://neo4j.com/docs/cypher-manual/current/syntax/naming/):
    - Names of resourcetypes and relationships should be in `PascalCase`, as they are created as Neo4j labels.
    - Relationships should be in `SCREAMING_SNAKE_CASE`.
- Booleans must be either `true` or `false`, though `null` is also accepted as an equivalent to `false`.
- The `dependent` attribute of a resource indicates whether it has independent existence (dependent=`false`, the default) or whether it only exists in the context of a parent resource. E.g, an IP address configured on an interface doesn't exist independently - the only reason not to define it as an attribute is that an interface may have any number of addresses configured on it.
    - This is an optional attribute; it defaults to `false`.
- The `dependent` attribute of a relationship indicates whether the target resource is dependent on the source resource, i.e. is a child to that parent resource.
    - This means that if `dependent` is true in a relationship type, the target resource will be created along with it.
    - A dependent resource can only have one parent resource.
    - A dependent relationship _cannot_ be created to a non-dependent target resource.
    - A non-dependent relationship _can_ be created to a dependent target resource, if the target already exists.
- The `notes` attribute of a resourcetype or relationship, and the `description` of an attribute, is optional. If you omit it altogether, or specify it as `null` or an empty string, it will not be added to the resourcetype definition in the database, and will be effectively a null or empty string when this is queried.
- The `values` attribute on an attribute is optional, and should only be used when you have a specific reason to constrain it to a fixed set of values. If you're considering using it, think about whether it makes more sense to use a separate resourcetype, enabling you to add/remove values in future.


# The Raw API (resources)

This is where you perform most CRUD operations on resources in the database. It´s called "raw" because it doesn´t have any domain-specific logic, like you find in the `files` API (described in the next section). It's expected to serve the majority of your interactions with a Restagraph system.

This API operates on resources, and on the relationships between them. You can create and delete resources, fetch their current state, and update their attributes, and you can create and delete relationships between them. Which attributes are applicable to a given resource depends on its type, which is defined in the schema. Likewise, the schema determines what relationships can be created between any two resource-types.

HTTP return codes are used according to RFC7231, and the Content-type header is set according to whether text or JSON is being returned. As a rule, JSON will be returned on success, and plain text for anything else. The one salient exception is when deleting a resource or relationship, where the MIME-type is "text/plain" and the return code is `NO CONTENT`.

The definitive API reference is in `test/test-rest-api.py`, along with usage examples.

The following patterns are recognised by the application server:


## Create a resource

```
POST /api/v1/<resource-type>/
```

With payload of `uid=<uid>`, plus optionally `<attribute-name>=<value>` pairs for any subset of the attributes defined for this resource type.

Example:
```
curl -X POST -d 'uid=Blake' http://localhost:4950/raw/v1/People
```

The UID must be unique for each resource-type. That is, if you define a `routers` resource and a `switches` resource, no two routers can have the same UID, but a router and a switch can. Bear this in mind when designing your schema.

On success, returns a status code of 201, and the URI for the newly-created resource, e.g. `/People/Blake`.


### Create a dependent resource

This works the same way as with primary resources, except that you append the dependent relationship and resourcetype to an existing parent resource, e.g:

```
curl -X POST -d 'uid=Hangar' http://localhost:4950/raw/v1/Buildings/Xenon/FLOORS/Floors
```

UIDs for dependent resources must be unique within each parent resource, but are not required to be globally unique the way that primary resources are.


## Retrieve a resource

```
GET /api/v1/<resource-type>/<uid>
```

On success, returns a status code of 200, and the response body is a JSON representation of the resource.

If no resource of that type exists with that UID, a status code of 404 is returned.

These URIs can be as long as the client supports, and can follow any relationship from one resource to another. As an example, you could fetch the details of the janitor for the hangar in the above example with this query:

```
curl http://localhost:4950/raw/v1/Buildings/Xenon/FLOORS/Floors/Hangar/JANITOR/People/Joe
```


## Retrieve all resources of a given type

```
GET /api/v1/<resource-type>/
```

Returns a JSON representation of all resources of that type, with a status code of 200.

If there aren´t any resources of this type, it still returns a status code of 200, and the response body is an empty JSON array, i.e. `[]`. This may seem inconsistent with the 404 returned for a failed request for a single resource, but it's the difference between "nothing at this URL" and "this URL is valid, but the search returned nothing."

The results can be filtered on amy of the attributes using [Java-style regexes](https://docs.oracle.com/javase/7/docs/api/java/util/regex/Pattern.html), e.g:

```
curl 'http://localhost:4950/raw/v1/People?displayname=Bla.*'
```

Regular expressions can be negated by putting `!` at the start of the regex, e.g. `!.*foo.*` would filter for every string that does _not_ include the substring "foo".


### Retrieve the type and UID of all resources to which this one has a specific relationship

This is essentially the extended version of retrieving all resources of a given type. Alternatively, the previous section is a special case of this feature:

```
GET /api/v1/<resource-type>/<Unique ID>/<relationship>
```

Either way, the same filtering works here as well.


## Update one or more attributes of a resource

```
PUT /api/v1/<resource-type>/<resource UID>
```

The payload must be supplied in the request body, POST-style. This is mainly to get past the 1024-character limit for GET-style parameters, which is a little short for something like a wiki page.

This always returns a status code of 204 (no content) on success.

Although [section 4.3.4 of RFC 7231](https://tools.ietf.org/html/rfc7231#section-4.3.4) states that 201 must be returned "[i]f the target resource does not have a current representation and the PUT successfully creates one," this API provides for updating multiple resources in a single request, making it entirely possible to create, update _and_ delete attributes in a single transaction. It seems like a backward step to restrict clients to updating a single attribute per request, so I´m making the counter-argument that unpopulated attributes have the de facto representation of `Null`, so _technically_ there aren´t any valid resources lacking representation in the context of this method.

There's currently no way to delete the value of an attribute, but I do plan to add this feature.


## Delete a resource

```
DELETE /api/v1/<resource-type>/uid
```

Returns 204 (no content) on success.

If the parameter `recursive=true` is supplied, all dependent resources depending on this one will also be deleted, from the bottom up. That parameter is accepted in both GET-style (within the URL) and POST-style (within the request body).


## Create a relationship from one resource to another
Note that, due to the way Neo4J works, these are always directional.

```
POST /api/v1/<resource-type>/<Unique ID>/<relationship>
with parameter: 'target' = '/type/uid'
```

The `target` parameter can actually be any valid path to an existing resource, but it _must_ uniquely identify a unique resource by including the `type` and `uid` values as the last two elements.

Returns the URI of the newly-created path through this relationship.


## Delete a relationship to another object
```
DELETE /api/v1/<resource-type>/<Unique ID>/<relationship>/<Unique ID>
```


## Delete a dependent resource

Either use the `DELETE` method on the full path to the resource in question to remove it specifically, or pass the `recursive=true` parameter to the API call to one of its parents further up the chain.
The `recursive` parameter acts recursively downward from whatever resource is being deleted.


## Move a dependent resource from one parent to another
Note that the new parent must be a valid parent for the child resource, and the new relationship must also be a valid dependent relationship.
```
POST /api/v1/path/to/dependent/resource
with parameter: 'target=/uri/path/to/new/parent/and/relationship'
```


# Files API

This is the API for uploading and deleting files; these operations cannot be done via the Raw API. You can retrieve the file itself with a GET request to this endpoint, or retrieve its metadata via a get request to `/raw/v1/Files/<file UID>`.

Endpoint is `/files/v1`

Three methods are supported:
- POST
- GET
- DELETE

To use them via `curl`:
```
curl -F "file=@/path/to/file.jpg" -F "name=NameOfMyFile" http://localhost:4950/files/v1/
```

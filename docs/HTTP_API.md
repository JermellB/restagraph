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
    - The default in a regular relationship is `many:many`.
    - The default in a dependen relationship is `1:many`.
- Whether a relationship is a parent-child one between a primary resource and a dependent one.

For a practical walkthrough of how this API looks when in use, read through the [demo session](Demo_session.md).


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

Among the information returned by the server, is a list of the relationships that can be created from this resourcetype to certain others. This list combines _three_ sets of permissible relationships:

- _from_ instances of this exact resourcetype, _to_ instances of another specific resourcetype.
- _from_ instances of _any_ other resourcetype, _to_ instances of this one.
- _from_ instances of this resourcetype, _to_ instances of any other.

The latter two are defined by relationships to and from the `any` resourcetype. The combined set is also (and most importantly) consulted when the server receives a request to create a relationship.


### GET a list of schema versions

You can fetch a list of the schema versions by appending the parameter `version=list` to a GET request to this API.

The return value is a JSON object of this form:

```
{
  "versions": [
    3835709813,
    3835709407,
    3835709370
  ],
  "current-version": 3835709813
}
```


## POST - upload a new schema

You can add resourcetypes and relationships to an existing schema, or add new attributes to resourcetypes in an existing schema. To do this, make a POST request to the URI `/schema/v1/`, with a payload of name "schema" whose value is a JSON-formatted file. The server returns `201/Created` on success.

The contents of that JSON document will be added to the schema, as long as they add new things; any existing resourcetypes, resourcetype-attributes and relationships will not be updated. This is a progressive, succeed-where-possible process, not an atomic operation; any duplicates will be discarded without warning to the client.

The intent of this feature is to enable clients to build on top of the core schema, and to mix-and-match subschemas from different sources, so users working in the same problem domain can share the common elements of their schemas for better interoperability.


To do this via `curl`, for example:

```
curl --data-urlencode schema@webcat.json -X POST http://localhost:4950/schema/v1/
```

The file is expected to be in this format:

    {
      "name": "Demo",
      "resourcetypes": [
        {
          "name": "Ideas",
          "dependent": true,
          "description": "An idea I had, that I want to do something about.",
          "attributes": [
            {
              "name": "description",
              "type": "text",
              "description": "More information about the idea, in free-text form."
            },
            ...
          ]
        },
        ...
      ],
      "relationships": [
        {
          "name": "HAD",
          "source-type": "People",
          "target-type": "Ideas",
          "cardinality": "1:many",
          "reltype": "dependent",
          "description": "This person had that idea, and should do something about it."
        },
        ...
      ],
    }

Note that if the file (re)defines an existing resourcetype, Restagraph will not create a duplicate or change anything about the existing one, but it will add any _new_ attributes to the existing one.

Duplicate relationships will simply be ignored.

For information about how to define your own subschema, see [Defining a schema](Defining_a_schema.md).


### Notes about the format and naming conventions

- It's recommended that you follow [Neo4j naming conventions](https://neo4j.com/docs/cypher-manual/current/syntax/naming/):
    - Names of resourcetypes and relationships should be in `PascalCase`, as they are created as Neo4j labels.
    - Relationships should be in `SCREAMING_SNAKE_CASE`.
- Booleans must be either `true` or `false`, though `null` is also accepted as an equivalent to `false`.
- The `dependent` attribute of a resource indicates whether it has independent existence (dependent=`false`, the default) or whether it only exists in the context of a parent resource. E.g, an IP address configured on an interface doesn't exist independently - the only reason not to define it as an attribute is that an interface may have any number of addresses configured on it.
    - This is an optional attribute; it defaults to `false`.
- The `reltype` attribute of a relationship indicates whether the target resource is dependent on the source resource, i.e. is a child to that parent resource.
    - This means that if `reltype` is "dependent" in a relationship type, the target resource will be created along with it.
    - A dependent resource can only have one parent resource.
    - A dependent relationship _cannot_ be created to a non-dependent target resource.
    - A non-dependent relationship _can_ be created to a dependent target resource, if the target already exists.
- The `description` attribute of a resourcetype or relationship, and the `description` of an attribute, is optional. If you omit it altogether, or specify it as `null` or an empty string, it will not be added to the resourcetype definition in the database, and will be effectively a null or empty string when this is queried.
- The `values` attribute on an attribute is optional, and should only be used when you have a specific reason to constrain it to a fixed set of values. If you're considering using it, think about whether it makes more sense to use a separate resourcetype, enabling you to add/remove values in future.


## Schema versioning

Restagraph has a built-in system for versioning schemas, so you can test new ideas, roll back to a previous version if the new one didn't work out, and garbage-collect old versions when you're sure they're no longer needed.

Version IDs are just timestamps of the date/time when that version was created (not when it was last updated) in Unix epoch time: seconds since midnight, 1970/01/01.


### Create a new schema version

This works in both senses, via POST requests.

If you add the parameter `create=true` to a POST request to this API, the server will create a new schema version and pre-populate it with the core schema. The parameter is expected in the body of the request, POST-style, not in the URL as with GET requests. This can be done without uploading a new subschema, so can be used to create a fresh start with just the core schema.

If you do upload a subschema in the same request, as described in the previous section, the server will create the new schema-version and then apply the uploaded schema to it, without touching the previously-current version.


### Switch between schema versions

If there's more than one schema version in the database, you can roll back or forward between them with a PUT request, using the `version=<version number>` argument, e.g:

```
curl -X PUT http://localhost:4950/schema/v1?version=3835709407
```

Server responses:

- On success: 200/OK
    - It's an idempotent operation, which doesn't create anything new, so 201/Created is not applicable here.
    - "Success" includes cases where the requested version was already the current one.
- If the requested version doesn't exist: 404/Not Found


### DELETE - remove a schema version

To remove a schema version, use an HTTP GET request against the Schema API, with the argument `version=<integer>`. On success, the server will respond with 200/OK.

Example with `curl`:

```
curl -X PUT http://localhost:4950/schema/v1?version=3835709407
```

If the requested version is the current version, the highest remaining version will be automatically set as the new current version.


# The Raw API (resources)

This is where you perform most CRUD operations on resources in the database. It´s called "raw" because it doesn´t have any domain-specific logic, like you find in the `files` API (described in the next section). It's expected to serve the majority of your interactions with a Restagraph system.

This API operates on resources ("things"), and on the relationships between them. You can create and delete resources, fetch their current state, and update their attributes, and you can create and delete relationships between them. Which attributes are applicable to a given resource depends on its type, which is defined in the schema. Likewise, the schema determines what relationships can be created between any two resource-types.

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

UIDs must also be URL-safe, so they're restricted to the set of "unreserved characters" from section 2.3 of [RFC 3986](https://www.ietf.org/rfc/rfc3986.txt): this is the unaccented Latin alphabet (a-z and A-Z), digits 0-9, and the four non-alphanumeric characters `-`, `_`, `.` and `~`.

On success, the server returns a status code of 201, and the URI for the newly-created resource, e.g. `/People/Blake`.

Note that it will return a 400 error if you try to set the `createddate` or `lastmodified` datestamps, and a 403 if you try to set any attribute whose `read-only` value in the schema is `True`.


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


### Filtering the "retrieve all resources" request

You can add filters to this request, as parameters in the URL.

- Exact text match: `<attribute-name>=<text>`, e.g. `uid=foo`
- Regular expression match: `<attribute-name>=<regex>`, e.g. `uid=f.*o.*`
    - These follow [Java-style regex rules](https://docs.oracle.com/javase/7/docs/api/java/util/regex/Pattern.html).
- Attribute exists/has a value: `<attribute-name>=exists`, e.g. `description=exists`
- Resource has an outbound link to another resource: `RGoutbound=</RELATIONSHIP/Resourcetype/uid>`, e.g. `RGoutbound=/TAGS/Tags/fooTag`
    - You can drop the UID, to filter for resources with an outbound connection to _any_ resource of a given type, e.g. `RGoutbound=/TAGS/Tags" will require that the resource is tagged, without specifying a particular tag.
    - The target path can be of any length; you just need to remember that you're following a path outward from the target resource, and relationships need to be present in that direction. E.g, you can link to this IPv4 address on that interface of this router via `RGoutbound=/CONNECTS_TO/Ipv4Addresses/192.168.1.1/CONFIGURED_ON/EthernetInterface/eth0/PRESENT_ON/Devices/thisRouter`
- Another resource links to this one: `RGinbound=</Resourcetype/resource/RELATIONSHIP>`, e.g. `RGinbound=/Movie/Speed_Racer/DIRECTED_BY`
    - The path to that resource can be any length - it doesn't have to be a primary resource.
    - UIDs in that path can be replaced with `*`, as a basic wildcard mechanism. E.g, you can find all tags associated with movies via `http://192.0.2.1:4950/raw/v1/Tags?RGinbound=/Movie/*/TAGS`
- Enum attributes can be filtered on multiple values, by supplying a comma-separated list.
    - E.g, `priority=high,medium`
    - Note that the separater is a comma, _not_ comma-plus-space.

Each of these can be applied as a negation, by prepending `!`. E.g, `!uid=foo` means "UID is _not_ equal to 'foo'".

Parameters are combined with AND, _not_ with OR.

Yes, these do have implications regarding attributes with a name of `outbound` or a value of `exists`. There's a workaround for the latter involving a regex, but I need to find a better solution for the `outbound` issue.


## Retrieve the type and UID of all resources to which this one has a specific relationship

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

Note that, like with POST requests, the API will return a 400 error if you try to set the `createddate` or `lastmodified` datestamps, and a 403 if you try to set any attribute whose `read-only` value in the schema is `True`.


## Delete a resource

```
DELETE /api/v1/<resource-type>/uid
```

Returns 204 (no content) on success.

If the parameter `recursive=true` is supplied, all dependent resources depending on this one will also be deleted, from the bottom up. That parameter is accepted in both GET-style (within the URL) and POST-style (within the request body).

If the parameter `yoink=true` is supplied, the resource's representation will be returned in the body of the reply in the same manner as a GET request.


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
DELETE /api/v1/<resource-type>/<Unique ID>/<relationship>/
with parameter: 'target=/<resource-type>/<Unique ID>'
```

The reason for doing it this way is that it's the only way to distinguish between deleting the relationship vs deleting the resource at the far end of it.


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

## How files are stored: information for site-admins

The database contains only metadata _about_ the files, not the files themselves. They're stored separately, using the SHA-256 checksum as the filename for a few reasons:

- It's logistically simpler for things like distributing the files across directories, to prevent the server getting bogged down in a search through thousands of files in one directory.
- It avoids character-set issues between the server and the storage filesystem.
- Deduplication: if 2000 clients upload the same picture with different names, the server stores 2000 sets of metadata in the database, but only one copy of the file itself.
    - Firstly, this avoids wasting disk space.
    - Secondly, if you detect a picture of child porn (for example) it's trivial to find anybody else who's uploaded the same file, by searching for any other instances with the same checksum.

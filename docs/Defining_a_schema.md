# Defining a Restagraph subschema

It'd be handy to know how to define subschemas of your own, that being the point of this thing.

Why "subschema"? Because each of these documents is a subset of the final schema that's actually in effect after they've all been installed. You can upload/install any number of subschemas, each of which can build on any previously-defined resourcetypes, which are combined by the server to make up the composite schema that controls the behaviour of the API.


# The format

In summary, a subschema is a JSON document defining a single object with three fields:

- `name`
    - Identifies the subschema being managed.
    - This isn't currently reflected in the final schema, but this behaviour may be revived in the future.
- `resourcetypes`
    - A list of objects, each of which describes a single resourcetype.
        - The list may be empty, if you only want to add relationships.
    - If the resourcetype has already been defined in the database by a previous subschema, any _additional_ elements will be added, but no existing elements will be changed or deleted. That is, you can add more attributes but not change or remove any, and you can add a `notes` attribute to a resourcetype that doesn't already have one.
    - It's not strictly an error to have multiple definitions for the same resourcetype, but it _is_ asking for trouble.
- `relationships`
    - A list of objects, each of which describes a single relationship from one resourcetype to another.
        - The list may be empty, if you only want to add resourcetypes.
        - It's not an error to have an empty list for both, but there also isn't any point to having such a definition, either.
    - Duplicate relationships will be ignored.
        - "Duplicate" means that it has the same name, source-type and target-type as a relationship that's already present in the database.


To go into more detail:

## Name

Not currently used, so there's no penalty for omitting it. They're still required for legacy reasons, but they're also a handy reminder of what that particular subschema is for while you're editing it.


## Resourcetypes

Each resourcetype is defined via several key-value pairs, most of which are mandatory:


### Name

The name of the resourcetype.

It's used as an identifier in the URI when interacting with the API, and as a node label when recording a resource in the database. Because it's used in the URI, it needs to be safe for such.

Should be in PascalCase, following the [Neo4j naming conventions](https://neo4j.com/docs/cypher-manual/current/syntax/naming/).

Type: string.


### Dependent

Whether this is a "dependent" resourcetype, i.e. whether it exists only in the context of another one. E.g, a room only exists in the context of a building.

A dependent resourcetype can only be created in relationship to a "parent" type, via a relationship that is _also_ defined as a dependent one, meaning that it defines the dependency between them. A resourcetype can be dependent on another dependent one, e.g. the ceiling of a room.

Type: boolean. In accordance with [Postel's Principle](https://en.wikipedia.org/wiki/Robustness_principle), acceptable values include `true`, "true", "True", `false`, "false" and "False". The preferred values are `true` and `false`.


### Notes

Description of what this type describes, and possibly how it's to be used.

Type: string.

Optional; the default is `null`.


### Attributes

A list of attributes objects. Their main keys, i.e. those shared by all attribute types, are:

- `name` 
    - The name by which this attribute is addressed, in both the Schema API and the Raw API.
    - Should be URI-safe, because you can use them to filter HTTP GET requests for resources, using URL parameters.
- `description`
    - As you'd expect, this is for elaborating on what you actually meant by the `name`.
- `type`
    - Determines what kinds of value will be accepted, and which additional constraints may be added.
    - Default value is `null`, which means anything goes.
- `readonly`
    - Denotes an attribute whose value can't be set or updated via the HTTP API.
    - This is only useful in combination with server-side logic that autogenerates the parameter's value, like checksums and MIME-types of files.


Available values of `type`:

- `varchar`
    - Variable-length character strings; useful for short stretches of text such as one-line descriptions, or people's names.
    - Intentionally named after the SQL type with the same semantics.
- `text`
    - Free-form text, up to 65 535 characters in length. 64K ought to be enough for anybody, right?
    - Like "varchar", this is a deliberate reference to SQL types.
- `integer`
    - Any integer that will fit in a 64-bit representation.
- `boolean`
    - Sometimes you just need to know whether it's a yes or a no, a true or a false.

In case you're wondering why there are two types of string variable, instead of just varchar(65535), it's for the benefit of GUI developers. This is a hint that a GUI can use to decide whether to present a one-line field or a resizeable box for editing the text of a given attribute.


For some attribute types, you can define further constraints on their values.

- `varchar`
    - `maxlength` = the maximum acceptable length for this string. This is in octets, not characters: something to watch out for in non-Roman character sets, since we're using UTF-8 here. This is not disabled by the `values` attribute, though it probably should be.
    - `values` = a list of valid values for this attribute, so you can effectively define it as an enum type. Note that the server doesn't try to reconcile `maxlength` with this, so it's entirely possible for the server to reject a valid member of this set on the grounds of it exceeding the maximum length.
- `integer`
    - `minimum` = the lowest value accepted for this attribute. This is an inclusive value, not an exclusive one.
    - `maximum` = the highest value accepted for this attribute. Also an inclusive value.


## Relationships

A list of relationships objects. Their keys are:

- `name`
    - The name of this relationship. Needs to be URL-safe.
    - Should be in SCREAMING_SNAKE_CASE.
    - Type: string.
- `source-type`
    - The name of the resourcetype that this relationship _comes_from_.
    - Type: string.
- `target-type`
    - The name of the resourcetype that this relationship _goes_to_.
    - Type: string.
- `cardinality`
    - How many relationships of this kind are to be permitted from an instance of the `sourcetype`, and how many to an instance of the `target-type`. Valid options are:
        - "many:many"
        - "1:many"
        - "many:1"
        - "1:1"
    - Type: string.
- `reltype`
    - What kind of relationship the target resource could bear to this one.
        - "dependent" means the target resource can only be of a `dependent` type, but otherwise asserts no restrictions.
        - "any" means there are no restrictions.
    - type: string.
- `notes`
    - Any clarifying notes about what this relationship means.
    - Type: string.


I usually define them in the order `source-type`, `name`, `target-type` because that matches the way I think about them. In Cypher, Neo4j's native syntax, it's represented as `(:source-type)-[:name]->(:target-type)`.


# Example

Let's lead with an example, for adding books and authors to the schema:

    {
      "name": "example_schema",
      "resourcetypes": [
        {
          "name": "Books",
          "dependent": "false",
          "notes": "Stuff printed on the corpses of trees.",
          "attributes": [
            {
              "name": "description",
              "type": "text",
              "description": "",
              "values": null
            },
            {
              "name": "ISBN",
              "type": "varchar",
              "description": "International Standard Book Number. Should be a 10- or 13-digit number, optionally interspersed with hyphens.",
              "maxlength": 17
            }
          ]
        }
      ],
      "relationships": [
        {
          "source-type": "Books",
          "name": "AUTHOR",
          "target-type": "People",
          "cardinality": "many:many",
          "reltype": "any",
          "notes": "Link from the book to its author."
        },
        {
          "source-type": "People",
          "name": "AUTHOR_OF",
          "target-type": "Books",
          "cardinality": "many:many",
          "reltype": "any",
          "notes": "Link to a book this person wrote."
        },
      ]
    }

Note that it assumes the existence of the `People` resourcetype. This is defined in the core schema, so you know it'll always be there. However, you can equally rely on resourcetypes created in other schemas, as long as they were installed before this one.

The server installs all resourcetypes defined in a subschema _before_ trying to create the relationships.

Note: reference errors are silent. If the schema defines a relationship that refers to a resourcetype not already defined, it will log the fact and move on. So it's fine to refer to resourcetypes defined in other subschemas (in fact, it's positively encouraged) but it _is_ important to make sure you a)only make backward references, not forward ones, and b)upload subschemas according to the order of their dependencies.

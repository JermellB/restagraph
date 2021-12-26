# Defining a Restagraph subschema

It'd be handy to know how to define subschemas of your own, since this thing's designed to enable you to do that.

Why "subschema"? Because each of these documents is a subset of the final schema that's actually in effect after they've all been installed.


# Summary of the format.

A subschema is a JSON document defining a single object, which has three fields:

- `name`
    - Entirely optional, but is a useful reminder.
    - This was used for subschema management in the past, and may yet be used again in future, combined with version identifiers.
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

Not currently used, so there's no penalty for omitting it. I'm still using them for legacy reasons, but they're also a handy reminder of what that particular subschema is for while you're editing it.

Restagraph ignores anything it's not specifically looking for, so it causes no ill-effects to include this.


## Resourcetypes

Each resourcetype is defined via several key-value pairs. They're all mandatory unless noted otherwise.


### Name

The name of the resourcetype.

It's used as an identifier in the URI when interacting with the API, and as a node label when recording a resource in the database. Because it's used in the URI, it needs to be safe for such.

Should be in PascalCase, following the [Neo4j naming conventions](https://neo4j.com/docs/cypher-manual/current/syntax/naming/).

Type: string.


### Dependent

Whether this is a "dependent" resourcetype, i.e. whether it exists only in the context of another one. E.g, a room only exists in the context of a building.

A dependent resourcetype can only be created in relationship to a "parent" type, via a relationship that is _also_ defined as a dependent one, meaning that it defines the dependency between them. A resourcetype can be dependent on another dependent one, e.g. the ceiling of a room.

Type: boolean. In accordance with Postel's Principle, acceptable values include `true`, "true", "True", `false`, "false" and "False".


### Notes

Description of what this type describes, and possibly how it's to be used.

Type: string.

Optional; the default is `null`.


### Attributes

A list of attributes objects. Their keys are:

- `name` 
    - The name by which this attribute is address, in both the Schema API and the Raw API.
    - Should be URI-safe, because you can use them to filter HTTP GET requests for resources, using URL parameters.
- `description`
    - As you'd expect, this is for elaborating on what you actually meant by the `name`.
- `values`
    - A list of valid values for this attribute, so you can effectively define it as an enum type.
    - Those values can currently only be defined as strings, because I haven't (yet) implemented a way of specifying their type as something else.
    - Default value is `null`, which means anything goes.
- `read-only`
    - Denotes an attribute that can't be updated via the HTTP API.
    - This is only useful in combination with server-side logic that autogenerates the parameter's value, like checksums and MIME-types of files.
    - Note that in the database, this is stored as `readonly` (no hyphen) due to Neo4j constraints on attribute names. I'm living with the inconsistency because I prefer the readability of the Lisp version.


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
- `dependent`
    - Whether this is the definitive relationship between a dependent resourcetype and its parent type.
    - Corresponds to the resourcetype attribute of the same name.
    - Type: boolean. In accordance with Postel's Principle, acceptable values include `true`, "true", "True", `false`, "false" and "False".
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
              "description": "",
              "values": null
            },
            {
              "name": "ISBN",
              "description": "International Standard Book Number. Should be a 10- or 13-digit number.",
              "values": null
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
          "dependent": null,
          "notes": "Link from the book to its author."
        },
        {
          "source-type": "People",
          "name": "AUTHOR_OF",
          "target-type": "Books",
          "cardinality": "many:many",
          "dependent": null,
          "notes": "Link to a book this person wrote."
        },
      ]
    }

Note that it assumes the existence of the `People` resourcetype. This is defined in the core schema, so you know it'll always be there, but you can equally rely on resourcetypes created in other schemas, as long as they were installed before this one.

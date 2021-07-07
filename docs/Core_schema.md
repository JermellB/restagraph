# The core schema

This framework depends crucially on a few core resourcetypes, which must always be designed in. A couple more are included because it's self-evident that they'll virtually always be needed.

This document doesn't mention the built-in attributes you'll find in all resourcetypes, such as UID, precisely because they're not unique to any of these types. Only the additional "user-defined" attributes are covered here.

All the core filetypes are primary; none of them are dependent.

You can add attributes to these resourcetypes, by defining them in your own schema. The server will merge the definitions, rather than duplicate them.

The names of resourcetypes and relationships follow the [Neo4j naming conventions](https://neo4j.com/docs/cypher-manual/current/syntax/naming/), which recommends PascalCase for labels (RG uses labels to identify the resourcetype of resources) and `SCREAMING_SNAKE_CASE` for relationships. The convention says nothing about attributes, so all lower-case is used here.


## any

The "any" resourcetype is one of a very few special-case resourcetypes, which are treated differently from the others. It's used for defining relationships where either the source or target could be, well, any resourcetype. E.g, you can tag anything by creating a `TAGS` relationship to a `Tags` resource.

It's not possible to create an instance of this resourcetype.


## Tags

Hopefully self-evident, these are for categorising things. Useful in searches.

UID: the tag name.
 
### Relationships

- "any" resourcetype can have a `TAGS` relationship to a `Tags` resource.


## Groups

The alter-ego of tags; equally self-evident.

UID: the group name.
 
### Relationships

- "any" resourcetype can have a `GROUPS` relationship to a `Groups` resource.


## People

Real people, imaginary people, security roles, members of an external organisation... if they're a person, this is the type.

UID: whatever makes most sense under the circumstances. Could be the full name, could be an employee ID. This is one of the reasons for having the `displayname` attribute.


### Additional attributes

- `displayname` The version of their name that can be more human-relatable than their UID.
    - You many also wish to add `GivenName` and `FamilyName` attributes. Given the profusion of naming conventions found around the world, think carefully about whether to use attributes or dependent resources.
- `notes` Any free-text notes you wish to record about this person.

### Relationships

- "any" resource can have a `CREATOR` relationship to a `People` resource.
    - In fact, all resources _will_ have this in a coming update, as part of the access-control model.


## Pronouns

She/her, they/them, he/him and whatever others you choose to add. These are defined as a separate resourcetype for two reasons:

- Some people accept more than one set, and this way you can associate as many sets of pronouns with a person as appropriate.
- It's simpler and easier to add more pronouns via the API as they're needed, than to update the `values` field in the schema.

### Relationships

- `People` can have a `PRONOUNS` relationship to `Pronouns`


## Files

Another special-case resourcetype. Strictly speaking, these resources contain metadata _about_ files, including the name under which the file is actually stored in the filesystem.

UID: the name given by the client, sanitised to be URI-safe.

### Additional attributes

- `title` The requested filename, recorded verbatim instead of having to be sanitised for URI-safety.
- `notes` Any notes you want to record about the file.
- `mimetype` The MIME-type of the file, as determined by inspection of the file after it's been uploaded.
- `sha3256sum` The SHA3-256 checksum/hash/digest of the file. This is used as the file's name on the filesystem, for a few reasons
    - It's logistically simpler for things like distributing the files across directories, to prevent the server getting bogged down in a search through thousands of files in one directory.
    - It avoids character-set issues between the server and the storage filesystem.
    - Deduplication: if 2000 clients upload the same picture with different names, the server stores 2000 sets of metadata in the database, but only one copy of the file itself.
        - Firstly, this avoids wasting disk space.
        - Secondly, if you detect a picture of child porn (for example) it's trivial to find anybody else who's uploaded the same file.

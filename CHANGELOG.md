# Changelog

All notable changes.

Issue references of the form #<number> refer to tickets on Github: https://github.com/equill/restagraph/issues


## [0.8.5]

### Bugs fixed

- Moving dependent resources: the function contained two fatal bugs.
- `delete-subnet` now behaves correctly when re-homing IPv6 subnets.
- `store-dependent-resource`: make it work correctly and reliably by replacing a faulty Cypher query with a call to `get-resources` that makes much more sense.


### Changed

- #91 Relationship `RG_CREATOR` replaces relationship `CREATOR` to denote that the source user created the target resource.
  This averts conflict with admins who (quite reasonably) want to credit a person with the creation of a work, and increases consistency with the general pattern of using the prefix `RG_` to denote things that are specific to the operation of Restagraph.
- #37 Prevent clients from interacting with dependent resources as though they were top-level primary resources.
- #87 Replace the `dependent` attribute in relationships with a `reltype` one that currently has acceptable values of `dependent` and `other`.
- #24 Reject any dependent relationships with cardinality `many:many` or `many:1`, to ensure that dependent resources can only have one parent.
- #24 Reject any attempt to create a dependent relationship to an existing dependent resource. Again, to ensure the single-parent policy.
- Improve detection of attempts to create multiple instances of a 1:1 dependent relationship from a given parent resource.
- In function `get-attribute`, rename the `attr` parameter to `rtype`. It's less confusing when the parameter names aren't misleading.
- Restrict the cardinality of dependent relationships to `1:1` and `1:many`, to ensure that a dependent resource can have exactly one parent.
    - The cardinality of dependent relationships defaults to `1:many`, because that's what the current user actually expects.
    - Non-dependent relationships still default to `many:many`.


### Added

- IPv6 IPAM tests, both internal and client tests.
- Internal functionality to derive the canonical path for a resource. Currently only partly implemented.
- More comprehensive testing.


### Removed

- Obsolete, commented-out client test for dependent resources, which has been superseded by a newer, shinier one.


## [0.8.1]

### Bugs fixed

- API hang in response to a POST request with a 2-element URI, i.e. a single resource (#67)
    - Previously, the server tried to handle this as a request to move a dependent resource to a new parent, which led to a stracktrace when it tried to use an element from the URI that wasn't there.
    - This is now met with a 400/Bad Request error, and a prompt that you may have intended to link that resource to another one, but forgotten to include the relationship at the end of the URI.
- API hangs in response to a POST request where the UID contains an apostrophe (#65)
    - Sanitisation of UIDs is now much stricter.
- #92 API hangs when creating a many:1 relationship.
- Default-resources test now installs the default resources before testing for their presence.


### Added

- API/Schema: Define read-only attributes.
  These are really only useful in conjunction with server-side logic which auto-populates attributes, and are mostly FYI for regular users. However, they mean that you can now rely on things like the SHA3-256 checksum and MIME-type of an uploaded image, because now you know that these can't be overwritten by a client.
- API: Add "yoink" parameter to DELETE operations (#82)
  If the client appends `yoink=true` as a GET-style URL parameter, the server will return a representation of the resource being deleted.


### Changed

- Logging improvement in `pure-tests.lisp`: log-messages starting with "TEST" now start with ";TEST" to better show up in a SLIME session.
- Core schema: change the cardinality of `any/CREATOR/People` from `1:many` to `many:1`. Many things should be able to be created by one person, and only one person.
- Update the client test to match the core schema: change the expected cardinality and description of the `/any/CREATOR/People` relationship, to match the changes in the core schema.
- Update `run_test_neo4` to use Neo4j 4.3.9.
- Upgrade SBCL from 2.1.9 to 2.1.10 in `default.nix`.
- UIDs are now only permitted to include "unreserved characters" as defined in section 2.3 of RFC 3986, to ensure URL-safety.


### Removed

- Dummy log message in `install-subschema-resourcetype`.
- Redundant handler-case from `store-resource`.
- Remove `libyaml` from `default.nix`: it's been commented out for long enough to be sure that it's now cruft.
- Tests for the Neo4cl HTTP API driver, which has been replaced by the Bolt driver.


### Security

- Read-only attributes (see "Added" section above) remove a data-integrity issue.


### Deprecated

Nothing deprecated in this release.


## [0.7.0b15]

### Added

- API: when creating a new resource (either primary or dependent), moving a dependent resource or creating a relationship between resources, return the new URI in a `Location` HTTP header. This is in addition to the existing behaviour of returning the URI in the response body.
- Debug logging in many places.

### Changed

- API: return 304 (not modified) instead of 200 when a client attempts to create a duplicate resource.
- API: change `Content-type` header to `text/plain` in 204 response when updating a resource attribute.
- API: reduce allowed-methods list for the Schema API to GET. Now all other methods will be explicitly rejected, instead of falling through to the default failure.
- Internal method `store-resource` returns the newly-created UID, instead of the output of the Neo4j API request.
    - Dispatcher functions that invoke `store-resource` now use this value, instead of calculating it for themselves. It´s true that they _should_ produce the value, but "dead reckoning" got that name for a reason.
- Documentation: split detailed coverage of the API out of the README and into its own file under the `docs` subdirectory.

### Deprecated

Nothing deprecated in this release.

### Removed

- Any reference in the README to storing the schema in the database.

### Fixed

- `dependent-relationship-p` included debug-logging which blindly dumped the target resource it had fetched. Because resource definitions contain links to all resources to which they have an outgoing relationship, this resulted in an effectively infinite loop and a guaranteed OOM. Now it just prints the name of the target resource.
- API: resource deletion didn´t properly check for the `recursive` option, causing it to always see it as True.
- Bad invocation of `log-message` in `digest-schema-yaml`, which still assumed that `log-message` auto-applied `format`.

### Security


## Previous version: [v0.6.7]

Here be dragons, in the form of an even messier series of commits and no changelog to this point.

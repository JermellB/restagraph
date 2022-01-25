# Changelog

All notable changes.

Issue references of the form #<number> refer to tickets on Github: https://github.com/equill/restagraph/issues


## [Unreleased]

### Bugs fixed

- API hang in response to a POST request with a 2-element URI, i.e. a single resource (#67)
    - Previously, the server tried to handle this as a request to move a dependent resource to a new parent, which led to a stracktrace when it tried to use an element from the URI that wasn't there.
    - This is now met with a 400/Bad Request error, and a prompt that you may have intended to link that resource to another one, but forgotten to include the relationship at the end of the URI.
- API hangs in response to a POST request where the UID contains an apostrophe (#65)
    - Sanitisation of UIDs is now much stricter.


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

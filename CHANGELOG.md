# Changelog

All notable changes 

## [Unreleased]

Changelog starts here.


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

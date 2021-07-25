# Access-control

Working notes as I flesh this out.


# Overview of how it works

There's a collection of policies that can be combined in a mix-and-match style, and these are conveyed via the slots in an `access-policy` object. The object has a slot for each HTTP method.

A variety of policies are predefined, and the active one is selected by passing its name to the helper-function `define-policy`. The default is "allow-all", and this can be overridden by specifying a valid policy in the `ACCESS_POLICY` environment variable. Currently-implemented policies are:

- `open`
  - Clients are unauthenticated, and everything is done as the `RgAdmin` user.
- `read-only`
  - Clients are unauthenticated, but GET is the only permitted HTTP method.

Each dispatcher implements the policies accordingly.


# Elements involved

## Resources

### RgAdmin user

As part of the `core-schema` injection, a `People` resource is created with the UID `RgAdmin`. This is the site-admin user, and the default owner of all objects. If the policy is "open", then `RgAdmin` is the owner of whatever is created.


## Objects

### access-policy

Object on which policy methods can be dispatched.

It has one slot for each of the supported HTTP methods, whose values are the keys to be used by each of the dispatchers.


## Functions

### define-policy

Helper function, which returns a predefined named policy.

That is, it takes a name and returns a corresponding policy (or a not-defined error).


### get-owner

Fetches the UID for the People resource to be assigned the owner of any new thing that's created, or simply to be logged as the user who took the action.


## Conditions

### authorisation-required

This action requires authorisation, and thus authentication, and the client is not authorised.


### forbidden

The client is authenticated, and is not authorised to perform this action.

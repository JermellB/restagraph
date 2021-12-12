# Restagraph core schema

When Restagraph is started up and doesn't detect an existing schema, it installs one automatically, which covers the very core essentials. Some are assumed to be there by the server, and it'll break if they're not; others are just so obvious that they'll be needed virtually every time.

This is a handy summary of what's in it: resource-types, their attributes, and relationships that can be created from them to other resourcetypes. It glosses over the cardinality of relationships, and whether they're dependent ones; by the time you need this, you'll be querying the API for the definitive version anyway.

Speaking of which, you _can_ get this information with a get request to `/schema/v1` on a fresh installation. However, it seems like a good idea to provide a convenient reference for exactly what you can rely on already being there, and thus what you can _always_ build on without having to add it yourself or rely on somebody else having added it.

Note that some of these things are specifically for IP Address Management (or IPAM for short). This is functionality that was bolted on for Syscat, but that I eventually folded into the core Restagraph engine. The reasons were firstly, maintaining it separately involved far more work than it was worth, and secondly, I realised there wasn't a strong argument for separating them if I'd open-sourced Syscat anyway. These are also good demonstrations of why I designed the API to be explicit about both the relationship name _and_ the target resourcetype in a URI-path.


## any

- Description: Special-case meta-resource, representing an instance of any type of resource. This is used for defining relationships where either the source or target could be, well, any resourcetype. The server refuses to create an instance of this resourcetype.
- Attributes: none.
- Outbound relationships:
    - `/TAGS/Tags` - many:many - Any resourcetype can be tagged.
    - `/GROUPS/Groups` - many:many - Any resourcetype can be assigned to a group.
    - `/CREATOR/People` - 1:many - All resources are linked to their creator. This is the first part of the permissions-management system.


## Tags

- Description: For categorising resources of any type. Useful in searches.
- Attributes:
    - `description` - Clarification of what the tag means.
- Outbound relationships: none


## Groups

- Description: For collecting resources into explicit groups.
- Attributes:
    - `description` - Clarification of what the group means.
- Outbound relationships: none


## Organisations

- Description: Any kind of organisation: professional, social or other.
- Attributes:
    - `description` - Notes about this particular organisation.
- Outbound relationships:
    - `/MEMBERS/People` - Denotes who belongs to this organisation. Counterpart to `/People/MEMBER_OF/Organisations`.
    - `/VRF_GROUPS/VrfGroups` - for IPAM purposes, the Virtual Routing and Forwarding circuits that you've allocated within your organisation's network.
    - `/SUBNETS/Ipv4Subnets` - the IPv4 supernets that you've allocated within your organisation, independent of any VRF Groups.
    - `/SUBNETS/Ipv6Subnets` - the IPv6 supernets that you've allocated within your organisation, independent of any VRF Groups.


## People

- Description: Real people, imaginary people, security roles, members of an external organisation... if they're a person, this is the type.
- Attributes:
    - `displayname` - The human-friendly version of their name, to be displayed in the UI.
    - `notes` - Notes about this person.
- Outbound relationships:
    - `/MEMBER_OF/Organisations` - Denotes membership of an organisation. Counterpart to /Organisations/MEMBERS/People.
    - `/PRONOUNS/Pronouns` - She/her, they/them, he/him and whatever others you choose to add. These are defined as a separate resourcetype partly because some people accept more than one set, and partly to make it easier to add more as necessary.


## Pronouns

- Description: The pronouns by which a person prefers to be addressed.
- Attributes:
    - `text` - The full, non-URL-safe text of the pronoun set. E.g, They/them.
- Outbound relationships: none


## Files

- Description: Metadata about files uploaded by users. The files themselves are stored separately, using the sha3-256 checksum as the filename.
- Attributes:
    - `title` - The requested filename, recorded verbatim instead of having to be sanitised for URI-safety.
    - `notes` - Notes about this file.
    - `mimetype` - The detected MIME-type of this file, i.e. the description used for attaching files to emails or transferring to/from webservers.
    - `sha3256sum` - The SHA3-256 checksum of the file. Chosen for resistance against length-extension collisions.
- Outbound relationships: none


## VrfGroups

- Description: VRF Groups, as allocated by an organisation.
- Attributes:
    - `description` - Helpful notes about what this group is for.
- Outbound relationships:
    - `/SUBNETS/Ipv4Subnets` - IPv4 supernets allocated to this VRF Group.
    - `/SUBNETS/Ipv6Subnets` - IPv6 supernets allocated to this VRF Group.


## Ipv4Subnets

- Description: IPv4 Subnets, as allocated rather than as configured.
- Attributes:
    - `description` - Who or what this subnet is allocated for, and possibly why.
    - `netaddress` - The network address of the subnets.
    - `prefixlength` - the prefix length of the subnet - an integer between 1 and 32.
- Outbound relationships:
    - `/SUBNETS/Ipv4Subnets` - A subnet of this subnet.
    - `/ADDRESSES/Ipv4Addresses` - An address allocated from within this subnet.


## Ipv4Addresses

- Description: Ipv4Addresses. Unqualified, so really only useful for allocating.
- Attributes:
    - `description` - What this address is allocated to, and possibly why.
- Outbound relationships: none.


## Ipv6Subnets

- Description: IPv6 Subnets, as allocated rather than as configured.
- Attributes:
    - `description` - Who or what this subnet is allocated for, and possibly why.
    - `netaddress` - The network address of the subnets.
    - `prefixlength` - the prefix length of the subnet - an integer between 1 and 128.
- Outbound relationships:
    - `/SUBNETS/Ipv6Subnets` - A subnet of this subnet.
    - `/ADDRESSES/Ipv6Addresses` - An address allocated from within this subnet.


## Ipv6Addresses

- Description: Ipv6Addresses. Unqualified, so really only useful for allocating.
- Attributes:
    - `description` - What this address is allocated to, and possibly why.
- Outbound relationships: none.

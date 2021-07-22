# IPAM API docs

The IP Address Management (IPAM) API warrants a HOWTO of its own.

This was designed by a network engineer, for other network engineers, so this page assumes you have at least a general understanding of managing TCP/IP networks in a commercial (or commercial-like) environment. In keeping with section 2(4) of [RFC 1925](https://datatracker.ietf.org/doc/html/rfc1925), much of what follows will only make sense if you've managed one.

If this document makes no sense to you, either you don't need this functionality and can ignore these APIs, or you're in for quite a learning experience.

This was actually built for Syscat, but it eventually made most sense to just fold the functionality into the core product and make it available to everybody who uses Restagraph. For the full context of how this was intended to be used (assuming you didn't come here from there in the first place) check out [Syscat](https://github.com/equill/syscat) - you might be glad you did :)


## Conceptual overview

IP network and address allocation is surprisingly difficult to get right. It gets even more complicated when you try to keep track of private address space across multiple organisations.

The subnet API automatically takes care of subnetting, and of merging/rebalancing subnets and address allocations when you delete a subnet that has subnets of its own.

The addresses API takes care of figuring out where to attach addresses as you allocate them. It's not separate for any ideological reason, but for the purely practical one that I haven't figured out how to manage them under the same endpoint.


### Allocated vs configured

It's important to bear in mind the distinction between _allocated_ subnets and addresses, and _configured_ addresses. Allocated means "I/we _intend_ for this subnet to be used over here, and for this address within the subnet to be used by that device." Configured means "this interface answers to traffic on that address, and possibly also sends traffic from it." Despite the fervent insistence by many people, these are not the same thing - and there's absolutely no guarantee that what's configured is what was allocated.

This functionality is about _allocating_ networks and addresses for use - it's about what's _intended_. If you want to record what address is _configured_ on an interface, the regular API serves just fine.


# Subnets API

This section is extremely terse, because it's more urgent that I write this stuff down at all. Once I've finished writing it, I'll come back and make it more human-friendly. There's a good chance that this will happen after I've documented Syscat, because that will provide the background that makes full sense of it.

Base URI: `/subnets/v1`

## POST

Create a subnet.

### Required parameters

- `subnet`
    - Either IPv4 or IPv6, in CIDR format.
- `org`
    - Must be the UID for an organisation that already exists in the database.

### Optional parameters

- `vrf`
    - Relevant if you've defined one or more VRF-groups under the organisation, and want to allocate this subnet within one of them.

### Return value

The canonical URI for the newly-created subnet, e.g. `/Organisations/MyCompany/SUBNETS/Ipv4Subnets/10.255.0.0_8/SUBNETS/Ipv4Subnets/10.255.0.1_24`

It's returned via the same method that's invoked via the GET request, so you can rely on the two results being consistent.


## GET

Search for a subnet.

### Required parameters

- `subnet`
    - Either IPv4 or IPv6, in CIDR format.
- `org`


### Optional parameters

- `vrf`


### Return value

The canonical URI for the subnet, e.g. `/Organisations/MyCompany/SUBNETS/Ipv4Subnets/10.255.0.0_8/SUBNETS/Ipv4Subnets/10.255.0.1_24`


## DELETE

Delete a subnet

### Required parameters

- `subnet`
    - Either IPv4 or IPv6, in CIDR format.
- `org`


### Optional parameters

- `vrf`


### Return value

On success: 204/No content



# Addresses API

## POST

Create an address.


### Required parameters

- `address`
    - A bare IP address, i.e. no prefix-length or other indicator of its subnet. Restagraph will figure that out.
- `org`


### Optional parameters

- `vrf`



### Return value

The canonical URI for the newly-created address, e.g. `/Organisations/MyCompany/SUBNETS/Ipv4Subnets/10.255.0.0_8/SUBNETS/Ipv4Subnets/10.255.0.1_24/ADDRESSES/Ipv4Addresses/10.255.0.9`

It uses the same function that's invoked via the GET method, so you can rely on their results being consistent.


## GET

Search for an address


### Required parameters

- `address`
    - A bare IP address, i.e. no prefix-length or other indicator of its subnet. Restagraph will figure that out.
- `org`


### Optional parameters

- `vrf`



### Return value

The canonical URI for the address, e.g. `/Organisations/MyCompany/SUBNETS/Ipv4Subnets/10.255.0.0_8/SUBNETS/Ipv4Subnets/10.255.0.1_24/ADDRESSES/Ipv4Addresses/10.255.0.9`


## DELETE

Delete an address entry


### Required parameters

- `address`
    - A bare IP address, i.e. no prefix-length or other indicator of its subnet. Restagraph will figure that out.
- `org`


### Optional parameters

- `vrf`



### Return value

On success: 204/No content

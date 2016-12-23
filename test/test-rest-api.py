#!/usr/bin/python3

# Test package for Restagraph's REST API.
#
# Written in Python because that's the language I expect most users to implement
# their code in.
# Using Python3 because it's about time we all moved forward, and Docker means
# we no longer need to stick with obsolete stuff.


# Included batteries
import unittest

# Third-party libraries
import requests


# Config variables
PROTOCOL = 'http'
SERVER_URL = 'localhost:4950'
PREFIX = '/api/v1'

BASE_URL = '%s://%s%s' % (PROTOCOL, SERVER_URL, PREFIX)


# Tests

class TestResources(unittest.TestCase):
    '''
    Basic CRD functions for resources
    '''
    routertype = 'routers'
    routername = 'amchitka'
    routercomment = 'Test router 1'
    def test_create_and_delete_single_resource(self):
        # Ensure it's not already present
        self.assertEqual(requests.get('%s/%s/%s' % (BASE_URL, self.routertype, self.routername)).status_code,
                404)
        # Create it without attributes
        self.assertEqual(requests.post('%s/%s/' % (BASE_URL, self.routertype), data={'uid': self.routername}).status_code,
                201)
        # Confirm that it's now there
        self.assertEqual(requests.get('%s/%s/%s' % (BASE_URL, self.routertype, self.routername)).json(),
                { 'uid': self.routername })
        # Delete it
        self.assertEqual(
                requests.delete('%s/%s/' % (BASE_URL, self.routertype), data={'uid': self.routername}).status_code,
                204)
        # Confirm it's gone
        self.assertEqual(requests.get('%s/%s/%s' % (BASE_URL, self.routertype, self.routername)).status_code,
                404)
        # Create it again, this time with a comment
        self.assertEqual(requests.post('%s/%s/' % (BASE_URL, self.routertype), data={'uid': self.routername, 'comment': self.routercomment}).status_code,
                201)
        # Confirm that it's there, complete with comment
        self.assertEqual(requests.get('%s/%s/%s' % (BASE_URL, self.routertype, self.routername)).json(),
                { 'uid': self.routername, 'comment': self.routercomment })
        # Delete it again
        self.assertEqual(
                requests.delete('%s/%s/' % (BASE_URL, self.routertype), data={'uid': self.routername}).status_code,
                204)
        # Confirm it's gone again
        self.assertEqual(requests.get('%s/%s/%s' % (BASE_URL, self.routertype, self.routername)).status_code,
                404)

class TestMultipleResources(unittest.TestCase):
    '''
    Retrieve details of all resources of a given type.
    '''
    resourcetype='routers'
    resource1uid='amchitka'
    resource1attrname='comment'
    resource1attrval='Test router'
    resource2uid='bikini'
    resource3uid='mururoa'
    def test_create_and_retrieve_multiple_resources(self):
        # Confirm we're starting with an empty set
        self.assertEqual(requests.get('%s/%s' % (BASE_URL, self.resourcetype)).status_code, 200)
        self.assertFalse(requests.get('%s/%s' % (BASE_URL, self.resourcetype)).json())
        # Add the first resource
        self.assertEqual(requests.post('%s/%s/' % (BASE_URL, self.resourcetype), data={'uid': self.resource1uid, self.resource1attrname: self.resource1attrval}).status_code, 201)
        # Check that we now get a list containing exactly that resource
        self.assertEqual(requests.get('%s/%s' % (BASE_URL, self.resourcetype)).json(),
                [[{'uid': self.resource1uid, self.resource1attrname: self.resource1attrval}]])
        # Add the second resource
        self.assertEqual(requests.post('%s/%s/' % (BASE_URL, self.resourcetype), data={'uid': self.resource2uid}).status_code, 201)
        # Check that we now get a list containing exactly both resources
        self.assertEqual(requests.get('%s/%s' % (BASE_URL, self.resourcetype)).json(),
                [[{'uid': self.resource1uid, self.resource1attrname: self.resource1attrval}],
                    [{'uid': self.resource2uid}]])
        # Add the third resource
        self.assertEqual(requests.post('%s/%s/' % (BASE_URL, self.resourcetype), data={'uid': self.resource3uid}).status_code, 201)
        # Check that we now get a list containing exactly both resources
        self.assertEqual(requests.get('%s/%s' % (BASE_URL, self.resourcetype)).json(),
                [[{'uid': self.resource1uid, self.resource1attrname: self.resource1attrval}],
                    [{'uid': self.resource2uid}],
                    [{'uid': self.resource3uid}]])
        # Delete the resources
        self.assertEqual( requests.delete('%s/%s/' % (BASE_URL, self.resourcetype), data={'uid': self.resource1uid}).status_code, 204)
        self.assertEqual( requests.delete('%s/%s/' % (BASE_URL, self.resourcetype), data={'uid': self.resource2uid}).status_code, 204)
        self.assertEqual( requests.delete('%s/%s/' % (BASE_URL, self.resourcetype), data={'uid': self.resource3uid}).status_code, 204)


class TestValidRelationships(unittest.TestCase):
    '''
    Basic CRD functions for relationships
    '''
    routertype = 'routers'
    routername = 'bikini'
    routercomment = 'Test router 2'
    rel_router_interface = 'Interfaces'
    interfacetype = 'interfaces'
    interfacename = 'ge-0_0_0'
    interfaceattributes = '{"enabled": "True", "mac-address": "12:34:56:ab:cd:ef"}'
    def test_basic_relationship(self):
        # Create two new resources
        self.assertEqual(requests.post('%s/%s/' % (BASE_URL, self.routertype), data={'uid': self.routername, 'comment': self.routercomment}).status_code,
                201)
        self.assertEqual(requests.post('%s/%s/' % (BASE_URL, self.interfacetype), data={'uid': self.interfacename}).status_code,
                201)
        # Create a valid relationship between them
        self.assertEqual(requests.post('%s/%s/%s/%s'% (BASE_URL, self.routertype, self.routername, self.rel_router_interface), data={'type': self.interfacetype, 'uid': self.interfacename}).status_code,
                201)
        # Confirm that the relationship is there
        self.assertEqual(requests.get('%s/%s/%s/%s' % (BASE_URL, self.routertype, self.routername, self.rel_router_interface)).status_code,
                200)
        self.assertEqual(requests.get('%s/%s/%s/%s' % (BASE_URL, self.routertype, self.routername, self.rel_router_interface)).json(),
                [{"resource-type": self.interfacetype, "uid": self.interfacename}])
        # Delete the relationship
        self.assertEqual(requests.delete('%s/%s/%s/%s'% (BASE_URL, self.routertype, self.routername, self.rel_router_interface), data={'type': self.interfacetype, 'uid': self.interfacename}).status_code,
                204)
        # Confirm the relationship is gone
        self.assertEqual(requests.get('%s/%s/%s/%s' % (BASE_URL, self.routertype, self.routername, self.rel_router_interface)).status_code, 404)
        # Delete the destination resource
        self.assertEqual(requests.delete('%s/%s' % (BASE_URL, self.interfacetype), data={'uid': self.interfacename}).status_code,
                204)
        # Create the relationship and the destination resource
        self.assertEqual(requests.post('%s/%s/%s/%s'% (BASE_URL, self.routertype, self.routername, self.rel_router_interface), data={'type': self.interfacetype, 'uid': self.interfacename}).status_code,
                201)
        # Confirm the relationship is there again
        self.assertEqual(requests.get('%s/%s/%s/%s' % (BASE_URL, self.routertype, self.routername, self.rel_router_interface)).status_code,
                200)
        self.assertEqual(requests.get('%s/%s/%s/%s' % (BASE_URL, self.routertype, self.routername, self.rel_router_interface)).json(),
                [{"resource-type": self.interfacetype, "uid": self.interfacename}])
        # Delete the relationship
        self.assertEqual(requests.delete('%s/%s/%s/%s'% (BASE_URL, self.routertype, self.routername, self.rel_router_interface), data={'type': self.interfacetype, 'uid': self.interfacename}).status_code,
                204)
        # Confirm the relationship is gone
        self.assertEqual(requests.get('%s/%s/%s/%s' % (BASE_URL, self.routertype, self.routername, self.rel_router_interface)).status_code, 404)
        # Delete the destination resource
        self.assertEqual(requests.delete('%s/%s' % (BASE_URL, self.interfacetype), data={'uid': self.interfacename}).status_code,
                204)
        # Create the relationship and the destination resource, but with attributes this time
        self.assertEqual(requests.post('%s/%s/%s/%s'% (BASE_URL, self.routertype, self.routername, self.rel_router_interface), data={'type': self.interfacetype, 'uid': self.interfacename, 'attributes': self.interfaceattributes}).status_code,
                201)
        # Confirm the relationship is there again
        self.assertEqual(requests.get('%s/%s/%s/%s' % (BASE_URL, self.routertype, self.routername, self.rel_router_interface)).status_code,
                200)
        self.assertEqual(requests.get('%s/%s/%s/%s' % (BASE_URL, self.routertype, self.routername, self.rel_router_interface)).json(),
                [{"resource-type": self.interfacetype, "uid": self.interfacename}])
        # Confirm the attributes are present in the destination resource
        self.assertEqual(requests.get('%s/%s/%s' % (BASE_URL, self.interfacetype, self.interfacename)).json(),
                { 'uid': self.interfacename, 'enabled': 'True', 'macAddress': '12:34:56:ab:cd:ef' })
        # Delete the relationship
        self.assertEqual(requests.delete('%s/%s/%s/%s'% (BASE_URL, self.routertype, self.routername, self.rel_router_interface), data={'type': self.interfacetype, 'uid': self.interfacename}).status_code,
                204)
        # Confirm the relationship is gone
        self.assertEqual(requests.get('%s/%s/%s/%s' % (BASE_URL, self.routertype, self.routername, self.rel_router_interface)).status_code, 404)
        # Delete the resources
        self.assertEqual(requests.delete('%s/%s' % (BASE_URL, self.routertype), data={'uid': self.routername}).status_code,
                204)
        self.assertEqual(requests.delete('%s/%s' % (BASE_URL, self.interfacetype), data={'uid': self.interfacename}).status_code,
                204)

class TestInvalidRelationships(unittest.TestCase):
    '''
    Basic CRD functions for relationships
    '''
    routertype = 'routers'
    routername = 'bikini'
    routercomment = 'Test router 2'
    addresstype = 'ipv4Addresses'
    address = '127.0.0.1'
    rel_router_interface = 'Interfaces'
    rel_invalid = 'dysfunctionalRelationship'
    def test_basic_relationship(self):
        # Create two new resources
        self.assertEqual(requests.post('%s/%s' % (BASE_URL, self.routertype), data={'uid': self.routername, 'comment': self.routercomment}).status_code,
                201)
        self.assertEqual(requests.post('%s/%s' % (BASE_URL, self.addresstype), data={'uid': self.address}).status_code,
                201)
        # Attempt to create a relationship between them, of a type that doesn't exist for the source resource-type
        self.assertEqual(requests.post('%s/%s/%s/%s'% (BASE_URL, self.routertype, self.routername, self.rel_invalid), data={'type': self.addresstype, 'uid': self.address}).status_code,
                409)
        # Attempt to create an invalid relationship between them
        self.assertEqual(requests.post('%s/%s/%s/%s'% (BASE_URL, self.routertype, self.routername, self.rel_router_interface), data={'type': self.addresstype, 'uid': self.address}).status_code,
                409)
        # Delete the resources
        self.assertEqual(requests.delete('%s/%s' % (BASE_URL, self.routertype), data={'uid': self.routername}).status_code,
                204)
        self.assertEqual(requests.delete('%s/%s' % (BASE_URL, self.addresstype), data={'uid': self.address}).status_code,
                204)

class TestDbSchema(unittest.TestCase):
    '''
    Check that the DB schema is being enforced.
    Principally, make sure we can't create duplicates.
    '''
    routertype = 'routers'
    routername = 'whitesands'
    routercomment = 'Test router 3'
    def test_unique_resources(self):
        # Create a new resource
        self.assertEqual(requests.post('%s/%s/' % (BASE_URL, self.routertype), data={'uid': self.routername, 'comment': self.routercomment}).status_code,
                201)
        # Confirm that it's now there
        self.assertEqual(requests.get('%s/%s/%s/' % (BASE_URL, self.routertype, self.routername)).json(),
                { 'uid': self.routername, 'comment': self.routercomment })
        # Attempt to create a duplicate.
        self.assertEqual(requests.post('%s/%s/' % (BASE_URL, self.routertype), data={'uid': self.routername, 'comment': self.routercomment}).status_code,
                409)
        # Delete the resource
        self.assertEqual(requests.delete('%s/%s' % (BASE_URL, self.routertype), data={'uid': self.routername}).status_code,
                204)

class TestBasicResourceErrors(unittest.TestCase):
    '''
    Confirm what happens when we make basic errors in resource-creation requests.
    '''
    invalid_resourcetype='IjustMadeThisUp'
    valid_resourcetype='routers'
    def test_basic_resource_errors(self):
        # Invalid resource-type
        self.assertEqual(requests.post('%s/%s' % (BASE_URL, self.invalid_resourcetype), data={'foo': 'bar'}).status_code,
        409)
        # Missing UID
        self.assertEqual(requests.post('%s/%s' % (BASE_URL, self.valid_resourcetype), data={'foo': 'bar'}).status_code,
        400)
        # Invalid non-UID parameters
        self.assertEqual(requests.post('%s/%s' % (BASE_URL, self.valid_resourcetype), data={'uid': 'amchitka', 'foo': 'bar'}).status_code,
        400)

# Make it happen
if __name__ == '__main__':
    unittest.main()

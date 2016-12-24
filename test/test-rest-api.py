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
    restype = 'routers'
    resuid = 'amchitka'
    resattrname = 'comment'
    resattrval = 'Test router 1'
    def test_create_and_delete_single_resource(self):
        # Ensure it's not already present
        self.assertEqual(requests.get('%s/%s/%s' % (BASE_URL, self.restype, self.resuid)).status_code,
                404)
        # Create it without attributes
        self.assertEqual(requests.post('%s/%s/' % (BASE_URL, self.restype), data={'uid': self.resuid}).status_code,
                201)
        # Confirm that it's now there
        self.assertEqual(requests.get('%s/%s/%s' % (BASE_URL, self.restype, self.resuid)).json(),
                {'uid': self.resuid})
        # Delete it
        self.assertEqual(
                requests.delete('%s/%s/%s' % (BASE_URL, self.restype, self.resuid)).status_code,
                204)
        # Confirm it's gone
        self.assertEqual(requests.get('%s/%s/%s' % (BASE_URL, self.restype, self.resuid)).status_code,
                404)
        # Create it again, this time with a comment
        self.assertEqual(requests.post('%s/%s/' % (BASE_URL, self.restype), data={'uid': self.resuid, self.resattrname: self.resattrval}).status_code,
                201)
        # Confirm that it's there, complete with comment
        self.assertEqual(requests.get('%s/%s/%s' % (BASE_URL, self.restype, self.resuid)).json(),
                {'uid': self.resuid, self.resattrname: self.resattrval})
        # Delete it again
        self.assertEqual(
                requests.delete('%s/%s/%s' % (BASE_URL, self.restype, self.resuid)).status_code,
                204)
        # Confirm it's gone again
        self.assertEqual(requests.get('%s/%s/%s' % (BASE_URL, self.restype, self.resuid)).status_code,
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
        self.assertEqual(requests.get('%s/%s' % (BASE_URL, self.resourcetype)).status_code, 404)
        self.assertEqual(requests.get('%s/%s' % (BASE_URL, self.resourcetype)).text, 'No resources found for /routers')
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
        self.assertEqual( requests.delete('%s/%s/%s' % (BASE_URL, self.resourcetype, self.resource1uid)).status_code, 204)
        self.assertEqual( requests.delete('%s/%s/%s' % (BASE_URL, self.resourcetype, self.resource2uid)).status_code, 204)
        self.assertEqual( requests.delete('%s/%s/%s' % (BASE_URL, self.resourcetype, self.resource3uid)).status_code, 204)

class TestDependentResources(unittest.TestCase):
    res1type='routers'
    res1uid='whitesands'
    relationship1='Interfaces'
    depres1type='interfaces'
    depres1uid='ethernet0'
    def test_create_and_delete_a_single_dependent_resource(self):
        # Create the parent resource
        self.assertEqual(requests.post('%s/%s/' % (BASE_URL, self.res1type), data={'uid': self.res1uid}).status_code,
                201)
        # Screw up creation of the dependent resource, to check error-handling
        self.assertEqual(requests.post('%s/%s/%s/%s' % (BASE_URL, self.depres1type, self.depres1uid, self.relationship1),
            data={'type': self.depres1type, 'uid': self.depres1uid}).status_code,
            400)
        # Now get it right
        self.assertEqual(requests.post('%s/%s/%s/%s' % (BASE_URL, self.res1type, self.res1uid, self.relationship1), data={'type': self.depres1type, 'uid': self.depres1uid}).status_code,
                201)
        # Confirm it's there
        self.assertEqual(requests.get('%s/%s/%s/%s/%s/%s' % (BASE_URL, self.res1type, self.res1uid, self.relationship1, self.depres1type, self.depres1uid)).status_code,
                200)
        self.assertEqual(requests.get('%s/%s/%s' % (BASE_URL, self.depres1type, self.depres1uid)).status_code,
                200)
        # Delete the dependent resource
        # First, fail by skipping the delete-dependent parameter
        self.assertEqual( requests.delete('%s/%s/%s/%s/%s/%s' % (BASE_URL, self.res1type, self.res1uid, self.relationship1, self.depres1type, self.depres1uid)).status_code,
                400)
        # Now get it right
        self.assertEqual( requests.delete('%s/%s/%s/%s/%s/%s' % (BASE_URL, self.res1type, self.res1uid, self.relationship1, self.depres1type, self.depres1uid), data={'delete-dependent': 'true'}).status_code,
                204)
        # Delete the parent resource
        self.assertEqual( requests.delete('%s/%s/%s' % (BASE_URL, self.res1type, self.res1uid)).status_code,
                204)
    def test_recursively_delete_the_parent(self):
        # Create the parent resource
        self.assertEqual(requests.post('%s/%s/' % (BASE_URL, self.res1type), data={'uid': self.res1uid}).status_code,
                201)
        # Create a dependent resource
        self.assertEqual(requests.post('%s/%s/%s/%s' % (BASE_URL, self.res1type, self.res1uid, self.relationship1), data={'type': self.depres1type, 'uid': self.depres1uid}).status_code,
                201)
        # Confirm it's there
        self.assertEqual(requests.get('%s/%s/%s/%s/%s/%s' % (BASE_URL, self.res1type, self.res1uid, self.relationship1, self.depres1type, self.depres1uid)).status_code,
                200)
        self.assertEqual(requests.get('%s/%s/%s' % (BASE_URL, self.depres1type, self.depres1uid)).status_code,
                200)
        # Delete the parent resource
        self.assertEqual( requests.delete('%s/%s/%s' % (BASE_URL, self.res1type, self.res1uid)).status_code,
                204)
        # Ensure the dependent resource is gone
        self.assertEqual(requests.get('%s/%s/%s' % (BASE_URL, self.depres1type, self.depres1uid)).status_code,
                404)

class TestValidRelationships(unittest.TestCase):
    '''
    Basic CRD functions for relationships
    '''
    res1type = 'routers'
    res1uid = 'bikini'
    res1attrname = 'comment'
    res1attrval = 'Test router 2'
    relationship = 'Asn'
    res2type = 'asn'
    res2name = '64512'
    res3type = 'make'
    res3uid = 'NetBoxes'
    depres1type = 'model'
    depres1uid = 'Packetshuffler3000'
    depres1deprel = 'HasModel'
    res1todepres1rel = 'Model'
    def test_basic_relationship(self):
        # Create two new resources
        self.assertEqual(requests.post('%s/%s/' % (BASE_URL, self.res1type), data={'uid': self.res1uid, self.res1attrname: self.res1attrval}).status_code,
                201)
        self.assertEqual(requests.post('%s/%s/' % (BASE_URL, self.res2type), data={'uid': self.res2name}).status_code,
                201)
        # Create a valid relationship between them
        self.assertEqual(requests.post('%s/%s/%s/%s'% (BASE_URL, self.res1type, self.res1uid, self.relationship),
            data={'target': '/%s/%s' % (self.res2type, self.res2name)}).status_code,
            201)
        # Confirm that the relationship is there
        self.assertEqual(requests.get('%s/%s/%s/%s' % (BASE_URL, self.res1type, self.res1uid, self.relationship)).status_code,
            200)
        self.assertEqual(requests.get('%s/%s/%s/%s' % (BASE_URL, self.res1type, self.res1uid, self.relationship)).json(),
            [{"resource-type": self.res2type, "uid": self.res2name}])
        # Delete the relationship
        self.assertEqual(requests.delete('%s/%s/%s/%s/%s/%s'%
            (BASE_URL, self.res1type, self.res1uid, self.relationship, self.res2type, self.res2name)).status_code,
            204)
        # Confirm the relationship is gone
        self.assertEqual(requests.get('%s/%s/%s/%s' % (BASE_URL, self.res1type, self.res1uid, self.relationship)).status_code, 404)
        # Delete the destination resource
        self.assertEqual(requests.delete('%s/%s/%s' % (BASE_URL, self.res2type, self.res2name)).status_code,
                204)
        # Delete the resources
        self.assertEqual(requests.delete('%s/%s/%s' % (BASE_URL, self.res1type, self.res1uid)).status_code,
                204)
    def test_rels_between_primary_and_secondary_resources(self):
        # Create two first-class resources, with a third dependent on the first
        self.assertEqual(requests.post('%s/%s/' % (BASE_URL, self.res1type), data={'uid': self.res1uid}).status_code,
                201)
        self.assertEqual(requests.post('%s/%s/' % (BASE_URL, self.res3type), data={'uid': self.res3uid}).status_code,
                201)
        self.assertEqual(requests.post('%s/%s/%s/%s' % (BASE_URL, self.res3type, self.res3uid, self.depres1deprel),
            data={'type': self.depres1type, 'uid': self.depres1uid}).status_code,
            201)
        # Link from the dependent to the other first-class
        # Confirm that the linked first-class resource is at the end of that path
        # Link from the first-class to the dependent
        self.assertEqual(requests.post('%s/%s/%s/%s' % (BASE_URL, self.res1type, self.res1uid, self.res1todepres1rel), data={'target': '/%s/%s/%s/%s/%s' % (self.res3type, self.res3uid, self.depres1deprel, self.depres1type, self.depres1uid)}).status_code, 201)
        # Confirm that the linked dependent resource is at the end of that path
        self.assertEqual(requests.get('%s/%s/%s/%s/%s/%s' % (BASE_URL, self.res1type, self.res1uid, self.res1todepres1rel, self.depres1type, self.depres1uid)).status_code,
                200)
        # Delete both first-class resources
        self.assertEqual(requests.delete('%s/%s/%s' % (BASE_URL, self.res1type, self.res1uid)).status_code,
                204)
        self.assertEqual(requests.delete('%s/%s/%s' % (BASE_URL, self.res3type, self.res3uid)).status_code,
                204)

class TestInvalidRelationships(unittest.TestCase):
    '''
    Basic CRD functions for relationships
    '''
    res1type = 'routers'
    res1uid = 'bikini'
    res1comment = 'Test router 2'
    res2type = 'asn'
    res2uid = '64512'
    relationship_valid = 'Asn'
    relationship_invalid = 'dysfunctionalRelationship'
    def test_basic_relationship(self):
        # Create two new resources
        self.assertEqual(requests.post('%s/%s' % (BASE_URL, self.res1type),
            data={'uid': self.res1uid, 'comment': self.res1comment}).status_code,
            201)
        self.assertEqual(requests.post('%s/%s' % (BASE_URL, self.res2type),
            data={'uid': self.res2uid}).status_code,
            201)
        # Attempt to create a relationship between them, of a type that doesn't exist for the source resource-type
        self.assertEqual(requests.post('%s/%s/%s/%s'% (BASE_URL, self.res1type, self.res1uid, self.relationship_invalid),
            data={'target': '/%s/%s' % (self.res2type, self.res2uid)}).status_code,
            409)
        # Attempt to create a valid relationship between them
        self.assertEqual(requests.post('%s/%s/%s/%s'% (BASE_URL, self.res1type, self.res1uid, self.relationship_valid),
            data={'target': '/%s/%s' % (self.res2type, self.res2uid)}).status_code,
            201)
        # Delete the resources
        self.assertEqual(requests.delete('%s/%s/%s' % (BASE_URL, self.res1type, self.res1uid)).status_code,
            204)
        self.assertEqual(requests.delete('%s/%s/%s' % (BASE_URL, self.res2type, self.res2uid)).status_code,
            204)

class TestDbSchema(unittest.TestCase):
    '''
    Check that the DB schema is being enforced.
    Principally, make sure we can't create duplicates.
    '''
    resourcetype = 'routers'
    resourcename = 'whitesands'
    resourcecomment = 'Test router 3'
    def test_unique_resources(self):
        # Create a new resource
        self.assertEqual(requests.post('%s/%s/' % (BASE_URL, self.resourcetype), data={'uid': self.resourcename, 'comment': self.resourcecomment}).status_code,
                201)
        # Confirm that it's now there
        self.assertEqual(requests.get('%s/%s/%s/' % (BASE_URL, self.resourcetype, self.resourcename)).json(),
                {'uid': self.resourcename, 'comment': self.resourcecomment})
        # Attempt to create a duplicate.
        self.assertEqual(requests.post('%s/%s/' % (BASE_URL, self.resourcetype), data={'uid': self.resourcename, 'comment': self.resourcecomment}).status_code,
                409)
        # Delete the resource
        self.assertEqual(requests.delete('%s/%s/%s' % (BASE_URL, self.resourcetype, self.resourcename)).status_code,
                204)

class TestBasicResourceErrors(unittest.TestCase):
    '''
    Confirm what happens when we make basic errors in resource-creation requests.
    '''
    invalid_resourcetype='IjustMadeThisUp'
    valid_resourcetype='routers'
    valid_uid='amchitka'
    def test_basic_resource_errors(self):
        # Invalid resource-type
        self.assertEqual(requests.post('%s/%s' % (BASE_URL, self.invalid_resourcetype), data={'foo': 'bar'}).status_code,
        409)
        # Missing UID
        self.assertEqual(requests.post('%s/%s' % (BASE_URL, self.valid_resourcetype), data={'foo': 'bar'}).status_code,
        400)
        # Invalid non-UID parameters
        self.assertEqual(requests.post('%s/%s' % (BASE_URL, self.valid_resourcetype), data={'uid': self.valid_uid, 'foo': 'bar'}).status_code,
        400)

# Make it happen
if __name__ == '__main__':
    unittest.main()

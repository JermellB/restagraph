#!/usr/bin/python3

# Test package for Restagraph's REST API.


#   Copyright 2017 James Fleming <james@electronic-quill.net>
#
#   Licensed under the Apache License, Version 2.0 (the "License");
#   you may not use this file except in compliance with the License.
#   You may obtain a copy of the License at
#
#       http://www.apache.org/licenses/LICENSE-2.0
#
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.


# Included batteries
import unittest
import re

# Third-party libraries
import requests


# Config variables
PROTOCOL = 'http'
SERVER_URL = 'localhost:4950'
PREFIX = '/api/v1'

BASE_URL = '%s://%s%s' % (PROTOCOL, SERVER_URL, PREFIX)


# Utilities
def sanitise_uid(uid):
    '''
    Sanitise a UID string in the same way Restagraph does
    '''
    return re.sub('[/ ]', '_', uid)


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
        print('Test: test_create_and_delete_single_resource')
        # Ensure it's not already present
        self.assertEqual(requests.get('%s/%s/%s' % (BASE_URL, self.restype, self.resuid)).status_code,
                404)
        # Create it without attributes
        self.assertEqual(requests.post('%s/%s/' % (BASE_URL, self.restype), data={'uid': self.resuid}).status_code,
                201)
        # Confirm that it's now there
        self.assertEqual(requests.get('%s/%s/%s' % (BASE_URL, self.restype, self.resuid)).json(),
                {'original_uid': sanitise_uid(self.resuid), 'uid': self.resuid})
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
                {'original_uid': sanitise_uid(self.resuid), 'uid': self.resuid, self.resattrname: self.resattrval})
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
        print('Test: test_create_and_retrieve_multiple_resources')
        # Confirm we're starting with an empty set
        self.assertEqual(requests.get('%s/%s' % (BASE_URL, self.resourcetype)).status_code, 404)
        self.assertEqual(requests.get('%s/%s' % (BASE_URL, self.resourcetype)).text, 'No resources found for /routers')
        # Add the first resource
        self.assertEqual(requests.post('%s/%s/' % (BASE_URL, self.resourcetype), data={'uid': self.resource1uid, self.resource1attrname: self.resource1attrval}).status_code, 201)
        # Check that we now get a list containing exactly that resource
        self.assertEqual(requests.get('%s/%s' % (BASE_URL, self.resourcetype)).json(),
                [[{'original_uid': sanitise_uid(self.resource1uid), 'uid': self.resource1uid, self.resource1attrname: self.resource1attrval}]])
        # Add the second resource
        self.assertEqual(requests.post('%s/%s/' % (BASE_URL, self.resourcetype), data={'uid': self.resource2uid}).status_code, 201)
        # Check that we now get a list containing exactly both resources
        self.assertEqual(requests.get('%s/%s' % (BASE_URL, self.resourcetype)).json(),
                [[{'original_uid': sanitise_uid(self.resource1uid), 'uid': self.resource1uid, self.resource1attrname: self.resource1attrval}],
                    [{'original_uid': sanitise_uid(self.resource2uid), 'uid': self.resource2uid}]])
        # Add the third resource
        self.assertEqual(requests.post('%s/%s/' % (BASE_URL, self.resourcetype), data={'uid': self.resource3uid}).status_code, 201)
        # Check that we now get a list containing exactly both resources
        self.assertEqual(requests.get('%s/%s' % (BASE_URL, self.resourcetype)).json(),
                [[{'original_uid': sanitise_uid(self.resource1uid), 'uid': self.resource1uid, self.resource1attrname: self.resource1attrval}],
                    [{'original_uid': sanitise_uid(self.resource2uid), 'uid': self.resource2uid}],
                    [{'original_uid': sanitise_uid(self.resource3uid), 'uid': self.resource3uid}]])
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
        print('Test: test_create_and_delete_a_single_dependent_resource')
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
        self.assertEqual( requests.delete('%s/%s/%s' % (BASE_URL, self.res1type, self.res1uid), data={'delete-dependent' : 'true'}).status_code,
                204)
        # Ensure the dependent resource is gone
        self.assertEqual(requests.get('%s/%s/%s' % (BASE_URL, self.depres1type, self.depres1uid)).status_code,
                404)

class TestMoveDependentResources(unittest.TestCase):
    p1type='routers'
    p1uid='woomera'
    p1targetrel='Addresses'
    p2type='interfaces'
    p2uid='eth1'
    p1p2rel='Interfaces'
    p2targetrel='Addresses'
    targettype='ipv4Addresses'
    targetuid='172.20.0.1'
    def test_move_dependent_resource(self):
        print('Test: test_move_dependent_resource')
        # Create the initial parent resource
        self.assertEqual(requests.post('%s/%s/' % (BASE_URL, self.p1type), data={'uid': self.p1uid}).status_code, 201)
        # Create the second parent resource, as a dependent to the first
        self.assertEqual(requests.post('%s/%s/%s/%s' % (BASE_URL, self.p1type, self.p1uid, self.p1p2rel), data={'type': self.p2type, 'uid': self.p2uid}).status_code, 201)
        # Create the dependent resource
        self.assertEqual(requests.post('%s/%s/%s/%s' % (BASE_URL, self.p1type, self.p1uid, self.p1targetrel), data={'type': self.targettype, 'uid': self.targetuid}).status_code, 201)
        # Move the dependent resource to its new parent
        self.assertEqual(requests.post('%s/%s/%s/%s/%s/%s' % (BASE_URL, self.p1type, self.p1uid, self.p1targetrel, self.targettype, self.targetuid), data={'target': '/%s/%s/%s/%s/%s/%s' % (self.p1type, self.p1uid, self.p1p2rel, self.p2type, self.p2uid, self.p2targetrel)}).status_code, 201)
        # Confirm it's where it should be
        self.assertEqual(requests.get('%s/%s/%s/%s/%s/%s/%s/%s/%s' % (BASE_URL, self.p1type, self.p1uid, self.p1p2rel, self.p2type, self.p2uid, self.p2targetrel, self.targettype, self.targetuid)).status_code, 200)
        # Confirm it's no longer attached to the initial parent
        self.assertEqual(requests.get('%s/%s/%s/%s/%s/%s' % (BASE_URL, self.p1type, self.p1uid, self.p1targetrel, self.targettype, self.targetuid)).status_code, 404)
        # Delete the parent resource, which should take the rest with it
        self.assertEqual( requests.delete('%s/%s/%s' % (BASE_URL, self.p1type, self.p1uid), data={'delete-dependent': 'true'}).status_code, 204)


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
    depres1deprel = 'Produces'
    res1todepres1rel = 'Model'
    def test_basic_relationship(self):
        print('Test: test_basic_relationship')
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
        print('Test: test_rels_between_primary_and_secondary_resources')
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
        self.assertEqual(requests.delete('%s/%s/%s' % (BASE_URL, self.res3type, self.res3uid), data={'delete-dependent': 'true'}).status_code,
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
        print('Test: test_basic_relationship')
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
        print('Test: test_unique_resources')
        # Create a new resource
        self.assertEqual(requests.post('%s/%s/' % (BASE_URL, self.resourcetype), data={'uid': self.resourcename, 'comment': self.resourcecomment}).status_code,
                201)
        # Confirm that it's now there
        self.assertEqual(requests.get('%s/%s/%s/' % (BASE_URL, self.resourcetype, self.resourcename)).json(),
                {'original_uid': sanitise_uid(self.resourcename), 'uid': self.resourcename, 'comment': self.resourcecomment})
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
        print('Test: test_basic_resource_errors')
        # Invalid resource-type
        self.assertEqual(requests.post('%s/%s' % (BASE_URL, self.invalid_resourcetype), data={'foo': 'bar'}).status_code,
        400)
        # Missing UID
        self.assertEqual(requests.post('%s/%s' % (BASE_URL, self.valid_resourcetype), data={'foo': 'bar'}).status_code,
        400)
        # Invalid non-UID parameters
        self.assertEqual(requests.post('%s/%s' % (BASE_URL, self.valid_resourcetype), data={'uid': self.valid_uid, 'foo': 'bar'}).status_code,
        400)

class TestAnyType(unittest.TestCase):
    '''
    Confirm handling of the 'any' type
    '''
    p1type='routers'
    p1uid='Enewetak'
    p1rel='Tags'
    invalidrel='invalid'
    t1type='tags'
    t1uid='Tagged'
    def test_rejection(self):
        print('Test: test_any_type')
        self.assertEqual(requests.get('%s/any/foo' % BASE_URL).status_code, 404)
    def test_create_valid_any_rel(self):
        print('Test: test_create_valid_any_rel')
        # Create the resources
        self.assertEqual(requests.post('%s/%s' % (BASE_URL, self.p1type), data={'uid': self.p1uid}).status_code, 201)
        self.assertEqual(requests.post('%s/%s' % (BASE_URL, self.t1type), data={'uid': self.t1uid}).status_code, 201)
        # Create the relationship between them
        self.assertEqual(requests.post('%s/%s/%s/%s' % (BASE_URL, self.p1type, self.p1uid, self.p1rel), data={'target': '/%s/%s' % (self.t1type, self.t1uid)}).status_code, 201)
        # Delete the relationship
        self.assertEqual(requests.delete('%s/%s/%s/%s/%s/%s' % (BASE_URL, self.p1type, self.p1uid, self.p1rel, self.t1type, self.t1uid)).status_code, 204)
        # Delete the resources
        self.assertEqual(requests.delete('%s/%s/%s' % (BASE_URL, self.p1type, self.p1uid)).status_code, 204)
        self.assertEqual(requests.delete('%s/%s/%s' % (BASE_URL, self.t1type, self.t1uid)).status_code, 204)
    def test_create_invalid_any_rel(self):
        print('Test: test_create_valid_any_rel')
        # Create the resources
        self.assertEqual(requests.post('%s/%s' % (BASE_URL, self.p1type), data={'uid': self.p1uid}).status_code, 201)
        self.assertEqual(requests.post('%s/%s' % (BASE_URL, self.t1type), data={'uid': self.t1uid}).status_code, 201)
        # Create the relationship between them
        self.assertEqual(requests.post('%s/%s/%s/%s' % (BASE_URL, self.p1type, self.p1uid, self.invalidrel), data={'target': '/%s/%s' % (self.t1type, self.t1uid)}).status_code, 409)
        # Delete the resources
        self.assertEqual(requests.delete('%s/%s/%s' % (BASE_URL, self.p1type, self.p1uid)).status_code, 204)
        self.assertEqual(requests.delete('%s/%s/%s' % (BASE_URL, self.t1type, self.t1uid)).status_code, 204)

class TestUpdateAttrs(unittest.TestCase):
    '''
    Update and remove attributes of existing resources.
    '''
    p1type='routers'
    p1uid='Trinity'
    comment1='This is a comment'
    def test_simple_update(self):
        print('test_simple_update')
        # Create the resources
        self.assertEqual(requests.post('%s/%s' % (BASE_URL, self.p1type), data={'uid': self.p1uid}).status_code, 201)
        # Add an attribute and confirm the result
        result=requests.put('%s/%s/%s' % (BASE_URL, self.p1type, self.p1uid), data={'comment': self.comment1})
        self.assertEqual(result.status_code, 200)
        self.assertEqual(result.json(), {'uid': self.p1uid, 'original_uid': self.p1uid, 'comment': self.comment1})
        # Remove that attribute and confirm the result
        result=requests.put('%s/%s/%s' % (BASE_URL, self.p1type, self.p1uid), data={'comment': ""})
        self.assertEqual(result.status_code, 200)
        self.assertEqual(result.json(), {'uid': self.p1uid, 'original_uid': self.p1uid, 'comment': ""})
        # Delete the resource
        self.assertEqual(requests.delete('%s/%s/%s' % (BASE_URL, self.p1type, self.p1uid)).status_code, 204)

# Make it happen
if __name__ == '__main__':
    unittest.main()

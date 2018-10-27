#!/usr/bin/env python
# Expects python3

'''
Test package for Restagraph's REST API.
'''


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
API_PREFIX = '/api/v1'
SCHEMA_PREFIX = '/schema/v1'

API_BASE_URL = '%s://%s%s' % (PROTOCOL, SERVER_URL, API_PREFIX)
SCHEMA_BASE_URL = '%s://%s%s' % (PROTOCOL, SERVER_URL, SCHEMA_PREFIX)


# Utilities
def sanitise_uid(uid):
    '''
    Sanitise a UID string in the same way Restagraph does
    '''
    return re.sub('[/ ]', '_', uid)


# Tests

class TestSchemaApi(unittest.TestCase):
    '''
    Confirm that the schema API works as expected.
    '''
    def test_primary_resourcetype(self):
        print('Test: test_primary_resourcetype')
        # Schema should be empty
        self.assertEqual(requests.get('%s/' % (SCHEMA_BASE_URL)).json(), None)
        # Create a resource
        self.assertEqual(requests.post('%s/resourcetype/foo' % (SCHEMA_BASE_URL)).status_code, 201)
        # Confirm it's the only one present
        self.assertEqual(requests.get('%s/' % SCHEMA_BASE_URL).json(),
                         [{'name': 'foo',
                           'attributes': None,
                           'dependent': 'false',
                           'notes': '',
                           'relationships': None}])
        # Try to create a duplicate
        self.assertEqual(requests.post('%s/resourcetype/foo' % (SCHEMA_BASE_URL)).status_code, 200)
        # Delete it
        self.assertEqual(requests.delete('%s/resourcetype/foo' % (SCHEMA_BASE_URL)).status_code,
                         204)
        # Confirm it's gone
        self.assertEqual(requests.get('%s/' % (SCHEMA_BASE_URL)).json(), None)
    def test_dependent_resourcetype(self):
        print('Test: test_dependent_resourcetype')
        # Schema should be empty
        self.assertEqual(requests.get('%s/' % (SCHEMA_BASE_URL)).json(), None)
        # Create a resource
        self.assertEqual(requests.post('%s/resourcetype/foo' % (SCHEMA_BASE_URL),
                                       data={'dependent': 'true'}).status_code,
                         201)
        # Confirm it's the only one present
        self.assertEqual(requests.get('%s/' % SCHEMA_BASE_URL).json(),
                         [{'name': 'foo',
                           'attributes': None,
                           'dependent': 'false',
                           'notes': '',
                           'relationships': None}])
        # Delete it
        self.assertEqual(requests.delete('%s/resourcetype/foo' % (SCHEMA_BASE_URL)).status_code,
                         204)
        # Confirm it's gone
        self.assertEqual(requests.get('%s/' % (SCHEMA_BASE_URL)).json(), None)
    def test_resource_relationships(self):
        print('Test: test_resource_relationships')
        # Create the resources to connect
        requests.post('%s/resourcetype/lolcat' % (SCHEMA_BASE_URL))
        requests.post('%s/resourcetype/cheeseburger' % (SCHEMA_BASE_URL))
        # Connect them
        print('Add a relationship between resourcetypes foo and bar.')
        self.assertEqual(requests.post('%s/relationships/lolcat/canHaz/cheeseburger'
                                       % (SCHEMA_BASE_URL)).status_code,
                         201)
        # Disconnect them
        print('Delete the relationship between resourcetypes foo and bar.')
        self.assertEqual(requests.delete('%s/relationships/lolcat/canHaz/cheeseburger'
                                         % (SCHEMA_BASE_URL)).status_code,
                         204)
        # Remove the fixtures
        requests.delete('%s/resourcetype/lolcat' % (SCHEMA_BASE_URL))
        requests.delete('%s/resourcetype/cheeseburger' % (SCHEMA_BASE_URL))
        print('Test: schema should be empty')
        self.assertEqual(requests.get('%s/' % (SCHEMA_BASE_URL)).json(), None)
    def test_invalid_relationships(self):
        print('Test: test_invalid_relationships')
        # Create the resources to connect
        requests.post('%s/resourcetype/dangler' % (SCHEMA_BASE_URL))
        # Fail to connect it to nonexistent things
        self.assertEqual(requests.post('%s/relationships/dangler/holds/subdangler'
                                       % SCHEMA_BASE_URL).status_code,
                         409)
        self.assertEqual(requests.post('%s/relationships/superdangler/holds/dangler'
                                       % SCHEMA_BASE_URL).status_code,
                         409)
        # Remove the fixtures
        requests.delete('%s/resourcetype/dangler' % (SCHEMA_BASE_URL))
        print('Test: schema should be empty')
        self.assertEqual(requests.get('%s/' % (SCHEMA_BASE_URL)).json(), None)
    def test_dependent_resource_relationships(self):
        print('Test: test_dependent_resource_relationships')
        # Create the resources to connect
        requests.post('%s/resourcetype/lolcat' % (SCHEMA_BASE_URL))
        requests.post('%s/resourcetype/hunger' % (SCHEMA_BASE_URL))
        # Connect them
        print('Add a relationship between resourcetypes foo and bar.')
        self.assertEqual(requests.post('%s/relationships/lolcat/canHaz/hunger' % (SCHEMA_BASE_URL),
                                       data={'dependent': 'true'}).status_code,
                         201)
        # Disconnect them
        print('Delete the relationship between resourcetypes foo and bar.')
        self.assertEqual(requests.delete('%s/relationships/lolcat/canHaz/hunger'
                                         % (SCHEMA_BASE_URL)).status_code,
                         204)
        # Remove the fixtures
        requests.delete('%s/resourcetype/lolcat' % (SCHEMA_BASE_URL))
        requests.delete('%s/resourcetype/hunger' % (SCHEMA_BASE_URL))
        print('Test: schema should be empty')
        self.assertEqual(requests.get('%s/' % (SCHEMA_BASE_URL)).json(), None)

class TestResources(unittest.TestCase):
    '''
    Basic CRD functions for resources
    '''
    restype = 'routers'
    resuid = 'amchitka'
    def test_create_and_delete_single_resource(self):
        print('Test: create fixtures')
        requests.post('%s/resourcetype/%s' % (SCHEMA_BASE_URL, self.restype))
        print('Test: test_create_and_delete_single_resource')
        # Ensure it's not already present
        self.assertEqual(requests.get('%s/%s/%s' % (API_BASE_URL,
                                                    self.restype,
                                                    self.resuid)).status_code,
                         200)
        self.assertEqual(requests.get('%s/%s/%s' % (API_BASE_URL,
                                                    self.restype,
                                                    self.resuid)).text,
                         "null")
        self.assertEqual(requests.get('%s/%s/%s' % (API_BASE_URL,
                                                    self.restype,
                                                    self.resuid)).json(),
                         None)
        # Ensure we have none of that kind of resource
        self.assertEqual(requests.get('%s/%s' % (API_BASE_URL,
                                                    self.restype)).status_code,
                         200)
        self.assertEqual(requests.get('%s/%s' % (API_BASE_URL,
                                                    self.restype)).json(),
                         [])
        # Create it
        self.assertEqual(requests.post('%s/%s/' % (API_BASE_URL, self.restype),
                                       data={'uid': self.resuid}).status_code,
                         201)
        # Confirm that it's now there
        self.assertEqual(requests.get('%s/%s/%s' % (API_BASE_URL,
                                                    self.restype,
                                                    self.resuid)).json(),
                         {'original_uid': sanitise_uid(self.resuid), 'uid': self.resuid})
        # Delete it
        self.assertEqual(
            requests.delete('%s/%s/%s' % (API_BASE_URL, self.restype, self.resuid)).status_code,
            204)
        # Confirm it's gone
        self.assertEqual(requests.get('%s/%s/%s' % (API_BASE_URL,
                                                    self.restype,
                                                    self.resuid)).status_code,
                         200)
        self.assertEqual(requests.get('%s/%s/%s' % (API_BASE_URL,
                                                    self.restype,
                                                    self.resuid)).json(),
                         None)
        # Remove the fixtures
        print('Test: Clean up the fixtures')
        requests.delete('%s/resourcetype/%s' % (SCHEMA_BASE_URL, self.restype))
        print('Test: schema should be empty')
        self.assertEqual(requests.get('%s/' % (SCHEMA_BASE_URL)).json(), None)

class TestResourceAttributes(unittest.TestCase):
    '''
    Basic CRUD functions for resource attributes
    '''
    resourcetype = 'whatchamacallit'
    resourceuid = 'whatsisface'
    attr1type = 'whosit'
    attr1val = 'thingy'
    attr2type = 'whatsit'
    attr2val = 'hoodacky'
    def test_create_and_remove_resourcetypeattrs(self):
        print('Test: create the fixtures')
        requests.post('%s/resourcetype/%s' % (SCHEMA_BASE_URL, self.resourcetype))
        requests.post('%s/%s' % (API_BASE_URL, self.resourcetype), data={'uid': self.resourceuid})
        print('Test: fail to add an attribute')
        self.assertEqual(requests.put('%s/%s/%s' % (API_BASE_URL,
                                                    self.resourcetype,
                                                    self.resourceuid),
                                      data={self.attr1type: self.attr1val}).status_code,
                         400)
        print('Test: Add the first attribute to the resourcetype')
        self.assertEqual(requests.post('%s/attribute/%s/%s' % (SCHEMA_BASE_URL,
                                                               self.resourcetype,
                                                               self.attr1type)).status_code,
                         201)
        print('Test: Successfully add the first attribute to the resource')
        self.assertEqual(requests.put('%s/%s/%s' % (API_BASE_URL,
                                                    self.resourcetype,
                                                    self.resourceuid),
                                      data={self.attr1type: self.attr1val}).status_code,
                         201)
        self.assertEqual(requests.get('%s/%s/%s' % (API_BASE_URL,
                                                    self.resourcetype,
                                                    self.resourceuid)).json(),
                         {'original_uid': sanitise_uid(self.resourceuid),
                          'uid': self.resourceuid,
                          self.attr1type: self.attr1val})
        print('Test: Add the second attribute to the resourcetype')
        self.assertEqual(requests.post('%s/attribute/%s/%s' % (SCHEMA_BASE_URL,
                                                               self.resourcetype,
                                                               self.attr2type)).status_code,
                         201)
        print('Test: Successfully add two attributes to the resource at once')
        requests.delete('%s/%s/%s' % (API_BASE_URL, self.resourcetype, self.resourceuid))
        requests.post('%s/%s' % (API_BASE_URL, self.resourcetype), data={'uid': self.resourceuid})
        self.assertEqual(requests.put('%s/%s/%s' % (API_BASE_URL,
                                                    self.resourcetype,
                                                    self.resourceuid),
                                      data={self.attr1type: self.attr1val,
                                            self.attr2type: self.attr2val}).status_code,
                         201)
        self.assertEqual(requests.get('%s/%s/%s' % (API_BASE_URL,
                                                    self.resourcetype,
                                                    self.resourceuid)).json(),
                         {'original_uid': sanitise_uid(self.resourceuid),
                          'uid': self.resourceuid,
                          self.attr1type: self.attr1val,
                          self.attr2type: self.attr2val})
        print('Test: Remove the attributes from the resourcetype')
        self.assertEqual(requests.delete('%s/attribute/%s/%s' % (SCHEMA_BASE_URL,
                                                                 self.resourcetype,
                                                                 self.attr1type)).status_code,
                         204)
        self.assertEqual(requests.delete('%s/attribute/%s/%s' % (SCHEMA_BASE_URL,
                                                                 self.resourcetype,
                                                                 self.attr2type)).status_code,
                         204)
        print('Test: fail to add the first attribute')
        self.assertEqual(requests.put('%s/%s/%s' % (API_BASE_URL,
                                                    self.resourcetype,
                                                    self.resourceuid),
                                      data={self.attr1type: self.attr1val}).status_code,
                         400)
        print('Test: fail to add the second attribute')
        self.assertEqual(requests.put('%s/%s/%s' % (API_BASE_URL,
                                                    self.resourcetype,
                                                    self.resourceuid),
                                      data={self.attr1type: self.attr1val}).status_code,
                         400)
        print('Test: delete the fixtures')
        requests.delete('%s/%s/%s' % (API_BASE_URL, self.resourcetype, self.resourceuid))
        requests.delete('%s/resourcetype/%s' % (SCHEMA_BASE_URL, self.resourcetype))

class TestMultipleResources(unittest.TestCase):
    '''
    Retrieve details of all resources of a given type.
    '''
    resourcetype = 'routers'
    resource1uid = 'amchitka'
    resource2uid = 'bikini'
    resource3uid = 'mururoa'
    def test_create_and_retrieve_multiple_resources(self):
        print('Test: Create the fixtures')
        requests.post('%s/resourcetype/%s' % (SCHEMA_BASE_URL, self.resourcetype))
        print('Test: test_create_and_retrieve_multiple_resources')
        # Confirm we're starting with an empty set
        self.assertEqual(requests.get('%s/%s' % (API_BASE_URL, self.resourcetype)).status_code,
                         200)
        self.assertEqual(requests.get('%s/%s' % (API_BASE_URL, self.resourcetype)).json(),
                         [])
        # Add the first resource
        self.assertEqual(requests.post('%s/%s/' % (API_BASE_URL, self.resourcetype),
                                       data={'uid': self.resource1uid}).status_code,
                         201)
        # Check that we now get a list containing exactly that resource
        self.assertEqual(requests.get('%s/%s' % (API_BASE_URL, self.resourcetype)).json(),
                         [{'original_uid': sanitise_uid(self.resource1uid),
                           'uid': self.resource1uid}])
        # Add the second resource
        self.assertEqual(requests.post('%s/%s/' % (API_BASE_URL, self.resourcetype),
                                       data={'uid': self.resource2uid}).status_code,
                         201)
        # Check that we now get a list containing exactly both resources
        self.assertEqual(
            sorted(requests.get('%s/%s' % (API_BASE_URL, self.resourcetype)).json(),
                   key=lambda i: i['uid']),
            sorted([{'original_uid': sanitise_uid(self.resource2uid),
                     'uid': self.resource2uid},
                    {'original_uid': sanitise_uid(self.resource1uid),
                     'uid': self.resource1uid}],
                   key=lambda i: i['uid']))
        # Add the third resource
        self.assertEqual(requests.post('%s/%s/' % (API_BASE_URL, self.resourcetype),
                                       data={'uid': self.resource3uid}).status_code,
                         201)
        # Check that we now get a list containing all three resources
        self.assertEqual(sorted(requests.get('%s/%s' % (API_BASE_URL, self.resourcetype)).json(),
                                key=lambda i: i['uid']),
                         sorted([{'original_uid': sanitise_uid(self.resource3uid),
                                  'uid': self.resource3uid},
                                 {'original_uid': sanitise_uid(self.resource2uid),
                                  'uid': self.resource2uid},
                                 {'original_uid': sanitise_uid(self.resource1uid),
                                  'uid': self.resource1uid}],
                                key=lambda i: i['uid']))
        # Delete the resources
        print('Test: clean up afterward')
        self.assertEqual(requests.delete('%s/%s/%s' % (API_BASE_URL,
                                                       self.resourcetype,
                                                       self.resource1uid)).status_code,
                         204)
        self.assertEqual(requests.delete('%s/%s/%s' % (API_BASE_URL,
                                                       self.resourcetype,
                                                       self.resource2uid)).status_code,
                         204)
        self.assertEqual(requests.delete('%s/%s/%s' % (API_BASE_URL,
                                                       self.resourcetype,
                                                       self.resource3uid)).status_code,
                         204)
        requests.delete('%s/resourcetype/%s' % (SCHEMA_BASE_URL, self.resourcetype))
        print('Test: schema should be empty')
        self.assertEqual(requests.get('%s/' % (SCHEMA_BASE_URL)).json(), None)

class TestDependentResources(unittest.TestCase):
    res1type = 'routers'
    res1uid = 'whitesands'
    relationship1 = 'Interfaces'
    depres1type = 'interfaces'
    depres1uid = 'ethernet0'
    def test_create_and_delete_a_single_dependent_resource(self):
        print('Test: test_create_and_delete_a_single_dependent_resource')
        print('Test: create the fixtures')
        requests.post('%s/resourcetype/%s' % (SCHEMA_BASE_URL, self.res1type))
        requests.post('%s/resourcetype/%s' % (SCHEMA_BASE_URL, self.depres1type),
                      data={'dependent': 'true'})
        requests.post('%s/relationships/%s/%s/%s' % (SCHEMA_BASE_URL,
                                                    self.res1type,
                                                    self.relationship1,
                                                    self.depres1type),
                      data={'dependent': 'true'})
        # Create the parent resource
        self.assertEqual(requests.post('%s/%s/' % (API_BASE_URL, self.res1type),
                                       data={'uid': self.res1uid}).status_code,
                         201)
        # Screw up creation of the dependent resource, to check error-handling
        # Invert the parent/child relationship, and break the API to boot
        self.assertEqual(requests.post('%s/%s/%s/%s' % (API_BASE_URL,
                                                        self.depres1type,
                                                        self.depres1uid,
                                                        self.relationship1),
                                       data={'type': self.depres1type,
                                             'uid': self.depres1uid}).status_code,
                         400)
        # Now get it right
        self.assertEqual(requests.post('%s/%s/%s/%s/%s' % (API_BASE_URL,
                                                           self.res1type,
                                                           self.res1uid,
                                                           self.relationship1,
                                                           self.depres1type),
                                       data={'uid': self.depres1uid}).status_code,
                         201)
        # Confirm it's there
        self.assertEqual(requests.get('%s/%s/%s/%s/%s/%s' % (API_BASE_URL,
                                                             self.res1type,
                                                             self.res1uid,
                                                             self.relationship1,
                                                             self.depres1type,
                                                             self.depres1uid)).status_code,
                         200)
        self.assertEqual(requests.get('%s/%s/%s' % (API_BASE_URL,
                                                    self.depres1type,
                                                    self.depres1uid)).status_code,
                         200)
        # Delete the dependent resource
        self.assertEqual(requests.delete('%s/%s/%s/%s/%s/%s' % (API_BASE_URL,
                                                                self.res1type,
                                                                self.res1uid,
                                                                self.relationship1,
                                                                self.depres1type,
                                                                self.depres1uid),
                                         data={'delete-dependent': 'true'}).status_code,
                         204)
        # Delete the parent resource
        self.assertEqual(requests.delete('%s/%s/%s' % (API_BASE_URL,
                                                       self.res1type,
                                                       self.res1uid)).status_code,
                         204)
        print('Test: remove the fixtures')
        requests.delete('%s/resourcetype/%s' % (SCHEMA_BASE_URL, self.res1type))
        requests.delete('%s/resourcetype/%s' % (SCHEMA_BASE_URL, self.depres1type))
        print('Test: schema should be empty')
        self.assertEqual(requests.get('%s/' % (SCHEMA_BASE_URL)).json(), None)
    def test_recursively_delete_the_parent(self):
        print('Test: create the fixtures')
        requests.post('%s/resourcetype/%s' % (SCHEMA_BASE_URL, self.res1type))
        requests.post('%s/resourcetype/%s' % (SCHEMA_BASE_URL, self.depres1type),
                      data={'dependent': 'true'})
        requests.post('%s/relationships/%s/%s/%s' % (SCHEMA_BASE_URL,
                                                    self.res1type,
                                                    self.relationship1,
                                                    self.depres1type),
                      data={'dependent': 'true'})
        # Create the parent resource
        self.assertEqual(requests.post('%s/%s/' % (API_BASE_URL, self.res1type),
                                       data={'uid': self.res1uid}).status_code,
                         201)
        # Create a dependent resource
        self.assertEqual(requests.post('%s/%s/%s/%s/%s' % (API_BASE_URL,
                                                           self.res1type,
                                                           self.res1uid,
                                                           self.relationship1,
                                                           self.depres1type),
                                       data={'uid': self.depres1uid}).status_code,
                         201)
        # Confirm it's there
        self.assertEqual(requests.get('%s/%s/%s/%s/%s/%s' % (API_BASE_URL,
                                                             self.res1type,
                                                             self.res1uid,
                                                             self.relationship1,
                                                             self.depres1type,
                                                             self.depres1uid)).status_code,
                         200)
        self.assertEqual(requests.get('%s/%s/%s' % (API_BASE_URL,
                                                    self.depres1type,
                                                    self.depres1uid)).status_code,
                         200)
        # Delete the parent resource
        self.assertEqual(requests.delete('%s/%s/%s' % (API_BASE_URL,
                                                       self.res1type,
                                                       self.res1uid),
                                         data={'recursive' : 'true'}).status_code,
                         204)
        # Ensure the dependent resource is gone
        self.assertEqual(requests.get('%s/%s/%s' % (API_BASE_URL,
                                                    self.depres1type,
                                                    self.depres1uid)).json(),
                         None)
        print('Test: remove the fixtures')
        requests.delete('%s/resourcetype/%s' % (SCHEMA_BASE_URL, self.res1type))
        requests.delete('%s/resourcetype/%s' % (SCHEMA_BASE_URL, self.depres1type))
        print('Test: schema should be empty')
        self.assertEqual(requests.get('%s/' % (SCHEMA_BASE_URL)).json(), None)

class TestMoveDependentResources(unittest.TestCase):
    p1type = 'routers'
    p1uid = 'woomera'
    p1targetrel = 'Addresses'
    p2type = 'interfaces'
    p2uid = 'eth1'
    p1p2rel = 'Interfaces'
    p2targetrel = 'Addresses'
    targettype = 'ipv4Addresses'
    targetuid = '172.20.0.1'
    def test_move_dependent_resource(self):
        print('Test: test_move_dependent_resource')
        # Create the fixtures
        print('Create the fixtures')
        requests.post('%s/resourcetype/%s' % (SCHEMA_BASE_URL, self.p1type))
        requests.post('%s/resourcetype/%s' % (SCHEMA_BASE_URL, self.p2type),
                      data={'dependent': 'true'})
        requests.post('%s/relationships/%s/%s/%s' % (SCHEMA_BASE_URL,
                                                    self.p1type,
                                                    self.p1p2rel,
                                                    self.p2type),
                      data={'dependent': 'true'})
        requests.post('%s/resourcetype/%s' % (SCHEMA_BASE_URL, self.targettype),
                      data={'dependent': 'true'})
        requests.post('%s/relationships/%s/%s/%s' % (SCHEMA_BASE_URL,
                                                    self.p1type,
                                                    self.p1targetrel,
                                                    self.targettype),
                      data={'dependent': 'true'})
        requests.post('%s/relationships/%s/%s/%s' % (SCHEMA_BASE_URL,
                                                    self.p2type,
                                                    self.p2targetrel,
                                                    self.targettype),
                      data={'dependent': 'true'})
        # Create the initial parent resource
        self.assertEqual(requests.post('%s/%s/' % (API_BASE_URL, self.p1type),
                                       data={'uid': self.p1uid}).status_code,
                         201)
        # Create the second parent resource, as a dependent to the first
        self.assertEqual(requests.post('%s/%s/%s/%s/%s' % (API_BASE_URL,
                                                           self.p1type,
                                                           self.p1uid,
                                                           self.p1p2rel,
                                                           self.p2type),
                                       data={'uid': self.p2uid}).status_code,
                         201)
        # Create the dependent resource
        self.assertEqual(requests.post('%s/%s/%s/%s/%s' % (API_BASE_URL,
                                                           self.p1type,
                                                           self.p1uid,
                                                           self.p1targetrel,
                                                           self.targettype),
                                       data={'uid': self.targetuid}).status_code,
                         201)
        # Move the dependent resource to its new parent
        self.assertEqual(
            requests.post('%s/%s/%s/%s/%s/%s' % (API_BASE_URL,
                                                 self.p1type,
                                                 self.p1uid,
                                                 self.p1targetrel,
                                                 self.targettype,
                                                 self.targetuid),
                          data={'target': '/%s/%s/%s/%s/%s/%s' % (self.p1type,
                                                                  self.p1uid,
                                                                  self.p1p2rel,
                                                                  self.p2type,
                                                                  self.p2uid,
                                                                  self.p2targetrel)}).status_code,
            201)
        # Confirm it's where it should be
        self.assertEqual(requests.get('%s/%s/%s/%s/%s/%s/%s/%s/%s' % (API_BASE_URL,
                                                                      self.p1type,
                                                                      self.p1uid,
                                                                      self.p1p2rel,
                                                                      self.p2type,
                                                                      self.p2uid,
                                                                      self.p2targetrel,
                                                                      self.targettype,
                                                                      self.targetuid)).status_code,
                         200)
        # Confirm it's no longer attached to the initial parent
        self.assertEqual(requests.get('%s/%s/%s/%s/%s/%s' % (API_BASE_URL,
                                                             self.p1type,
                                                             self.p1uid,
                                                             self.p1targetrel,
                                                             self.targettype,
                                                             self.targetuid)).json(),
                         None)
        # Delete the parent resource, which should take the rest with it
        self.assertEqual(requests.delete('%s/%s/%s' % (API_BASE_URL, self.p1type, self.p1uid),
                                         data={'recursive': 'true'}).status_code,
                         204)
        # Remove the fixtures
        requests.delete('%s/resourcetype/%s' % (SCHEMA_BASE_URL, self.p1type))
        requests.delete('%s/resourcetype/%s' % (SCHEMA_BASE_URL, self.p2type))
        requests.delete('%s/resourcetype/%s' % (SCHEMA_BASE_URL, self.targettype))
        print('Test: schema should be empty')
        self.assertEqual(requests.get('%s/' % (SCHEMA_BASE_URL)).json(), None)


class TestValidRelationships(unittest.TestCase):
    '''
    Basic CRD functions for relationships
    '''
    res1type = 'routers'
    res1uid = 'bikini'
    relationship = 'Asn'
    res2type = 'asn'
    res2name = '64512'
    res3type = 'makes'
    res3uid = 'NetBoxes'
    depres1type = 'models'
    depres1uid = 'Packetshuffler3000'
    depres1deprel = 'Produces'
    res1todepres1rel = 'Model'
    def test_basic_relationship(self):
        print('Test: test_basic_relationship')
        print('Test: create the fixtures')
        requests.post('%s/resourcetype/%s' % (SCHEMA_BASE_URL, self.res1type))
        requests.post('%s/resourcetype/%s' % (SCHEMA_BASE_URL, self.res2type))
        requests.post('%s/relationships/%s/%s/%s' % (SCHEMA_BASE_URL,
                                                    self.res1type,
                                                    self.relationship,
                                                    self.res2type))
        # Create two new resources
        self.assertEqual(requests.post('%s/%s/' % (API_BASE_URL, self.res1type),
                                       data={'uid': self.res1uid}).status_code,
                         201)
        self.assertEqual(requests.post('%s/%s/' % (API_BASE_URL, self.res2type),
                                       data={'uid': self.res2name}).status_code,
                         201)
        # Create a valid relationship between them
        self.assertEqual(requests.post('%s/%s/%s/%s'% (API_BASE_URL,
                                                       self.res1type,
                                                       self.res1uid,
                                                       self.relationship),
                                       data={'target': '/%s/%s' % (self.res2type,
                                                                   self.res2name)}).status_code,
                         201)
        # Confirm that the relationship is there
        self.assertEqual(requests.get('%s/%s/%s/%s' % (API_BASE_URL,
                                                       self.res1type,
                                                       self.res1uid,
                                                       self.relationship)).status_code,
                         200)
        self.assertEqual(requests.get('%s/%s/%s/%s' % (API_BASE_URL,
                                                       self.res1type,
                                                       self.res1uid,
                                                       self.relationship)).json(),
                         [{'original_uid': self.res2name,
                           'type': self.res2type,
                           'uid': self.res2name}])
        # Delete the relationship
        self.assertEqual(requests.delete('%s/%s/%s/%s/%s/%s' % (API_BASE_URL,
                                                                self.res1type,
                                                                self.res1uid,
                                                                self.relationship,
                                                                self.res2type,
                                                                self.res2name)).status_code,
                         204)
        # Confirm the relationship is gone
        self.assertEqual(requests.get('%s/%s/%s/%s' % (API_BASE_URL,
                                                       self.res1type,
                                                       self.res1uid,
                                                       self.relationship)).json(),
                         [])
        # Delete the destination resource
        self.assertEqual(requests.delete('%s/%s/%s' % (API_BASE_URL,
                                                       self.res2type,
                                                       self.res2name)).status_code,
                         204)
        # Delete the resources
        self.assertEqual(requests.delete('%s/%s/%s' % (API_BASE_URL,
                                                       self.res1type,
                                                       self.res1uid)).status_code,
                         204)
        print('Test: remove the fixtures')
        requests.delete('%s/resourcetype/%s' % (SCHEMA_BASE_URL, self.res1type))
        requests.delete('%s/resourcetype/%s' % (SCHEMA_BASE_URL, self.res2type))
        print('Test: schema should be empty')
        self.assertEqual(requests.get('%s/' % (SCHEMA_BASE_URL)).json(), None)
    def test_rels_between_primary_and_secondary_resources(self):
        print('Test: test_rels_between_primary_and_secondary_resources')
        print('Test: create the fixtures')
        requests.post('%s/resourcetype/%s' % (SCHEMA_BASE_URL, self.res1type))
        requests.post('%s/resourcetype/%s' % (SCHEMA_BASE_URL, self.res3type))
        requests.post('%s/resourcetype/%s' % (SCHEMA_BASE_URL, self.depres1type),
                      data={'dependent': 'true'})
        requests.post('%s/relationships/%s/%s/%s' % (SCHEMA_BASE_URL,
                                                    self.res1type,
                                                    self.res1todepres1rel,
                                                    self.depres1type),
                      data={'dependent': 'true'})
        requests.post('%s/relationships/%s/%s/%s' % (SCHEMA_BASE_URL,
                                                    self.res3type,
                                                    self.depres1deprel,
                                                    self.depres1type),
                      data={'dependent': 'true'})
        # Create two first-class resources, with a third dependent on the first
        self.assertEqual(requests.post('%s/%s/' % (API_BASE_URL, self.res1type),
                                       data={'uid': self.res1uid}).status_code,
                         201)
        self.assertEqual(requests.post('%s/%s/' % (API_BASE_URL, self.res3type),
                                       data={'uid': self.res3uid}).status_code,
                         201)
        self.assertEqual(requests.post('%s/%s/%s/%s/%s' % (API_BASE_URL,
                                                           self.res3type,
                                                           self.res3uid,
                                                           self.depres1deprel,
                                                           self.depres1type),
                                       data={'uid': self.depres1uid}).status_code,
                         201)
        # Link to the dependent from the other first-class
        # Confirm that the linked first-class resource is at the end of that path
        # Link from the first-class to the dependent
        self.assertEqual(
            requests.post('%s/%s/%s/%s' % (API_BASE_URL,
                                           self.res1type,
                                           self.res1uid,
                                           self.res1todepres1rel),
                          data={'target': '/%s/%s/%s/%s/%s' % (self.res3type,
                                                               self.res3uid,
                                                               self.depres1deprel,
                                                               self.depres1type,
                                                               self.depres1uid)}).status_code,
            201)
        # Confirm that the linked dependent resource is at the end of that path
        self.assertEqual(requests.get('%s/%s/%s/%s/%s/%s' % (API_BASE_URL,
                                                             self.res1type,
                                                             self.res1uid,
                                                             self.res1todepres1rel,
                                                             self.depres1type,
                                                             self.depres1uid)).status_code,
                         200)
        # What do we get when we just ask for what's at the end of that relationship?
        self.assertEqual(requests.get('%s/%s/%s/%s' % (API_BASE_URL,
                                                       self.res1type,
                                                       self.res1uid,
                                                       self.res1todepres1rel)).status_code,
                         200)
        # Delete both first-class resources
        self.assertEqual(requests.delete('%s/%s/%s' % (API_BASE_URL,
                                                       self.res1type,
                                                       self.res1uid),
                                         data={'recursive': 'true'}).status_code,
                         204)
        self.assertEqual(requests.delete('%s/%s/%s' % (API_BASE_URL,
                                                       self.res3type,
                                                       self.res3uid),
                                         data={'recursive': 'true'}).status_code,
                         204)
        print('Test: remove the fixtures')
        requests.delete('%s/resourcetype/%s' % (SCHEMA_BASE_URL, self.res1type))
        requests.delete('%s/resourcetype/%s' % (SCHEMA_BASE_URL, self.res3type))
        requests.delete('%s/resourcetype/%s' % (SCHEMA_BASE_URL, self.depres1type))
        print('Test: schema should be empty')
        self.assertEqual(requests.get('%s/' % (SCHEMA_BASE_URL)).json(), None)

class TestInvalidRelationships(unittest.TestCase):
    '''
    Basic CRD functions for relationships
    '''
    res1type = 'routers'
    res1uid = 'bikini'
    res2type = 'asn'
    res2uid = '64512'
    relationship_valid = 'Asn'
    relationship_invalid = 'dysfunctionalRelationship'
    def test_basic_relationship(self):
        print('Test: test_basic_relationship')
        print('Test: create the fixtures')
        requests.post('%s/resourcetype/%s' % (SCHEMA_BASE_URL, self.res1type))
        requests.post('%s/resourcetype/%s' % (SCHEMA_BASE_URL, self.res2type))
        requests.post('%s/relationships/%s/%s/%s' % (SCHEMA_BASE_URL,
                                                    self.res1type,
                                                    self.relationship_valid,
                                                    self.res2type))
        # Create two new resources
        print('Test: create the resources to link')
        self.assertEqual(requests.post('%s/%s' % (API_BASE_URL, self.res1type),
                                       data={'uid': self.res1uid}).status_code,
                         201)
        self.assertEqual(requests.post('%s/%s' % (API_BASE_URL, self.res2type),
                                       data={'uid': self.res2uid}).status_code,
                         201)
        # Attempt to create a relationship between them,
        # of a type that doesn't exist for the source resource-type
        print('Test: create invalid relationship')
        self.assertEqual(requests.post('%s/%s/%s/%s'% (API_BASE_URL,
                                                       self.res1type,
                                                       self.res1uid,
                                                       self.relationship_invalid),
                                       data={'target': '/%s/%s' % (self.res2type,
                                                                   self.res2uid)}).status_code,
                         409)
        # Attempt to create a valid relationship between them
        print('Test: create valid relationship')
        self.assertEqual(requests.post('%s/%s/%s/%s'% (API_BASE_URL,
                                                       self.res1type,
                                                       self.res1uid,
                                                       self.relationship_valid),
                                       data={'target': '/%s/%s' % (self.res2type,
                                                                   self.res2uid)}).status_code,
                         201)
        # Delete the resources
        print('Test: delete the resources')
        self.assertEqual(requests.delete('%s/%s/%s' % (API_BASE_URL,
                                                       self.res1type,
                                                       self.res1uid)).status_code,
                         204)
        self.assertEqual(requests.delete('%s/%s/%s' % (API_BASE_URL,
                                                       self.res2type,
                                                       self.res2uid)).status_code,
                         204)
        print('Test: remove the fixtures')
        requests.delete('%s/resourcetype/%s' % (SCHEMA_BASE_URL, self.res1type))
        requests.delete('%s/resourcetype/%s' % (SCHEMA_BASE_URL, self.res2type))
        print('Test: schema should be empty')
        self.assertEqual(requests.get('%s/' % (SCHEMA_BASE_URL)).json(), None)

class TestDbSchema(unittest.TestCase):
    '''
    Check that the DB schema is being enforced.
    Principally, make sure we can't create duplicates.
    '''
    resourcetype = 'routers'
    resourcename = 'whitesands'
    def test_unique_resources(self):
        print('Test: test_unique_resources')
        print('Test: create the fixtures')
        requests.post('%s/resourcetype/%s' % (SCHEMA_BASE_URL, self.resourcetype))
        # Create a new resource
        self.assertEqual(requests.post('%s/%s/' % (API_BASE_URL, self.resourcetype),
                                       data={'uid': self.resourcename}).status_code,
                         201)
        # Confirm that it's now there
        self.assertEqual(requests.get('%s/%s/%s/' % (API_BASE_URL,
                                                     self.resourcetype,
                                                     self.resourcename)).json(),
                         {'original_uid': sanitise_uid(self.resourcename),
                          'uid': self.resourcename})
        # Attempt to create a duplicate.
        self.assertEqual(requests.post('%s/%s/' % (API_BASE_URL, self.resourcetype),
                                       data={'uid': self.resourcename}).status_code,
                         200)
        # Delete the resource
        self.assertEqual(requests.delete('%s/%s/%s' % (API_BASE_URL,
                                                       self.resourcetype,
                                                       self.resourcename)).status_code,
                         204)
        print('Test: remove the fixtures')
        requests.delete('%s/resourcetype/%s' % (SCHEMA_BASE_URL, self.resourcetype))
        print('Test: schema should be empty')
        self.assertEqual(requests.get('%s/' % (SCHEMA_BASE_URL)).json(), None)

class TestBasicResourceErrors(unittest.TestCase):
    '''
    Confirm what happens when we make basic errors in resource-creation requests.
    '''
    invalid_resourcetype = 'IjustMadeThisUp'
    valid_resourcetype = 'routers'
    valid_uid = 'amchitka'
    def test_basic_resource_errors(self):
        print('Test: test_basic_resource_errors')
        print('Test: create the fixtures')
        requests.post('%s/resourcetype/%s' % (SCHEMA_BASE_URL, self.valid_resourcetype))
        # Invalid resource-type
        self.assertEqual(requests.post('%s/%s' % (API_BASE_URL, self.invalid_resourcetype),
                                       data={'foo': 'bar'}).status_code,
                         400)
        # Missing UID
        self.assertEqual(requests.post('%s/%s' % (API_BASE_URL, self.valid_resourcetype),
                                       data={'foo': 'bar'}).status_code,
                         400)
        # Invalid non-UID parameters
        self.assertEqual(requests.post('%s/%s' % (API_BASE_URL, self.valid_resourcetype),
                                       data={'uid': self.valid_uid, 'foo': 'bar'}).status_code,
                         400)
        print('Test: remove the fixtures')
        requests.delete('%s/resourcetype/%s' % (SCHEMA_BASE_URL, self.valid_resourcetype))
        print('Test: schema should be empty')
        self.assertEqual(requests.get('%s/' % (SCHEMA_BASE_URL)).json(), None)

class TestAnyType(unittest.TestCase):
    '''
    Confirm handling of the 'any' type
    '''
    p1type = 'routers'
    p1uid = 'Enewetak'
    p1rel = 'Tags'
    invalidrel = 'invalid'
    t1type = 'tags'
    t1uid = 'Tagged'
    def test_rejection(self):
        print('Test: test_any_type')
        self.assertEqual(requests.get('%s/any/foo' % API_BASE_URL).status_code,
                         200)
        self.assertEqual(requests.get('%s/any/foo' % API_BASE_URL).json(),
                         None)
    def test_create_valid_any_rel(self):
        print('Test: test_create_valid_any_rel')
        print('Test: create the fixtures')
        requests.post('%s/resourcetype/%s' % (SCHEMA_BASE_URL, self.p1type))
        requests.post('%s/resourcetype/%s' % (SCHEMA_BASE_URL, self.t1type))
        requests.post('%s/resourcetype/any' % (SCHEMA_BASE_URL))
        requests.post('%s/relationships/any/%s/%s' % (SCHEMA_BASE_URL, self.p1rel, self.t1type))
        # Create the resources
        self.assertEqual(requests.post('%s/%s' % (API_BASE_URL, self.p1type),
                                       data={'uid': self.p1uid}).status_code,
                         201)
        self.assertEqual(requests.post('%s/%s' % (API_BASE_URL, self.t1type),
                                       data={'uid': self.t1uid}).status_code,
                         201)
        # Create the relationship between them
        self.assertEqual(requests.post('%s/%s/%s/%s' % (API_BASE_URL,
                                                        self.p1type,
                                                        self.p1uid,
                                                        self.p1rel),
                                       data={'target': '/%s/%s' % (self.t1type,
                                                                   self.t1uid)}).status_code,
                         201)
        # Delete the relationship
        self.assertEqual(requests.delete('%s/%s/%s/%s/%s/%s' % (API_BASE_URL,
                                                                self.p1type,
                                                                self.p1uid,
                                                                self.p1rel,
                                                                self.t1type,
                                                                self.t1uid)).status_code,
                         204)
        # Delete the resources
        self.assertEqual(requests.delete('%s/%s/%s' % (API_BASE_URL,
                                                       self.p1type,
                                                       self.p1uid)).status_code,
                         204)
        self.assertEqual(requests.delete('%s/%s/%s' % (API_BASE_URL,
                                                       self.t1type,
                                                       self.t1uid)).status_code,
                         204)
        print('Test: remove the fixtures')
        requests.delete('%s/resourcetype/%s' % (SCHEMA_BASE_URL, self.p1type))
        requests.delete('%s/resourcetype/%s' % (SCHEMA_BASE_URL, self.t1type))
        requests.delete('%s/resourcetype/any' % (SCHEMA_BASE_URL))
        print('Test: schema should be empty')
        self.assertEqual(requests.get('%s/' % (SCHEMA_BASE_URL)).json(), None)
    def test_create_invalid_any_rel(self):
        print('Test: test_create_valid_any_rel')
        print('Test: create the fixtures')
        requests.post('%s/resourcetype/%s' % (SCHEMA_BASE_URL, self.p1type))
        requests.post('%s/resourcetype/%s' % (SCHEMA_BASE_URL, self.t1type))
        requests.post('%s/resourcetype/any' % (SCHEMA_BASE_URL))
        requests.post('%s/relationships/any/%s/%s' % (SCHEMA_BASE_URL, self.p1rel, self.t1type))
        # Create the resources
        self.assertEqual(requests.post('%s/%s' % (API_BASE_URL, self.p1type),
                                       data={'uid': self.p1uid}).status_code,
                         201)
        self.assertEqual(requests.post('%s/%s' % (API_BASE_URL, self.t1type),
                                       data={'uid': self.t1uid}).status_code,
                         201)
        # Attempt to create the relationship between them.
        # This should fail, because we're trying to create a relationship
        # that hasn't been defined.
        self.assertEqual(requests.post('%s/%s/%s/%s' % (API_BASE_URL,
                                                        self.p1type,
                                                        self.p1uid,
                                                        self.invalidrel),
                                       data={'target': '/%s/%s' % (self.t1type,
                                                                   self.t1uid)}).status_code,
                         409)
        # Delete the resources
        self.assertEqual(requests.delete('%s/%s/%s' % (API_BASE_URL,
                                                       self.p1type,
                                                       self.p1uid)).status_code,
                         204)
        self.assertEqual(requests.delete('%s/%s/%s' % (API_BASE_URL,
                                                       self.t1type,
                                                       self.t1uid)).status_code,
                         204)
        print('Test: remove the fixtures')
        requests.delete('%s/resourcetype/%s' % (SCHEMA_BASE_URL, self.p1type))
        requests.delete('%s/resourcetype/%s' % (SCHEMA_BASE_URL, self.t1type))
        requests.delete('%s/resourcetype/any' % (SCHEMA_BASE_URL))
        print('Test: schema should be empty')
        self.assertEqual(requests.get('%s/' % (SCHEMA_BASE_URL)).json(), None)

# Make it happen
if __name__ == '__main__':
    unittest.main()

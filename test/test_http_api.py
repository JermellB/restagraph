#!/usr/bin/env python
# Expects python3

'''
Test package for Restagraph's REST API.
'''


#   Copyright 2017-2020 James Fleming <james@electronic-quill.net>
#
#   Licensed under the GNU General Public License
#   - for details, see LICENSE.txt in the top-level directory

# pylint: disable=missing-class-docstring
# pylint: disable=missing-function-docstring

# Included batteries
import filecmp
import os
import re
import unittest

# Third-party libraries
import pytest
import requests


# Config variables
PROTOCOL = 'http'
SERVER_URL = 'localhost:4950'
API_PREFIX = '/raw/v1'
FILES_PREFIX = '/files/v1'

API_BASE_URL = '%s://%s%s' % (PROTOCOL, SERVER_URL, API_PREFIX)
FILES_BASE_URL = '%s://%s%s' % (PROTOCOL, SERVER_URL, FILES_PREFIX)


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
    restype = 'People'
    resuid = 'Sam Spade'
    result = None

    @pytest.mark.dependency()
    def test_create_and_delete_single_resource(self):
        print('Test: test_create_and_delete_single_resource')
        # Ensure it's not already present
        assert requests.get('%s/%s/%s' % (API_BASE_URL,
                                                    self.restype,
                                                    self.resuid)).status_code == 200
        assert requests.get('%s/%s/%s' % (API_BASE_URL,
                                                    self.restype,
                                                    self.resuid)).json() == []
        assert requests.get('%s/%s/%s' % (API_BASE_URL,
                                                    self.restype,
                                                    self.resuid)).json() == []
        # Ensure we have none of that kind of resource
        assert requests.get('%s/%s' % (API_BASE_URL,
                                                 self.restype)).status_code == 200
        assert requests.get('%s/%s' % (API_BASE_URL,
                                                 self.restype)).json() == []
        # Create it
        self.result = requests.post('%s/%s/' % (API_BASE_URL, self.restype),
                                    data={'uid': self.resuid})
        assert self.result.status_code == 201
        assert self.result.text == '/{rtype}/{uid}'.format(rtype=self.restype,
                                                                   uid=sanitise_uid(self.resuid))
        # Confirm that it's now there
        self.result = requests.get('%s/%s/%s' % (API_BASE_URL,
                                                 self.restype,
                                                 sanitise_uid(self.resuid))).json()
        print('Found resource: {}'.format(self.result))
        assert self.result['original_uid'] == self.resuid
        assert self.result['uid'] == sanitise_uid(self.resuid)
        # Delete it
        assert requests.delete('%s/%s/%s' % (
            API_BASE_URL, self.restype, sanitise_uid(self.resuid))).status_code == 204
        # Confirm it's gone
        assert requests.get('%s/%s/%s' % (
            API_BASE_URL, self.restype, sanitise_uid(self.resuid))).status_code == 200
        assert requests.get('%s/%s/%s' % (
            API_BASE_URL, self.restype, sanitise_uid(self.resuid))).json() == []

@pytest.mark.dependency(depends=["TestResources::test_create_and_delete_single_resource"])
class TestDuplicateResistance(unittest.TestCase):
    '''
    Check that we can't create duplicate primary resources.
    '''
    resourcetype = 'People'
    resourcename = 'Dante'
    result = None
    def test_unique_resources(self):
        print('Test: test_unique_resources')
        # Create a new resource
        self.assertEqual(requests.post('%s/%s/' % (API_BASE_URL, self.resourcetype),
                                       data={'uid': self.resourcename}).status_code,
                         201)
        # Confirm that it's now there
        self.result = requests.get('%s/%s/%s/' % (API_BASE_URL,
                                                  self.resourcetype,
                                                  self.resourcename)).json()
        print('Received result {}'.format(self.result))
        self.assertEqual(self.result['original_uid'], sanitise_uid(self.resourcename))
        self.assertEqual(self.result['uid'], self.resourcename)
        # Attempt to create a duplicate.
        self.assertEqual(requests.post('%s/%s/' % (API_BASE_URL, self.resourcetype),
                                       data={'uid': self.resourcename}).status_code,
                         304)
        # Delete the resource
        self.assertEqual(requests.delete('%s/%s/%s' % (API_BASE_URL,
                                                       self.resourcetype,
                                                       self.resourcename)).status_code,
                         204)

@pytest.mark.dependency(depends=["TestDuplicateResistance::test_unique_resources"])
class TestMultipleResources(unittest.TestCase):
    '''
    Retrieve details of all resources of a given type.
    '''
    resourcetype = 'People'
    resource1uid = 'Dude'
    resource2uid = 'Fifi'
    resource3uid = 'Trixibelle'
    result = None
    def test_create_and_retrieve_multiple_resources(self):
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
        self.result = requests.get('%s/%s' % (API_BASE_URL, self.resourcetype)).json()
        self.assertEqual(self.result[0]['original_uid'], sanitise_uid(self.resource1uid))
        self.assertEqual(self.result[0]['uid'], self.resource1uid)
        # Add the second resource
        self.assertEqual(requests.post('%s/%s/' % (API_BASE_URL, self.resourcetype),
                                       data={'uid': self.resource2uid}).status_code,
                         201)
        # Check that we now get a list containing exactly both resources
        self.result = sorted(requests.get('%s/%s' % (API_BASE_URL, self.resourcetype)).json(),
                             key=lambda i: i['uid'])
        self.assertEqual(self.result[0]['original_uid'], sanitise_uid(self.resource1uid))
        self.assertEqual(self.result[0]['uid'], self.resource1uid)
        self.assertEqual(self.result[1]['original_uid'], sanitise_uid(self.resource2uid))
        self.assertEqual(self.result[1]['uid'], self.resource2uid)
        # Add the third resource
        self.assertEqual(requests.post('%s/%s/' % (API_BASE_URL, self.resourcetype),
                                       data={'uid': self.resource3uid}).status_code,
                         201)
        # Check that we now get a list containing all three resources
        self.result = sorted(requests.get('%s/%s' % (API_BASE_URL, self.resourcetype)).json(),
                             key=lambda i: i['uid'])
        self.assertEqual(self.result[0]['original_uid'], sanitise_uid(self.resource1uid))
        self.assertEqual(self.result[0]['uid'], self.resource1uid)
        self.assertEqual(self.result[1]['original_uid'], sanitise_uid(self.resource2uid))
        self.assertEqual(self.result[1]['uid'], self.resource2uid)
        self.assertEqual(self.result[2]['original_uid'], sanitise_uid(self.resource3uid))
        self.assertEqual(self.result[2]['uid'], self.resource3uid)
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

@pytest.mark.dependency(depends=["TestDuplicateResistance::test_unique_resources"])
class TestBasicResourceErrors(unittest.TestCase):
    '''
    Confirm what happens when we make basic errors in resource-creation requests.
    '''
    invalid_resourcetype = 'IjustMadeThisUp'
    valid_resourcetype = 'routers'
    valid_uid = 'amchitka'
    def test_basic_resource_errors(self):
        print('Test: test_basic_resource_errors')
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
                         409)

@pytest.mark.dependency(depends=[
    "TestDuplicateResistance::test_unique_resources",
    "TestBasicResourceErrors::test_basic_resource_errors"])
class TestFilesApi(unittest.TestCase):
    '''
    Upload, download, metadata and deletion of files.
    '''
    file1source = 'cats_cuddling.jpg'
    file1name = 'Cuddling cats'
    file1sha3_256sum = '305664EB53010D52642594A3B3877A1BB811EAD9B610C5C1210F7F3B88B4C184'
    file2source = 'cats_cuddling.jpg'
    file2name = 'Cute kitties'
    def test_files_api_basic(self):
        print('Test: file upload')
        fhandle = open(self.file1source, 'rb')
        self.assertEqual(
            requests.post('%s/files/' % (FILES_BASE_URL),
                          data={'name': self.file1name},
                          files={'file': fhandle}).status_code,
            201)
        fhandle.close()
        print('Test: file metadata')
        result = requests.get('%s/Files/%s' % (API_BASE_URL, sanitise_uid(self.file1name)))
        self.assertEqual(result.status_code, 200)
        self.assertEqual(result.json()['title'], self.file1name)
        self.assertEqual(result.json()['sha3256sum'], self.file1sha3_256sum)
        print('Test: file content')
        result = requests.get('%s/%s' % (FILES_BASE_URL, sanitise_uid(self.file1name)))
        # Write it to a file
        fhandle = open('testfile', 'wb')
        fhandle.write(result.content)
        fhandle.close()
        # Confirm that the file we got back is the same as the one we uploaded
        self.assertTrue(filecmp.cmp(self.file1source, 'testfile'))
        # Clean up
        os.remove('testfile')
        print('Test: file deletion')
        self.assertEqual(requests.delete('%s/%s' % (FILES_BASE_URL,
                                                    sanitise_uid(self.file1name))).status_code,
                         204)
        result = requests.get('%s/files/%s' % (API_BASE_URL, sanitise_uid(self.file1name)))
        self.assertEqual(result.status_code, 200)
        self.assertEqual(result.json(), [])
    def test_files_conditional_deletion(self):
        print("Test: only delete files from the filesystem if they're fully dereferenced.")
        # Upload the first file
        fhandle = open(self.file1source, 'rb')
        self.assertEqual(
            requests.post('%s/files/' % (FILES_BASE_URL),
                          data={'name': self.file1name},
                          files={'file': fhandle}).status_code,
            201)
        fhandle.close()
        # Upload the second file
        fhandle = open(self.file2source, 'rb')
        self.assertEqual(
            requests.post('%s/files/' % (FILES_BASE_URL),
                          data={'name': self.file2name},
                          files={'file': fhandle}).status_code,
            201)
        fhandle.close()
        # Delete the first file
        self.assertEqual(requests.delete('%s/%s' % (FILES_BASE_URL,
                                                    sanitise_uid(self.file1name))).status_code,
                         204)
        # Confirm that the second file can still be downloaded
        result = requests.get('%s/%s' % (FILES_BASE_URL, sanitise_uid(self.file2name)))
        # Write it to a file
        fhandle = open('testfile', 'wb')
        fhandle.write(result.content)
        fhandle.close()
        # Confirm that the file we got back is the same as the one we uploaded
        self.assertTrue(filecmp.cmp(self.file2source, 'testfile'))
        # Clean up
        os.remove('testfile')
        # Delete the second file
        self.assertEqual(requests.delete('%s/%s' % (FILES_BASE_URL,
                                                    sanitise_uid(self.file2name))).status_code,
                         204)
        # Confirm that it's no longer there
        result = requests.get('%s/Files/%s' % (API_BASE_URL, sanitise_uid(self.file2name)))
        self.assertEqual(result.status_code, 200)
        self.assertEqual(result.json(), [])


#
# Untested tests follow
#

@pytest.mark.skip()
class TestResourceAttributesEnums(unittest.TestCase):
    '''
    Test enumerated resource-attributes.
    '''
    resourcetype = 'places'
    resourceuid = 'Midian'
    attr1name = 'kind'
    attr1val = 'Fictional'
    attr1valbad = 'Real'
    result = None
    def test_create_and_use_enums(self):
        print('Test: create a resource with a valid enum attribute')
        self.assertEqual(requests.post('%s/%s' % (API_BASE_URL, self.resourcetype),
                                       data={'uid': self.resourceuid,
                                             self.attr1name: self.attr1val}).status_code,
                         201)
        requests.delete('%s/%s/%s' % (API_BASE_URL, self.resourcetype, self.resourceuid))
        print('Test: fail to create a resource with an invalid enum attribute')
        self.assertEqual(requests.post('%s/%s' % (API_BASE_URL, self.resourcetype),
                                       data={'uid': self.resourceuid,
                                             self.attr1name: self.attr1valbad}).status_code,
                         400)
        print('Test: fail to add an invalid enum attribute to an existing resource')
        self.assertEqual(requests.post('%s/%s' % (API_BASE_URL, self.resourcetype),
                                       data={'uid': self.resourceuid}).status_code,
                         201)
        self.assertEqual(requests.put('%s/%s/%s' % (API_BASE_URL,
                                                    self.resourcetype,
                                                    self.attr1name),
                                      data={'uid': self.resourceuid,
                                            self.attr1name: self.attr1valbad}).status_code,
                         400)
        print('Test: add a valid attribute to an existing resource.')
        self.assertEqual(requests.put('%s/%s/%s' % (API_BASE_URL,
                                                    self.resourcetype,
                                                    self.attr1name),
                                      data={'uid': self.resourceuid,
                                            self.attr1name: self.attr1val}).status_code,
                         204)
        requests.delete('%s/%s/%s' % (API_BASE_URL, self.resourcetype, self.resourceuid))

@pytest.mark.skip()
class TestDependentResources(unittest.TestCase):
    res1type = 'brands'
    res1uid = 'Acme'
    relationship1 = 'Produces'
    depres1type = 'models'
    depres1uid = 'Exploding rocket-arrow'
    result = None
    def test_create_and_delete_a_single_dependent_resource(self):
        print('Test: test_create_and_delete_a_single_dependent_resource')
        # Create the parent resource
        print('Test: create the parent resource')
        self.assertEqual(requests.post('%s/%s/' % (API_BASE_URL, self.res1type),
                                       data={'uid': self.res1uid}).status_code,
                         201)
        # Screw up creation of the dependent resource, to check error-handling
        # Invert the parent/child relationship, and break the API to boot
        print('Test: fail to create the dependent resource')
        self.assertEqual(requests.post('%s/%s/%s/%s' % (API_BASE_URL,
                                                        self.depres1type,
                                                        self.depres1uid,
                                                        self.relationship1),
                                       data={'type': self.depres1type,
                                             'uid': self.depres1uid}).status_code,
                         400)
        # Now get it right
        print('Test: successfully create the dependent resource')
        self.result = requests.post('%s/%s/%s/%s/%s' % (API_BASE_URL,
                                                        self.res1type,
                                                        sanitise_uid(self.res1uid),
                                                        self.relationship1,
                                                        self.depres1type),
                                    data={'uid': self.depres1uid})
        self.assertEqual(self.result.status_code, 201)
        self.assertEqual(self.result.text, '/{r1t}/{r1u}/{rel}/{drt}/{dru}'.format(
            r1t=self.res1type,
            r1u=sanitise_uid(self.res1uid),
            rel=self.relationship1,
            drt=self.depres1type,
            dru=sanitise_uid(self.depres1uid)))
        # Confirm it's there
        print('Test: confirm the dependent resource is there')
        self.assertEqual(requests.get('%s/%s/%s/%s/%s/%s' %
                                      (API_BASE_URL,
                                       self.res1type,
                                       sanitise_uid(self.res1uid),
                                       self.relationship1,
                                       self.depres1type,
                                       sanitise_uid(self.depres1uid))).status_code,
                         200)
        self.assertEqual(requests.get('%s/%s/%s' % (API_BASE_URL,
                                                    self.depres1type,
                                                    sanitise_uid(self.depres1uid))).status_code,
                         200)
        # Try to create a duplicate. We should get 200/<URI>
        print('Test: fail to create a duplicate')
        self.result = requests.post('%s/%s/%s/%s/%s' % (API_BASE_URL,
                                                        self.res1type,
                                                        sanitise_uid(self.res1uid),
                                                        self.relationship1,
                                                        self.depres1type),
                                    data={'uid': self.depres1uid})
        self.assertEqual(self.result.status_code, 200)
        self.assertEqual(self.result.text, '/{r1t}/{r1u}/{rel}/{drt}/{dru}'.format(
            r1t=self.res1type,
            r1u=sanitise_uid(self.res1uid),
            rel=self.relationship1,
            drt=self.depres1type,
            dru=sanitise_uid(self.depres1uid)))
        # Delete the dependent resource
        print('Test: delete the dependent resource')
        self.assertEqual(requests.delete('%s/%s/%s/%s/%s/%s' % (API_BASE_URL,
                                                                self.res1type,
                                                                sanitise_uid(self.res1uid),
                                                                self.relationship1,
                                                                self.depres1type,
                                                                sanitise_uid(self.depres1uid)),
                                         data={'delete-dependent': 'true'}).status_code,
                         204)
        # Delete the parent resource
        print('Test: delete the parent resource')
        self.assertEqual(requests.delete('%s/%s/%s' % (API_BASE_URL,
                                                       self.res1type,
                                                       sanitise_uid(self.res1uid))).status_code,
                         204)
    def test_recursively_delete_the_parent(self):
        # Create the parent resource
        self.assertEqual(requests.post('%s/%s/' % (API_BASE_URL, self.res1type),
                                       data={'uid': self.res1uid}).status_code,
                         201)
        # Create a dependent resource
        self.assertEqual(requests.post('%s/%s/%s/%s/%s' % (API_BASE_URL,
                                                           self.res1type,
                                                           sanitise_uid(self.res1uid),
                                                           self.relationship1,
                                                           self.depres1type),
                                       data={'uid': self.depres1uid}).status_code,
                         201)
        # Confirm it's there
        self.assertEqual(requests.get('%s/%s/%s/%s/%s/%s' %
                                      (API_BASE_URL,
                                       self.res1type,
                                       sanitise_uid(self.res1uid),
                                       self.relationship1,
                                       self.depres1type,
                                       sanitise_uid(self.depres1uid))).status_code,
                         200)
        self.assertEqual(requests.get('%s/%s/%s' % (API_BASE_URL,
                                                    self.depres1type,
                                                    sanitise_uid(self.depres1uid))).status_code,
                         200)
        # Delete the parent resource
        self.assertEqual(requests.delete('%s/%s/%s' % (API_BASE_URL,
                                                       self.res1type,
                                                       sanitise_uid(self.res1uid)),
                                         data={'recursive' : 'true'}).status_code,
                         204)
        # Ensure the dependent resource is gone
        self.assertEqual(requests.get('%s/%s/%s' % (API_BASE_URL,
                                                    self.depres1type,
                                                    sanitise_uid(self.depres1uid))).json(),
                         [])

@pytest.mark.skip()
class TestMoveDependentResources(unittest.TestCase):
    p1type = 'countries'
    p1uid = 'Netherlands'
    p1targetrel = 'Cities'
    p2type = 'states'
    p2uid = 'Noord Holland'
    p1p2rel = 'States'
    p2targetrel = 'Cities'
    targettype = 'cities'
    targetuid = 'Amsterdam'
    result = None
    def test_move_dependent_resource(self):
        print('Test: test_move_dependent_resource')
        # Create the initial parent resource
        self.assertEqual(requests.post('%s/%s/' % (API_BASE_URL, self.p1type),
                                       data={'uid': self.p1uid}).status_code,
                         201)
        # Create the second parent resource, as a dependent to the first
        self.assertEqual(requests.post('%s/%s/%s/%s/%s' % (API_BASE_URL,
                                                           self.p1type,
                                                           sanitise_uid(self.p1uid),
                                                           self.p1p2rel,
                                                           self.p2type),
                                       data={'uid': self.p2uid}).status_code,
                         201)
        # Create the dependent resource
        self.assertEqual(requests.post('%s/%s/%s/%s/%s' % (API_BASE_URL,
                                                           self.p1type,
                                                           sanitise_uid(self.p1uid),
                                                           self.p1targetrel,
                                                           self.targettype),
                                       data={'uid': self.targetuid}).status_code,
                         201)
        # Move the dependent resource to its new parent
        self.result = requests.post('%s/%s/%s/%s/%s/%s' % (API_BASE_URL,
                                                           self.p1type,
                                                           sanitise_uid(self.p1uid),
                                                           self.p1targetrel,
                                                           self.targettype,
                                                           sanitise_uid(self.targetuid)),
                                    data={'target': '/%s/%s/%s/%s/%s/%s' %
                                          (self.p1type,
                                           sanitise_uid(self.p1uid),
                                           self.p1p2rel,
                                           self.p2type,
                                           sanitise_uid(self.p2uid),
                                           self.p2targetrel)})
        self.assertEqual(self.result.status_code, 201)
        self.assertEqual(self.result.text,
                         '/%s/%s/%s/%s/%s/%s/%s/%s' % (self.p1type,
                                                       sanitise_uid(self.p1uid),
                                                       self.p1p2rel,
                                                       self.p2type,
                                                       sanitise_uid(self.p2uid),
                                                       self.p2targetrel,
                                                       self.targettype,
                                                       sanitise_uid(self.targetuid)))
        # Confirm it's where it should be
        self.assertEqual(requests.get('%s/%s/%s/%s/%s/%s/%s/%s/%s' %
                                      (API_BASE_URL,
                                       self.p1type,
                                       sanitise_uid(self.p1uid),
                                       self.p1p2rel,
                                       self.p2type,
                                       sanitise_uid(self.p2uid),
                                       self.p2targetrel,
                                       self.targettype,
                                       sanitise_uid(self.targetuid))).status_code,
                         200)
        # Confirm it's no longer attached to the initial parent
        self.assertEqual(requests.get('%s/%s/%s/%s/%s/%s' % (API_BASE_URL,
                                                             self.p1type,
                                                             sanitise_uid(self.p1uid),
                                                             self.p1targetrel,
                                                             self.targettype,
                                                             sanitise_uid(self.targetuid))).json(),
                         [])
        # Delete the parent resource, which should take the rest with it
        self.assertEqual(requests.delete('%s/%s/%s' % (API_BASE_URL,
                                                       self.p1type,
                                                       sanitise_uid(self.p1uid)),
                                         data={'recursive': 'true'}).status_code,
                         204)


@pytest.mark.skip()
class TestValidRelationships(unittest.TestCase):
    '''
    Basic CRD functions for relationships
    '''
    res1type = 'routers'
    res1uid = 'bikini'
    relationship = 'Asn'
    res2type = 'asn'
    res2name = '64512'
    res3type = 'brands'
    res3uid = 'NetBoxes'
    depres1type = 'models'
    depres1uid = 'Packetshuffler3000'
    depres1deprel = 'Produces'
    res1todepres1rel = 'Model'
    result = None
    def test_basic_relationship(self):
        print('Test: test_basic_relationship')
        # Create two new resources
        self.assertEqual(requests.post('%s/%s/' % (API_BASE_URL, self.res1type),
                                       data={'uid': self.res1uid}).status_code,
                         201)
        self.assertEqual(requests.post('%s/%s/' % (API_BASE_URL, self.res2type),
                                       data={'uid': self.res2name}).status_code,
                         201)
        # Create a valid relationship between them
        self.result = requests.post('%s/%s/%s/%s'% (API_BASE_URL,
                                                    self.res1type,
                                                    self.res1uid,
                                                    self.relationship),
                                    data={'target': '/%s/%s' % (self.res2type,
                                                                self.res2name)})
        self.assertEqual(self.result.text, '/%s/%s/%s/%s/%s'% (self.res1type,
                                                               self.res1uid,
                                                               self.relationship,
                                                               self.res2type,
                                                               self.res2name))
        self.assertEqual(self.result.status_code, 201)
        # Confirm that the relationship is there
        self.assertEqual(requests.get('%s/%s/%s/%s' % (API_BASE_URL,
                                                       self.res1type,
                                                       self.res1uid,
                                                       self.relationship)).status_code,
                         200)
        self.result = requests.get('%s/%s/%s/%s' % (API_BASE_URL,
                                                    self.res1type,
                                                    self.res1uid,
                                                    self.relationship)).json()
        self.assertEqual(self.result[0]['original_uid'], self.res2name)
        self.assertEqual(self.result[0]['uid'], self.res2name)
        self.assertEqual(self.result[0]['type'], self.res2type)
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
    def test_rels_between_primary_and_secondary_resources(self):
        print('Test: test_rels_between_primary_and_secondary_resources')
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


@pytest.mark.skip()
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

@pytest.mark.skip()
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
    def test_01_rejection(self):
        print('Test: test_any_type')
        self.assertEqual(requests.get('%s/any/foo' % API_BASE_URL).status_code,
                         200)
        self.assertEqual(requests.get('%s/any/foo' % API_BASE_URL).json(),
                         [])
    def test_02_create_valid_any_rel(self):
        print('Test: test_create_valid_any_rel')
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
    def test_03_create_invalid_any_rel(self):
        print('Test: test_create_valid_any_rel')
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

# Make it happen
if __name__ == '__main__':
    unittest.main()

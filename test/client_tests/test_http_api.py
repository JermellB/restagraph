#!/usr/bin/env python
# Expects python3

'''
Test package for Restagraph's REST API.
'''


#   Copyright 2017-2021 James Fleming <james@electronic-quill.net>
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
SCHEMA_PREFIX = '/schema/v1'
IPAM_PREFIX = '/ipam/v1'

API_BASE_URL = '%s://%s%s' % (PROTOCOL, SERVER_URL, API_PREFIX)
FILES_BASE_URL = '%s://%s%s' % (PROTOCOL, SERVER_URL, FILES_PREFIX)
SCHEMA_BASE_URL = '%s://%s%s' % (PROTOCOL, SERVER_URL, SCHEMA_PREFIX)
IPAM_BASE_URL = '%s://%s%s' % (PROTOCOL, SERVER_URL, IPAM_PREFIX)


# Utilities
def sanitise_uid(uid):
    '''
    Sanitise a UID string in the same way Restagraph does
    '''
    return re.sub('[/ ]', '_', uid)


# Tests

@pytest.mark.dependency()
class TestResources(unittest.TestCase):
    '''
    Basic CRD functions for resources
    '''
    restype = 'People'
    resuid = 'Sam Spade'
    result = None
    invalidtype = 'interfaces'
    invaliduid = 'eth0'
    adminuser = 'RgAdmin'

    @pytest.mark.dependency()
    def test_create_and_delete_single_resource(self):
        print('Test: test_create_and_delete_single_resource')
        # Ensure it's not already present
        assert requests.get('%s/%s/%s' % (API_BASE_URL,
                                          self.restype,
                                          (sanitise_uid(self.resuid)))).status_code == 404
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
        # Confirm we have two people,
        # Testing the mod/3 == 1 case in the process
        assert len(requests.get('%s/%s' % (API_BASE_URL, self.restype)).json())
        # Test the mod/3 == 0 case while we're here
        creators = requests.get('%s/%s/%s/CREATOR' % (API_BASE_URL,
                                                      self.restype,
                                                      sanitise_uid(self.resuid)))
        assert creators.status_code == 200
        assert len(creators.json()) == 1
        assert creators.json()[0]['uid'] == self.adminuser
        assert creators.json()[0]['type'] == 'People'
        # Delete it
        assert requests.delete('%s/%s/%s' % (
            API_BASE_URL, self.restype, sanitise_uid(self.resuid))).status_code == 204
        # Confirm it's gone
        assert requests.get('%s/%s/%s' % (
            API_BASE_URL, self.restype, sanitise_uid(self.resuid))).status_code == 404
    @pytest.mark.dependency()
    def test_fail_to_create_invalid_resourcetype(self):
        print('Test: Fail to create an instance of an invalid resourcetype')
        assert requests.post('%s/%s' % (API_BASE_URL, self.invalidtype),
                             data={'uid': self.invaliduid}).status_code == 400


@pytest.mark.dependency(depends=["TestResources::test_create_and_delete_single_resource"])
class TestDuplicateResistance(unittest.TestCase):
    '''
    Check that we can't create duplicate primary resources.
    '''
    resourcetype = 'People'
    resourcename = 'Dante'
    result = None
    @pytest.mark.dependency()
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
    @pytest.mark.dependency()
    def test_create_and_retrieve_multiple_resources(self):
        print('Test: test_create_and_retrieve_multiple_resources')
        # Confirm we're starting with an empty set
        self.assertEqual(requests.get('%s/%s' % (API_BASE_URL, self.resourcetype)).status_code,
                         200)
        self.assertEqual(len(requests.get('%s/%s' % (API_BASE_URL, self.resourcetype)).json()), 1)
        # Add the first resource
        self.assertEqual(requests.post('%s/%s/' % (API_BASE_URL, self.resourcetype),
                                       data={'uid': self.resource1uid}).status_code,
                         201)
        # Check that we now have two resources, including the site admin.
        self.assertEqual(len(requests.get('%s/%s' % (API_BASE_URL, self.resourcetype)).json()), 2)
        # Add the second resource
        self.assertEqual(requests.post('%s/%s/' % (API_BASE_URL, self.resourcetype),
                                       data={'uid': self.resource2uid}).status_code,
                         201)
        # Check that we now get a list containing exactly both resources
        self.assertEqual(len(requests.get('%s/%s' % (API_BASE_URL, self.resourcetype)).json()), 3)
        # Add the third resource
        self.assertEqual(requests.post('%s/%s/' % (API_BASE_URL, self.resourcetype),
                                       data={'uid': self.resource3uid}).status_code,
                         201)
        # Check that we now get a list containing all three resources
        self.assertEqual(len(requests.get('%s/%s' % (API_BASE_URL, self.resourcetype)).json()), 4)
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
    valid_resourcetype = 'People'
    valid_uid = 'Soolin'
    @pytest.mark.dependency()
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
                         400)

@pytest.mark.dependency(depends=[
    "TestMultipleResources::test_create_and_retrieve_multiple_resources",
    "TestBasicResourceErrors::test_basic_resource_errors"])
class TestAttributesBasic(unittest.TestCase):
    '''
    Add an attribute to a resource
    '''
    person1 = 'Blake'
    attr1name = 'displayname'
    attr1val = 'Roj Blake'
    person2 = 'Vila'
    attr2name = 'displayname'
    attr2val = 'Vila Restal'
    file1source = 'cats_cuddling.jpg'
    file1name = 'The popular cat-cuddling front of Judea'
    file1sha3_256sum = '305664EB53010D52642594A3B3877A1BB811EAD9B610C5C1210F7F3B88B4C184'
    file1sha3_256sum_wrong = '5555555555555555555555555555555555555555555555555555555555555555'
    response = None
    currentDate = None
    @pytest.mark.dependency()
    def test_add_and_remove_single_attribute(self):
        # Create the resource
        requests.post('%s/People/' % (API_BASE_URL), data={"uid": self.person1})
        # Check that it doesn't yet have the attribute we're adding
        result1 = requests.get('%s/People/%s' % (API_BASE_URL, self.person1)).json()
        assert result1['uid'] == self.person1
        with pytest.raises(KeyError):
            _ = result1[self.attr1name] # Assign to discard-var to shut pylint up
        # Add the attribute
        response = requests.put('%s/People/%s' % (API_BASE_URL, self.person1),
                                data={self.attr1name: self.attr1val})
        assert response.status_code == 200
        assert response.text == 'Updated'
        # Confirm that the attribute is there
        assert requests.get('%s/People/%s' % (
            API_BASE_URL, self.person1)).json()[self.attr1name] == self.attr1val
        # Remove the resource
        assert requests.delete('%s/People/%s' % (API_BASE_URL, self.person1)).status_code == 204
        # Confirm the resource is gone
        assert requests.get('%s/People/%s' % (API_BASE_URL, self.person1)).status_code == 404
    def test_nonaddressable_attribute(self):
        # Try to change an attribute that is not explicitly defined, and is only set on creation.
        # Create the resource
        requests.post('%s/People/' % (API_BASE_URL), data={"uid": self.person2})
        # Get the current createddate
        currentDate = requests.get('%s/People/%s' % (API_BASE_URL, self.person2)).json()['createddate']
        # Try to set the createddate; the server should return "400/Client error".
        # It should pass straight through the read-only-attribute validation, because
        # the API dispatcher doesn't even recognise it as an attribute that might need validation.
        assert requests.put('%s/People/%s' % (API_BASE_URL, self.person2),
                            data = {'createddate': 13245}).status_code == 400
        # Confirm that the createddate hasn't changed anyway.
        assert currentDate == requests.get('%s/People/%s' % (API_BASE_URL, self.person2)).json()['createddate']
        #
        ## Second variation: an internal-only attribute that is only added subsequent to creation.
        # Confirm that the lastmodified datestamp is null.
        # Try to set the lastmodified datestamp. This should fail, too.
        # Confirm that the lastmodified datestamp is _still_ null.
        assert requests.get('%s/People/%s' % (API_BASE_URL, self.person2)).json().get('updateddate', None) is None
        # Set an attribute.
        assert requests.put('%s/People/%s' % (API_BASE_URL, self.person2),
                                              data={self.attr2name: self.attr2val}).status_code == 200
        # Get the updated lastmodified datestamp.
        lastmodified = requests.get('%s/People/%s' % (API_BASE_URL, self.person2)).json().get('updateddate', None)
        # Try to set the lastmodified datestamp; the server should return an error.
        assert requests.put('%s/People/%s' % (API_BASE_URL, self.person2),
                            data={'updateddate': 12345643}).status_code == 400
        # Confirm that the lastmodified datestamp hasn't changed.
        assert lastmodified == requests.get('%s/People/%s' %
                                            (API_BASE_URL, self.person2)).json().get('updateddate', None)
        # Remove the resource
        assert requests.delete('%s/People/%s' % (API_BASE_URL, self.person2)).status_code == 204
        # Confirm the resource is gone
        assert requests.get('%s/People/%s' % (API_BASE_URL, self.person2)).status_code == 404
    def test_readonly_attr_set(self):
        # Upload the file
        fhandle = open(self.file1source, 'rb')
        assert(requests.post('%s/files/' % (FILES_BASE_URL),
                             data={'name': self.file1name},
                             files={'file': fhandle}).status_code == 201)
        # Fail to set the checksum
        assert(requests.put('%s/Files/%s' % (API_BASE_URL, sanitise_uid(self.file1name)),
                            data={'sha3256sum': self.file1sha3_256sum_wrong}).status_code == 403)
        # Confirm the checksum is correct
        assert(requests.get('%s/Files/%s' % (API_BASE_URL, sanitise_uid(self.file1name))).json()['sha3256sum'] == self.file1sha3_256sum )
        # Delete the file
        requests.delete('%s/%s' % (FILES_BASE_URL, sanitise_uid(self.file1name)))

@pytest.mark.dependency(depends=["TestAttributesBasic::test_add_and_remove_single_attribute"])
class TestRelationshipsBasic(unittest.TestCase):
    '''
    The most rudimentary of relationship testing
    '''
    person1 = 'Blake'
    tag1 = 'Idealist'
    group1 = 'Seven'
    rgadmin = 'RgAdmin'
    def test_tag_a_person(self):
        # Setup
        requests.post('%s/People/' % (API_BASE_URL), data={"uid": self.person1})
        requests.post('%s/Tags/' % (API_BASE_URL), data={"uid": self.tag1})
        # Tag the person
        assert requests.post('%s/People/%s/TAGS' % (API_BASE_URL, self.person1),
                data={"target": "/Tags/{tag}".format(tag=self.tag1)}).status_code == 201
        # Confirm they've been tagged
        assert len(requests.get('{base}/People/{person}/TAGS/Tags'.format(
                base=API_BASE_URL, person=self.person1)).json()) == 1
        # Remove the tag
        assert requests.delete('{base}/People/{person}/TAGS'.format(
                base=API_BASE_URL, person=self.person1),
                data={"target": "/Tags/{tag}".format(tag=self.tag1)}).status_code == 204
        # Confirm it's gone
        assert len(requests.get('{base}/People/{person}/TAGS/Tags'.format(
                base=API_BASE_URL, person=self.person1)).json()) == 0
        # Teardown: delete the person and the tag
        assert requests.delete('%s/Tags/%s' % (API_BASE_URL, self.tag1)).status_code == 204
        assert requests.delete('%s/People/%s' % (API_BASE_URL, self.person1)).status_code == 204
        # Confirm teardown
        assert requests.get('%s/People/%s' % (API_BASE_URL, self.person1)).status_code == 404
        assert requests.get('%s/Tags/%s' % (API_BASE_URL, self.tag1)).status_code == 404
    def test_fail_to_set_creator(self):
        # Setup
        requests.post('%s/People/' % (API_BASE_URL), data={"uid": self.person1})
        requests.post('%s/Tags/' % (API_BASE_URL), data={"uid": self.tag1})
        # Test
        assert requests.post('%s/Tags/%s/CREATOR' % (API_BASE_URL, self.tag1),
                             data={"target": '/People/%s' % (self.person1)}).status_code == 403
        # Teardown
        assert requests.delete('%s/Tags/%s' % (API_BASE_URL, self.tag1)).status_code == 204
        assert requests.delete('%s/People/%s' % (API_BASE_URL, self.person1)).status_code == 204
        # Confirm teardown
        assert requests.get('%s/People/%s' % (API_BASE_URL, self.person1)).status_code == 404
        assert requests.get('%s/Tags/%s' % (API_BASE_URL, self.tag1)).status_code == 404
    def test_fail_to_delete_creator(self):
        # Setup
        requests.post('%s/Tags/' % (API_BASE_URL), data={"uid": self.tag1})
        # Test
        assert requests.delete('%s/Tags/%s/CREATOR' % (API_BASE_URL, self.tag1),
                             data={"target": '/People/%s' % (self.rgadmin)}).status_code == 403
        # Teardown
        assert requests.delete('%s/Tags/%s' % (API_BASE_URL, self.tag1)).status_code == 204
        # Confirm teardown
        assert requests.get('%s/Tags/%s' % (API_BASE_URL, self.tag1)).status_code == 404
    def test_fail_to_create_invalid_relationship(self):
        # Setup
        requests.post('%s/People/' % (API_BASE_URL), data={"uid": self.person1})
        requests.post('%s/Groups/' % (API_BASE_URL), data={"uid": self.group1})
        # Test
        assert requests.post('%s/People/%s/MEMBER_OF' % (API_BASE_URL, self.person1),
                             data={'target': '/Groups/%s' % (self.group1)}).status_code == 409
        # Teardown
        assert requests.delete('%s/Groups/%s' % (API_BASE_URL, self.group1)).status_code == 204
        assert requests.delete('%s/People/%s' % (API_BASE_URL, self.person1)).status_code == 204
    def test_fail_to_delete_invalid_relationship(self):
        # Setup
        requests.post('%s/People/' % (API_BASE_URL), data={"uid": self.person1})
        requests.post('%s/Groups/' % (API_BASE_URL), data={"uid": self.group1})
        # Test
        response = requests.delete('%s/People/%s/MEMBER_OF' % (API_BASE_URL, self.person1),
                                   data={'target': '/Groups/%s' % (self.group1)})
        assert response.status_code == 400
        assert response.text == "Client error: There is no relationship between these resource-types. Are you sure there's something here to delete?"
        # Teardown
        assert requests.delete('%s/Groups/%s' % (API_BASE_URL, self.group1)).status_code == 204
        assert requests.delete('%s/People/%s' % (API_BASE_URL, self.person1)).status_code == 204

@pytest.mark.dependency(depends=["TestRelationshipsBasic::"])
class TestRelationshipFailures(unittest.TestCase):
    '''
    Regression tests for relationships
    '''
    person1 = 'Travis'
    tag1 = 'villain'
    def test_create_relationship_without_relationship(self):
        # Request creation of a relationship, but leave the relationship off the source URL
        #
        # Setup
        requests.post('%s/People/' % (API_BASE_URL), data={"uid": self.person1})
        requests.post('%s/Tags/' % (API_BASE_URL), data={"uid": self.tag1})
        # Test
        assert requests.post('%s/People/%s' % (API_BASE_URL, self.person1),
                             data={'target': '/Tags/%s' % (self.tag1)}).status_code == 400
        # Teardown
        requests.delete('%s/Tags/%s' % (API_BASE_URL, self.tag1)).status_code
        requests.delete('%s/People/%s' % (API_BASE_URL, self.person1)).status_code

@pytest.mark.dependency(depends=["TestRelationshipsBasic::"])
class TestFilesApi(unittest.TestCase):
    '''
    Upload, download, metadata and deletion of files.
    '''
    file1source = 'cats_cuddling.jpg'
    file1name = 'Cuddling cats'
    file1sha3_256sum = '305664EB53010D52642594A3B3877A1BB811EAD9B610C5C1210F7F3B88B4C184'
    file2source = 'cats_cuddling.jpg'
    file2name = 'Cute kitties'
    @pytest.mark.dependency()
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
        self.assertEqual(result.status_code, 404)
    @pytest.mark.dependency()
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
        self.assertEqual(result.status_code, 404)


@pytest.mark.dependency(depends=[
    "TestDuplicateResistance::test_unique_resources",
    "TestBasicResourceErrors::test_basic_resource_errors"])
class TestSchemaBasic(unittest.TestCase):
    @pytest.mark.dependency()
    def test_schema_any(self):
        assert requests.get('%s/any' % (SCHEMA_BASE_URL)).json() == {
                "name": "any",
                "attributes": None,
                "dependent": None,
                #! pylint: disable=line-too-long
                "description": "Special-case meta-resource, representing an instance of any type of resource. This is used for defining relationships where either the source or target could be, well, any resourcetype. The server refuses to create an instance of this resourcetype.",
                "relationships": [
                    {
                        "name": "CREATOR",
                        "dependent": None,
                        "cardinality": "many:1",
                        "description": "All resources are linked to their creator. This is the first part of the permissions-management system.",
                        "target-type": "People"
                        },
                    {
                        "name": "GROUPS",
                        "dependent": None,
                        "cardinality": "many:many",
                        "description": "Any resourcetype can be assigned to a group.",
                        "target-type": "Groups"
                        },
                    {
                        "name": "TAGS",
                        "dependent": None,
                        "cardinality": "many:many",
                        "description": "Any resourcetype can be tagged.",
                        "target-type": "Tags"
                        }
                    ]}
    @pytest.mark.dependency()
    def test_schema_tags(self):
        assert requests.get('%s/Tags' % (SCHEMA_BASE_URL)).json() == {
                "name": "Tags",
                "attributes": [
                    {
                        "name": "description",
                        "description": "Clarification of what the tag means.",
                        "read-only": None,
                        "values": None
                        }
                    ],
                "dependent": None,
                "description": "For categorising resources of any type. Useful in searches.",
                "relationships": None
                }


@pytest.mark.dependency(["TestSchemaBasic::test_schema_tags"])
#@pytest.mark.skip()
class TestSchemaUpdates(unittest.TestCase):
    @pytest.mark.dependency()
    def test_schema_atomic_upload_and_delete(self):
        # Check the existing list of versions
        versions_existing = requests.get('%s?version=list' % (SCHEMA_BASE_URL)).json()
        assert isinstance(versions_existing['versions'], list)
        assert isinstance(versions_existing['current-version'], int)
        # Upload the file
        assert requests.post(SCHEMA_BASE_URL,
                             files={'schema': open('test_schema.json', 'rb')},
                             data={'create': 'true'}).status_code == 201
        # Confirm that the list of versions has gained a newer one, and that the new one is current
        versions_after_create = requests.get('%s?version=list' % (SCHEMA_BASE_URL)).json()
        assert len(versions_after_create['versions']) == (len(versions_existing['versions']) + 1)
        assert versions_after_create['current-version'] > versions_existing['current-version']
        # Request the schema and inspect it
        schema = requests.get(SCHEMA_BASE_URL).json()
        assert list(filter(lambda x: x['name'] == 'Buildings', schema))[0]['dependent'] is None
        # Delete the schema
        assert requests.delete('%s?version=%s' % (SCHEMA_BASE_URL,
                                                  versions_after_create['current-version'])).status_code == 200
        # Confirm that things are back the way they were
        versions_after_delete = requests.get('%s?version=list' % (SCHEMA_BASE_URL)).json()
        assert versions_after_delete['current-version'] == versions_existing['current-version']
        assert sorted(versions_after_delete['versions']) == sorted(versions_existing['versions'])
    def test_schema_stepwise_upload_and_delete(self):
        # Check the existing list of versions
        versions_existing = requests.get('%s?version=list' % (SCHEMA_BASE_URL)).json()
        assert isinstance(versions_existing['versions'], list)
        assert isinstance(versions_existing['current-version'], int)
        # Create a new schema-version, without adding anything to it
        assert requests.post(SCHEMA_BASE_URL, data={'create': 'true'}).status_code == 201
        # Confirm that the list of versions has gained a newer one, and that the new one is current
        versions_after_create = requests.get('%s?version=list' % (SCHEMA_BASE_URL)).json()
        assert len(versions_after_create['versions']) == (len(versions_existing['versions']) + 1)
        assert versions_after_create['current-version'] > versions_existing['current-version']
        # Upload the file
        assert requests.post(SCHEMA_BASE_URL, files={'schema': open('test_schema.json', 'rb')}).status_code == 201
        # Request the schema and inspect it
        schema = requests.get(SCHEMA_BASE_URL).json()
        assert list(filter(lambda x: x['name'] == 'Buildings', schema))[0]['dependent'] is None
        # Set the current-version back to the previous current, and confirm
        assert requests.put('%s?version=%s' % (SCHEMA_BASE_URL,
                                               versions_existing['current-version'])).status_code == 200
        versions_after_current_change = requests.get('%s?version=list' % (SCHEMA_BASE_URL)).json()
        assert versions_after_current_change['current-version'] == versions_existing['current-version']
        assert sorted(versions_after_current_change['versions']) == sorted(versions_after_create['versions'])
        # Delete the schema we created
        assert requests.delete('%s?version=%s' % (SCHEMA_BASE_URL,
                                                  versions_after_create['current-version'])).status_code == 200
        # Confirm that things are back the way they were
        versions_after_delete = requests.get('%s?version=list' % (SCHEMA_BASE_URL)).json()
        assert versions_after_delete['current-version'] == versions_existing['current-version']
        assert sorted(versions_after_delete['versions']) == sorted(versions_existing['versions'])

@pytest.mark.dependency(["TestSchemaUpdates::test_schema_upload"])
class TestAttributeValues(unittest.TestCase):
    res1type = "People"
    res1uid = "Dorian"
    res1attrname = "Real"
    res1attrval_valid = True
    res1attrval_invalid = "Banana"
    def test_attribute_values(self):
        # Add the test schema
        requests.post(SCHEMA_BASE_URL, files={'schema': open('test_schema.json', 'rb')}, data={'create': 'true'})
        # Create the resource
        assert requests.post('%s/%s' % (API_BASE_URL, self.res1type),
                             data={"uid": self.res1uid}).status_code == 201
        # Confirm the "values" feature as well as the added subschema:
        # Add a valid attribute (per the "values" attribute-attribute)
        assert requests.put('%s/%s/%s' % (API_BASE_URL, self.res1type, self.res1uid),
                            data={self.res1attrname: self.res1attrval_valid}).status_code == 200
        # Fail to add an invalid attribute
        assert requests.put('%s/%s/%s' % (API_BASE_URL, self.res1type, self.res1uid),
                            data={self.res1attrname: self.res1attrval_invalid}).status_code == 400
        # Clean up the resource
        assert requests.delete('%s/%s/%s' % (API_BASE_URL, self.res1type, self.res1uid)).status_code == 204
        # Clean up the schema we added
        versions = requests.get('%s?version=list' % (SCHEMA_BASE_URL)).json()
        requests.delete('%s?version=%s' % (SCHEMA_BASE_URL, versions['current-version']))
    # - new relationships from an existing resourcetype to a new one.
    # Test robustness against
    # - duplicate attributes in new subschemas
    #     - both within the same document, and in a subsequent one
    # - duplicate relationship definitions
    #     - both within the same document, and in a subsequent one

@pytest.mark.dependency(["TestSchemaUpdates"])
class TestDependentResources(unittest.TestCase):
    res1type = "Buildings"
    res1uid = "Xenon"
    depres1rel = "FLOORS"
    depres1type = "Floors"
    depres1uid = "Hangar"
    depres2rel = "ROOMS"
    depres2type = "Rooms"
    depres2uid = "Toolshed"
    owner1type = "People"
    owner1uid = "Frank"
    def test_dependent_resourcetypes_basic(self):
        # Add the test schema
        requests.post(SCHEMA_BASE_URL, files={'schema': open('test_schema.json', 'rb')}, data={'create': 'true'})
        # Create the parent resource
        requests.post('%s/%s' % (API_BASE_URL, self.res1type), data={"uid": self.res1uid})
        # Create the owner
        requests.post('%s/%s' % (API_BASE_URL, self.owner1type), data={"uid": self.owner1uid})
        # Connect the building to its owner
        assert requests.post('%s/%s/%s/OWNER' % (API_BASE_URL, self.res1type, self.res1uid),
                             data={"target": '/%s/%s' % (self.owner1type, self.owner1uid)}).status_code == 201
        # Fail to connect the owner to the building because it isn't dependent
        assert requests.post('%s/%s/%s/OWNS' % (API_BASE_URL, self.owner1type, self.owner1uid),
                             data={"target": '/%s/%s' % (self.res1type, self.res1uid)}).status_code == 409
        # Delete the owner
        requests.delete('%s/%s/%s' % (API_BASE_URL, self.owner1type, self.owner1uid))
        # Create the dependent resource
        assert requests.post('%s/%s/%s/%s/%s' % (API_BASE_URL,
                                              self.res1type,
                                              self.res1uid,
                                              self.depres1rel,
                                              self.depres1type),
                             data={"uid": self.depres1uid}).status_code == 201
        # Confirm the dependent resource is there
        assert requests.get('%s/%s/%s/%s/%s' % (API_BASE_URL,
                                                self.res1type,
                                                self.depres1rel,
                                                self.depres1type,
                                                self.depres1uid)).status_code == 200
        # Delete the dependent resource
        assert requests.delete('%s/%s/%s/%s/%s/%s' % (API_BASE_URL,
                                                self.res1type,
                                                self.res1uid,
                                                self.depres1rel,
                                                self.depres1type,
                                                self.depres1uid)).status_code == 204
        # Confirm it's gone
        assert requests.get('%s/%s/%s/%s/%s/%s' % (API_BASE_URL,
                                                self.res1type,
                                                self.res1uid,
                                                self.depres1rel,
                                                self.depres1type,
                                                self.depres1uid)).status_code == 404
        # Confirm the parent is still there
        assert requests.get('%s/%s/%s' % (API_BASE_URL,
                                          self.res1type,
                                          self.res1uid)).status_code == 200
        # Clean up by deleting the parent resource
        assert requests.delete('%s/%s/%s' % (API_BASE_URL,
                                             self.res1type,
                                             self.res1uid)).status_code == 204
        # Clean up the schema we added
        versions = requests.get('%s?version=list' % (SCHEMA_BASE_URL)).json()
        requests.delete('%s?version=%s' % (SCHEMA_BASE_URL, versions['current-version']))
    # Check for recursive deletion, from the parent down 1 level.
    def test_dependent_resourcetypes_recursive_1(self):
        # Add the test schema
        requests.post(SCHEMA_BASE_URL, files={'schema': open('test_schema.json', 'rb')}, data={'create': 'true'})
        # Create the parent resource
        assert requests.post('%s/%s' % (API_BASE_URL, self.res1type),
                             data={"uid": self.res1uid}).status_code == 201
        # Create the dependent resource
        assert requests.post('%s/%s/%s/%s/%s' % (API_BASE_URL,
                                              self.res1type,
                                              self.res1uid,
                                              self.depres1rel,
                                              self.depres1type),
                             data={"uid": self.depres1uid}).status_code == 201
        # Confirm the dependent resource is there
        assert requests.get('%s/%s/%s/%s/%s/%s' % (API_BASE_URL,
                                                self.res1type,
                                                self.res1uid,
                                                self.depres1rel,
                                                self.depres1type,
                                                self.depres1uid)).status_code == 200
        # Recursively delete the parent resource
        assert requests.delete('%s/%s/%s?recursive=true' % (API_BASE_URL,
                                                            self.res1type,
                                                            self.res1uid)).status_code == 204
        # Confirm the child is gone
        assert requests.get('%s/%s/%s/%s/%s/%s' % (API_BASE_URL,
                                                self.res1type,
                                                self.res1uid,
                                                self.depres1rel,
                                                self.depres1type,
                                                self.depres1uid)).status_code == 404
        # Confirm the parent is gone
        assert requests.get('%s/%s/%s' % (API_BASE_URL, self.res1type, self.res1uid)).status_code == 404
        # Clean up the schema we added
        versions = requests.get('%s?version=list' % (SCHEMA_BASE_URL)).json()
        requests.delete('%s?version=%s' % (SCHEMA_BASE_URL, versions['current-version']))
    # Create a grandchild resource, and check recursive deletion from the parent.
    # - check for orphaned grandchildren.
    # Check for recursive deletion, from the parent down 2 levels.
    def test_dependent_resourcetypes_recursive_2(self):
        # Add the test schema
        requests.post(SCHEMA_BASE_URL, files={'schema': open('test_schema.json', 'rb')}, data={'create': 'true'})
        # Create the parent resource
        assert requests.post('%s/%s' % (API_BASE_URL, self.res1type),
                             data={"uid": self.res1uid}).status_code == 201
        # Create the child resource
        assert requests.post('%s/%s/%s/%s/%s' % (API_BASE_URL,
                                              self.res1type,
                                              self.res1uid,
                                              self.depres1rel,
                                              self.depres1type),
                             data={"uid": self.depres1uid}).status_code == 201
        # Create the grandchild resource
        assert requests.post('%s/%s/%s/%s/%s/%s/%s/%s' % (API_BASE_URL,
                                                          self.res1type,
                                                          self.res1uid,
                                                          self.depres1rel,
                                                          self.depres1type,
                                                          self.depres1uid,
                                                          self.depres2rel,
                                                          self.depres2type),
                             data={"uid": self.depres2uid}).status_code == 201
        # Confirm the grandchild resource is there
        assert requests.get('%s/%s/%s/%s/%s/%s/%s/%s/%s' % (API_BASE_URL,
                                                            self.res1type,
                                                            self.res1uid,
                                                            self.depres1rel,
                                                            self.depres1type,
                                                            self.depres1uid,
                                                            self.depres2rel,
                                                            self.depres2type,
                                                            self.depres2uid)).status_code == 200
        # Recursively delete the parent resource
        assert requests.delete('%s/%s/%s?recursive=true' % (API_BASE_URL,
                                                            self.res1type,
                                                            self.res1uid)).status_code == 204
        # Confirm the grandchild is gone
        assert requests.get('%s/%s/%s/%s/%s/%s/%s/%s/%s' % (API_BASE_URL,
                                                            self.res1type,
                                                            self.res1uid,
                                                            self.depres1rel,
                                                            self.depres1type,
                                                            self.depres1uid,
                                                            self.depres2rel,
                                                            self.depres2type,
                                                            self.depres2uid)).status_code == 404
        # Confirm the child is gone
        assert requests.get('%s/%s/%s/%s/%s/%s' % (API_BASE_URL,
                                                self.res1type,
                                                self.res1uid,
                                                self.depres1rel,
                                                self.depres1type,
                                                self.depres1uid)).status_code == 404
        # Confirm the parent is gone
        assert requests.get('%s/%s/%s' % (API_BASE_URL,
                                          self.res1type,
                                          self.res1uid)).status_code == 404
        # Confirm the grandchild doesn't live on as an orphan
        assert requests.get('%s/%s/%s' % (API_BASE_URL,
                                          self.depres2type,
                                          self.depres2uid)).status_code == 404
        # Confirm the child doesn't live on as an orphan
        assert requests.get('%s/%s/%s' % (API_BASE_URL,
                                          self.depres1type,
                                          self.depres1uid)).status_code == 404
        # Clean up the schema we added
        versions = requests.get('%s?version=list' % (SCHEMA_BASE_URL)).json()
        requests.delete('%s?version=%s' % (SCHEMA_BASE_URL, versions['current-version']))
    # Check that `recursive=true` _only_ deletes dependent resources,
    # and _doesn't_ go on a rampage.
    # Check that _only_ dependent resources can be created with a dependent relationship.
    # Check that dependent relationships _cannot_ be created to existing resources, whether dependent or not. Positively confirm both cases.
    # Check that the `recursive` parameter really does work in both GET- and POST-styles.

@pytest.mark.dependency(["TestSchemaUpdates::test_schema_upload"])
class TestRelationshipsToAny(unittest.TestCase):
    sourceType = 'Thingy'
    sourceUid = 'Whatsit'
    targetType = 'Tags'
    targetUid = 'fluffy'
    relationship = 'DISCOMBOBULATES'
    invalidSourceType = 'People'
    invalidSourceUid = 'Wrongun'
    def test_create_valid_rel_to_any(self):
        # Add the test schema
        requests.post(SCHEMA_BASE_URL, files={'schema': open('test_schema.json', 'rb')}, data={'create': 'true'})
        # Create the resources
        requests.post('%s/%s' % (API_BASE_URL, self.sourceType), data={'uid': self.sourceUid})
        requests.post('%s/%s' % (API_BASE_URL, self.targetType), data={'uid': self.targetUid})
        # Create the relationship we're testing for
        requests.post('%s/%s/%s/%s' % (API_BASE_URL, self.sourceType, self.sourceUid, self.relationship),
                      data={'target': '/%s/%s' % (self.targetType, self.targetUid)})
        # Delete the resources
        requests.delete('%s/%s/%s' % (API_BASE_URL, self.sourceType, self.sourceUid))
        requests.delete('%s/%s/%s' % (API_BASE_URL, self.targetType, self.targetUid))
        # Clean up the schema we added
        versions = requests.get('%s?version=list' % (SCHEMA_BASE_URL)).json()
        requests.delete('%s?version=%s' % (SCHEMA_BASE_URL, versions['current-version']))
    #def test_fail_invalid_rel_to_any(self):

class TestFilters(unittest.TestCase):
    tag1 = 'foo'
    tag2 = 'bar'
    person1 = 'RgAdmin'
    person2 = 'Bruce'
    def test_filter_for_tags_in_use(self):
        # Create and link the resources
        requests.post('%s/Tags' % (API_BASE_URL), data={'uid': self.tag1})
        requests.post('%s/Tags' % (API_BASE_URL), data={'uid': self.tag2})
        requests.post('%s/People' % (API_BASE_URL), data={'uid': self.person2})
        requests.post('%s/People/%s/TAGS' % (API_BASE_URL, self.person2),
                      data={'target': '/Tags/%s' % (self.tag1)})
        # Confirm both tags are there
        assert requests.get('%s/Tags/%s' % (API_BASE_URL, self.tag1)).status_code == 200
        assert requests.get('%s/Tags/%s' % (API_BASE_URL, self.tag2)).status_code == 200
        # Test the filtering
        response1 = requests.get('%s/Tags?RGinbound=/People/*/TAGS' % API_BASE_URL)
        assert response1.status_code == 200
        assert len(response1.json()) == 1
        assert response1.json()[0]['uid'] == self.tag1
        # Delete the resources
        requests.delete('%s/Tags/%s' % (API_BASE_URL, self.tag1))
        requests.delete('%s/Tags/%s' % (API_BASE_URL, self.tag2))
        requests.delete('%s/People/%s' % (API_BASE_URL, self.person2))

@pytest.mark.skip()
class TestDependentResources_ignoreme(unittest.TestCase):
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


## IPAM tests

@pytest.mark.dependency("TestDependentResources")
class TestIpv4SubnetsBasicNoVrf(unittest.TestCase):
    '''
    Basic CRD functions for subnets
    '''
    subnet1 = '172.16.0.0/12'
    organisation = 'testco'
    def test_create_and_delete_single_subnet(self):
        print('Test: test_create_and_delete_single_subnet')
        # Create the organisation that it goes under
        self.assertEqual(requests.post('%s/Organisations' % (API_BASE_URL),
                                       data={'uid': self.organisation}).status_code,
                         201)
        # Add the subnet
        self.assertEqual(requests.post('%s/subnets' % (IPAM_BASE_URL),
                                       data={'org': self.organisation,
                                             'subnet': self.subnet1}).status_code,
                         201)
        # Confirm it's there
        self.assertEqual(requests.get('%s/subnets?subnet=%s&org=%s'
                                      % (IPAM_BASE_URL,
                                         self.subnet1,
                                         self.organisation)).status_code,
                         200)
        # Delete it
        self.assertEqual(requests.delete('%s/subnets' % (IPAM_BASE_URL),
                                         data={'org': self.organisation,
                                               'subnet': self.subnet1}).status_code,
                         204)
        # Ensure it's gone
        self.assertEqual(requests.get('%s/subnets?subnet=%s&org=%s'
                                      % (IPAM_BASE_URL,
                                         self.subnet1,
                                         self.organisation)).status_code,
                         404)
        # Remove the organisation
        self.assertEqual(requests.delete('%s/Organisations/%s'
                                         % (API_BASE_URL, self.organisation)).status_code,
                         204)

@pytest.mark.dependency("TestIpv4SubnetsBasicNoVrf")
class TestIpv4AddressesBasicNoVrf(unittest.TestCase):
    '''
    Basic CRD functions for Ipv4 addresses
    '''
    org = 'testco'
    subnet1 = '172.16.0.0/12'
    address1 = '172.16.23.4'
    organisation = 'testcorp'
    def test_create_and_delete_single_address(self):
        print('Test: test_create_and_delete_single_address')
        # Create the organisation that it goes under
        self.assertEqual(requests.post('%s/Organisations' % (API_BASE_URL),
                                       data={'uid': self.organisation}).status_code,
                         201)
        # Add the subnet
        self.assertEqual(requests.post('%s/subnets' % (IPAM_BASE_URL),
                                       data={'org': self.organisation,
                                             'subnet': self.subnet1}).status_code,
                         201)
        # Add the address
        self.assertEqual(requests.post('%s/addresses' % (IPAM_BASE_URL),
                                       data={'org': self.organisation,
                                             'address': self.address1}).status_code,
                         201)
        # Confirm it's there
        self.assertEqual(requests.get('%s/addresses?address=%s&org=%s'
                                      % (IPAM_BASE_URL, self.address1, self.organisation)).status_code,
                         200)
        # Delete it
        self.assertEqual(requests.delete('%s/addresses' % (IPAM_BASE_URL),
                                         data={'org': self.organisation,
                                               'address': self.address1}).status_code,
                         204)
        # Ensure it's gone
        self.assertEqual(requests.get('%s/addresses?address=%s&org=%s'
                                      % (IPAM_BASE_URL, self.address1, self.organisation)).status_code,
                         404)
        # Remove the organisation and subnet
        self.assertEqual(requests.delete('%s/Organisations/%s'
                                         % (API_BASE_URL, self.organisation),
                                         data={'recursive': 'true'}).status_code,
                         204)

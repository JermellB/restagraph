#!/usr/bin/env python
# Expects python3

"Test operations on primary (i.e, non-dependent) resources."


#   Copyright 2017-2022 James Fleming <james@electronic-quill.net>
#
#   Licensed under the GNU General Public License
#   - for details, see LICENSE.txt in the top-level directory


# pylint: disable=relative-beyond-top-level
# pylint: disable=missing-class-docstring
# pylint: disable=missing-function-docstring
# pylint: disable=wrong-import-order

# Local stuff
from . import config
from . import utilities

# Included batteries
import unittest

# Third-party libraries
import pytest
import requests


@pytest.mark.dependency(depends=["Testutilities.sanitise_uid"])
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
    refcopy = None
    yoinkcopy = None

    @pytest.mark.dependency()
    def test_create_and_delete_single_resource(self):
        print('Test: test_create_and_delete_single_resource')
        # Ensure it's not already present
        assert requests.get('%s/%s/%s' % (config.API_BASE_URL,
                                          self.restype,
                                          (utilities.sanitise_uid(self.resuid)))).status_code == 404
        # Create it
        self.result = requests.post('%s/%s/' % (config.API_BASE_URL, self.restype),
                                    data={'uid': self.resuid})
        assert self.result.status_code == 201
        assert self.result.text == '/{rtype}/{uid}'.format(rtype=self.restype,
                                                           uid=utilities.sanitise_uid(self.resuid))
        # Confirm that it's now there
        self.result = requests.get('%s/%s/%s' % (config.API_BASE_URL,
                                                 self.restype,
                                                 utilities.sanitise_uid(self.resuid))).json()
        print('Found resource: {}'.format(self.result))
        assert self.result['original_uid'] == self.resuid
        assert self.result['uid'] == utilities.sanitise_uid(self.resuid)
        # Confirm we have two people,
        # Testing the mod/3 == 1 case in the process
        assert len(requests.get('%s/%s' % (config.API_BASE_URL, self.restype)).json())
        # Test the mod/3 == 0 case while we're here
        creators = requests.get('%s/%s/%s/RG_CREATOR' % (config.API_BASE_URL,
                                                      self.restype,
                                                      utilities.sanitise_uid(self.resuid)))
        assert creators.status_code == 200
        assert len(creators.json()) == 1
        assert creators.json()[0]['uid'] == self.adminuser
        assert creators.json()[0]['type'] == 'People'
        # Delete it
        assert requests.delete('%s/%s/%s' % (
            config.API_BASE_URL, self.restype, utilities.sanitise_uid(self.resuid))).status_code == 204
        # Confirm it's gone
        assert requests.get('%s/%s/%s' % (
            config.API_BASE_URL, self.restype, utilities.sanitise_uid(self.resuid))).status_code == 404
    @pytest.mark.dependency()
    def test_fail_to_create_invalid_resourcetype(self):
        print('Test: Fail to create an instance of an invalid resourcetype')
        assert requests.post('%s/%s' % (config.API_BASE_URL, self.invalidtype),
                             data={'uid': self.invaliduid}).status_code == 400
    @pytest.mark.dependency(depends=["TestResources::test_create_and_delete_single_resource"])
    def test_yoink_parameter(self):
        print('Test: Delete a resource, and receive a representation as a parting gift.')
        requests.post('%s/%s/' % (config.API_BASE_URL, self.restype), data={'uid': self.resuid})
        # Confirm that it's now there, and grab a reference copy.
        self.refcopy = requests.get('%s/%s/%s' % (config.API_BASE_URL,
                                                 self.restype,
                                                 utilities.sanitise_uid(self.resuid))).json()
        # Delete it
        self.yoinkcopy = requests.delete('%s/%s/%s?yoink=true' % (config.API_BASE_URL,
                                                  self.restype,
                                                  utilities.sanitise_uid(self.resuid)))
        # Check that the copy we got from yoinking matches the reference copy
        assert self.yoinkcopy.json() == self.refcopy
        # Confirm it's gone
        assert requests.get('%s/%s/%s' % (
            config.API_BASE_URL, self.restype, utilities.sanitise_uid(self.resuid))).status_code == 404

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
        self.assertEqual(requests.post('%s/%s/' % (config.API_BASE_URL, self.resourcetype),
                                       data={'uid': self.resourcename}).status_code,
                         201)
        # Confirm that it's now there
        self.result = requests.get('%s/%s/%s/' % (config.API_BASE_URL,
                                                  self.resourcetype,
                                                  self.resourcename)).json()
        print('Received result {}'.format(self.result))
        self.assertEqual(self.result['original_uid'], utilities.sanitise_uid(self.resourcename))
        self.assertEqual(self.result['uid'], self.resourcename)
        # Attempt to create a duplicate.
        self.assertEqual(requests.post('%s/%s/' % (config.API_BASE_URL, self.resourcetype),
                                       data={'uid': self.resourcename}).status_code,
                         304)
        # Delete the resource
        self.assertEqual(requests.delete('%s/%s/%s' % (config.API_BASE_URL,
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
        self.assertEqual(requests.get('%s/%s' % (config.API_BASE_URL, self.resourcetype)).status_code,
                         200)
        self.assertEqual(len(requests.get('%s/%s' % (config.API_BASE_URL, self.resourcetype)).json()), 1)
        # Add the first resource
        self.assertEqual(requests.post('%s/%s/' % (config.API_BASE_URL, self.resourcetype),
                                       data={'uid': self.resource1uid}).status_code,
                         201)
        # Check that we now have two resources, including the site admin.
        self.assertEqual(len(requests.get('%s/%s' % (config.API_BASE_URL, self.resourcetype)).json()), 2)
        # Add the second resource
        self.assertEqual(requests.post('%s/%s/' % (config.API_BASE_URL, self.resourcetype),
                                       data={'uid': self.resource2uid}).status_code,
                         201)
        # Check that we now get a list containing exactly both resources
        self.assertEqual(len(requests.get('%s/%s' % (config.API_BASE_URL, self.resourcetype)).json()), 3)
        # Add the third resource
        self.assertEqual(requests.post('%s/%s/' % (config.API_BASE_URL, self.resourcetype),
                                       data={'uid': self.resource3uid}).status_code,
                         201)
        # Check that we now get a list containing all three resources
        self.assertEqual(len(requests.get('%s/%s' % (config.API_BASE_URL, self.resourcetype)).json()), 4)
        # Delete the resources
        print('Test: clean up afterward')
        self.assertEqual(requests.delete('%s/%s/%s' % (config.API_BASE_URL,
                                                       self.resourcetype,
                                                       self.resource1uid)).status_code,
                         204)
        self.assertEqual(requests.delete('%s/%s/%s' % (config.API_BASE_URL,
                                                       self.resourcetype,
                                                       self.resource2uid)).status_code,
                         204)
        self.assertEqual(requests.delete('%s/%s/%s' % (config.API_BASE_URL,
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
    valid_uid1 = 'Soolin'
    valid_uid2 = 'Del-Tarrant'
    invalid_uid1 = 'Jenna$Stannis'
    invalid_uid2 = "Dayna'Mellanby"
    @pytest.mark.dependency()
    def test_basic_resource_errors(self):
        print('Test: test_basic_resource_errors')
        # Invalid resource-type
        self.assertEqual(requests.post('%s/%s' % (config.API_BASE_URL, self.invalid_resourcetype),
                                       data={'foo': 'bar'}).status_code,
                         400)
        # Missing UID
        self.assertEqual(requests.post('%s/%s' % (config.API_BASE_URL, self.valid_resourcetype),
                                       data={'foo': 'bar'}).status_code,
                         400)
        # Invalid non-UID parameters
        self.assertEqual(requests.post('%s/%s' % (config.API_BASE_URL, self.valid_resourcetype),
                                       data={'uid': self.valid_uid1, 'foo': 'bar'}).status_code,
                         400)
        # Test filtering of valid/invalid UID characters
        assert requests.post('%s/People' % (config.API_BASE_URL),
                             data={'uid': self.valid_uid1}).text == '/People/%s' % (
                                   utilities.sanitise_uid(self.valid_uid1))
        requests.delete('%s/People/%s' % (config.API_BASE_URL, utilities.sanitise_uid(self.valid_uid1)))
        assert requests.post('%s/People' % (config.API_BASE_URL),
                             data={'uid': self.invalid_uid1}).text == '/People/%s' % (
                                   utilities.sanitise_uid(self.invalid_uid1))
        requests.delete('%s/People/%s' % (config.API_BASE_URL, utilities.sanitise_uid(self.invalid_uid1)))
        assert requests.post('%s/People' % (config.API_BASE_URL),
                             data={'uid': self.invalid_uid2}).text == '/People/%s' % (
                                   utilities.sanitise_uid(self.invalid_uid2))
        requests.delete('%s/People/%s' % (config.API_BASE_URL, utilities.sanitise_uid(self.invalid_uid2)))

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
        requests.post('%s/People/' % (config.API_BASE_URL), data={"uid": self.person1})
        # Check that it doesn't yet have the attribute we're adding
        result1 = requests.get('%s/People/%s' % (config.API_BASE_URL, self.person1)).json()
        assert result1['uid'] == self.person1
        with pytest.raises(KeyError):
            _ = result1[self.attr1name] # Assign to discard-var to shut pylint up
        # Add the attribute
        response = requests.put('%s/People/%s' % (config.API_BASE_URL, self.person1),
                                data={self.attr1name: self.attr1val})
        assert response.status_code == 200
        assert response.text == 'Updated'
        # Confirm that the attribute is there
        assert requests.get('%s/People/%s' % (
            config.API_BASE_URL, self.person1)).json()[self.attr1name] == self.attr1val
        # Remove the resource
        assert requests.delete('%s/People/%s' % (config.API_BASE_URL, self.person1)).status_code == 204
        # Confirm the resource is gone
        assert requests.get('%s/People/%s' % (config.API_BASE_URL, self.person1)).status_code == 404
    def test_nonaddressable_attribute(self):
        # Try to change an attribute that is not explicitly defined, and is only set on creation.
        # Create the resource
        requests.post('%s/People/' % (config.API_BASE_URL), data={"uid": self.person2})
        # Get the current createddate
        currentDate = requests.get('%s/People/%s' % (config.API_BASE_URL, self.person2)).json()['createddate']
        # Try to set the createddate; the server should return "400/Client error".
        # It should pass straight through the read-only-attribute validation, because
        # the API dispatcher doesn't even recognise it as an attribute that might need validation.
        assert requests.put('%s/People/%s' % (config.API_BASE_URL, self.person2),
                            data = {'createddate': 13245}).status_code == 400
        # Confirm that the createddate hasn't changed anyway.
        assert currentDate == requests.get('%s/People/%s' % (config.API_BASE_URL, self.person2)).json()['createddate']
        #
        ## Second variation: an internal-only attribute that is only added subsequent to creation.
        # Confirm that the lastmodified datestamp is null.
        # Try to set the lastmodified datestamp. This should fail, too.
        # Confirm that the lastmodified datestamp is _still_ null.
        assert requests.get('%s/People/%s' % (config.API_BASE_URL,
                                              self.person2)).json().get('lastmodified', None) is None
        # Set an attribute.
        assert requests.put('%s/People/%s' % (config.API_BASE_URL, self.person2),
                                              data={self.attr2name: self.attr2val}).status_code == 200
        # Get the updated lastmodified datestamp.
        lastmodified = requests.get('%s/People/%s' % (config.API_BASE_URL,
                                                      self.person2)).json().get('lastmodified', None)
        # Try to set the lastmodified datestamp; the server should return an error.
        assert requests.put('%s/People/%s' % (config.API_BASE_URL, self.person2),
                            data={'lastmodified': 12345643}).status_code == 400
        # Confirm that the lastmodified datestamp hasn't changed.
        assert lastmodified == requests.get('%s/People/%s' %
                                            (config.API_BASE_URL, self.person2)).json().get('lastmodified', None)
        # Remove the resource
        assert requests.delete('%s/People/%s' % (config.API_BASE_URL, self.person2)).status_code == 204
        # Confirm the resource is gone
        assert requests.get('%s/People/%s' % (config.API_BASE_URL, self.person2)).status_code == 404
    def test_readonly_attr_set(self):
        # Upload the file
        fhandle = open(self.file1source, 'rb')
        assert(requests.post('%s/files/' % (config.FILES_BASE_URL),
                             data={'name': self.file1name},
                             files={'file': fhandle}).status_code == 201)
        # Fail to set the checksum
        assert(requests.put('%s/Files/%s' % (config.API_BASE_URL, utilities.sanitise_uid(self.file1name)),
                            data={'sha3256sum': self.file1sha3_256sum_wrong}).status_code == 403)
        # Confirm the checksum is correct
        assert(requests.get('%s/Files/%s' % (config.API_BASE_URL,
                                             utilities.sanitise_uid(self.file1name))).json()['sha3256sum']
               == self.file1sha3_256sum)
        # Delete the file
        requests.delete('%s/%s' % (config.FILES_BASE_URL, utilities.sanitise_uid(self.file1name)))


@pytest.mark.dependency(["TestSchemaUpdates::test_schema_upload"])
class TestAttributeValues(unittest.TestCase):
    res1type = "People"
    res1uid = "Dorian"
    res1attr1name = "real"
    res1attrval1_valid = True
    res1attrval1_invalid = "Banana"
    res1attr2name = "birthyear"
    res1attrval2_valid = 1970
    res1attrval2_invalid1 = 1898
    res1attrval2_invalid2 = 3898
    res1attrval2_invalid3 = "wrong"
    res2type = "Buildings"
    res2uid = "ResidenceOne"
    res2attr1name = "type"
    res2attr1val1_valid = "House"
    res2attr1val1_invalid1 = "Apartment"
    def test_attribute_values(self):
        # Add the test schema
        requests.post(config.SCHEMA_BASE_URL, files={'schema': open('test_schema.json', 'rb')}, data={'create': 'true'})
        # Create the resources
        assert requests.post('%s/%s' % (config.API_BASE_URL, self.res1type),
                             data={"uid": self.res1uid}).status_code == 201
        assert requests.post('%s/%s' % (config.API_BASE_URL, self.res2type),
                             data={"uid": self.res2uid}).status_code == 201
        ## Boolean attribute
        # Set a valid attribute
        assert requests.put('%s/%s/%s' % (config.API_BASE_URL, self.res1type, self.res1uid),
                            data={self.res1attr1name: self.res1attrval1_valid}).status_code == 200
        # Fail to set an invalid value
        assert requests.put('%s/%s/%s' % (config.API_BASE_URL, self.res1type, self.res1uid),
                            data={self.res1attr1name: self.res1attrval1_invalid}).status_code == 400
        ## Integer attribute
        # Fail to set invalid attributes
        # Too low
        assert requests.put('%s/%s/%s' % (config.API_BASE_URL, self.res1type, self.res1uid),
                            data={self.res1attr2name: self.res1attrval2_invalid1}).status_code == 400
        # Too high
        assert requests.put('%s/%s/%s' % (config.API_BASE_URL, self.res1type, self.res1uid),
                            data={self.res1attr2name: self.res1attrval2_invalid2}).status_code == 400
        # Not a number
        assert requests.put('%s/%s/%s' % (config.API_BASE_URL, self.res1type, self.res1uid),
                            data={self.res1attr2name: self.res1attrval2_invalid3}).status_code == 400
        # Set a valid attribute
        assert requests.put('%s/%s/%s' % (config.API_BASE_URL, self.res1type, self.res1uid),
                            data={self.res1attr2name: self.res1attrval2_valid}).status_code == 200
        ## Enum attribute
        # Fail to set a value that isn't a member of the set
        assert requests.put('%s/%s/%s' % (config.API_BASE_URL, self.res2type, self.res2uid),
                            data={self.res2attr1name: self.res2attr1val1_invalid1}).status_code == 400
        # Set a valid value
        assert requests.put('%s/%s/%s' % (config.API_BASE_URL, self.res2type, self.res2uid),
                            data={self.res2attr1name: self.res2attr1val1_valid}).status_code == 200
        # Cleanup
        # Delete the resources
        assert requests.delete('%s/%s/%s' % (config.API_BASE_URL, self.res1type, self.res1uid)).status_code == 204
        assert requests.delete('%s/%s/%s' % (config.API_BASE_URL, self.res2type, self.res2uid)).status_code == 204
        # Remove the schema we added
        versions = requests.get('%s?version=list' % (config.SCHEMA_BASE_URL)).json()
        requests.delete('%s?version=%s' % (config.SCHEMA_BASE_URL, versions['current-version']))
    # - new relationships from an existing resourcetype to a new one.
    # Test robustness against
    # - duplicate attributes in new subschemas
    #     - both within the same document, and in a subsequent one
    # - duplicate relationship definitions
    #     - both within the same document, and in a subsequent one

class TestFilters(unittest.TestCase):
    tag1 = 'foo'
    tag2 = 'bar'
    person1 = 'RgAdmin'
    person2 = 'Bruce'
    def test_filter_for_tags_in_use(self):
        # Create and link the resources
        requests.post('%s/Tags' % (config.API_BASE_URL), data={'uid': self.tag1})
        requests.post('%s/Tags' % (config.API_BASE_URL), data={'uid': self.tag2})
        requests.post('%s/People' % (config.API_BASE_URL), data={'uid': self.person2})
        requests.post('%s/People/%s/TAGS' % (config.API_BASE_URL, self.person2),
                      data={'target': '/Tags/%s' % (self.tag1)})
        # Confirm both tags are there
        assert requests.get('%s/Tags/%s' % (config.API_BASE_URL, self.tag1)).status_code == 200
        assert requests.get('%s/Tags/%s' % (config.API_BASE_URL, self.tag2)).status_code == 200
        # Test the filtering
        response1 = requests.get('%s/Tags?RGinbound=/People/*/TAGS' % config.API_BASE_URL)
        assert response1.status_code == 200
        assert len(response1.json()) == 1
        assert response1.json()[0]['uid'] == self.tag1
        # Delete the resources
        requests.delete('%s/Tags/%s' % (config.API_BASE_URL, self.tag1))
        requests.delete('%s/Tags/%s' % (config.API_BASE_URL, self.tag2))
        requests.delete('%s/People/%s' % (config.API_BASE_URL, self.person2))

#!/usr/bin/env python
# Expects python3

"Test operations on relationships."


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

# Included batteries
import unittest

# Third-party libraries
import pytest
import requests


class TestRelationshipsBasic(unittest.TestCase):
    '''
    The most rudimentary of relationship testing
    '''
    person1 = 'Blake'
    tag1 = 'Idealist'
    invalidrel1 = 'Tagged'
    rgadmin = 'RgAdmin'
    def test_tag_a_person(self):
        # Setup
        requests.post('%s/People/' % (config.API_BASE_URL), data={"uid": self.person1})
        requests.post('%s/Tags/' % (config.API_BASE_URL), data={"uid": self.tag1})
        # Tag the person
        assert requests.post('%s/People/%s/TAGS' % (config.API_BASE_URL, self.person1),
                data={"target": "/Tags/{tag}".format(tag=self.tag1)}).status_code == 201
        # Confirm they've been tagged
        assert len(requests.get('{base}/People/{person}/TAGS/Tags'.format(
                base=config.API_BASE_URL, person=self.person1)).json()) == 1
        # Remove the tag
        assert requests.delete('{base}/People/{person}/TAGS'.format(
                base=config.API_BASE_URL, person=self.person1),
                data={"target": "/Tags/{tag}".format(tag=self.tag1)}).status_code == 204
        # Confirm it's gone
        assert len(requests.get('{base}/People/{person}/TAGS/Tags'.format(
                base=config.API_BASE_URL, person=self.person1)).json()) == 0
        # Teardown: delete the person and the tag
        assert requests.delete('%s/Tags/%s' % (config.API_BASE_URL, self.tag1)).status_code == 204
        assert requests.delete('%s/People/%s' % (config.API_BASE_URL, self.person1)).status_code == 204
        # Confirm teardown
        assert requests.get('%s/People/%s' % (config.API_BASE_URL, self.person1)).status_code == 404
        assert requests.get('%s/Tags/%s' % (config.API_BASE_URL, self.tag1)).status_code == 404
    def test_fail_to_set_creator(self):
        # Setup
        requests.post('%s/People/' % (config.API_BASE_URL), data={"uid": self.person1})
        requests.post('%s/Tags/' % (config.API_BASE_URL), data={"uid": self.tag1})
        # Test
        assert requests.post('%s/Tags/%s/RG_CREATOR' % (config.API_BASE_URL, self.tag1),
                             data={"target": '/People/%s' % (self.person1)}).status_code == 403
        # Teardown
        assert requests.delete('%s/Tags/%s' % (config.API_BASE_URL, self.tag1)).status_code == 204
        assert requests.delete('%s/People/%s' % (config.API_BASE_URL, self.person1)).status_code == 204
        # Confirm teardown
        assert requests.get('%s/People/%s' % (config.API_BASE_URL, self.person1)).status_code == 404
        assert requests.get('%s/Tags/%s' % (config.API_BASE_URL, self.tag1)).status_code == 404
    def test_fail_to_delete_creator(self):
        # Setup
        requests.post('%s/Tags/' % (config.API_BASE_URL), data={"uid": self.tag1})
        # Test
        assert requests.delete('%s/Tags/%s/RG_CREATOR' % (config.API_BASE_URL, self.tag1),
                             data={"target": '/People/%s' % (self.rgadmin)}).status_code == 403
        # Teardown
        assert requests.delete('%s/Tags/%s' % (config.API_BASE_URL, self.tag1)).status_code == 204
        # Confirm teardown
        assert requests.get('%s/Tags/%s' % (config.API_BASE_URL, self.tag1)).status_code == 404
    def test_fail_to_create_invalid_relationship(self):
        # Setup
        requests.post('%s/People/' % (config.API_BASE_URL), data={"uid": self.person1})
        requests.post('%s/%s/' % (config.API_BASE_URL, self.invalidrel1), data={"uid": self.tag1})
        # Test
        assert requests.post('%s/People/%s/MEMBER_OF' % (config.API_BASE_URL, self.person1),
                             data={'target': '/%s/%s' % (self.invalidrel1, self.tag1)}).status_code == 409
        # Teardown
        assert requests.delete('%s/%s/%s' % (config.API_BASE_URL, self.invalidrel1, self.tag1)).status_code == 204
        assert requests.delete('%s/People/%s' % (config.API_BASE_URL, self.person1)).status_code == 204
    def test_fail_to_delete_invalid_relationship(self):
        # Setup
        requests.post('%s/People/' % (config.API_BASE_URL), data={"uid": self.person1})
        requests.post('%s/Tags/' % (config.API_BASE_URL), data={"uid": self.tag1})
        # Test
        response = requests.delete('%s/People/%s/MEMBER_OF' % (config.API_BASE_URL, self.person1),
                                   data={'target': '/Tags/%s' % (self.tag1)})
        assert response.status_code == 400
        # pylint: disable=line-too-long
        assert response.text == "Client error: There is no relationship between these resource-types. Are you sure there's something here to delete?"
        # Teardown
        assert requests.delete('%s/Tags/%s' % (config.API_BASE_URL, self.tag1)).status_code == 204
        assert requests.delete('%s/People/%s' % (config.API_BASE_URL, self.person1)).status_code == 204

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
        requests.post('%s/People/' % (config.API_BASE_URL), data={"uid": self.person1})
        requests.post('%s/Tags/' % (config.API_BASE_URL), data={"uid": self.tag1})
        # Test
        assert requests.post('%s/People/%s' % (config.API_BASE_URL, self.person1),
                             data={'target': '/Tags/%s' % (self.tag1)}).status_code == 400
        # Teardown
        requests.delete('%s/Tags/%s' % (config.API_BASE_URL, self.tag1))
        requests.delete('%s/People/%s' % (config.API_BASE_URL, self.person1))

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
        requests.post(config.SCHEMA_BASE_URL, files={'schema': open('test_schema.json', 'rb')}, data={'create': 'true'})
        # Create the resources
        requests.post('%s/%s' % (config.API_BASE_URL, self.sourceType), data={'uid': self.sourceUid})
        requests.post('%s/%s' % (config.API_BASE_URL, self.targetType), data={'uid': self.targetUid})
        # Create the relationship we're testing for
        requests.post('%s/%s/%s/%s' % (config.API_BASE_URL, self.sourceType, self.sourceUid, self.relationship),
                      data={'target': '/%s/%s' % (self.targetType, self.targetUid)})
        # Delete the resources
        requests.delete('%s/%s/%s' % (config.API_BASE_URL, self.sourceType, self.sourceUid))
        requests.delete('%s/%s/%s' % (config.API_BASE_URL, self.targetType, self.targetUid))
        # Clean up the schema we added
        versions = requests.get('%s?version=list' % (config.SCHEMA_BASE_URL)).json()
        requests.delete('%s?version=%s' % (config.SCHEMA_BASE_URL, versions['current-version']))
    #def test_fail_invalid_rel_to_any(self):

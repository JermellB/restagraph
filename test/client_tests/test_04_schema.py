#!/usr/bin/env python
# Expects python3

"Test operations on the schema."


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


class TestSchemaBasic(unittest.TestCase):
    def test_schema_any(self):
        assert requests.get('%s/any' % (config.SCHEMA_BASE_URL)).json() == {
                "name": "any",
                "attributes": None,
                "dependent": None,
                #! pylint: disable=line-too-long
                "description": "Special-case meta-resource, representing an instance of any type of resource. This is used for defining relationships where either the source or target could be, well, any resourcetype. The server refuses to create an instance of this resourcetype.",
                "relationships": [
                    {
                        "name": "RG_CREATOR",
                        "reltype": "any",
                        "cardinality": "many:1",
                        "description": "All resources are linked to their creator. This is the first part of the permissions-management system.",
                        "target-type": "People"
                        },
                    {
                        "name": "TAGS",
                        "reltype": "any",
                        "cardinality": "many:many",
                        "description": "Any resourcetype can be tagged.",
                        "target-type": "Tags"
                        }
                    ]}
    def test_schema_tags(self):
        assert requests.get('%s/Tags' % (config.SCHEMA_BASE_URL)).json() == {
                "name": "Tags",
                "attributes": [
                    {
                        "name": "description",
                        "type": "varchar",
                        "description": "Clarification of what the tag means.",
                        "maxlength": 256,
                        "readonly": None,
                        "values": None
                        }
                    ],
                "dependent": None,
                "description": "For categorising resources of any type. Useful in searches.",
                "relationships": None
                }


class TestSchemaUpdates(unittest.TestCase):
    def test_schema_atomic_upload_and_delete(self):
        # Check the existing list of versions
        versions_existing = requests.get('%s?version=list' % (config.SCHEMA_BASE_URL)).json()
        assert isinstance(versions_existing['versions'], list)
        assert isinstance(versions_existing['current-version'], int)
        # Upload the file
        assert requests.post(config.SCHEMA_BASE_URL,
                             files={'schema': open('test_schema.json', 'rb')},
                             data={'create': 'true'}).status_code == 201
        # Confirm that the list of versions has gained a newer one, and that the new one is current
        versions_after_create = requests.get('%s?version=list' % (config.SCHEMA_BASE_URL)).json()
        assert len(versions_after_create['versions']) == (len(versions_existing['versions']) + 1)
        assert versions_after_create['current-version'] > versions_existing['current-version']
        # Request the schema and inspect it
        schema = requests.get(config.SCHEMA_BASE_URL).json()
        assert list(filter(lambda x: x['name'] == 'Buildings', schema))[0]['dependent'] is None
        # Delete the schema
        assert requests.delete('%s?version=%s' % (config.SCHEMA_BASE_URL,
                                                  versions_after_create['current-version'])).status_code == 200
        # Confirm that things are back the way they were
        versions_after_delete = requests.get('%s?version=list' % (config.SCHEMA_BASE_URL)).json()
        assert versions_after_delete['current-version'] == versions_existing['current-version']
        assert sorted(versions_after_delete['versions']) == sorted(versions_existing['versions'])
    def test_schema_stepwise_upload_and_delete(self):
        # Check the existing list of versions
        versions_existing = requests.get('%s?version=list' % (config.SCHEMA_BASE_URL)).json()
        assert isinstance(versions_existing['versions'], list)
        assert isinstance(versions_existing['current-version'], int)
        # Create a new schema-version, without adding anything to it
        assert requests.post(config.SCHEMA_BASE_URL, data={'create': 'true'}).status_code == 201
        # Confirm that the list of versions has gained a newer one, and that the new one is current
        versions_after_create = requests.get('%s?version=list' % (config.SCHEMA_BASE_URL)).json()
        assert len(versions_after_create['versions']) == (len(versions_existing['versions']) + 1)
        assert versions_after_create['current-version'] > versions_existing['current-version']
        # Upload the file
        assert requests.post(config.SCHEMA_BASE_URL,
                             files={'schema': open('test_schema.json', 'rb')}).status_code == 201
        # Request the schema and inspect it
        schema = requests.get(config.SCHEMA_BASE_URL).json()
        assert list(filter(lambda x: x['name'] == 'Buildings', schema))[0]['dependent'] is None
        # Set the current-version back to the previous current, and confirm
        assert requests.put('%s?version=%s' % (config.SCHEMA_BASE_URL,
                                               versions_existing['current-version'])).status_code == 200
        versions_after_current_change = requests.get('%s?version=list' % (config.SCHEMA_BASE_URL)).json()
        assert versions_after_current_change['current-version'] == versions_existing['current-version']
        assert sorted(versions_after_current_change['versions']) == sorted(versions_after_create['versions'])
        # Delete the schema we created
        assert requests.delete('%s?version=%s' % (config.SCHEMA_BASE_URL,
                                                  versions_after_create['current-version'])).status_code == 200
        # Confirm that things are back the way they were
        versions_after_delete = requests.get('%s?version=list' % (config.SCHEMA_BASE_URL)).json()
        assert versions_after_delete['current-version'] == versions_existing['current-version']
        assert sorted(versions_after_delete['versions']) == sorted(versions_existing['versions'])

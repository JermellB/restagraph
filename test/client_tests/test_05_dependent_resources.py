#!/usr/bin/env python
# Expects python3

"Test operations on dependent resources."


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


class TestDependentResources(unittest.TestCase):
    res1type = "Buildings"
    res1uid = "Xenon"
    depres1rel = "FLOORS"
    depres1type = "Floors"
    depres1uid = "Hangar"
    depres1attr1 = "description"
    depres1parentrel = "IN_BUILDING"
    res2uid = "ResidenceOne"
    depres2rel = "ROOMS"
    depres2type = "Rooms"
    depres2uid = "Toolshed"
    owner1type = "People"
    owner1uid = "Frank"
    invalidparenttype1 = "Building"   # Minor typo, for added realism
    invalidreltype1 = "FLOOR"
    invaliddeptype1 = "Floor"
    def test_dependent_resourcetypes_basic(self):
        # Add the test schema
        requests.post(config.SCHEMA_BASE_URL, files={'schema': open('test_schema.json', 'rb')}, data={'create': 'true'})
        # Create the parent resource
        requests.post('%s/%s' % (config.API_BASE_URL, self.res1type), data={"uid": self.res1uid})
        # Create the owner
        requests.post('%s/%s' % (config.API_BASE_URL, self.owner1type), data={"uid": self.owner1uid})
        # Connect the building to its owner
        assert requests.post('%s/%s/%s/OWNER' % (config.API_BASE_URL, self.res1type, self.res1uid),
                             data={"target": '/%s/%s' % (self.owner1type, self.owner1uid)}).status_code == 201
        # Fail to connect the owner to the building because it isn't dependent
        assert requests.post('%s/%s/%s/OWNS' % (config.API_BASE_URL, self.owner1type, self.owner1uid),
                             data={"target": '/%s/%s' % (self.res1type, self.res1uid)}).status_code == 409
        # Delete the owner
        requests.delete('%s/%s/%s' % (config.API_BASE_URL, self.owner1type, self.owner1uid))
        # Fail to create the dependent resource at the top level
        assert requests.post('%s/%s/%s' % (config.API_BASE_URL,
                                           self.depres1rel,
                                           self.depres1type),
                             data={"uid": self.depres1uid}).status_code == 400
        # Create the dependent resource
        assert requests.post('%s/%s/%s/%s/%s' % (config.API_BASE_URL,
                                                 self.res1type,
                                                 self.res1uid,
                                                 self.depres1rel,
                                                 self.depres1type),
                             data={"uid": self.depres1uid}).status_code == 201
        # Confirm the dependent resource is there
        assert requests.get('%s/%s/%s/%s/%s/%s' % (config.API_BASE_URL,
                                                   self.res1type,
                                                   self.res1uid,
                                                   self.depres1rel,
                                                   self.depres1type,
                                                   self.depres1uid)).status_code == 200
        # Confirm the dependent resource can't be fetched directly
        assert requests.get('%s/%s/%s' % (config.API_BASE_URL,
                                          self.depres1type,
                                          self.depres1uid)).status_code == 400
        # Confirm the dependent resource can't be deleted directly
        assert requests.delete('%s/%s/%s' % (config.API_BASE_URL,
                                             self.depres1type,
                                             self.depres1uid)).status_code == 400
        # Confirm the dependent resource can't be modified directly
        assert requests.post('%s/%s/%s' % (config.API_BASE_URL,
                                           self.depres1type,
                                           self.depres1uid),
                             data={self.depres1attr1: "blah"}).status_code == 400
        # Delete the dependent resource
        assert requests.delete('%s/%s/%s/%s/%s/%s' % (config.API_BASE_URL,
                                                      self.res1type,
                                                      self.res1uid,
                                                      self.depres1rel,
                                                      self.depres1type,
                                                      self.depres1uid)).status_code == 204
        # Confirm it's gone
        assert requests.get('%s/%s/%s/%s/%s/%s' % (config.API_BASE_URL,
                                                   self.res1type,
                                                   self.res1uid,
                                                   self.depres1rel,
                                                   self.depres1type,
                                                   self.depres1uid)).status_code == 404
        # Confirm the parent is still there
        assert requests.get('%s/%s/%s' % (config.API_BASE_URL,
                                          self.res1type,
                                          self.res1uid)).status_code == 200
        # Clean up by deleting the parent resource
        assert requests.delete('%s/%s/%s' % (config.API_BASE_URL,
                                             self.res1type,
                                             self.res1uid)).status_code == 204
        # Clean up the schema we added
        versions = requests.get('%s?version=list' % (config.SCHEMA_BASE_URL)).json()
        requests.delete('%s?version=%s' % (config.SCHEMA_BASE_URL, versions['current-version']))
    def test_dependent_resourcetypes_recursive_1(self):
        "Check for recursive deletion, from the parent down 1 level."
        # Add the test schema
        requests.post(config.SCHEMA_BASE_URL, files={'schema': open('test_schema.json', 'rb')}, data={'create': 'true'})
        # Create the parent resource
        assert requests.post('%s/%s' % (config.API_BASE_URL, self.res1type),
                             data={"uid": self.res1uid}).status_code == 201
        # Create the dependent resource
        assert requests.post('%s/%s/%s/%s/%s' % (config.API_BASE_URL,
                                              self.res1type,
                                              self.res1uid,
                                              self.depres1rel,
                                              self.depres1type),
                             data={"uid": self.depres1uid}).status_code == 201
        # Confirm the dependent resource is there
        assert requests.get('%s/%s/%s/%s/%s/%s' % (config.API_BASE_URL,
                                                self.res1type,
                                                self.res1uid,
                                                self.depres1rel,
                                                self.depres1type,
                                                self.depres1uid)).status_code == 200
        # Recursively delete the parent resource
        assert requests.delete('%s/%s/%s?recursive=true' % (config.API_BASE_URL,
                                                            self.res1type,
                                                            self.res1uid)).status_code == 204
        # Confirm the child is gone
        assert requests.get('%s/%s/%s/%s/%s/%s' % (config.API_BASE_URL,
                                                self.res1type,
                                                self.res1uid,
                                                self.depres1rel,
                                                self.depres1type,
                                                self.depres1uid)).status_code == 404
        # Confirm the parent is gone
        assert requests.get('%s/%s/%s' % (config.API_BASE_URL, self.res1type, self.res1uid)).status_code == 404
        # Clean up the schema we added
        versions = requests.get('%s?version=list' % (config.SCHEMA_BASE_URL)).json()
        requests.delete('%s?version=%s' % (config.SCHEMA_BASE_URL, versions['current-version']))
    def test_dependent_resourcetypes_recursive_2(self):
        """
        Create a grandchild resource, and check recursive deletion from the parent.
        - check for orphaned grandchildren.
        - check for recursive deletion, from the parent down 2 levels.
        """
        # Add the test schema
        requests.post(config.SCHEMA_BASE_URL, files={'schema': open('test_schema.json', 'rb')}, data={'create': 'true'})
        # Create the parent resource
        assert requests.post('%s/%s' % (config.API_BASE_URL, self.res1type),
                             data={"uid": self.res1uid}).status_code == 201
        # Create the child resource
        assert requests.post('%s/%s/%s/%s/%s' % (config.API_BASE_URL,
                                              self.res1type,
                                              self.res1uid,
                                              self.depres1rel,
                                              self.depres1type),
                             data={"uid": self.depres1uid}).status_code == 201
        # Create the grandchild resource
        assert requests.post('%s/%s/%s/%s/%s/%s/%s/%s' % (config.API_BASE_URL,
                                                          self.res1type,
                                                          self.res1uid,
                                                          self.depres1rel,
                                                          self.depres1type,
                                                          self.depres1uid,
                                                          self.depres2rel,
                                                          self.depres2type),
                             data={"uid": self.depres2uid}).status_code == 201
        # Confirm the grandchild resource is there
        assert requests.get('%s/%s/%s/%s/%s/%s/%s/%s/%s' % (config.API_BASE_URL,
                                                            self.res1type,
                                                            self.res1uid,
                                                            self.depres1rel,
                                                            self.depres1type,
                                                            self.depres1uid,
                                                            self.depres2rel,
                                                            self.depres2type,
                                                            self.depres2uid)).status_code == 200
        # Recursively delete the parent resource
        assert requests.delete('%s/%s/%s?recursive=true' % (config.API_BASE_URL,
                                                            self.res1type,
                                                            self.res1uid)).status_code == 204
        # Confirm the grandchild is gone
        assert requests.get('%s/%s/%s/%s/%s/%s/%s/%s/%s' % (config.API_BASE_URL,
                                                            self.res1type,
                                                            self.res1uid,
                                                            self.depres1rel,
                                                            self.depres1type,
                                                            self.depres1uid,
                                                            self.depres2rel,
                                                            self.depres2type,
                                                            self.depres2uid)).status_code == 404
        # Confirm the child is gone
        assert requests.get('%s/%s/%s/%s/%s/%s' % (config.API_BASE_URL,
                                                self.res1type,
                                                self.res1uid,
                                                self.depres1rel,
                                                self.depres1type,
                                                self.depres1uid)).status_code == 404
        # Confirm the parent is gone
        assert requests.get('%s/%s/%s' % (config.API_BASE_URL,
                                          self.res1type,
                                          self.res1uid)).status_code == 404
        # Clean up the schema we added
        versions = requests.get('%s?version=list' % (config.SCHEMA_BASE_URL)).json()
        requests.delete('%s?version=%s' % (config.SCHEMA_BASE_URL, versions['current-version']))
        ## Test failure modes
    def test_dependent_resourcetype_parent_restrictions(self):
        """
        Confirm that the server prevents a dependent resource having two parents
        via the same relationship.
        """
        # Add the test schema
        requests.post(config.SCHEMA_BASE_URL, files={'schema': open('test_schema.json', 'rb')}, data={'create': 'true'})
        # Create the parent resource
        requests.post('%s/%s' % (config.API_BASE_URL, self.res1type), data={"uid": self.res1uid})
        # Create the dependent resource
        assert requests.post('%s/%s/%s/%s/%s' % (config.API_BASE_URL,
                                                 self.res1type,
                                                 self.res1uid,
                                                 self.depres1rel,
                                                 self.depres1type),
                             data={"uid": self.depres1uid}).status_code == 201
        # Add the new parent resource
        requests.post('%s/%s' % (config.API_BASE_URL, self.res1type), data={"uid": self.res2uid})
        # Fail to add a duplicate dependent relationship from the new parent
        assert requests.post('%s/%s/%s/%s' % (config.API_BASE_URL,
                                              self.res1type,
                                              self.res2uid,
                                              self.depres1rel),
                             data={"target": '/%s/%s/%s/%s/%s' % (self.res1type,
                                                                  self.res1uid,
                                                                  self.depres1rel,
                                                                  self.depres1type,
                                                                  self.depres1uid)}).status_code == 409
        # Move the child to the new parent
        assert requests.post('%s/%s/%s/%s/%s/%s' % (config.API_BASE_URL,
                                                    self.res1type,
                                                    self.res1uid,
                                                    self.depres1rel,
                                                    self.depres1type,
                                                    self.depres1uid),
                             data={"target": '/%s/%s/%s' % (self.res1type,
                                                            self.res2uid,
                                                            self.depres1rel)}).status_code == 201
        # Confirm that the child is now accessible via the new parent
        assert requests.get('%s/%s/%s/%s/%s/%s' % (config.API_BASE_URL,
                                                   self.res1type,
                                                   self.res2uid,
                                                   self.depres1rel,
                                                   self.depres1type,
                                                   self.depres1uid)).status_code == 200
        # Confirm that the child is *not* accessible via the *old* parent
        assert requests.get('%s/%s/%s/%s/%s/%s' % (config.API_BASE_URL,
                                                   self.res1type,
                                                   self.res1uid,
                                                   self.depres1rel,
                                                   self.depres1type,
                                                   self.depres1uid)).status_code == 404
        # Remove both parents, recursively to ensure the child is gone
        assert requests.delete('%s/%s/%s?recursive=true' % (config.API_BASE_URL,
                                                            self.res1type,
                                                            self.res1uid)).status_code == 204
        assert requests.delete('%s/%s/%s?recursive=true' % (config.API_BASE_URL,
                                                            self.res1type,
                                                            self.res2uid)).status_code == 204
        # Clean up the schema we added
        versions = requests.get('%s?version=list' % (config.SCHEMA_BASE_URL)).json()
        requests.delete('%s?version=%s' % (config.SCHEMA_BASE_URL, versions['current-version']))
    def test_dependent_resourcetypes_failures(self):
        # Add the test schema
        requests.post(config.SCHEMA_BASE_URL, files={'schema': open('test_schema.json', 'rb')}, data={'create': 'true'})
        # Try to link a parent resource of invalid type to a dependent resource, neither of which actually exist.
        # Yes, this is a real test. Yes, the server stacktraced if you did that, so it's a regression test.
        assert requests.post('%s/%s/%s/%s' % (config.API_BASE_URL,
                                              self.invalidparenttype1,
                                              self.res1uid,
                                              self.depres1rel),
                             data={"target": '/%s/%s' % (self.depres1type, self.depres1uid)}).status_code == 409
        # Try to link a parent resource to a dependent resource of invalid type, neither of which exist.
        assert requests.post('%s/%s/%s/%s' % (config.API_BASE_URL, self.res1type , self.res1uid, self.depres1rel),
                             data={"target": '/%s/%s' % (self.invaliddeptype1, self.depres1uid)}).status_code == 409
        # Try to link a parent resource to a dependent resource, neither of which exist, via an invalid relationship.
        assert requests.post('%s/%s/%s/%s' % (config.API_BASE_URL, self.res1type , self.res1uid, self.invalidreltype1),
                             data={"target": '/%s/%s' % (self.depres1type, self.depres1uid)}).status_code == 409
        # Try to add a dependent resource from a nonexistent parent type
        assert requests.post('%s/%s/%s/%s/%s' % (config.API_BASE_URL,
                                                 self.invalidparenttype1,
                                                 self.res1uid,
                                                 self.depres1rel,
                                                 self.depres1type),
                             data={"uid": self.depres1uid}).status_code == 400
        # Try to add a dependent resource of a type that doesn't exist
        assert requests.post('%s/%s/%s/%s/%s' % (config.API_BASE_URL,
                                                 self.res1type,
                                                 self.res1uid,
                                                 self.depres1rel,
                                                 self.invaliddeptype1),
                             data={"uid": self.depres1uid}).status_code == 400
        # Try to create a dependent resource of a parent that doesn't exist, where both types are valid.
        assert requests.post('%s/%s/%s/%s/%s' % (config.API_BASE_URL,
                                                 self.res1type,
                                                 self.res1uid,
                                                 self.depres1rel,
                                                 self.depres1type),
                             data={"uid": self.depres1uid}).status_code == 400
        # Try to create a link from a dependent resource as though it's a top-level one.
        requests.post('%s/%s' % (config.API_BASE_URL, self.res1type), data={"uid": self.res1uid})
        requests.post('%s/%s/%s/%s/%s' % (config.API_BASE_URL,
                                          self.res1type,
                                          self.res1uid,
                                          self.depres1rel,
                                          self.depres1type),
                      data={"uid": self.depres1uid})
        assert requests.post('%s/%s/%s' % (config.API_BASE_URL, self.depres1type, self.depres1uid),
                             data={"target": '/%s/%s' % (self.res1type, self.res1uid)}).status_code == 400
        requests.delete('%s/%s/%s/%s/%s/%s' % (config.API_BASE_URL,
                                               self.res1type,
                                               self.res1uid,
                                               self.depres1rel,
                                               self.depres1type,
                                               self.depres1uid))
        requests.delete('%s/%s/%s' % (config.API_BASE_URL, self.res1type, self.res1uid))
        # Clean up the schema we added
        versions = requests.get('%s?version=list' % (config.SCHEMA_BASE_URL)).json()
        requests.delete('%s?version=%s' % (config.SCHEMA_BASE_URL, versions['current-version']))
    #
    # Check that `recursive=true` _only_ deletes dependent resources,
    # and _doesn't_ go on a rampage.
    # Check that _only_ dependent resources can be created with a dependent relationship.
    # Check that dependent relationships _cannot_ be created to existing resources, whether dependent or not.
    # Positively confirm both cases.
    # Check that the `recursive` parameter really does work in both GET- and POST-styles.

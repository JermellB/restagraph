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
    resourcetype = 'routers'
    uid = 'amchitka'
    comment = 'Test router'
    def test_create_and_delete_single_resource(self):
        # Ensure it's not already present
        self.assertEqual(requests.get('%s/%s/%s' % (BASE_URL, self.resourcetype, self.uid)).status_code, 404)
        # Create it
        self.assertEqual(
                requests.post('%s/%s' % (BASE_URL, self.resourcetype),
                    data={'uid': self.uid, 'comment': self.comment}).status_code,
                201)
        # Confirm that it's now there
        self.assertEqual(requests.get('%s/%s/%s' % (BASE_URL, self.resourcetype, self.uid)).json(),
                { 'uid': self.uid, 'comment': self.comment })
        # Delete it
        self.assertEqual(
                requests.delete('%s/%s' % (BASE_URL, self.resourcetype), data={'uid': self.uid}).status_code,
                200)
        # Confirm it's gone
        self.assertEqual(requests.get('%s/%s/%s' % (BASE_URL, self.resourcetype, self.uid)).status_code, 404)

# Make it happen

if __name__ == '__main__':
    unittest.main()

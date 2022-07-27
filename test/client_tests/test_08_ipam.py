#!/usr/bin/env python
# Expects python3

"Test operations on primary (i.e, non-dependent) resources."


#   Copyright 2017-2022 James Fleming <james@electronic-quill.net>
#
#   Licensed under the GNU General Public License
#   - for details, see LICENSE.txt in the top-level directory


# pylint: disable=relative-beyond-top-level
# pylint: disable=missing-function-docstring
# pylint: disable=wrong-import-order

# Local stuff
from . import config

# Included batteries
import unittest

# Third-party libraries
import pytest
import requests


@pytest.mark.dependency("TestDependentResources")
class TestIpv4SubnetsBasicNoVrf(unittest.TestCase):
    '''
    Basic CRD functions for IPv4 subnets.
    '''
    subnet1 = '172.16.0.0/12'
    organisation = 'testco'
    def test_create_and_delete_single_subnet(self):
        print('Test: test_create_and_delete_single_subnet')
        # Create the organisation that it goes under
        self.assertEqual(requests.post('%s/Organisations' % (config.API_BASE_URL),
                                       data={'uid': self.organisation}).status_code,
                         201)
        # Add the subnet
        self.assertEqual(requests.post('%s/subnets' % (config.IPAM_BASE_URL),
                                       data={'org': self.organisation,
                                             'subnet': self.subnet1}).status_code,
                         201)
        # Confirm it's there
        self.assertEqual(requests.get('%s/subnets?subnet=%s&org=%s'
                                      % (config.IPAM_BASE_URL,
                                         self.subnet1,
                                         self.organisation)).status_code,
                         200)
        self.assertEqual(requests.get('%s/subnets?subnet=%s&org=%s'
                                      % (config.IPAM_BASE_URL,
                                         self.subnet1,
                                         self.organisation)).text,
                         "/Organisations/testco/SUBNETS/Ipv4Subnets/172.16.0.0_12")
        # Delete it
        self.assertEqual(requests.delete('%s/subnets' % (config.IPAM_BASE_URL),
                                         data={'org': self.organisation,
                                               'subnet': self.subnet1}).status_code,
                         204)
        # Ensure it's gone
        self.assertEqual(requests.get('%s/subnets?subnet=%s&org=%s'
                                      % (config.IPAM_BASE_URL,
                                         self.subnet1,
                                         self.organisation)).status_code,
                         404)
        # Remove the organisation
        self.assertEqual(requests.delete('%s/Organisations/%s'
                                         % (config.API_BASE_URL, self.organisation)).status_code,
                         204)

@pytest.mark.dependency("TestDependentResources")
class TestIpv6SubnetsBasicNoVrf(unittest.TestCase):
    '''
    Basic CRD functions for IPv6 subnets.
    '''
    subnet1 = '2001:db8::/32'
    organisation = 'testco'
    def test_create_and_delete_single_subnet(self):
        print('Test: test_create_and_delete_single_subnet')
        # Create the organisation that it goes under
        self.assertEqual(requests.post('%s/Organisations' % (config.API_BASE_URL),
                                       data={'uid': self.organisation}).status_code,
                         201)
        # Add the subnet
        self.assertEqual(requests.post('%s/subnets' % (config.IPAM_BASE_URL),
                                       data={'org': self.organisation,
                                             'subnet': self.subnet1}).status_code,
                         201)
        # Confirm it's there
        self.assertEqual(requests.get('%s/subnets?subnet=%s&org=%s'
                                      % (config.IPAM_BASE_URL,
                                         self.subnet1,
                                         self.organisation)).status_code,
                         200)
        self.assertEqual(requests.get('%s/subnets?subnet=%s&org=%s'
                                      % (config.IPAM_BASE_URL,
                                         self.subnet1,
                                         self.organisation)).text,
                         "/Organisations/testco/SUBNETS/Ipv6Subnets/2001:0db8:0000:0000:0000:0000:0000:0000_32")
        # Delete it
        self.assertEqual(requests.delete('%s/subnets' % (config.IPAM_BASE_URL),
                                         data={'org': self.organisation,
                                               'subnet': self.subnet1}).status_code,
                         204)
        # Ensure it's gone
        self.assertEqual(requests.get('%s/subnets?subnet=%s&org=%s'
                                      % (config.IPAM_BASE_URL,
                                         self.subnet1,
                                         self.organisation)).status_code,
                         404)
        # Remove the organisation
        self.assertEqual(requests.delete('%s/Organisations/%s'
                                         % (config.API_BASE_URL, self.organisation)).status_code,
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
        self.assertEqual(requests.post('%s/Organisations' % (config.API_BASE_URL),
                                       data={'uid': self.organisation}).status_code,
                         201)
        # Add the subnet
        self.assertEqual(requests.post('%s/subnets' % (config.IPAM_BASE_URL),
                                       data={'org': self.organisation,
                                             'subnet': self.subnet1}).status_code,
                         201)
        # Add the address
        self.assertEqual(requests.post('%s/addresses' % (config.IPAM_BASE_URL),
                                       data={'org': self.organisation,
                                             'address': self.address1}).status_code,
                         201)
        # Confirm it's there
        self.assertEqual(requests.get('%s/addresses?address=%s&org=%s'
                                      % (config.IPAM_BASE_URL, self.address1, self.organisation)).status_code,
                         200)
        # Delete it
        self.assertEqual(requests.delete('%s/addresses' % (config.IPAM_BASE_URL),
                                         data={'org': self.organisation,
                                               'address': self.address1}).status_code,
                         204)
        # Ensure it's gone
        self.assertEqual(requests.get('%s/addresses?address=%s&org=%s'
                                      % (config.IPAM_BASE_URL, self.address1, self.organisation)).status_code,
                         404)
        # Remove the organisation and subnet
        self.assertEqual(requests.delete('%s/Organisations/%s'
                                         % (config.API_BASE_URL, self.organisation),
                                         data={'recursive': 'true'}).status_code,
                         204)

@pytest.mark.dependency("TestIpv4SubnetsBasicNoVrf")
class TestIpv6AddressesBasicNoVrf(unittest.TestCase):
    '''
    Basic CRD functions for Ipv6 addresses
    '''
    org = 'testco'
    subnet1 = '2001:db8::/32'
    address1 = '2001:db8::4'
    organisation = 'testcorp'
    def test_create_and_delete_single_address(self):
        print('Test: test_create_and_delete_single_address')
        # Create the organisation that it goes under
        self.assertEqual(requests.post('%s/Organisations' % (config.API_BASE_URL),
                                       data={'uid': self.organisation}).status_code,
                         201)
        # Add the subnet
        self.assertEqual(requests.post('%s/subnets' % (config.IPAM_BASE_URL),
                                       data={'org': self.organisation,
                                             'subnet': self.subnet1}).status_code,
                         201)
        # Add the address
        self.assertEqual(requests.post('%s/addresses' % (config.IPAM_BASE_URL),
                                       data={'org': self.organisation,
                                             'address': self.address1}).status_code,
                         201)
        # Confirm it's there
        self.assertEqual(requests.get('%s/addresses?address=%s&org=%s'
                                      % (config.IPAM_BASE_URL, self.address1, self.organisation)).status_code,
                         200)
        # Delete it
        self.assertEqual(requests.delete('%s/addresses' % (config.IPAM_BASE_URL),
                                         data={'org': self.organisation,
                                               'address': self.address1}).status_code,
                         204)
        # Ensure it's gone
        self.assertEqual(requests.get('%s/addresses?address=%s&org=%s'
                                      % (config.IPAM_BASE_URL, self.address1, self.organisation)).status_code,
                         404)
        # Remove the organisation and subnet
        self.assertEqual(requests.delete('%s/Organisations/%s'
                                         % (config.API_BASE_URL, self.organisation),
                                         data={'recursive': 'true'}).status_code,
                         204)

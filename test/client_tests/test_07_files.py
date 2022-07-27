#!/usr/bin/env python
# Expects python3

"Test operations on relationships."


#   Copyright 2017-2022 James Fleming <james@electronic-quill.net>
#
#   Licensed under the GNU General Public License
#   - for details, see LICENSE.txt in the top-level directory


# pylint: disable=relative-beyond-top-level
# pylint: disable=missing-function-docstring
# pylint: disable=wrong-import-order

# Local stuff
from . import config
from . import utilities

# Included batteries
import filecmp
import os
import unittest

# Third-party libraries
import pytest
import requests


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
            requests.post('%s/files/' % (config.FILES_BASE_URL),
                          data={'name': self.file1name},
                          files={'file': fhandle}).status_code,
            201)
        fhandle.close()
        print('Test: file metadata')
        result = requests.get('%s/Files/%s' % (config.API_BASE_URL, utilities.sanitise_uid(self.file1name)))
        self.assertEqual(result.status_code, 200)
        self.assertEqual(result.json()['title'], self.file1name)
        self.assertEqual(result.json()['sha3256sum'], self.file1sha3_256sum)
        print('Test: file content')
        result = requests.get('%s/%s' % (config.FILES_BASE_URL, utilities.sanitise_uid(self.file1name)))
        # Write it to a file
        fhandle = open('testfile', 'wb')
        fhandle.write(result.content)
        fhandle.close()
        # Confirm that the file we got back is the same as the one we uploaded
        self.assertTrue(filecmp.cmp(self.file1source, 'testfile'))
        # Clean up
        os.remove('testfile')
        print('Test: file deletion')
        self.assertEqual(requests.delete('%s/%s' % (config.FILES_BASE_URL,
                                                    utilities.sanitise_uid(self.file1name))).status_code,
                         204)
        result = requests.get('%s/files/%s' % (config.API_BASE_URL, utilities.sanitise_uid(self.file1name)))
        self.assertEqual(result.status_code, 404)
    @pytest.mark.dependency()
    def test_files_conditional_deletion(self):
        print("Test: only delete files from the filesystem if they're fully dereferenced.")
        # Upload the first file
        fhandle = open(self.file1source, 'rb')
        self.assertEqual(
            requests.post('%s/files/' % (config.FILES_BASE_URL),
                          data={'name': self.file1name},
                          files={'file': fhandle}).status_code,
            201)
        fhandle.close()
        # Upload the second file
        fhandle = open(self.file2source, 'rb')
        self.assertEqual(
            requests.post('%s/files/' % (config.FILES_BASE_URL),
                          data={'name': self.file2name},
                          files={'file': fhandle}).status_code,
            201)
        fhandle.close()
        # Delete the first file
        self.assertEqual(requests.delete('%s/%s' % (config.FILES_BASE_URL,
                                                    utilities.sanitise_uid(self.file1name))).status_code,
                         204)
        # Confirm that the second file can still be downloaded
        result = requests.get('%s/%s' % (config.FILES_BASE_URL, utilities.sanitise_uid(self.file2name)))
        # Write it to a file
        fhandle = open('testfile', 'wb')
        fhandle.write(result.content)
        fhandle.close()
        # Confirm that the file we got back is the same as the one we uploaded
        self.assertTrue(filecmp.cmp(self.file2source, 'testfile'))
        # Clean up
        os.remove('testfile')
        # Delete the second file
        self.assertEqual(requests.delete('%s/%s' % (config.FILES_BASE_URL,
                                                    utilities.sanitise_uid(self.file2name))).status_code,
                         204)
        # Confirm that it's no longer there
        result = requests.get('%s/Files/%s' % (config.API_BASE_URL, utilities.sanitise_uid(self.file2name)))
        self.assertEqual(result.status_code, 404)

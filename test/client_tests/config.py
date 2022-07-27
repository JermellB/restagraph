#!/usr/bin/env python
# Expects python3

"Shared config values"


#   Copyright 2017-2022 James Fleming <james@electronic-quill.net>
#
#   Licensed under the GNU General Public License
#   - for details, see LICENSE.txt in the top-level directory


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

#!/usr/bin/env python
# Expects python3

"Utility functions"


#   Copyright 2017-2022 James Fleming <james@electronic-quill.net>
#
#   Licensed under the GNU General Public License
#   - for details, see LICENSE.txt in the top-level directory


# Included batteries
import re


def sanitise_uid(uid):
    '''
    Sanitise a UID string in the same way Restagraph does
    '''
    return re.sub('[^A-Za-z0-9._~-]', '', re.sub('[/ ]', '_', uid))

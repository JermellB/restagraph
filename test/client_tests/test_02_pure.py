#!/usr/bin/env python
# Expects python3

"Test pure functions, i.e. those without side-effects."


#   Copyright 2017-2022 James Fleming <james@electronic-quill.net>
#
#   Licensed under the GNU General Public License
#   - for details, see LICENSE.txt in the top-level directory


# pylint: disable=relative-beyond-top-level
# pylint: disable=missing-function-docstring
# pylint: disable=wrong-import-order

# Local stuff
from . import utilities

# Included batteries
import unittest

# Third-party libraries
import pytest


class TestSanitiseUid(unittest.TestCase):
    "Check that sanitise_uid() does what I want it to."
    def test_sanitise_uid(self):
        assert utilities.sanitise_uid("foo bar") == "foo_bar"
        assert utilities.sanitise_uid("foo-bar") == "foo-bar"
        assert utilities.sanitise_uid("foo1bar") == "foo1bar"
        assert utilities.sanitise_uid('foo=bar') == 'foobar'
        assert utilities.sanitise_uid('foo,bar') == 'foobar'
        assert utilities.sanitise_uid('foo$bar') == 'foobar'

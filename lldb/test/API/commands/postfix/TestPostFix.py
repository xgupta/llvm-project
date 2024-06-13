"""
Test postfix with different postfix expressions.
"""

import re
import lldb
from lldbsuite.test.lldbtest import *
from lldbsuite.test.decorators import *
import lldbsuite.test.lldbutil as lldbutil

class TestPostfixCommand(TestBase):

    def test_simple_postfix_command(self):
        """Test the postfix command."""
        self.build()
        self.runCmd("postfix 12+")
        self.expect("postfix 12+", substrs=["3"])

    def test_complex_postfix_command(self):
        """Test the complex postfix command."""
        self.build()
        self.runCmd("postfix 12*3+")
        self.expect("postfix 12*3+", substrs=["5"])

if __name__ == '__main__':
    unittest.main()
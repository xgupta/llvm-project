"""
Test infix with different postfix expressions.
"""

import re
import lldb
from lldbsuite.test.lldbtest import *
from lldbsuite.test.decorators import *
import lldbsuite.test.lldbutil as lldbutil

class TestInfixCommand(TestBase):

    def test_simple_infix_command(self):
        """Test the infix command."""
        self.build()
        self.runCmd("infix 12+")
        self.expect("infix 12+", substrs=["(1+2)"])

    def test_complex_infix_command(self):
        """Test the complex infix command."""
        self.build()
        self.runCmd("infix ab*c+")
        self.expect("infix ab*c+", substrs=["((a*b)+c)"])

if __name__ == '__main__':
    unittest.main()

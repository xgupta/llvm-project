"""Test that Cobol global variables can be inspected by name."""

from __future__ import print_function


from lldbsuite.test.decorators import *
from lldbsuite.test.lldbtest import *
from lldbsuite.test import lldbutil


class GlobalVariablesCobolTestCase(TestBase):

    mydir = TestBase.compute_mydir(__file__)

    def setUp(self):
        TestBase.setUp(self)
        self.source = lldb.SBFileSpec('gvar.cob')

    @add_test_categories(["dwarf"])
    @skipUnlessPlatform(["linux"])
    def test(self):
        self.build()

        (target, _, _, _) = lldbutil.run_to_source_breakpoint(self, "// Set break point #1", self.source)

        # Check that we can access g_file_global_int by its name
        self.expect("target variable GLB", VARIABLES_DISPLAYED_CORRECTLY,
                    substrs=['GLOBAL'])
        self.expect("target variable glb", VARIABLES_DISPLAYED_CORRECTLY,
                   substrs=['GLOBAL'])

        self.expect("target variable glb1", VARIABLES_DISPLAYED_CORRECTLY,
                    error=True, substrs=['can\'t find global variable'])
        self.expect("target variable GLB1", VARIABLES_DISPLAYED_CORRECTLY,
                    error=True, substrs=['can\'t find global variable'])


        self.expect("p GLB", substrs=['[8]) $', 'GLOBAL'])
        self.expect("p glb", substrs=['[8]) $', 'GLOBAL'])

        self.expect("fr v GLB", substrs=['[8]) $', 'GLOBAL'])
        self.expect("fr v glb", substrs=['[8]) $', 'GLOBAL'])

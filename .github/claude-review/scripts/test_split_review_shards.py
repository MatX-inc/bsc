"""Regression coverage for BSC source layout and language fallbacks."""

import os
import sys
import unittest

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import split_review_shards as shards


class TestBscShards(unittest.TestCase):
    def test_bsc_sources_and_build_files(self):
        cases = {
            "src/comp/ASyntax.hs": "compiler",
            "src/Parsec/Parsec.hs": "compiler",
            "src/Libraries/Base1/Prelude.bs": "libraries",
            "src/Libraries/Makefile": "build",
            "src/Verilog/FIFO2.v": "hdl",
            "src/Verilog.Quartus/BRAM2.v": "hdl",
            "src/Verilog.Vivado/BRAM2.v": "hdl",
            "src/bluesim/bs_kernel.cxx": "runtime",
            "src/VPI/libbdpi.c": "runtime",
            "src/bluetcl/bluespec.tcl": "runtime",
            "src/vendor/yices/solver.c": "solvers",
            "src/vendor/stp/CMakeLists.txt": "build",
            "testsuite/bsc.bugs/b123/Example.bsv": "tests",
            "testsuite/config/unix.exp": "tests",
            "testsuite/Makefile": "tests",
            ".github/workflows/ci.yml": "build",
            "Makefile": "build",
            "some/Makefile.common": "build",
            "some/project.cabal": "build",
            "doc/BSV_ref_guide/intro.tex": "docs",
            "INSTALL.md": "docs",
            "util/emacs/README": "docs",
        }
        for path, expected in cases.items():
            with self.subTest(path=path):
                self.assertEqual(shards.shard_for(path), expected)

    def test_language_fallbacks_outside_known_directories(self):
        for path, expected in (("util/helper.hs", "compiler"), ("examples/Example.bsv", "hdl"),
                               ("examples/Example.bs", "hdl"), ("util/helper.cpp", "runtime"),
                               ("other/data.txt", "other")):
            with self.subTest(path=path):
                self.assertEqual(shards.shard_for(path), expected)

    def test_group_order_is_stable_and_paths_are_preserved(self):
        groups = shards.assign_shards(["other/data.txt", "src/comp/B.hs", "src/comp/A.hs", "testsuite/ odd name.bsv", ""])
        self.assertEqual(list(groups), ["compiler", "tests", "other"])
        self.assertEqual(groups["compiler"], ["src/comp/B.hs", "src/comp/A.hs"])
        self.assertEqual(groups["tests"], ["testsuite/ odd name.bsv"])


if __name__ == "__main__":
    unittest.main()

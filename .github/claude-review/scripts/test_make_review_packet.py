"""Test BSC maintained-doc routing and packet inclusion/failure behavior."""

import os
import sys
import tempfile
import types
import unittest
from unittest.mock import patch

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import make_review_packet as mp


class TestConventionDocRouting(unittest.TestCase):
    def test_compiler_and_library_changes_include_existing_base_docs(self):
        for path in ("src/comp/ASyntax.hs", "src/Libraries/Base1/Prelude.bs"):
            self.assertEqual(mp.select_convention_docs([path]), ["DEVELOP.md", "INSTALL.md"])

    def test_testsuite_files_include_test_docs_regardless_of_filename(self):
        for path in ("testsuite/bsc.bugs/b123/Example.bsv", "testsuite/config/unix.exp", "testsuite/Makefile"):
            self.assertIn("testsuite/README.md", mp.select_convention_docs([path]))

    def test_compiler_changes_do_not_include_test_docs(self):
        self.assertNotIn("testsuite/README.md", mp.select_convention_docs(["src/comp/TestUtils.hs"]))

    def test_utility_docs_are_selected_only_for_their_areas(self):
        cases = (
            ("util/haskell-language-server/hie.yaml", "util/haskell-language-server/README.md"),
            ("util/tree-sitter-bluespec/grammar.js", "util/tree-sitter-bluespec/README.md"),
        )
        for path, doc in cases:
            self.assertIn(doc, mp.select_convention_docs([path]))
            self.assertNotIn(doc, mp.select_convention_docs(["src/comp/Parser.hs"]))

    def test_no_duplicates_when_many_files_share_a_doc(self):
        docs = mp.select_convention_docs(["testsuite/a.bsv", "testsuite/b.bs", "util/haskell-language-server/a", "util/haskell-language-server/b"])
        self.assertEqual(len(docs), len(set(docs)))

    def test_every_routable_doc_exists(self):
        root = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..", ".."))
        docs = mp.select_convention_docs(["testsuite/a.bsv", "util/haskell-language-server/a", "util/tree-sitter-bluespec/a"])
        for doc in docs:
            self.assertTrue(os.path.isfile(os.path.join(root, doc)), "maintained convention doc missing: " + doc)


class TestPacketMaintainedDocs(unittest.TestCase):
    def test_packet_inlines_bsc_docs_verbatim_and_fails_if_a_doc_moves(self):
        with tempfile.TemporaryDirectory() as root:
            policy_dir = os.path.join(root, "review")
            os.mkdir(policy_dir)
            for name, content in (("DEVELOP.md", "Developer invariants"), ("INSTALL.md", "Supported build settings")):
                with open(os.path.join(root, name), "w", encoding="utf-8") as fh:
                    fh.write(content)
            with open(os.path.join(policy_dir, "REVIEW.md"), "w", encoding="utf-8") as fh:
                fh.write("Review policy")
            args = types.SimpleNamespace(repo=root, review_dir=policy_dir, base="base", head="head", pr_json=None,
                                         ci_file=None, pr_number="1", pr_author="author", max_diff_chars=40000)
            with patch.object(mp, "changed_files", return_value=["src/comp/ASyntax.hs"]), patch.object(mp, "shard_diff", return_value="+new"):
                packet = mp.build_packet(args)
                self.assertIn("Developer invariants", packet)
                self.assertIn("Supported build settings", packet)
                self.assertIn("Review policy", packet)
                self.assertNotIn("rust_model", packet)
                os.unlink(os.path.join(root, "DEVELOP.md"))
                with self.assertRaisesRegex(SystemExit, "maintained convention doc missing: DEVELOP.md"):
                    mp.build_packet(args)


if __name__ == "__main__":
    unittest.main()

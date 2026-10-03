"""Unit tests for make_verify_packet.py — rendering pass-1 candidates into the verify packet.

python3 .github/claude-review/scripts/test_make_verify_packet.py
"""

import os
import sys
import unittest

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import make_verify_packet as mvp  # noqa: E402


def candidate(**kw):
    base = {
        "severity": "important",
        "confidence": "high",
        "file": "rtl/foo.bs",
        "line": 42,
        "anchor": "let x = mkReg(0);",
        "title": "stale reg",
        "claim": "x is never cleared",
        "evidence": "rtl/foo.bs:42",
        "why_it_matters": "reads go stale",
        "skeptic_rebuttal": "maybe a DReg",
        "why_rebuttal_fails": "it's a plain Reg",
    }
    base.update(kw)
    return base


class TestRenderVerifyPacket(unittest.TestCase):
    def test_indexes_each_candidate(self):
        out = mvp.render([candidate(title="a"), candidate(title="b")])
        self.assertIn("## index 0", out)
        self.assertIn("## index 1", out)

    def test_includes_steelman_fields(self):
        out = mvp.render([candidate()])
        # The verifier must see the claim, the anchored line, and the reviewer's own rebuttal.
        self.assertIn("x is never cleared", out)
        self.assertIn("let x = mkReg(0);", out)
        self.assertIn("reviewer's own rebuttal", out)
        self.assertIn("maybe a DReg", out)

    def test_points_at_review_packet(self):
        self.assertIn("review-packet.md", mvp.render([candidate()]))

    def test_empty_findings(self):
        out = mvp.render([])
        self.assertIn("Verify packet", out)
        self.assertNotIn("## index", out)


if __name__ == "__main__":
    unittest.main()

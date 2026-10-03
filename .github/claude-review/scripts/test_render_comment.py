"""Unit tests for render_comment.py (v2) — content-addressed inline placement, the tiered keep-gate
(important/question vs. nits), ```suggestion blocks, the always-on summary body, and adversarial
verify-pass filtering. stdlib only:

    python3 .github/claude-review/scripts/test_render_comment.py
"""

import json
import os
import sys
import tempfile
import types
import unittest

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import render_comment as rc  # noqa: E402

# foo.py RIGHT-side lines (new-side numbering): 1:"line1", 2:"new2", 3:"new3", 4:"line3".
SAMPLE_DIFF = """\
diff --git a/foo.py b/foo.py
index 1111111..2222222 100644
--- a/foo.py
+++ b/foo.py
@@ -1,3 +1,4 @@
 line1
-old2
+new2
+new3
 line3
diff --git a/bar.txt b/bar.txt
new file mode 100644
index 0000000..3333333
--- /dev/null
+++ b/bar.txt
@@ -0,0 +1,2 @@
+alpha
+beta
diff --git a/gone.txt b/gone.txt
deleted file mode 100644
index 4444444..0000000
--- a/gone.txt
+++ /dev/null
@@ -1,2 +0,0 @@
-x
-y
"""


def finding(**kw):
    base = {
        "severity": "important",
        "confidence": "high",
        "file": "foo.py",
        "line": 3,
        "anchor": "new3",
        "title": "t",
        "claim": "c",
        "evidence": "e",
        "why_it_matters": "w",
        "suggested_fix_or_check": "s",
        "skeptic_rebuttal": "r",
        "why_rebuttal_fails": "rf",
    }
    base.update(kw)
    return base


class TestParseRightSideLines(unittest.TestCase):
    def setUp(self):
        self.anchors = rc.parse_right_side_lines(SAMPLE_DIFF)

    def test_captures_line_text(self):
        self.assertEqual(
            self.anchors["foo.py"], {1: "line1", 2: "new2", 3: "new3", 4: "line3"}
        )

    def test_new_file(self):
        self.assertEqual(self.anchors["bar.txt"], {1: "alpha", 2: "beta"})

    def test_deleted_file_absent(self):
        self.assertNotIn("gone.txt", self.anchors)

    def test_empty_diff(self):
        self.assertEqual(rc.parse_right_side_lines(""), {})

    def test_added_line_starting_with_plusplus_not_treated_as_header(self):
        # Regression: an added line whose content begins with "++ " appears as `+++ ...` in the
        # diff; count-based hunk parsing must treat it as added content, not a file header.
        diff = (
            "diff --git a/foo.txt b/foo.txt\n"
            "--- a/foo.txt\n"
            "+++ b/foo.txt\n"
            "@@ -1 +1,3 @@\n"
            " one\n"
            "+++ heading\n"
            "+two\n"
        )
        self.assertEqual(
            rc.parse_right_side_lines(diff),
            {"foo.txt": {1: "one", 2: "++ heading", 3: "two"}},
        )

    def test_multiple_files_without_diff_git_separator(self):
        # Plain `diff -u` output (no `diff --git`): hunk counts must still bound each file.
        diff = (
            "--- a/x\n+++ b/x\n@@ -1 +1,2 @@\n a\n+b\n"
            "--- a/y\n+++ b/y\n@@ -1 +1,2 @@\n c\n+d\n"
        )
        self.assertEqual(
            rc.parse_right_side_lines(diff),
            {"x": {1: "a", 2: "b"}, "y": {1: "c", 2: "d"}},
        )

    def test_crlf_does_not_oversplit(self):
        # split("\n") (not splitlines) so a lone \r or other separator inside content can't mis-split.
        diff = "--- a/z\n+++ b/z\n@@ -0,0 +1,1 @@\n+keep\rtail\n"
        self.assertEqual(rc.parse_right_side_lines(diff), {"z": {1: "keep\rtail"}})

    def test_c_quoted_path_skips_anchoring(self):
        # A C-quoted +++ path (tab/control char in the name) is not decoded -> that file contributes
        # no anchors (its findings fall to the body), never a wrong inline path.
        diff = '--- a/x\n+++ "b/tab\\tfile.txt"\n@@ -0,0 +1,1 @@\n+content\n'
        self.assertEqual(rc.parse_right_side_lines(diff), {})


class TestContentAnchoring(unittest.TestCase):
    def setUp(self):
        self.anchors = rc.parse_right_side_lines(SAMPLE_DIFF)

    def test_anchor_by_content(self):
        self.assertEqual(
            rc.finding_anchor(finding(anchor="new3"), self.anchors), ("foo.py", 3)
        )

    def test_content_beats_wrong_line_number(self):
        # The whole point: a wrong `line` is ignored when the anchor text is found.
        self.assertEqual(
            rc.finding_anchor(finding(anchor="new2", line=999), self.anchors),
            ("foo.py", 2),
        )

    def test_duplicate_content_tiebreaks_on_line_hint(self):
        anchors = {"foo.py": {2: "dup", 8: "dup", 14: "dup"}}
        self.assertEqual(
            rc.finding_anchor(finding(anchor="dup", line=9), anchors), ("foo.py", 8)
        )

    def test_anchor_not_in_diff_falls_to_body_not_line(self):
        # Anchor given but absent -> None (do NOT trust the line hint).
        self.assertIsNone(
            rc.finding_anchor(finding(anchor="nonexistent line", line=2), self.anchors)
        )

    def test_empty_anchor_is_file_level_even_with_a_line(self):
        # Per the contract, an empty anchor is file-level: it must NOT anchor by line number alone.
        self.assertIsNone(rc.finding_anchor(finding(anchor="", line=4), self.anchors))

    def test_empty_anchor_file_level_line_zero(self):
        self.assertIsNone(rc.finding_anchor(finding(anchor="", line=0), self.anchors))

    def test_unknown_file(self):
        self.assertIsNone(
            rc.finding_anchor(finding(file="other.py", anchor="new3"), self.anchors)
        )

    def test_whitespace_normalized_match(self):
        anchors = {"foo.py": {5: "    indented_call()"}}
        self.assertEqual(
            rc.finding_anchor(finding(anchor="indented_call()", line=5), anchors),
            ("foo.py", 5),
        )

    def test_blank_line_not_matched_by_unfound_anchor(self):
        # Regression: an unmatched anchor must NOT land on a blank changed line via substring.
        anchors = {"foo.py": {10: "", 11: "actual_code()"}}
        self.assertIsNone(
            rc.finding_anchor(finding(anchor="missing line text", line=10), anchors)
        )

    def test_short_line_not_over_matched(self):
        # A short line like "}" must not absorb an unmatched anchor via containment.
        anchors = {"foo.py": {10: "}", 11: "other"}}
        self.assertIsNone(
            rc.finding_anchor(finding(anchor="return something();", line=10), anchors)
        )

    def test_exact_raw_distinguishes_indentation_distinct_duplicates(self):
        # Regression: with two same-text lines differing only by indentation, the exact raw match
        # picks the one the model copied verbatim, not whichever the (possibly wrong) hint points at.
        anchors = {"foo.py": {10: "  call();", 20: "    call();"}}
        self.assertEqual(
            rc.finding_anchor(finding(anchor="    call();", line=10), anchors),
            ("foo.py", 20),
        )


class TestGateTiers(unittest.TestCase):
    def test_caps_high_signal_separately_from_nits(self):
        findings = [
            finding(severity="important", title="i%d" % i) for i in range(5)
        ] + [finding(severity="nit", title="n%d" % i) for i in range(9)]
        kept = rc.gate_findings(findings, 160, 600, max_findings=3, max_nits=5)
        sig = [f for f in kept if f["severity"] in rc.HIGH_SIGNAL]
        nit = [f for f in kept if f["severity"] == "nit"]
        self.assertEqual(len(sig), 3)
        self.assertEqual(len(nit), 5)

    def test_drops_incomplete(self):
        bad = finding(claim="")  # missing required field
        self.assertEqual(rc.gate_findings([bad], 160, 600, 3, 5), [])

    def test_drops_unknown_severity(self):
        self.assertEqual(
            rc.gate_findings([finding(severity="style")], 160, 600, 3, 5), []
        )


class TestInlineRendering(unittest.TestCase):
    def test_nit_prefix_and_suggestion_block(self):
        f = finding(
            severity="nit",
            title="rename x",
            claim="x is unclear",
            suggestion="y = compute()",
        )
        body = rc.render_inline_comment(f)
        self.assertTrue(body.startswith("Nit: "))
        self.assertIn("```suggestion\ny = compute()\n```", body)

    def test_no_suggestion_when_absent(self):
        self.assertNotIn(
            "```suggestion", rc.render_inline_comment(finding(suggestion=""))
        )

    def test_inline_has_no_file_line_prefix(self):
        self.assertNotIn("foo.py:3", rc.render_inline_comment(finding()))


class TestBuildReviewPayload(unittest.TestCase):
    def setUp(self):
        self.anchors = rc.parse_right_side_lines(SAMPLE_DIFF)

    def test_summary_always_in_body(self):
        review = rc.build_review_payload(
            {"summary": "High-level read here."}, [], self.anchors, "abc"
        )
        self.assertIn("High-level read here.", review["body"])
        self.assertIn("not a sign-off", review["body"])
        self.assertEqual(review["comments"], [])
        self.assertEqual(review["commit_id"], "abc")

    def test_anchorable_goes_inline(self):
        review = rc.build_review_payload(
            {"summary": "s"},
            [finding(anchor="new2", line=2, title="x")],
            self.anchors,
            "abc",
        )
        self.assertEqual(len(review["comments"]), 1)
        self.assertEqual(
            (review["comments"][0]["path"], review["comments"][0]["line"]),
            ("foo.py", 2),
        )

    def test_unanchorable_high_signal_goes_to_body(self):
        f = finding(
            severity="question", anchor="not in the diff at all", title="floatq"
        )
        review = rc.build_review_payload({"summary": "s"}, [f], self.anchors, "abc")
        self.assertEqual(review["comments"], [])
        self.assertIn("floatq", review["body"])

    def test_unanchorable_nit_is_dropped(self):
        f = finding(severity="nit", anchor="not in the diff at all", title="droppednit")
        review = rc.build_review_payload({"summary": "s"}, [f], self.anchors, "abc")
        self.assertEqual(review["comments"], [])
        self.assertNotIn("droppednit", review["body"])


class TestPayloadShapeHardening(unittest.TestCase):
    def _load(self, raw):
        os.environ["SO_TEST"] = raw
        args = types.SimpleNamespace(input=None, input_env="SO_TEST")
        return rc.load_payload(args)

    def test_list_rejected(self):
        payload, err = self._load("[]")
        self.assertIsNone(payload)
        self.assertTrue(err)

    def test_null_rejected(self):
        payload, err = self._load("null")
        self.assertIsNone(payload)
        self.assertTrue(err)

    def test_non_string_summary_coerced(self):
        payload, err = self._load('{"summary":["x"],"findings":[],"suppressed":[]}')
        self.assertIsNone(err)
        self.assertEqual(payload["summary"], "")

    def test_non_list_findings_coerced(self):
        payload, err = self._load('{"summary":"ok","findings":{"a":1}}')
        self.assertIsNone(err)
        self.assertEqual(payload["findings"], [])
        self.assertEqual(payload["suppressed"], [])


class TestVerdicts(unittest.TestCase):
    def test_load_verdicts(self):
        with tempfile.NamedTemporaryFile("w", suffix=".json", delete=False) as fh:
            json.dump(
                {
                    "verdicts": [
                        {"index": 0, "refuted": True, "reason": "guarded above"},
                        {"index": 1, "refuted": False, "reason": "stands"},
                    ]
                },
                fh,
            )
            path = fh.name
        try:
            refuted, reasons = rc.load_verdicts(path)
            self.assertEqual(refuted, {0})
            self.assertEqual(reasons[0], "guarded above")
        finally:
            os.unlink(path)

    def test_no_verdicts_file(self):
        self.assertEqual(rc.load_verdicts(None), (set(), {}))


if __name__ == "__main__":
    unittest.main()

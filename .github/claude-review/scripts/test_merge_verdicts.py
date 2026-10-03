"""Unit tests for merge_verdicts.py — union verify verdicts by index across retry attempts.

python3 -m unittest discover -s .github/claude-review/scripts -p 'test_*.py'
"""

import json
import os
import sys
import tempfile
import unittest

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import merge_verdicts as mv  # noqa: E402


class TestMergeVerdicts(unittest.TestCase):
    def setUp(self):
        self.into = tempfile.NamedTemporaryFile("w", suffix=".json", delete=False).name

    def tearDown(self):
        if os.path.exists(self.into):
            os.unlink(self.into)

    def _merge(self, blob):
        os.environ["VERDICTS"] = blob
        return mv.main(
            ["merge_verdicts.py", "--into", self.into, "--from-env", "VERDICTS"]
        )

    def _read(self):
        with open(self.into) as fh:
            return {v["index"]: v for v in json.load(fh)["verdicts"]}

    def test_attempt2_does_not_drop_attempt1_coverage(self):
        # The bug the merge fixes: attempt 1 refutes index 0; attempt 2 covers only index 1.
        os.unlink(self.into)  # start with no file (first attempt)
        self._merge('{"verdicts":[{"index":0,"refuted":true,"reason":"guarded"}]}')
        self._merge('{"verdicts":[{"index":1,"refuted":false,"reason":"stands"}]}')
        got = self._read()
        self.assertEqual(set(got), {0, 1})
        self.assertTrue(got[0]["refuted"])  # attempt 1's refutation survived
        self.assertFalse(got[1]["refuted"])

    def test_refutation_is_monotonic_not_resurrected(self):
        # The bug Codex found: attempt 1 refutes index 0; a later attempt reporting it not-refuted
        # must NOT resurrect (re-publish) the finding.
        os.unlink(self.into)
        self._merge('{"verdicts":[{"index":0,"refuted":true,"reason":"guarded"}]}')
        self._merge(
            '{"verdicts":[{"index":0,"refuted":false},{"index":1,"refuted":false}]}'
        )
        got = self._read()
        self.assertTrue(got[0]["refuted"])  # stays refuted
        self.assertFalse(got[1]["refuted"])

    def test_not_refuted_can_be_upgraded_to_refuted(self):
        os.unlink(self.into)
        self._merge('{"verdicts":[{"index":0,"refuted":false}]}')
        self._merge('{"verdicts":[{"index":0,"refuted":true}]}')
        self.assertTrue(self._read()[0]["refuted"])  # a later refutation does win

    def test_empty_blob_is_noop(self):
        os.unlink(self.into)
        self._merge('{"verdicts":[{"index":0,"refuted":true}]}')
        self._merge("")  # failed attempt -> must not wipe existing coverage
        self.assertEqual(set(self._read()), {0})

    def test_rejects_bool_and_negative_index(self):
        os.unlink(self.into)
        self._merge(
            '{"verdicts":[{"index":true,"refuted":true},'
            '{"index":-1,"refuted":true},{"index":2,"refuted":false}]}'
        )
        self.assertEqual(set(self._read()), {2})


if __name__ == "__main__":
    unittest.main()

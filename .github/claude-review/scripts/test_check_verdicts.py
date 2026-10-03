"""Unit tests for check_verdicts.py — the verify-retry completeness gate.

python3 -m unittest discover -s .github/claude-review/scripts -p 'test_*.py'
"""

import json
import os
import sys
import tempfile
import unittest

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import check_verdicts as cv  # noqa: E402


def _write(obj):
    fh = tempfile.NamedTemporaryFile("w", suffix=".json", delete=False)
    json.dump(obj, fh)
    fh.close()
    return fh.name


class TestCheckVerdicts(unittest.TestCase):
    def setUp(self):
        self.paths = []

    def tearDown(self):
        for p in self.paths:
            os.unlink(p)

    def _run(self, findings, verdicts):
        f = _write({"findings": findings})
        v = _write({"verdicts": verdicts})
        self.paths += [f, v]
        return cv.main(["check_verdicts.py", "--findings", f, "--verdicts", v])

    def test_complete(self):
        rc = self._run(
            [{"title": "a"}, {"title": "b"}],
            [{"index": 0, "refuted": True}, {"index": 1, "refuted": False}],
        )
        self.assertEqual(rc, 0)

    def test_incomplete_missing_index(self):
        rc = self._run(
            [{"title": "a"}, {"title": "b"}], [{"index": 0, "refuted": True}]
        )
        self.assertEqual(rc, 1)

    def test_empty_verdicts_incomplete(self):
        self.assertEqual(self._run([{"title": "a"}], []), 1)

    def test_unreadable_verdicts_file(self):
        f = _write({"findings": [{"title": "a"}]})
        self.paths.append(f)
        rc = cv.main(
            ["check_verdicts.py", "--findings", f, "--verdicts", "/no/such/file.json"]
        )
        self.assertEqual(rc, 1)


if __name__ == "__main__":
    unittest.main()

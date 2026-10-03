"""Unit tests for extract_structured.py (stdlib only)."""

import json
import os
import subprocess
import sys
import tempfile
import unittest

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
import extract_structured as ex  # noqa: E402


class TestExtractJson(unittest.TestCase):
    def test_plain_object(self):
        self.assertEqual(ex.extract_json('{"a": 1}'), '{"a": 1}')

    def test_object_in_fence_and_prose(self):
        text = (
            'Here:\n```json\n{"verdicts": [{"index": 0, "refuted": true}]}\n```\ndone'
        )
        self.assertEqual(
            json.loads(ex.extract_json(text)),
            {"verdicts": [{"index": 0, "refuted": True}]},
        )

    def test_braces_inside_strings_ignored(self):
        text = '{"s": "a } b { c", "n": 1}'
        self.assertEqual(json.loads(ex.extract_json(text)), {"s": "a } b { c", "n": 1})

    def test_first_balanced_object_returned(self):
        self.assertEqual(
            ex.extract_json('prefix {"a": {"b": 2}} suffix'), '{"a": {"b": 2}}'
        )

    def test_no_object(self):
        self.assertIsNone(ex.extract_json("no json here"))

    def test_unbalanced(self):
        self.assertIsNone(ex.extract_json('{"a": 1'))


class TestFinalText(unittest.TestCase):
    def _write(self, msgs):
        fd, path = tempfile.mkstemp(suffix=".json")
        os.write(fd, json.dumps(msgs).encode())
        os.close(fd)
        self.addCleanup(os.unlink, path)
        return path

    def test_last_result_wins(self):
        path = self._write(
            [
                {"type": "result", "result": "first"},
                {"type": "result", "result": "second"},
            ]
        )
        text, _seq = ex.final_text(path)
        self.assertEqual(text, "second")

    def test_assistant_text_block_fallback(self):
        path = self._write(
            [
                {
                    "type": "assistant",
                    "message": {"content": [{"type": "text", "text": "hello"}]},
                }
            ]
        )
        text, _seq = ex.final_text(path)
        self.assertEqual(text, "hello")

    def test_empty_when_nothing_usable(self):
        path = self._write([{"type": "system", "subtype": "init"}])
        text, _seq = ex.final_text(path)
        self.assertEqual(text, "")


class TestMain(unittest.TestCase):
    def _run(self, msgs=None, missing=False):
        if missing:
            path = os.path.join(tempfile.gettempdir(), "definitely-not-here-exec.json")
        else:
            fd, path = tempfile.mkstemp(suffix=".json")
            os.write(fd, json.dumps(msgs).encode())
            os.close(fd)
            self.addCleanup(os.unlink, path)
        return subprocess.run(
            [sys.executable, os.path.join(HERE, "extract_structured.py"), path],
            capture_output=True,
            text=True,
        )

    def test_extracts_verdicts_to_stdout(self):
        verdicts = {"verdicts": [{"index": 0, "refuted": False, "reason": "stands"}]}
        msgs = [
            {
                "type": "result",
                "result": "Verdicts:\n```json\n" + json.dumps(verdicts) + "\n```",
            }
        ]
        proc = self._run(msgs)
        self.assertEqual(proc.returncode, 0)
        self.assertEqual(json.loads(proc.stdout), verdicts)

    def test_no_json_exits_nonzero_empty_stdout(self):
        proc = self._run([{"type": "result", "result": "I could not produce output."}])
        self.assertEqual(proc.returncode, 1)
        self.assertEqual(proc.stdout, "")

    def test_missing_file_exits_nonzero(self):
        self.assertEqual(self._run(missing=True).returncode, 1)


if __name__ == "__main__":
    unittest.main()

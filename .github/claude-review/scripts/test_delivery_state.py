import json
import os
import sys
import unittest
from unittest import mock

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import delivery_state  # noqa: E402
import render_comment  # noqa: E402


class BotBodiesTest(unittest.TestCase):
    def _run(self, stdout):
        with mock.patch.object(delivery_state.subprocess, "run") as run:
            run.return_value = mock.Mock(stdout=stdout)
            return delivery_state._bot_bodies("o/r", "pulls/1/reviews")

    def test_decodes_concatenated_pages(self):
        page = json.dumps([{"user": {"login": "github-actions[bot]"}, "body": "x"}])
        self.assertEqual(self._run(page + "\n" + page), ["x", "x"])

    def test_filters_other_authors_and_null_bodies(self):
        out = json.dumps(
            [
                {"user": {"login": "someone"}, "body": "y"},
                {"user": {"login": "github-actions[bot]"}, "body": None},
                {"user": None, "body": "z"},
            ]
        )
        self.assertEqual(self._run(out), [""])


class CountsTest(unittest.TestCase):
    def test_delivered_excludes_cross_posts_and_counts_notices(self):
        delivered_body = "summary\n\n" + render_comment.CAVEAT
        xpost_body = (
            "First-pass automated review of PR #7, posted on another PR.\n"
            + delivery_state.XPOST_MARKER
            + "\n"
            + render_comment.CAVEAT
        )
        notice_body = "no output. " + delivery_state.NOTICE_MARKER
        self.assertEqual(
            delivery_state.counts([delivered_body, xpost_body, notice_body, "plain"]),
            (1, 1),
        )


if __name__ == "__main__":
    unittest.main()

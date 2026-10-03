"""Counts delivered first-pass reviews and undelivered-run notices on a PR.

Single source of truth for the delivery markers used by claude-review.yml (the
catch-up gate, the post-time re-check, and the outcome notice), so the workflow
never carries its own copy of the marker strings. Delivered bodies are detected
by the renderer's CAVEAT string (render_comment.py); cross-PR test posts are
excluded by the prelude line they carry. Prints two integers: "<delivered> <notices>".
Exits nonzero on any API failure — callers treat that as fail-closed.
"""

import argparse
import json
import subprocess

import render_comment

# Must match the hidden token render_comment._prelude() emits on cross-posts.
XPOST_MARKER = "<!-- claude-review:cross-post -->"
# Hidden token appended to undelivered-run notice comments by claude-review.yml.
NOTICE_MARKER = "<!-- claude-review:undelivered -->"


def _bot_bodies(repo, path):
    """Bodies of github-actions[bot] items from a paginated gh api endpoint.

    `gh api --paginate` emits one JSON array per page, concatenated; decode them all.
    """
    out = subprocess.run(
        ["gh", "api", f"repos/{repo}/{path}", "--paginate"],
        capture_output=True,
        text=True,
        check=True,
    ).stdout
    decoder = json.JSONDecoder()
    bodies = []
    idx = 0
    while idx < len(out):
        while idx < len(out) and out[idx].isspace():
            idx += 1
        if idx >= len(out):
            break
        page, idx = decoder.raw_decode(out, idx)
        for item in page:
            if (item.get("user") or {}).get("login") == "github-actions[bot]":
                bodies.append(item.get("body") or "")
    return bodies


def counts(bodies):
    delivered = sum(
        1 for b in bodies if render_comment.CAVEAT in b and XPOST_MARKER not in b
    )
    notices = sum(1 for b in bodies if NOTICE_MARKER in b)
    return delivered, notices


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--repo", required=True)
    parser.add_argument("--pr", required=True)
    args = parser.parse_args()
    bodies = _bot_bodies(args.repo, f"pulls/{args.pr}/reviews") + _bot_bodies(
        args.repo, f"issues/{args.pr}/comments"
    )
    delivered, notices = counts(bodies)
    print(f"{delivered} {notices}")


if __name__ == "__main__":
    main()

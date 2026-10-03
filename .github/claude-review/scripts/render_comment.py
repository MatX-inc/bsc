"""Deterministic publication gate for the PR reviewer.

The model returns validated JSON (against output-schema.json) and posts nothing. This
script decides what a human sees:

  Phase 0: write the full record (summary + findings + suppressed + verify-refuted) to the
           Actions job summary ($GITHUB_STEP_SUMMARY). Post nothing to the PR.
  Phase 1: additionally write, for a later separately-gated workflow step to post:
             - a review payload (--review-payload-out) for the GitHub reviews API: an always-on
               top-level summary body plus line-anchored inline comments (important / question /
               nit), with one-click ```suggestion blocks where the model supplied a fix;
             - a top-level comment body (--comment-out) used as a fallback when the reviews API
               post fails (an anchor not on the diff, or a cross-PR test target).
           This script never calls the GitHub API.

Discipline lives here, not in the schema: per-tier budgets (few important/question; a separate
small nit cap), drop any finding missing a required field, and — if a verify pass ran — drop any
finding the verifier refuted.

Inline placement is CONTENT-ADDRESSED: a finding carries `anchor` (the verbatim text of the line
it attaches to). The renderer finds the line on the RIGHT (new) side of the diff whose text
matches `anchor` (tie-broken by the `line` hint), so a wrong line number does not matter. A
file-level finding (empty anchor / line 0) or one whose text isn't in the diff falls back to the
top-level body; an unanchorable nit is dropped (nits are inline-only). stdlib only.

Usage:
    render_comment.py [--input PATH|-] [--input-env NAME] [--phase 0|1]
                      [--summary-file PATH] [--comment-out PATH]
                      [--diff-file PATH] [--review-payload-out PATH] [--head-sha SHA]
                      [--verdicts-file PATH]
                      [--max-findings 3] [--max-nits 5]
                      [--max-words-per-finding 160] [--max-words-total 600]
"""

import argparse
import json
import os
import re
import sys

# Fields a postable finding must carry (non-empty). `anchor` is intentionally NOT here: it is
# empty for a legitimate file-level finding, and placement degrades gracefully without it.
REQUIRED_FINDING_FIELDS = (
    "severity",
    "confidence",
    "file",
    "line",
    "title",
    "claim",
    "evidence",
    "why_it_matters",
    "suggested_fix_or_check",
    "skeptic_rebuttal",
    "why_rebuttal_fails",
)

HIGH_SIGNAL = ("important", "question")
ALL_SEVERITIES = ("important", "question", "nit")


def load_payload(args):
    if args.input == "-":
        raw = sys.stdin.read()
    elif args.input:
        try:
            with open(args.input, "r", encoding="utf-8") as fh:
                raw = fh.read()
        except OSError as exc:
            return None, "could not read input file %s: %s" % (args.input, exc)
    else:
        raw = os.environ.get(args.input_env, "")
    raw = raw.strip()
    if not raw:
        return None, "no structured output provided"
    try:
        data = json.loads(raw)
    except json.JSONDecodeError as exc:
        return None, "structured output is not valid JSON: %s" % exc
    # Valid JSON of the wrong shape (a list, null, a string) must degrade to "no usable output",
    # not crash the renderer. Require a JSON object and coerce the three expected keys to safe types
    # so nothing downstream blows up on `.get`/`.strip`/iteration.
    if not isinstance(data, dict):
        return None, "structured output is not a JSON object"
    if not isinstance(data.get("summary"), str):
        data["summary"] = ""
    if not isinstance(data.get("findings"), list):
        data["findings"] = []
    if not isinstance(data.get("suppressed"), list):
        data["suppressed"] = []
    return data, None


def load_verdicts(path):
    """Adversarial verify-pass result: (refuted_indices, {index: reason}). Keyed by index into the
    original findings array. Absent/unreadable file -> empty (treated as "no verify pass ran")."""
    if not path:
        return set(), {}
    try:
        with open(path, "r", encoding="utf-8") as fh:
            data = json.load(fh)
    except (OSError, ValueError) as exc:
        sys.stderr.write("could not read verdicts file %s: %s\n" % (path, exc))
        return set(), {}
    refuted, reasons = set(), {}
    for v in data.get("verdicts") or []:
        # type(...) is int rejects bool; reject negative indices too.
        if isinstance(v, dict) and type(v.get("index")) is int and v["index"] >= 0:
            reasons[v["index"]] = str(v.get("reason", ""))
            if v.get("refuted"):
                refuted.add(v["index"])
    return refuted, reasons


def truncate_words(text, limit):
    words = text.split()
    if len(words) <= limit:
        return text
    return " ".join(words[:limit]) + " ..."


def gate_findings(findings, max_per, max_total, max_findings, max_nits):
    """Keep well-formed findings within the per-tier budgets.

    `important`/`question` are the high-signal tier: capped at `max_findings` and a shared word
    budget. `nit` is a separate, inline-only tier capped at `max_nits` (not subject to the
    high-signal word budget — nits are short). Order is preserved within the kept list.
    """
    kept = []
    spent = 0
    n_signal = 0
    n_nit = 0
    for f in findings:
        if not isinstance(f, dict):
            continue
        if any(not str(f.get(k, "")).strip() for k in REQUIRED_FINDING_FIELDS):
            continue  # incomplete finding -> drop
        sev = f.get("severity")
        if sev not in ALL_SEVERITIES:
            continue
        g = dict(f)
        g["claim"] = truncate_words(str(f["claim"]).strip(), max_per)
        if sev == "nit":
            if n_nit >= max_nits:
                continue
            n_nit += 1
        else:  # important / question
            if n_signal >= max_findings:
                continue
            cost = len(g["claim"].split())
            if spent + cost > max_total:
                continue
            spent += cost
            n_signal += 1
        kept.append(g)
    return kept


# ---- diff parsing + content-addressed anchoring -------------------------------------------------

# Captures (old_count, new_start, new_count) from "@@ -a,b +c,d @@" (counts default to 1 if absent).
_HUNK_RE = re.compile(r"^@@ -\d+(?:,(\d+))? \+(\d+)(?:,(\d+))? @@")


def parse_right_side_lines(diff_text):
    """Map each new-file path -> {line_number: line_text} for lines commentable on the RIGHT side.

    Walks a unified diff, tracking each hunk's remaining old/new line counts from its `@@` header.
    The RIGHT (new) side advances on added (`+`) and context (` `) lines; deletions (`-`) do not.
    Those are exactly the lines the GitHub reviews API accepts an inline comment on with side=RIGHT,
    and we keep their text so findings can be placed by content. New paths come from `+++ b/...`;
    a `+++ /dev/null` (deleted file) contributes nothing. Hunk body lines are consumed by COUNT, not
    by guessing on the leading character — so an added line whose content begins with `++ ` (raw
    `+++ ...`) is not mistaken for a file header, and a hunk that ends without a `diff --git` line is
    still bounded correctly.
    """
    anchors = {}
    path = None
    new_line = None
    new_rem = 0
    old_rem = 0
    # Split on "\n" only (not str.splitlines(), which also breaks on \r and other separators and
    # could mis-split content); the diff itself is newline-delimited.
    for raw in diff_text.split("\n"):
        # Inside a hunk body (counts not yet exhausted): consume by the per-line +/-/space/\ tag.
        if path is not None and new_line is not None and (new_rem > 0 or old_rem > 0):
            tag = raw[:1]
            if tag == "+":
                anchors[path][new_line] = raw[1:]
                new_line += 1
                new_rem -= 1
                continue
            if tag == "-":
                old_rem -= 1
                continue
            if (
                tag == "\\"
            ):  # "\ No newline at end of file" — counts toward neither side
                continue
            # context line (space-prefixed, or a genuinely empty body line)
            anchors[path][new_line] = raw[1:] if raw else ""
            new_line += 1
            new_rem -= 1
            old_rem -= 1
            continue
        # Structural lines (outside any hunk body).
        if raw.startswith("+++ "):
            target = raw[4:].strip()
            if target == "/dev/null" or target.startswith('"'):
                # /dev/null = deleted file; a quoted target = a C-quoted path (tab/control/unicode in
                # the name) we don't decode — leave path None so findings on it fall to the body
                # rather than risk a wrong inline path the API would reject.
                path = None
            else:
                path = target[2:] if target[:2] in ("b/", "a/", "i/", "w/") else target
                anchors.setdefault(path, {})
            new_line = None
            continue
        if raw.startswith("--- "):
            continue
        m = _HUNK_RE.match(raw)
        if m:
            old_rem = int(m.group(1)) if m.group(1) else 1
            new_line = int(m.group(2))
            new_rem = int(m.group(3)) if m.group(3) else 1
    return anchors


def _nearest(lines, hint):
    """Pick the line in `lines` (list of ints) closest to `hint`; smallest if no usable hint."""
    if not lines:
        return None
    if not hint or hint < 1:
        return min(lines)
    return min(lines, key=lambda ln: (abs(ln - hint), ln))


def _norm(s):
    """Collapse all runs of whitespace (incl. leading/trailing) to single spaces."""
    return " ".join(s.split())


def finding_anchor(f, anchors):
    """Resolve a finding to a RIGHT-side diff line by CONTENT, not position. Returns (path, line)
    or None (-> belongs in the body, or — for a nit — should be dropped).

    When the model gives an `anchor`, we place by it (never by the line number, which would
    reintroduce the position flakiness anchors exist to avoid). Match order, most precise first:
      1. exact raw text — preserves indentation, so indentation-distinct duplicate lines stay distinct;
      2. trailing-whitespace-only difference;
      3. normalized (all whitespace collapsed) — tolerates the model respacing or dropping indentation;
      4. guarded containment — only when both sides have >= 6 non-whitespace-collapsed chars, so an
         empty/short line can't absorb an unmatched anchor.
    Ties are broken by the `line` hint. An EMPTY anchor means a file-level finding (body / nit
    dropped) — we never anchor by line number alone.
    """
    path = str(f.get("file", "")).strip()
    file_lines = anchors.get(path)
    if not file_lines:
        return None
    try:
        hint = int(f.get("line"))
    except (TypeError, ValueError):
        hint = None

    anchor_raw = str(f.get("anchor", ""))
    anchor_norm = _norm(anchor_raw)
    if anchor_norm:
        cands = [ln for ln, txt in file_lines.items() if txt == anchor_raw]
        if not cands:
            cands = [
                ln
                for ln, txt in file_lines.items()
                if txt.rstrip() == anchor_raw.rstrip()
            ]
        if not cands:
            cands = [ln for ln, txt in file_lines.items() if _norm(txt) == anchor_norm]
        if not cands and len(anchor_norm) >= 6:
            cands = [
                ln
                for ln, txt in file_lines.items()
                if len(_norm(txt)) >= 6
                and (anchor_norm in _norm(txt) or _norm(txt) in anchor_norm)
            ]
        if cands:
            return (path, _nearest(cands, hint))
        return None

    # Empty anchor = a file-level finding (per the schema contract). It goes to the body (or a nit is
    # dropped); we do NOT anchor by line number alone, which would reintroduce position flakiness.
    return None


# ---- rendering ----------------------------------------------------------------------------------


def render_inline_comment(f):
    """Inline comment body — no `file:line` prefix (it is already anchored). Nits are marked; a
    concrete `suggestion` becomes a one-click GitHub ```suggestion block."""
    sev = f.get("severity")
    prefix = "Nit: " if sev == "nit" else ""
    body = "%s%s. %s" % (prefix, f["title"], f["claim"])
    sfix = str(f.get("suggested_fix_or_check", "")).strip()
    if sev == "important":
        body += " %s — %s" % (sfix, f.get("why_it_matters", ""))
    elif sfix:
        body += " %s" % sfix
    suggestion = str(f.get("suggestion", "")).strip()
    if suggestion:
        body += "\n\n```suggestion\n%s\n```" % suggestion
    return body


def render_body_finding(f):
    """A non-anchorable high-signal finding, rendered as a bullet in the top-level body (keeps the
    `file:line` since it is not attached to a line)."""
    body = "%s (`%s:%s`). %s" % (f["title"], f["file"], f["line"], f["claim"])
    if f.get("severity") == "question":
        body += " %s" % f.get("suggested_fix_or_check", "")
    else:
        body += " %s — %s" % (
            f.get("suggested_fix_or_check", ""),
            f.get("why_it_matters", ""),
        )
    return body


# delivery_state.py greps published bodies for this exact string to detect a delivered
# review; rewording it silently un-delivers every past review for the catch-up gate.
CAVEAT = (
    "First-pass static review only — I can't build, run, or simulate, so this is a read of the "
    "diff, not a sign-off."
)


def _prelude():
    parts = []
    note = os.environ.get("NOTE", "").strip()
    if note:
        parts += [note, ""]
    pr = os.environ.get("PR_NUMBER", "").strip()
    target = os.environ.get("COMMENT_PR", "").strip()
    if pr and target and target != pr:
        # The hidden token is delivery_state.XPOST_MARKER; visible prose would be spoofable
        # by a model summary that happens to contain the same words.
        parts += [
            "First-pass automated review of PR #%s, posted here rather than on the reviewed PR."
            % pr,
            "",
            "<!-- claude-review:cross-post -->",
            "",
        ]
    return parts


def build_review_body(payload, body_findings):
    """Top-level review comment: always the model's high-level summary, then the static-read
    caveat, then any high-signal findings that could not be anchored to a changed line."""
    parts = _prelude()
    summary = (payload.get("summary") or "").strip()
    parts.append(summary if summary else "Took a first pass over the diff.")
    parts += ["", CAVEAT]
    if body_findings:
        parts += [
            "",
            "A couple of points I couldn't tie to a specific changed line:",
            "",
        ]
        parts += ["- " + render_body_finding(f) for f in body_findings]
    return "\n".join(parts) + "\n"


def build_review_payload(payload, kept, anchors, head_sha):
    """GitHub reviews-API body: always-on summary body + content-anchored inline comments. event
    = COMMENT (a bot never approves). Unanchorable high-signal findings move to the body; an
    unanchorable nit is dropped (nits are inline-only)."""
    comments = []
    body_findings = []
    for f in kept:
        anchor = finding_anchor(f, anchors)
        if anchor:
            comments.append(
                {
                    "path": anchor[0],
                    "line": anchor[1],
                    "side": "RIGHT",
                    "body": render_inline_comment(f),
                }
            )
        elif f.get("severity") in HIGH_SIGNAL:
            body_findings.append(f)
        # else: unanchorable nit -> dropped
    review = {
        "event": "COMMENT",
        "body": build_review_body(payload, body_findings),
        "comments": comments,
    }
    if head_sha:
        review["commit_id"] = head_sha
    return review


def build_comment(payload, kept):
    """Self-contained fallback top-level comment, posted ONLY when the reviews API rejects the
    structured review. It carries the summary plus the high-signal findings (important/question)
    rendered as text with their `file:line`. Nits are omitted: they are an inline-only nicety, and
    dumping them into a top-level comment is exactly the noise this reviewer avoids — they remain in
    the job summary."""
    parts = _prelude()
    summary = (payload.get("summary") or "").strip()
    parts.append(summary if summary else "Took a first pass over the diff.")
    parts += ["", CAVEAT]
    high_signal = [f for f in kept if f.get("severity") in HIGH_SIGNAL]
    if high_signal:
        parts += ["", "Findings:", ""]
        parts += ["- " + render_body_finding(f) for f in high_signal]
    return "\n".join(parts) + "\n"


# ---- job summary --------------------------------------------------------------------------------


def render_finding_md(f, anchored=None):
    where = ""
    if anchored:
        where = " → inline `%s:%s`" % anchored
    return (
        "- `%s:%s` [%s, %s]%s %s\n"
        "  - %s\n"
        "  - Why it matters: %s\n"
        "  - Suggested: %s%s\n"
        % (
            f["file"],
            f["line"],
            f["severity"],
            f["confidence"],
            where,
            f["title"],
            f["claim"],
            f.get("why_it_matters", ""),
            f.get("suggested_fix_or_check", ""),
            " (with ```suggestion)" if str(f.get("suggestion", "")).strip() else "",
        )
    )


def context_header(phase):
    pr = os.environ.get("PR_NUMBER", "").strip()
    base = os.environ.get("BASE_SHA", "").strip()[:12]
    head = os.environ.get("HEAD_SHA", "").strip()[:12]
    parts = []
    if pr:
        parts.append("Reviewed PR #%s" % pr)
    if base and head:
        parts.append("diff `%s`...`%s`" % (base, head))
    mode = (
        "Phase 0 — job summary only; nothing posted to the PR."
        if phase == 0
        else "Phase 1 — one review (summary + inline comments) posted."
    )
    return ("%s. %s" % (" — ".join(parts), mode)) if parts else mode


def build_summary(payload, kept, anchor_map, refuted, n_unverified, context):
    lines = ["## Automated first-pass review\n"]
    if context:
        lines.append(context + "\n")
    if n_unverified:
        lines.append(
            "> Verify coverage incomplete after the retry cap: %d finding(s) posted without an "
            "independent verdict.\n" % n_unverified
        )
    lines.append((payload.get("summary") or "").strip() or "(no summary)")
    lines.append("")
    if kept:
        lines.append("### Findings (postable: %d)\n" % len(kept))
        for f in kept:
            lines.append(render_finding_md(f, anchored=anchor_map.get(id(f))))
    else:
        lines.append("### Findings\n")
        lines.append("No postable findings — clean pass (summary posted).\n")
    if refuted:
        lines.append("### Refuted by the verify pass (%d)\n" % len(refuted))
        for f in refuted:
            lines.append(
                "- `%s:%s` [%s] %s\n  - %s"
                % (
                    f.get("file", "?"),
                    f.get("line", "?"),
                    f.get("severity", "?"),
                    f.get("title", ""),
                    f.get("_verify_reason", ""),
                )
            )
    suppressed = payload.get("suppressed") or []
    lines.append("### Considered and dropped by the reviewer (%d)\n" % len(suppressed))
    if suppressed:
        for s in suppressed:
            if isinstance(s, dict):
                lines.append(
                    "- `%s:%s` [%s, %s] %s\n  - rebuttal: %s\n  - dropped: %s"
                    % (
                        s.get("file", "?"),
                        s.get("line", "?"),
                        s.get("severity", "?"),
                        s.get("confidence", "?"),
                        s.get("title", ""),
                        s.get("skeptic_rebuttal", ""),
                        s.get("why_dropped", ""),
                    )
                )
    else:
        lines.append("(none)")
    return "\n".join(lines) + "\n"


# ---- main ---------------------------------------------------------------------------------------


def main(argv):
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--input", default=None, help="JSON path, or '-' for stdin")
    ap.add_argument("--input-env", default="STRUCTURED_OUTPUT")
    ap.add_argument("--phase", type=int, choices=(0, 1), default=0)
    ap.add_argument("--summary-file", default=os.environ.get("GITHUB_STEP_SUMMARY"))
    ap.add_argument("--comment-out", default="review-comment.md")
    ap.add_argument(
        "--diff-file",
        default=None,
        help="unified diff (base...head); used to place findings inline by content",
    )
    ap.add_argument("--review-payload-out", default="review-payload.json")
    ap.add_argument("--head-sha", default=os.environ.get("HEAD_SHA"))
    ap.add_argument(
        "--verdicts-file",
        default=None,
        help="adversarial verify-pass verdicts; findings marked refuted are dropped",
    )
    ap.add_argument(
        "--max-findings", type=int, default=3, help="cap on important+question"
    )
    ap.add_argument(
        "--max-nits", type=int, default=5, help="separate cap on inline nits"
    )
    ap.add_argument("--max-words-per-finding", type=int, default=160)
    ap.add_argument("--max-words-total", type=int, default=600)
    args = ap.parse_args(argv[1:])

    payload, err = load_payload(args)
    if err:
        # A malformed/empty payload must not post anything; record the error to the summary.
        msg = (
            "## Automated first-pass review\n\n%s\n\nReviewer produced no usable output: %s\n"
            % (context_header(args.phase), err)
        )
        if args.summary_file:
            with open(args.summary_file, "a", encoding="utf-8") as fh:
                fh.write(msg)
        sys.stderr.write(err + "\n")
        return 0

    findings = payload.get("findings") or []

    # Adversarial verify pass: drop findings the verifier refuted (by index into the original
    # findings array). Keep them aside for the job-summary record.
    refuted_idx, verdict_reason = load_verdicts(args.verdicts_file)
    verify_ran = bool(verdict_reason)
    surviving, refuted, n_unverified = [], [], 0
    for i, f in enumerate(findings):
        if i in refuted_idx:
            if isinstance(f, dict):
                rf = dict(f)
                rf["_verify_reason"] = verdict_reason.get(i, "")
                refuted.append(rf)
        else:
            surviving.append(f)
            # Kept despite the verify pass producing no verdict for it (incomplete coverage after
            # the retry cap). Floor behavior is keep-and-flag, never block.
            if verify_ran and i not in verdict_reason:
                n_unverified += 1

    kept = gate_findings(
        surviving,
        args.max_words_per_finding,
        args.max_words_total,
        args.max_findings,
        args.max_nits,
    )

    # Record, per kept finding, where it will be placed (for the job summary).
    diff_text = ""
    if args.diff_file:
        try:
            with open(args.diff_file, "r", encoding="utf-8") as fh:
                diff_text = fh.read()
        except OSError as exc:
            sys.stderr.write(
                "could not read diff file %s: %s\n" % (args.diff_file, exc)
            )
    anchors = parse_right_side_lines(diff_text)
    anchor_map = {}
    for f in kept:
        a = finding_anchor(f, anchors)
        if a:
            anchor_map[id(f)] = a

    summary = build_summary(
        payload, kept, anchor_map, refuted, n_unverified, context_header(args.phase)
    )
    if args.summary_file:
        with open(args.summary_file, "a", encoding="utf-8") as fh:
            fh.write(summary)
    else:
        sys.stdout.write(summary)

    if args.phase == 1:
        review = build_review_payload(payload, kept, anchors, args.head_sha)
        with open(args.review_payload_out, "w", encoding="utf-8") as fh:
            json.dump(review, fh)
        with open(args.comment_out, "w", encoding="utf-8") as fh:
            fh.write(build_comment(payload, kept))

        n_inline = len(review["comments"])
        n_sugg = sum(1 for c in review["comments"] if "```suggestion" in c["body"])
        sys.stderr.write(
            "wrote %s + %s (%d kept: %d inline incl %d suggestions, %d in body; %d refuted)\n"
            % (
                args.review_payload_out,
                args.comment_out,
                len(kept),
                n_inline,
                n_sugg,
                len(kept) - n_inline,
                len(refuted),
            )
        )
    return 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv))

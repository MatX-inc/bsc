"""Render pass-1 candidate findings into a verify packet for the adversarial verify pass.

Takes the first reviewer's structured output (--findings, the JSON it returned) and emits a
markdown doc (--out) listing each candidate finding by `index`, with its claim, evidence, and the
reviewer's OWN strongest rebuttal — so the verifier attacks the steelmanned form. The material
(diff, source, conventions) lives in review-packet.md, which the verifier reads alongside this.
The verify prompt + schema are the only instructions. stdlib only.

Usage:
    make_verify_packet.py --findings pass1.json [--out verify-packet.md]
"""

import argparse
import json
import sys


def render(findings):
    out = ["# Verify packet — candidate findings to refute\n"]
    out.append(
        "Each candidate below has an `index`. For every index, return a verdict (refuted true/false "
        "+ reason). The material — diff, source, conventions, CI — is in `review-packet.md`; read it "
        "and the tree (Read/Glob/Grep) to attack each claim. This prompt and the schema are the only "
        "instructions.\n"
    )
    for i, f in enumerate(findings):
        if not isinstance(f, dict):
            continue
        out.append(
            "\n## index %d — [%s, %s] %s"
            % (
                i,
                f.get("severity", "?"),
                f.get("confidence", "?"),
                f.get("title", ""),
            )
        )
        out.append("- location: `%s:%s`" % (f.get("file", "?"), f.get("line", "?")))
        anchor = str(f.get("anchor", "")).strip()
        if anchor:
            out.append("- anchored line: `%s`" % anchor)
        out.append("- claim: %s" % f.get("claim", ""))
        out.append("- evidence: %s" % f.get("evidence", ""))
        out.append("- why it matters: %s" % f.get("why_it_matters", ""))
        out.append("- reviewer's own rebuttal: %s" % f.get("skeptic_rebuttal", ""))
        out.append(
            "- why the reviewer thinks the rebuttal fails: %s"
            % f.get("why_rebuttal_fails", "")
        )
    return "\n".join(out) + "\n"


def main(argv):
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--findings", required=True, help="pass-1 structured output JSON")
    ap.add_argument("--out", default="verify-packet.md")
    args = ap.parse_args(argv[1:])

    try:
        with open(args.findings, "r", encoding="utf-8") as fh:
            data = json.load(fh)
    except (OSError, ValueError) as exc:
        sys.stderr.write("could not read findings %s: %s\n" % (args.findings, exc))
        return 1

    findings = data.get("findings") or []
    with open(args.out, "w", encoding="utf-8") as fh:
        fh.write(render(findings))
    sys.stderr.write("wrote %s (%d candidates)\n" % (args.out, len(findings)))
    return 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv))

"""Exit 0 iff the verify pass produced a verdict for EVERY candidate finding (one per index).

The workflow runs this after a verify attempt to decide whether to retry: a non-zero exit means
the verdict set is missing or incomplete, so the verifier should run again (up to the attempt cap).
stdlib only.

Usage:
    check_verdicts.py --findings pass1.json --verdicts verdicts.json
"""

import argparse
import json
import sys


def _load(path, key):
    with open(path, "r", encoding="utf-8") as fh:
        return json.load(fh).get(key) or []


def main(argv):
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--findings", required=True)
    ap.add_argument("--verdicts", required=True)
    args = ap.parse_args(argv[1:])

    try:
        n = len(_load(args.findings, "findings"))
    except (OSError, ValueError):
        sys.stderr.write("cannot read candidate findings; treating as incomplete\n")
        return 2
    try:
        verdicts = _load(args.verdicts, "verdicts")
    except (OSError, ValueError):
        sys.stderr.write("no readable verdicts yet\n")
        return 1

    have = {
        v["index"]
        for v in verdicts
        if isinstance(v, dict) and type(v.get("index")) is int and v["index"] >= 0
    }
    missing = [i for i in range(n) if i not in have]
    if missing:
        sys.stderr.write("incomplete: missing verdicts for indices %s\n" % missing)
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv))

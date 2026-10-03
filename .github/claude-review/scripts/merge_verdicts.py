"""Merge a verify-pass verdict blob into an accumulating verdicts file, unioned by `index`.

The verify pass is retried; each attempt may cover a different subset of finding indices. Merging
(rather than overwriting) means a later attempt can't drop a refutation an earlier attempt
established for an index it didn't re-cover — a later verdict only REPLACES the same index. Tolerant
of an empty/malformed new blob (the existing file is left as-is). stdlib only.

Usage:
    merge_verdicts.py --into verdicts.json --from-env VERDICTS
"""

import argparse
import json
import os
import sys


def _verdicts(obj):
    out = {}
    for v in (obj.get("verdicts") if isinstance(obj, dict) else None) or []:
        # `type(...) is int` rejects bool (True would otherwise count as index 1); also reject negatives.
        if isinstance(v, dict) and type(v.get("index")) is int and v["index"] >= 0:
            out[v["index"]] = v
    return out


def main(argv):
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument(
        "--into", required=True, help="accumulating verdicts file (created if absent)"
    )
    ap.add_argument(
        "--from-env", required=True, help="env var holding the new verdicts JSON"
    )
    args = ap.parse_args(argv[1:])

    raw = os.environ.get(args.from_env, "").strip()
    try:
        new = json.loads(raw) if raw else {}
    except ValueError:
        new = {}
    try:
        with open(args.into, "r", encoding="utf-8") as fh:
            cur = json.load(fh)
    except (OSError, ValueError):
        cur = {}

    by_idx = _verdicts(cur)
    for idx, v in _verdicts(new).items():
        prev = by_idx.get(idx)
        # Refutation is MONOTONIC: once any attempt refutes an index, a later attempt cannot
        # resurrect the finding by reporting it not-refuted. A not-refuted index CAN be upgraded to
        # refuted. (The retry exists to cover MISSING indices, not to re-litigate decided ones.)
        if (
            prev is not None
            and bool(prev.get("refuted"))
            and not bool(v.get("refuted"))
        ):
            continue  # keep the earlier refutation
        by_idx[idx] = v
    merged = {"verdicts": [by_idx[i] for i in sorted(by_idx)]}
    with open(args.into, "w", encoding="utf-8") as fh:
        json.dump(merged, fh)
    sys.stderr.write("merged verdicts: %d total\n" % len(by_idx))
    return 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv))

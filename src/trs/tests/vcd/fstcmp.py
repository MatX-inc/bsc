#!/usr/bin/env python3
"""Semantic VCD comparison for FST parity checks.

FST bytes are not comparable (libfst embeds a timestamp) and fst2vcd
re-letters id codes and zero-pads values, so FST parity is asserted
on the DECODED stream: same scope tree, same (scope-qualified) var
names and widths with the same alias grouping, and the same
per-time name->value change sets (values canonicalized by stripping
leading zeros).

usage: fstcmp.py a.vcd b.vcd   (either side may come from fst2vcd)
"""
import sys


def parse(path):
    scopes, defs, changes = [], {}, {}
    stack, code2names, t = [], {}, 0
    code_width = {}
    with open(path) as f:
        it = iter(f.read().split("\n"))
        for line in it:
            w = line.split()
            if not w:
                continue
            if w[0] == "$scope":
                stack.append(w[2])
                scopes.append(tuple(stack))
            elif w[0] == "$upscope":
                stack.pop()
            elif w[0] == "$var":
                width, code, name = int(w[2]), w[3], w[4]
                qual = ".".join(stack + [name])
                code2names.setdefault(code, []).append(qual)
                code_width[code] = width
                defs[qual] = (width, code)
            elif w[0].startswith("#") and w[0][1:].isdigit():
                t = int(w[0][1:])
            elif w[0].startswith("$"):
                continue  # $date/$version/$dump* task markers/$end
            elif w[0][0] in "01x" and len(w) == 1 and len(w[0]) > 1:
                code, v = w[0][1:], w[0][0]
                for n in code2names.get(code, []):
                    changes.setdefault(t, {})[n] = v
            elif w[0][0] == "b" and len(w) == 2:
                v = w[0][1:].lstrip("0") or ("x" if "x" in w[0] else "0")
                if "x" in w[0]:
                    v = "x"
                for n in code2names.get(w[1], []):
                    changes.setdefault(t, {})[n] = v
    # alias groups: the sets of names sharing a code, order-free
    aliases = sorted(
        tuple(sorted(ns)) for ns in code2names.values() if len(ns) > 1
    )
    widths = {q: wd for q, (wd, _) in defs.items()}
    return scopes, widths, aliases, changes


def main():
    a, b = parse(sys.argv[1]), parse(sys.argv[2])
    labels = ["scope tree", "var widths", "alias groups", "changes"]
    for i, lbl in enumerate(labels):
        if a[i] != b[i]:
            print(f"MISMATCH: {lbl}")
            if lbl == "changes":
                ts = sorted(set(a[3]) | set(b[3]))
                for t in ts:
                    if a[3].get(t) != b[3].get(t):
                        print(f"  first diff at #{t}:")
                        da, db = a[3].get(t, {}), b[3].get(t, {})
                        for k in sorted(set(da) | set(db)):
                            if da.get(k) != db.get(k):
                                print(f"    {k}: {da.get(k)} vs {db.get(k)}")
                        break
            sys.exit(1)
    print("MATCH")


main()

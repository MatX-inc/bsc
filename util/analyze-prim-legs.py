#!/usr/bin/env python3
"""Join per-action Bsc timings from two sets of bazel JSON profiles."""
import gzip, json, sys

def load(paths):
    acts = {}
    for p in paths:
        raw = gzip.open(p, 'rt', errors='ignore').read()
        try:
            data = json.loads(raw)
        except json.JSONDecodeError:
            idx = raw.rfind('},')
            data = json.loads(raw[:idx+1] + ']}')
        for e in data['traceEvents']:
            if e.get('ph') != 'X' or e.get('cat') != 'action processing':
                continue
            a = e.get('args') or {}
            if a.get('mnemonic') not in ('BscV', 'BscBa'):
                continue
            # later profiles win (resume re-ran only what was missing)
            acts[e['name']] = e['dur'] / 1e6
    return acts

a = load(sys.argv[1].split(','))
b = load(sys.argv[2].split(','))
common = sorted(set(a) & set(b))
only_a, only_b = set(a) - set(b), set(b) - set(a)
ta = sum(a[k] for k in common)
tb = sum(b[k] for k in common)
print(f"actions: A={len(a)} B={len(b)} common={len(common)} onlyA={len(only_a)} onlyB={len(only_b)}")
print(f"sum over common actions: A={ta:.1f}s  B={tb:.1f}s  B/A={tb/ta:.3f}")
rows = [(b[k] - a[k], a[k], b[k], k) for k in common]
rows.sort()
print("\n== biggest B improvements (delta s, A s, B s) ==")
for d, xa, xb, k in rows[:15]:
    print(f"{d:9.1f} {xa:8.1f} {xb:8.1f}  {k}")
print("\n== biggest B regressions ==")
for d, xa, xb, k in rows[-15:][::-1]:
    print(f"{d:9.1f} {xa:8.1f} {xb:8.1f}  {k}")
n_faster = sum(1 for d, *_ in rows if d < -0.05)
n_slower = sum(1 for d, *_ in rows if d > 0.05)
print(f"\nfaster in B: {n_faster}  slower in B: {n_slower}  ~same: {len(rows)-n_faster-n_slower}")
if only_a: print("only in A (sample):", sorted(only_a)[:5])
if only_b: print("only in B (sample):", sorted(only_b)[:5])

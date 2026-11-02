"""Checkpoint data, full state against working sets: for genbench instances, the total size of
full-state checkpoints (what the proved chain writes) and of per-segment working-set checkpoints
(the slots each segment's steps read or write: every slot of each child they touch).
usage: wset.py USES STEPS SEGSTEPS"""
import sys, os
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import scanlib as S
import genbench as G
U, NS, SEG = int(sys.argv[1]), int(sys.argv[2]), int(sys.argv[3])
D, nK = G.gen(U, NS)
ok, states, info = S.run_scan(D)
assert ok
fps = list(D.fpM) + list(D.fpR)
n = len(fps)
cuts = list(range(SEG, n, SEG))
full = sum(len(states[c - 1]) for c in cuts)
nports = {}
for kd, tab in enumerate(D.kt):
    nports[kd] = len(tab)
segs = [0] + cuts + [n]
ws = 0
for a, b in zip(segs, segs[1:]):
    w = set()
    for g in range(a, b):
        for (base, kd, c) in fps[g]:
            w.update(base + x for x in range(nports[kd]))
    ws += len(w)
uses = sum(len(f) for f in fps)
print('uses %d steps %d segments %d | full-state checkpoints: %d slot records | working sets: %d slot records (%.2f per use)'
      % (uses, n, len(segs) - 1, full, ws, ws / uses))

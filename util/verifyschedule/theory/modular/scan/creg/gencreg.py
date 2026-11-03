"""Scan probe shaped like mkCregInlineBlowup: nR rules, rule i reads and writes its own register
(two slots of its own child), full-state checkpoints every SEG rules, as gentasksC.py emits.
usage: gencreg.py NR SEG OUT.lean SCANTOOLS"""
import sys, os
sys.path.insert(0, sys.argv[4])
import scanlib as S
nR, SEG, out = int(sys.argv[1]), int(sys.argv[2]), sys.argv[3]
k = S.creg(1)
kt = [[k.kc(c) for c in range(len(k.ports))]]
d = (2 * nR - 1).bit_length()
fpR = [[(2 * i, 0, 0), (2 * i, 0, 1)] for i in range(nR)]
D = S.Design(0, nR, d, [], fpR, kt, [0], [0], [0], [0], [0], [0], [0] * nR, [0] * nR, [0] * nR)
if os.environ.get('SCAN_CODES', '1') == '1':
    S.compress(D)      # rules in no exR/escR mask share code 0: slot records do not grow with nR
ok, states, info = S.run_scan(D)
assert ok, info
cuts = list(range(SEG, nR, SEG))
L = ['import Scan', 'set_option maxRecDepth 100000', 'set_option maxHeartbeats 0', 'namespace CregB',
     'open Modular Scan Design', '', S.emit_design(D), '']
cp_bytes = 0
for j, c in enumerate(cuts):
    t = S.emit_tree(states[c - 1], D.d, 'cp%d' % (j + 1))
    cp_bytes += len(t)
    L.append(t)
L.append('set_option profiler true')
prev, prevn, names = 0, 'ST.init', []
for j, c in enumerate(cuts):
    L.append('theorem seg%d : D.seg %d %d %s cp%d = true := by decide +kernel' % (j, prev, c - prev, prevn, j + 1))
    names.append((c - prev, 'cp%d' % (j + 1), 'seg%d' % j))
    prev, prevn = c, 'cp%d' % (j + 1)
L.append('theorem segZ : D.segLast %d %s = true := by decide +kernel' % (prev, prevn))
L.append('set_option profiler false')
CS = '[' + ', '.join('(%d, %s)' % (m, cp) for (m, cp, _) in names) + ']'
PF = ('⟨' + ', '.join(nm for (_, _, nm) in names) + ', segZ⟩') if names else 'segZ'
L += ['theorem ok : D.Ok := D.ok_of_chain %s %s' % (CS, PF), 'end CregB', '#print axioms CregB.ok']
open(out, 'w').write('\n'.join(L) + '\n')
print('rules', nR, 'uses', 2 * nR, 'segments', len(cuts) + 1, 'checkpoint bytes', cp_bytes, 'file', sum(len(x) + 1 for x in L))

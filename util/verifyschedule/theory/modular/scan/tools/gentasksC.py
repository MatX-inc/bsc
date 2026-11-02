"""Scan probe, task statements as a chain kind: one Tasks n child (one slot, code 0), nR rules
calling consecutive statements; D.Ok plus the chain checks (positions in each rule's interval,
increasing, intervals monotone).  gentasksC.py N NR OUT.lean SCANTOOLS"""
import sys, os
sys.path.insert(0, sys.argv[4])
import scanlib as S
n, nR, out = int(sys.argv[1]), int(sys.argv[2]), sys.argv[3]
SEG = int(sys.argv[5]) if len(sys.argv) > 5 else nR
kt = [[([], [0], [], [0])]]
per = n // nR
fpR = [[(0, 0, 0)] * per for i in range(nR)]
pos = [list(range(i * per, (i + 1) * per)) for i in range(nR)]
D = S.Design(0, nR, 1, [], fpR, kt, [0], [0], [0], [0], [0], [0], [0] * nR, [0] * nR, [0] * nR)
if os.environ.get('SCAN_CODES', '1') == '1':
    S.compress(D)      # rules in no exR/escR mask share code 0: slot records do not grow with nR
ok, states, info = S.run_scan(D)
assert ok, info


cuts = list(range(SEG, nR, SEG))
SEGS = [S.emit_tree(states[c - 1], D.d, 'cp%d' % (j + 1)) for j, c in enumerate(cuts)]
prev, prevn, names = 0, 'ST.init', []
for j, c in enumerate(cuts):
    SEGS.append('theorem seg%d : D.seg %d %d %s cp%d = true := by decide +kernel' % (j, prev, c - prev, prevn, j + 1))
    names.append((c - prev, 'cp%d' % (j + 1), 'seg%d' % j))
    prev, prevn = c, 'cp%d' % (j + 1)
SEGS.append('theorem segZ : D.segLast %d %s = true := by decide +kernel' % (prev, prevn))
CS = '[' + ', '.join('(%d, %s)' % (m, cp) for (m, cp, _) in names) + ']'
PF = ('⟨' + ', '.join(nm for (_, _, nm) in names) + ', segZ⟩') if names else 'segZ'


def tab(items):
    def go(lo, hi):
        if hi - lo == 1:
            return '(.leaf (%s))' % items[lo]
        mid = (lo + hi) // 2
        return '(.node %d %s %s)' % (mid, go(lo, mid), go(mid, hi))
    return go(0, len(items))


MK = lambda l: '#eval show IO Unit from do let t ← IO.monoMsNow; IO.println s!"@@ %s {t}"' % l
rl = lambda xs: '[' + ', '.join('nat_lit %d' % x for x in xs) + ']'
L = ['import Scan', MK('start'), 'set_option maxRecDepth 100000', 'set_option maxHeartbeats 0', 'namespace TBenchC',
     'open Modular Scan Design', 'open Emit (Tab)', '', '-- tasks %d, rules %d (chain kind)' % (n, nR), '', S.emit_design(D), '',
     S.emit_table('posT', 'List Nat', [rl(p) for p in pos], [len(p) + 1 for p in pos]),
     S.emit_table('cloT', 'Nat', ['nat_lit %d' % (i * per) for i in range(nR)]),
     S.emit_table('chiT', 'Nat', ['nat_lit %d' % ((i + 1) * per) for i in range(nR)]),
     MK('data'), 'set_option profiler true'] + SEGS + [
     'theorem inT : Emit.Tab.allB (fun i ps => incB ps && ps.all (fun x => Nat.ble (cloT.get i) x && Nat.blt x (chiT.get i))) posT 0 %d = true := by decide +kernel' % nR,
     'theorem monoT : Emit.Tab.allB (fun i h => Nat.beq i %d || Nat.ble h (cloT.get (i + 1))) chiT 0 %d = true := by decide +kernel' % (nR - 1, nR),
     'set_option profiler false',
     'theorem ok : D.Ok := D.ok_of_chain %s %s' % (CS, PF), MK('end'), 'end TBenchC', '#print axioms TBenchC.ok', '#print axioms TBenchC.inT']
open(out, 'w').write('\n'.join(L) + '\n')
print('n', n, 'rules', nR, 'bytes', os.path.getsize(out))

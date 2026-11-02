"""Mux-chain probe, suffix form (cost review r7-1 M1): u members in one mutually_exclusive group whose
checks are bsc's suffixes, stated once as SufAsm over the member list (O(u) data); the mux's chain
from amoChain_of_grp (a sublist check, no per-member data); member lists are definitions, as in the e2e.  usage: genchain8.py U OUT.lean [STRIDE]
STRIDE > 1 makes the mux every STRIDE-th group member (a proper sublist)."""
import sys
u, out = int(sys.argv[1]), sys.argv[2]
stride = int(sys.argv[3]) if len(sys.argv) > 3 else 1
grp = [3 * k + 7 for k in range(u)]
mem = grp[::stride]
MK = lambda l: '#eval show IO Unit from do let t ← IO.monoMsNow; IO.println s!"@@ %s {t}"' % l
lst = lambda xs: '[' + ', '.join('nat_lit %d' % x for x in xs) + ']'   # raw literals: no OfNat search
L = ['import Scan', MK('start'), 'set_option maxRecDepth 100000', 'set_option maxHeartbeats 0',
     'namespace ChainS', 'open Modular Scan', '',
     'noncomputable def grp : List Nat := ' + lst(grp),
     'noncomputable def memL : List Nat := ' + lst(mem), MK('data'),
     'variable (f : Nat → Bool)',
     '/-- the group\'s assertions: each member against every later one -/',
     'def asmP : Prop := SufAsm f grp',
     'set_option profiler true',
     'theorem chain (hA : asmP f) : amoChain false (memL.map f) :=',
     '  amoChain_of_grp f memL grp (by decide +kernel) hA',
     'set_option profiler false', MK('chain'), 'end ChainS', '#print axioms ChainS.chain']
open(out, 'w').write('\n'.join(L) + '\n')
print('u', u, 'mux members', len(mem), 'pairs', len(mem) * (len(mem) - 1) // 2)

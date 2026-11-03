"""Mux-chain probe, linear form: u members, member k's run-time check covers members k+1.. (suffix
lists, as bsc's mutually_exclusive checks; the reviewer's genchain.py shape). Coverage is one
decided pass (covB) over (member, cover bit set) pairs; no tail lists.  usage: genchain7.py U OUT.lean [UNC]
UNC > 0 leaves the last UNC later members of every member uncovered (pairwise proofs)."""
import sys
u, out = int(sys.argv[1]), sys.argv[2]
unc_n = int(sys.argv[3]) if len(sys.argv) > 3 else 0
mem = [3 * k + 7 for k in range(u)]          # rule numbers, ascending as in the e2e
def tab(items):
    def go(lo, hi):
        if hi - lo == 1:
            return '(.leaf (%s))' % items[lo]
        mid = (lo + hi) // 2
        return '(.node %d %s %s)' % (mid, go(lo, mid), go(mid, hi))
    return go(0, len(items))
def tabp(items):
    def go(lo, hi):
        if hi - lo == 1:
            return items[lo]
        mid = (lo + hi) // 2
        return '⟨%s,\n     %s⟩' % (go(lo, mid), go(mid, hi))
    return go(0, len(items))
lst = lambda xs: '[' + ', '.join(map(str, xs)) + ']'
MK = lambda l: '#eval show IO Unit from do let t ← IO.monoMsNow; IO.println s!"@@ %s {t}"' % l
rlst = lambda xs: '[' + ', '.join('nat_lit %d' % x for x in xs) + ']'   # raw literals: no OfNat search
nC = u - 1
# check c (head mem[c]) covers the later members except the last unc_n ones
chk = [mem[c + 1:len(mem) - unc_n] if unc_n else mem[c + 1:] for c in range(nC)]
L = ['import Scan', MK('start'), 'set_option maxRecDepth 100000', 'set_option maxHeartbeats 0', 'set_option profiler.threshold 200',
     'namespace ChainP', 'open Modular Scan', 'open Emit (Tab)', '',
     'noncomputable def chkHead : Tab Nat := ' + tab(['nat_lit %d' % mem[c] for c in range(nC)]),
     '\n'.join('noncomputable def chk_%d : List Nat := %s' % (c, rlst(chk[c])) for c in range(nC)),
     'noncomputable def chkLst : Tab (List Nat) := ' + tab(['chk_%d' % c for c in range(nC)]),
     MK('data'),
     'abbrev nC : Nat := %d' % nC, '',
     'variable (f : Nat → Bool)',
     'def asmP : Prop := ∀ c, Nat.blt c nC = true → (f (chkHead.get c) && orL ((chkLst.get c).map f)) = false',
     'theorem chkC (hA : asmP f) (c : Nat) (hc : Nat.blt c nC = true) :',
     '    ∀ y ∈ chkLst.get c, f (chkHead.get c) = true → f y = true → False :=',
     '  forall_of_check (hA c hc)',
     'set_option profiler true',
     '/-- `hx`: the facts the uncovered pairs are proved from, one by one -/',
     'theorem chain (hA : asmP f) (hx : ∀ x y, x < y → f x = true → f y = true → False) : amoChain false (%s.map f) :=' % lst(mem),
     '  amoChain_of_cover f (fun x y => f x = true → f y = true → False) (fun _ _ h => h)']
ms, pf = [], []
for k in range(u):
    later = mem[k + 1:]
    unc = [y for y in later if (k == u - 1) or (y not in chk[k] if k < nC else True)]
    if k < nC:
        cov = 'chkLst.get %d' % k
        h1 = '(chkC f hA %d rfl)' % k
    else:
        cov = '[]'
        h1 = '(fun y hy => absurd hy (List.not_mem_nil))'
    ms.append('(%d, cvS (%s) %s)' % (mem[k], cov, lst(unc)))
    pw = ('⟨' + ', '.join(['hx %d %d (by decide)' % (mem[k], y) for y in unc] + ['trivial']) + '⟩') if unc else 'trivial'
    pf.append('cover_of %s %s' % (h1, pw))
L.append('    msT %s (by decide +kernel) (by decide +kernel)' % lst(mem))
L.append('    ' + tabp(pf))
L.insert(L.index('set_option profiler true'), 'noncomputable def msT : Tab (Nat × Nat) := ' + tab(ms))
L += [MK('chain'), 'set_option profiler false', 'end ChainP', '#print axioms ChainP.chain']
open(out, 'w').write('\n'.join(L) + '\n')
print('u', u, 'pairs', u * (u - 1) // 2, 'uncovered per member', unc_n)

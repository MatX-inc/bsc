# Lookups in a table parameterized by the operators against a closed table of
# functions (conventions.md C2): n entries, 256 lemmas each looking one up.
# usage: python3 -I mktabbench.py n out.lean
import sys
n = int(sys.argv[1]); out = sys.argv[2]
NC = ''
# leaves of the closed table: binder types annotated unless argv[3] == 'bare'
LAM = 'fun V ops x => ops.g %d x' if len(sys.argv) > 3 and sys.argv[3] == 'bare' else 'fun (V : Type) (ops : Ops V) (x : V) => ops.g %d x'
def tab(items):
    def go(lo, hi):
        if hi - lo == 1: return "(.leaf (%s))" % items[lo]
        mid = (lo + hi) // 2
        return "(.node %d %s\n  %s)" % (mid, go(lo, mid), go(mid, hi))
    return go(0, len(items))
L = ["import EmitLemmas\nopen Emit\nset_option maxRecDepth 100000\nset_option maxHeartbeats 0\n",
     "structure Ops (V : Type) where\n  g : Nat → V → V\n",
     "def mark (s : String) : IO Unit := do let h ← IO.getNumHeartbeats; let t ← IO.monoMsNow; IO.println s!\"@@ {s} {t} {h}\"\n",
     "#eval mark \"start\"\n",
     NC + "def tp {V : Type} (ops : Ops V) : Tab (V → V) :=\n  %s\n" % tab(["fun x => ops.g %d x" % i for i in range(n)]),
     "#eval mark \"tp\"\n",
     NC + "def tc : Tab ((V : Type) → Ops V → V → V) :=\n  %s\n" % tab([(LAM % i) for i in range(n)]),
     "#eval mark \"tables\"\n"]
idx = [(i * 7919) % n for i in range(256)]
for k, i in enumerate(idx):
    L.append("theorem p%d {V : Type} (ops : Ops V) (x : V) : (tp ops).get %d x = ops.g %d x := rfl\n" % (k, i, i))
L.append("#eval mark \"parametrized\"\n")
for k, i in enumerate(idx):
    L.append("theorem c%d {V : Type} (ops : Ops V) (x : V) : tc.get %d V ops x = ops.g %d x := rfl\n" % (k, i, i))
L.append("#eval mark \"closed\"\n")
open(out, "w").write("".join(L))

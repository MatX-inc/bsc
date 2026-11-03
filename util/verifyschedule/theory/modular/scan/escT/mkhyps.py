"""The emitter's fact structures at N facts in a small emitted file's context (cost review r8b-1 M2): the group
structures and `Unconfirmed` exactly as mke2e.py writes them for genescE's `facts N` (FULL, emitted with raised
bounds), spliced into the 8-fact file CTX in place of its own, plus one theorem per group using each of its facts'
projections (`hr.gK.esc_i`, as the escape proofs use them); profiler regions and marks around both.
usage: mkhyps.py CTX.lean FULL.lean OUT.lean"""
import re, sys
ctx, full, out = sys.argv[1:4]
MK = ('#eval show IO Unit from do let t ← IO.monoMsNow; let h ← IO.getNumHeartbeats; '
      'let st ← IO.FS.readFile "/proc/self/status"; '
      'let m := (st.splitOn "\\n").filter (fun l => l.startsWith "VmRSS" || l.startsWith "VmHWM"); '
      'IO.eprintln s!"@@ %s {t} {h} {m}"')


def block(txt):
    a = txt.index('/-- Unconfirmed, facts 0 to')
    b = txt.index('\ntheorem meX ')
    return a, b


C = open(ctx).read()
F = open(full).read()
ca, cb = block(C)
fa, fb = block(F)
blk = F[fa:fb]
groups = re.findall(r'^structure UnconfirmedG(\d+) ', blk, re.M)
stmt = {}
for m in re.finditer(r'^  (esc_\d+) : (.*)$', blk, re.M):
    stmt[m.group(1)] = m.group(2)
uses = []
for g in groups:
    gb = blk[blk.index('structure UnconfirmedG%s ' % g):]
    gb = gb[:gb.index('\n/--', 1)] if '\n/--' in gb[1:] else gb
    names = re.findall(r'^  (esc_\d+) :', gb, re.M)

    def conj(lo, hi):
        if hi - lo == 1:
            return '(%s)' % stmt[names[lo]]
        m = (lo + hi) // 2
        return '(%s ∧ %s)' % (conj(lo, m), conj(m, hi))

    def prf(lo, hi):
        if hi - lo == 1:
            return 'hr.g%s.%s' % (g, names[lo])
        m = (lo + hi) // 2
        return '⟨%s, %s⟩' % (prf(lo, m), prf(m, hi))
    uses.append('theorem useG%s (ops : Ops V) (E : EqC V) (hr : Unconfirmed ops E) : %s :=\n  %s'
                % (g, conj(0, len(names)), prf(0, len(names))))
pre = '\n'.join([MK % 'hypsA', 'set_option profiler true', 'set_option profiler.threshold 0', ''])
mid = '\n'.join(['set_option profiler false', MK % 'hypsB', MK % 'useA', 'set_option profiler true'] + uses +
                ['set_option profiler false', MK % 'useB', ''])
open(out, 'w').write(C[:ca] + pre + blk + '\n' + mid + C[cb:])
print(out, 'facts', len(stmt), 'groups', len(groups), 'bytes', len(C[:ca] + pre + blk + mid + C[cb:]))

"""Timing marks and profiler regions around a generated file's fact structures (from the first group structure, or
`Hyps`' docstring, to `theorem meX`) and its escape block (`def escB_0` .. `def escL`), as the cost review's
insmarks.py, which started at `Hyps`' docstring. usage: insmarks2.py FILE.lean... (in place)"""
import sys

MK = ('#eval show IO Unit from do let t ← IO.monoMsNow; let h ← IO.getNumHeartbeats; '
      'let st ← IO.FS.readFile "/proc/self/status"; '
      'let m := (st.splitOn "\\n").filter (fun l => l.startsWith "VmRSS" || l.startsWith "VmHWM"); '
      'IO.eprintln s!"@@ %s {t} {h} {m}"')
for f in sys.argv[1:]:
    L = open(f).read().split('\n')
    out, n, ha = [], 0, False
    for i, ln in enumerate(L):
        if not ha and (ln.startswith('/-- Hyps, facts 0') or ln.startswith('/-- Unconfirmed, facts 0') or
                       ln.startswith("/-- bsc's solver verdicts")):
            out += [MK % 'hypsA', 'set_option profiler true', 'set_option profiler.threshold 0']; n += 1; ha = True
        if ln.startswith('theorem meX '):
            out += ['set_option profiler false', MK % 'hypsB']; n += 1
        if ln.startswith('def escB_0') and not any('escA' in x for x in out):
            out += [MK % 'escA', 'set_option profiler true', 'set_option profiler.threshold 0']; n += 1
        if ln.startswith('def escL '):
            out += ['set_option profiler false', MK % 'escB']; n += 1
        out.append(ln)
    open(f, 'w').write('\n'.join(out))
    print(f, 'marks inserted', n)

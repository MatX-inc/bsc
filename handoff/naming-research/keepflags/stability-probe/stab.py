# usage: python3 -I stab.py <probe/out> : for each probe and edit, per variant, the netlist names
# of the base design that disappear after the edit (lost), and the names the edit adds, split into
# names that mention the edited entity (related) and the rest (unrelated)
import os, sys, collections
sys.path.insert(0, '/home/ravi/claude-tmp/claude-1548697660/-home-ravi/6640a2f0-3ba9-46af-8f63-2f02110094cd/scratchpad/isimp/work/keepflags/keep-suites/bin')
from vnames import modules
out = sys.argv[1]
REL = {'addrule': ['clear_acc', 'reset_held'], 'addrule_top': ['clear_acc', 'reset_held'],
       'addreg': ['seen'], 'addmeth': ['flush'], 'addvmeth': ['busy', 'isBusy'],
       'addsub': ['peak', 'track', 'hits', 'count_hits']}
TOP = {'Probe': 'mkProbe', 'Probe2': 'mkProbe2'}
detail = '-v' in sys.argv
tot = collections.defaultdict(lambda: [0, 0, 0, 0])
for p in ['Probe', 'Probe2']:
    for e in ['addrule', 'addrule_top', 'addreg', 'addmeth', 'addvmeth', 'addsub']:
        row = []
        for v in ['MAIN', 'K', 'KM', 'M']:
            b = modules(os.path.join(out, v, p, 'base', 'v', TOP[p] + '.v'))[TOP[p]]
            x = modules(os.path.join(out, v, p, e, 'v', TOP[p] + '.v'))[TOP[p]]
            lost = sorted(set(b) - set(x)); new = sorted(set(x) - set(b))
            rel = [n for n in new if any(r in n for r in REL[e])]
            unrel = [n for n in new if n not in rel]
            lostk = collections.Counter(b[n] for n in lost)
            row.append('%s: base %d, lost %d %s, new %d (related %d, unrelated %d)' % (
                v, len(b), len(lost), dict(lostk) if lost else '', len(new), len(rel), len(unrel)))
            t = tot[v]; t[0] += len(b); t[1] += len(lost); t[2] += len(rel); t[3] += len(unrel)
            if detail and (lost or unrel):
                row.append('    lost: %s' % ' '.join(lost))
                row.append('    unrelated new: %s' % ' '.join(unrel))
        print('== %s %s' % (p, e)); print('\n'.join('  ' + r for r in row))
print('== totals over 12 edits: base names, lost, related new, unrelated new')
for v, t in tot.items(): print('  %s: %s' % (v, t))

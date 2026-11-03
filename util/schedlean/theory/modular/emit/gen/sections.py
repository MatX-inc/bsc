# Section costs from @@ markers in profile outputs: wall seconds and
# heartbeats (allocations, x1e6; independent of machine load).
import sys
rows = {}
for f in sys.argv[1:]:
    marks = [l.split() for l in open(f) if l.startswith('@@ ')]
    tot = [l.strip() for l in open(f) if l.startswith('TOTAL')]
    d = {}
    for a, b in zip(marks, marks[1:]):
        hb = (int(b[3]) - int(a[3])) / 1e6 if len(b) > 3 else 0
        d[b[1]] = '%.2f s %6.1f Mhb' % ((int(b[2]) - int(a[2])) / 1000, hb)
    d['TOTAL'] = tot[0][6:] if tot else '?'
    rows[f] = d
names = list(next(iter(rows.values())).keys())
w = 26
print('section'.ljust(16) + ''.join(f.split('/')[-1].ljust(w) for f in rows))
for n in names:
    print(n.ljust(16) + ''.join(str(rows[f].get(n, '')).ljust(w) for f in rows))

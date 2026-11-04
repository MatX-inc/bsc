"""Per-section wall time (s) and heartbeats (Mhb) from a log's `@@ label ms hb` markers."""
import re, sys
for f in sys.argv[1:]:
    rows = [(m.group(1), int(m.group(2)), int(m.group(3)) if m.group(3) else 0)
            for m in re.finditer(r'@@ (\S+) (\d+)(?: (\d+))?', open(f).read())]
    out = []
    for (a, t0, h0), (b, t1, h1) in zip(rows, rows[1:]):
        out.append('%s %.1fs %.0fMhb' % (b, (t1 - t0) / 1000, (h1 - h0) / 1e6))
    print(f, '|', ', '.join(out))

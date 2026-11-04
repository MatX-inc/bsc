"""Per-section wall seconds, Mhb, RSS and HWM (GiB) at each section's end, from @@ marks in logs/*.full."""
import re, sys
for f in sys.argv[1:]:
    ms = []
    for l in open(f):
        if l.startswith('@@ '):
            p = l.split()
            hwm = re.search(r'VmHWM:\s*(\d+)', l); rss = re.search(r'VmRSS:\s*(\d+)', l)
            ms.append((p[1], int(p[2]), int(p[3]), int(hwm.group(1)) / 2**20 if hwm else 0, int(rss.group(1)) / 2**20 if rss else 0))
    tot = [l for l in open(f) if l.startswith('WALL')]
    print('==', f.split('/')[-1], tot[-1].strip() if tot else '')
    for a, b in zip(ms, ms[1:]):
        print('  %-10s %8.2f s %8.0f Mhb  RSS %5.2f  HWM %5.2f' % (b[0], (b[1] - a[1]) / 1000, (b[2] - a[2]) / 1e6, b[4], b[3]))

"""Totals over a parts run (logs/TAG_p*.full, TAG_last.full): user, wall, max RSS, section Mhb and seconds."""
import glob, re, sys
for tag in sys.argv[1:]:
    fs = sorted(glob.glob('logs/%s_p*.full' % tag), key=lambda f: int(re.search(r'_p(\d+)\.full', f).group(1))) + ['logs/%s_last.full' % tag]
    U = W = 0.0; R = 0; mhb = 0.0; secs = {}
    for f in fs:
        txt = open(f).read()
        m = re.search(r'WALL ([\d.]+) USER ([\d.]+) SYS [\d.]+ MAXRSS_KB (\d+)', txt)
        W += float(m.group(1)); U += float(m.group(2)); R = max(R, int(m.group(3)))
        ms = [l.split() for l in txt.split('\n') if l.startswith('@@ ')]
        for a, b in zip(ms, ms[1:]):
            d = (int(b[3]) - int(a[3])) / 1e6
            mhb += d
            secs[b[1]] = secs.get(b[1], 0) + d
    print('%s: %d files, wall %.1f s, user %.1f s, max RSS %.2f GiB, %.0f Mhb between marks' % (tag, len(fs), W, U, R / 2**20, mhb))
    print('   ' + ', '.join('%s %.0f' % (k, v) for k, v in secs.items()))

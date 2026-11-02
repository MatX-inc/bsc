"""Every count of the site tool's summary lines that changes between the round-7 tool (out_base_*) and
the new one (out_new_*), per mode and design; the new readiness counters are listed separately."""
import os, re, sys, ast
HERE = os.path.dirname(os.path.abspath(__file__))
NEWKEYS = ('hw_lits', 'hw_sites', 'eq_lits', 'eq_over_cond', 'nolit', 'lit_not_first')


def fields(line):
    name = line.split()[0]
    out = {}
    for part in line.split(' | '):
        part = part.strip()
        m = re.match(r'^(bscFacts|all|rdyRule|rdySiteValue) (.*)$', part)
        if m:
            try:
                v = ast.literal_eval(m.group(2))
            except Exception:
                v = m.group(2)
            if isinstance(v, dict):
                for k, x in v.items():
                    out['%s.%s' % (m.group(1), k)] = x
            else:
                out[m.group(1)] = v
            continue
        if part.startswith(name):
            part = part[len(name):]
        # "label number" pairs, labels possibly multi-word
        toks = re.findall(r'([A-Za-z][A-Za-z ()/:;,]*?)\s+(-?\d+)', part)
        seen = {}
        for lab, num in toks:
            lab = re.sub(r'\s+', ' ', lab.strip(' ,;:()'))
            k = lab
            seen[k] = seen.get(k, 0) + 1
            if seen[k] > 1:
                k = '%s#%d' % (k, seen[k])
            out['%s' % k] = int(num)
    return name, out


def load(p):
    res = {}
    for ln in open(p):
        if ln.startswith(' ') or ' sites ' not in ln:
            continue
        n, f = fields(ln.rstrip('\n'))
        res[n] = f
    return res


tot = 0
for mode in ('default', 'noagg', 'nosched', 'both', 'corpus'):
    b = load(os.path.join(HERE, 'out_base_' + mode, 'run.txt'))
    n = load(os.path.join(HERE, 'out_new_' + mode, 'run.txt'))
    for d in sorted(set(b) | set(n)):
        if d not in b or d not in n:
            print(mode, d, 'only in', 'new' if d in n else 'base')
            continue
        for k in sorted(set(b[d]) | set(n[d])):
            if k.split('.')[-1] in NEWKEYS and k not in b[d]:
                if n[d][k]:
                    print('%-8s %-24s new counter %s = %s' % (mode, d, k, n[d][k]))
                continue
            if b[d].get(k) != n[d].get(k):
                tot += 1
                print('%-8s %-24s CHANGED %s: %s -> %s' % (mode, d, k, b[d].get(k), n[d].get(k)))
print('changed counts:', tot)

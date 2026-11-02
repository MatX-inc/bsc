"""Run the proved scan (Python mirror of Scan.lean) on corpus dumps.

corpus.py DIR...   (each DIR has dump.txt from schedlean --dump-design)

Per design: footprints from the step uses (out-of-order reads skipped, as
r5/tools/portscan.py); one kind per child, tables from the child's
annotations (unlisted pairs C, copies of one method CF, as portscan); a value
port is flagged (argument-less) when no copy has an argument; methods merge
their split steps; exceptions are the dump's disjoint / urgency /
mutually_exclusive pairs as masks.  Failing checks are counted and the pass
continues (as if escaped).  Unlike portscan, a share port with arguments
used by two steps conflicts (the scan cannot see that the arguments agree).
"""
import os
import re
import sys


def split_top(s):
    out, depth, cur = [], 0, ''
    for ch in s:
        if ch == '[':
            depth += 1
        elif ch == ']':
            depth -= 1
        if ch == ',' and depth == 0:
            out.append(cur.strip())
            cur = ''
        else:
            cur += ch
    if cur.strip():
        out.append(cur.strip())
    return out


def names(t):
    t = t.strip()
    if t.startswith('['):
        return [x.strip() for x in t[1:-1].split(',') if x.strip()]
    return [t]


def parse_ann(s):
    rel = {}
    for item in split_top(s):
        m = re.match(r'^(\[[^\]]*\]|\S+)\s+(CF|SB|SBR|C|SBRS|SA|SAR|EXT)\s+(\[[^\]]*\]|\S+)$', item)
        if not m:
            continue
        for p in names(m.group(1)):
            for q in names(m.group(3)):
                rel[(p, q)] = m.group(2)
    return rel


def pairs(line):
    return re.findall(r'\(([^,()]+), ([^,()]+)\)', line)


def parse(path):
    lines = open(path).read().split('\n')
    kids, steps, sec, cur, curkid, curm = {}, [], None, None, None, None
    hasarg, isval = {}, {}
    info = {'CF': [], 'C': [], 'SBSBR': '', 'disj': [], 'urg': [], 'me': []}
    for ln in lines:
        if ln.startswith('children:'):
            sec = 'kids'
            continue
        if ln.startswith('execution order:'):
            sec = 'steps'
            continue
        if ln and not ln.startswith(' '):
            sec = None
        if sec == 'kids':
            m = re.match(r'^  (\S+) : (\S+)\s+\[(.*)\]\s*$', ln)
            if m:
                curkid = m.group(1)
                kids[curkid] = parse_ann(m.group(3))
                continue
            m = re.match(r'^    (\S+) \((value|action|actionvalue)[^)]*\)', ln)
            if m and curkid:
                curm = m.group(1)
                isval[(curkid, curm)] = m.group(2) == 'value'
                hasarg.setdefault((curkid, curm), False)
                continue
            if curkid and curm and re.match(r'^      copy', ln) and ' arg ' in ln:
                hasarg[(curkid, curm)] = True
            continue
        if sec == 'steps':
            m = re.match(r'^\s+\d+\. (\S+)\s+\[(value method|method|rule)\s*(\S*)', ln)
            if m:
                kind = 'rule' if m.group(2) == 'rule' else 'method'
                meth = m.group(3).rstrip(']') if kind == 'method' else None
                cur = {'name': m.group(1), 'kind': kind, 'meth': meth, 'uses': []}
                steps.append(cur)
                continue
            m = re.match(r'^\s+(reads|calls) (\S+?)\.([^\s\[]+)(\[\d+\])? ', ln)
            if m and cur is not None and 'out of order' not in ln:
                cur['uses'].append((m.group(1), m.group(2), m.group(3), m.group(4) or ''))
            continue
        if ln.startswith('exported CF:'):
            info['CF'] = pairs(ln)
        elif ln.startswith('exported C:'):
            info['C'] = pairs(ln)
        elif ln.startswith('exported SB/SBR:'):
            info['SBSBR'] = ln.split(':', 1)[1]
        elif ln.startswith('disjoint guards (bsc):'):
            info['disj'] = pairs(ln)
        elif ln.startswith('urgency blocking'):
            info['urg'] = pairs(ln)
        elif ln.startswith('mutually_exclusive (user):'):
            info['me'] = pairs(ln)
    return kids, steps, info, hasarg, isval


def bit(i):
    return 1 << i


def clr(a, b):
    return a ^ (a & b)


def subB(a, b):
    return a & b == a


def analyse(d):
    kids, steps, info, hasarg, isval = parse(os.path.join(d, 'dump.txt'))
    # ports per child: (method, copy) as used, plus annotated names
    ports = {k: [] for k in kids}
    for s in steps:
        for (_, k, m, c) in s['uses']:
            if k in kids and (m, c) not in ports[k]:
                ports[k].append((m, c))
    base, b = {}, 0
    for k in kids:
        base[k] = b
        b += max(1, len(ports[k]))

    def crel(k, p, q):
        ann = kids[k]
        bp, bq = p[0], q[0]
        if p != q and bp == bq:
            return 'CF'
        r = ann.get((bp, bq))
        if r is not None:
            return r
        if ann.get((bq, bp)) is not None:
            return 'rev'
        return 'C'

    def prec(k, p, q):
        return p != q and crel(k, p, q) in ('SB', 'SBR')

    def conf(k, p, q):
        return crel(k, p, q) == 'C' or crel(k, q, p) == 'C'

    def sbr(k, p, q):
        return crel(k, p, q) == 'SBR'

    def share(k, p):
        return isval.get((k, p[0]), False)

    def fx(k, p):
        return isval.get((k, p[0]), False) and not hasarg.get((k, p[0]), False)

    def folP(k, p, q):
        return (p == q or not prec(k, q, p)) and not conf(k, p, q) and not conf(k, q, p)

    def grpP(k, p, q):
        return folP(k, p, q) and not sbr(k, p, q) and not sbr(k, q, p)

    def argP(k, p, q):
        return p != q or not share(k, p) or fx(k, p)

    kc = {}
    for k in kids:
        P = ports[k]
        for c, q0 in enumerate(P):
            kc[(k, c)] = (
                [i for i, p in enumerate(P) if not (folP(k, p, q0) and argP(k, p, q0))],
                [i for i, q in enumerate(P) if not (folP(k, q0, q) and argP(k, q0, q))],
                [i for i, p in enumerate(P) if not (grpP(k, p, q0) and argP(k, p, q0))],
                [i for i, q in enumerate(P) if not (grpP(k, q0, q) and argP(k, q0, q))])

    def ents(s):
        return [(k, ports[k].index((m, c))) for (_, k, m, c) in s['uses'] if k in kids]

    meths = []
    mfp = {}
    for s in steps:
        if s['kind'] == 'method':
            if s['meth'] not in mfp:
                meths.append(s['meth'])
                mfp[s['meth']] = []
            mfp[s['meth']] += ents(s)
    rules = [s for s in steps if s['kind'] == 'rule']
    nM, nR = len(meths), len(rules)
    mi = {m: i for i, m in enumerate(meths)}
    ri = {s['name']: i for i, s in enumerate(rules)}
    stepmeth = {s['name']: s['meth'] for s in steps if s['kind'] == 'method'}
    # exported relations
    cf, sb, sbrm, cm = set(), set(), set(), set()
    for (a, b2) in info['CF']:
        cf.add((a, b2)); cf.add((b2, a))
    toks = info['SBSBR'].split()
    for x in range(0, len(toks) - 2, 3):
        a, r, b2 = toks[x], toks[x + 1], toks[x + 2]
        (sbrm if r == 'SBR' else sb).add((a, b2))
    for (a, b2) in info['C']:
        cm.add((a, b2)); cm.add((b2, a))

    def mprec(a, b2):
        return a != b2 and ((a, b2) in sb or (a, b2) in sbrm)

    def mconf(a, b2):
        return not ((a, b2) in cf or (a, b2) in sb or (a, b2) in sbrm or (b2, a) in sb or (b2, a) in sbrm)

    def mfol(a, b2):
        return (a == b2 or not mprec(b2, a)) and not mconf(a, b2) and not mconf(b2, a)

    def mgrp(a, b2):
        return mfol(a, b2) and (a, b2) not in sbrm and (b2, a) not in sbrm

    nfI = [sum(bit(mi[a]) for a in meths if not mfol(a, m2)) for m2 in meths]
    nfO = [sum(bit(mi[b2]) for b2 in meths if not mfol(m1, b2)) for m1 in meths]
    ngI = [sum(bit(mi[a]) for a in meths if not mgrp(a, m2)) for m2 in meths]
    ngO = [sum(bit(mi[b2]) for b2 in meths if not mgrp(m1, b2)) for m1 in meths]
    h3ok = [sum(bit(mi[m]) for m in meths if m != mp and ((m, mp) in sbrm or mconf(m, mp))) for mp in meths]
    exM, exR, exMR = [0] * nM, [0] * nR, [0] * nR
    for (a, b2) in info['disj'] + info['me'] + info['urg']:
        for (x, y) in ((a, b2), (b2, a)):
            if x in ri and y in ri:
                exR[ri[x]] |= bit(ri[y])
            elif x in ri and y in stepmeth:
                exMR[ri[x]] |= bit(mi[stepmeth[y]])
            elif x in stepmeth and y in stepmeth and stepmeth[x] != stepmeth[y]:
                exM[mi[stepmeth[x]]] |= bit(mi[stepmeth[y]])
    # the pass, counting failures
    fails = {'grp': 0, 'mm': 0, 'h4': 0, 'h3': 0}
    h4pairs = []
    st = {}
    get = lambda s: st.get(s, (0, 0, 0, 0))
    for k in range(nM):
        for (kid, c) in mfp[meths[k]]:
            b0 = base[kid]
            cin, cout, gin, gout = kc[(kid, c)]
            if any(get(b0 + c2)[0] == k + 1 for c2 in gin):
                fails['grp'] += 1
            o = get(b0 + c)
            st[b0 + c] = (k + 1, o[1] | bit(k), o[2], o[3])
            ex = clr(exM[k], bit(k))
            orA = lambda cs: _or(get(b0 + c2)[1] for c2 in cs)
            if not (subB(orA(cin), nfI[k] | ex) and subB(orA(cout), nfO[k] | ex) and
                    subB(orA(gin), ngI[k] | ex) and subB(orA(gout), ngO[k] | ex)):
                fails['mm'] += 1
    for i in range(nR):
        g = nM + i
        dh = ph = bm = 0
        es = ents(rules[i])
        for (kid, c) in es:
            b0 = base[kid]
            cin, cout, gin, gout = kc[(kid, c)]
            if any(get(b0 + c2)[0] == g + 1 for c2 in gin):
                fails['grp'] += 1
            o = get(b0 + c)
            st[b0 + c] = (g + 1, o[1], o[2] | bit(i), o[3])
            bad = clr(_or(get(b0 + c2)[2] for c2 in cin), exR[i] | bit(i))
            for i1 in range(nR):
                if (bad >> i1) & 1 and (i1, i) not in h4pairs:
                    h4pairs.append((i1, i))
            dh |= _or(get(b0 + c2)[1] for c2 in cout)
            ph |= _or(get(b0 + c2)[3] for c2 in cout)
            bm |= _or(get(b0 + c2)[1] for c2 in cin)
        H = clr(dh | ph, exMR[i])
        B = clr(bm, exMR[i])
        if H:
            for m in range(nM):
                if (B >> m) & 1 and not subB(H, h3ok[m] | clr(exM[m], bit(m))):
                    fails['h3'] += 1
        for (kid, c) in es:
            o = get(base[kid] + c)
            st[base[kid] + c] = (o[0], o[1], o[2], o[3] | H)
    fails['h4'] = len(h4pairs)
    uses = sum(len(mfp[m]) for m in meths) + sum(len(ents(r)) for r in rules)
    return nR, nM, len(kids), uses, b, fails


def _or(it):
    r = 0
    for x in it:
        r |= x
    return r


if __name__ == '__main__':
    print('%-24s %5s %5s %5s %6s %6s | %4s %4s %4s %4s' % ('design', 'rules', 'meths', 'kids', 'uses', 'slots',
                                                           'grp', 'mm', 'h3', 'h4'))
    for d in sys.argv[1:]:
        if not os.path.exists(os.path.join(d, 'dump.txt')):
            continue
        R, M, J, U, S, f = analyse(d)
        print('%-24s %5d %5d %5d %6d %6d | %4d %4d %4d %4d' % (
            os.path.basename(d.rstrip('/')), R, M, J, U, S, f['grp'], f['mm'], f['h3'], f['h4']))

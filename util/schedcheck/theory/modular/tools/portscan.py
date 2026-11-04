"""Static scan of the round-5 certificate's footprint facts, per child port.

portscan.py DIR...   (each DIR has dump.txt from schedcheck --dump-design)

For every design: each step's footprint is the list of child ports it reads
or calls (in order).  Child annotations come from the dump (bsc's .ba);
unlisted pairs are C, as in bsc.  The checks are the certificate's fields,
statically, with only the exceptions the dump lists (bsc's disjoint guards,
urgency blocking, user mutually_exclusive):

  grp  a step's own calls of one child may be one caller's (Group)
  mm   exported Follow(m1, m2)  =>  OkT(fp m1, fp m2)
  mmg  exported Group(m1, m2)   =>  TogT(fp m1, fp m2)
  h3   m hits r (r may not precede m, closed over rules that must follow),
       m' may not precede r  =>  m SBR m' or m C m' exported (m != m')
  h4   r1 may not precede r2  =>  r2 ranked first

A failure here is a fact the generator must prove dynamically (a bv_decide
fact under fire / Avail, as WILL_FIRE suppression or a disjoint guard), or a
real schedule error.  Arguments are ignored (a share port's calls are taken
to agree), so 'share' never fails here.
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
    kids, steps, sec, cur = {}, [], None, None
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
                kids[m.group(1)] = parse_ann(m.group(3))
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
    return kids, steps, info


def analyse(d):
    kids, steps, info = parse(os.path.join(d, 'dump.txt'))
    # child relation, with bsc's default C for unlisted pairs
    def crel(k, p, q):
        ann = kids.get(k)
        if ann is None:
            return None
        bp, bq = p[0], q[0]
        if p != q and bp == bq:
            return 'CF'                      # two copies of one method
        r = ann.get((bp, bq))
        if r is not None:
            return r
        if ann.get((bq, bp)) is not None:
            return 'rev'
        return 'C'

    def prec(k, p, q):
        return p != q and crel(k, p, q) in ('SB', 'SBR')

    def conf(k, p, q):
        r1, r2 = crel(k, p, q), crel(k, q, p)
        return r1 == 'C' or r2 == 'C'

    def sbr(k, p, q):
        return crel(k, p, q) == 'SBR'

    def follow(k, a, b):
        return not prec(k, b, a) and not conf(k, a, b)

    def group(k, a, b):
        return follow(k, a, b) and not sbr(k, a, b) and not sbr(k, b, a)

    fp = []
    for s in steps:
        fp.append([(k, (m, c)) for (_, k, m, c) in s['uses'] if k in kids])

    def okT(i, j):
        for (k, a) in fp[i]:
            for (k2, b) in fp[j]:
                if k == k2 and not follow(k, a, b):
                    return False
        return True

    def togT(i, j):
        for (k, a) in fp[i]:
            for (k2, b) in fp[j]:
                if k == k2 and not group(k, a, b):
                    return False
        return True

    def grpT(i):
        f = fp[i]
        for x in range(len(f)):
            for y in range(x + 1, len(f)):
                if f[x][0] == f[y][0] and not group(f[x][0], f[x][1], f[y][1]):
                    return False
        return True

    name = [s['name'] for s in steps]
    exc = set()
    for (a, b) in info['disj'] + info['me'] + info['urg']:
        exc.add((a, b))
        exc.add((b, a))
    # exported relations among methods
    cf = set()
    for (a, b) in info['CF']:
        cf.add((a, b)); cf.add((b, a))
    toks = info['SBSBR'].split()
    sb, sbrm = set(), set()
    for x in range(0, len(toks) - 2, 3):
        a, r, b = toks[x], toks[x + 1], toks[x + 2]
        (sbrm if r == 'SBR' else sb).add((a, b))
    cm = set()
    for (a, b) in info['C']:
        cm.add((a, b)); cm.add((b, a))

    def pfollow(m1, m2):
        return (m1, m2) in cf or (m1, m2) in sb or (m1, m2) in sbrm

    def pgroup(m1, m2):
        return (m1, m2) in cf or (m1, m2) in sb

    meths = [i for i, s in enumerate(steps) if s['kind'] == 'method']
    rules = [i for i, s in enumerate(steps) if s['kind'] == 'rule']
    res = {'grp': 0, 'mm': 0, 'mmg': 0, 'h3': 0, 'h3self': 0, 'h4': 0}
    res['grp'] = sum(1 for i in range(len(steps)) if not grpT(i))
    for i in meths:
        for j in meths:
            if i == j or (name[i], name[j]) in exc:
                continue
            m1, m2 = steps[i]['meth'], steps[j]['meth']
            if m1 == m2:
                continue
            if pfollow(m1, m2) and not okT(i, j):
                res['mm'] += 1
            if pgroup(m1, m2) and not togT(i, j):
                res['mmg'] += 1
    # hits, closed over rules that must follow
    for i in meths:
        hit = set(r for r in rules if (name[i], name[r]) not in exc and not okT(r, i))
        changed = True
        while changed:
            changed = False
            for r1 in list(hit):
                for r2 in rules:
                    if r2 not in hit and (name[r1], name[r2]) not in exc and \
                            (name[i], name[r2]) not in exc and not okT(r2, r1):
                        hit.add(r2)
                        changed = True
        for r in hit:
            for j in meths:
                if (name[j], name[r]) in exc or (name[i], name[j]) in exc or okT(j, r):
                    continue
                m, m2 = steps[i]['meth'], steps[j]['meth']
                if i == j:
                    res['h3self'] += 1
                elif not ((m, m2) in sbrm or (m, m2) in cm or m == m2):
                    res['h3'] += 1
    for x in range(len(rules)):
        for y in range(x + 1, len(rules)):
            r1, r2 = rules[x], rules[y]
            if (name[r1], name[r2]) in exc:
                continue
            if not okT(r1, r2):
                res['h4'] += 1
    uses = sum(len(f) for f in fp)
    return len(rules), len(meths), len(kids), uses, res


print('%-24s %5s %5s %5s %6s | %4s %4s %4s %4s %6s %4s' % (
    'design', 'rules', 'meths', 'kids', 'uses', 'grp', 'mm', 'mmg', 'h3', 'h3self', 'h4'))
for d in sys.argv[1:]:
    if not os.path.exists(os.path.join(d, 'dump.txt')):
        continue
    R, M, J, U, r = analyse(d)
    print('%-24s %5d %5d %5d %6d | %4d %4d %4d %4d %6d %4d' % (
        os.path.basename(d.rstrip('/')), R, M, J, U, r['grp'], r['mm'], r['mmg'], r['h3'], r['h3self'], r['h4']))

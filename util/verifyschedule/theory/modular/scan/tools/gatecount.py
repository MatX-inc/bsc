"""The gate's site counts per corpus design, as the emitter counts them (cost review r7-2 M1):
a step's sites are its dump reads and calls plus, for a rule, its task statements (APackage);
the emitter's scan uses are the same entries (one footprint entry per site; statements are
chain-kind uses), so `segs` is the scan's segments at 4,096 uses (cut before a step).  Also the
largest port's users (steps calling one child action port), the largest exception group, and
`pairs`: the most callers of one child action port whose enable is proved pair by pair (`en_pairs`: a
module with action methods, or a port with a conditional call), against the emitter's 64 (GATE_SELMUX).
usage: GATE_TMP=DIR gatecount.py EMITTER_TOOLS DUMPROOT BADATA DESIGN..."""
import os
import re
import sys

sys.path.insert(0, sys.argv[1])
import design  # noqa: E402

root, data = sys.argv[2], sys.argv[3]
def largest_group(dump):
    """bsc's run-time checks grouped as the emitter does (mke2e.py GROUPS): the largest group."""
    checks = []
    for ln in open(dump):
        m = re.match(r'^       check (.*)$', ln)
        if m:
            nm = re.findall(r'WILL_FIRE_(\S+?)(?=[\s)&|]|$)', m.group(1))
            checks.append((nm[0], tuple(nm[1:])))
    bylst, done, best = {}, set(), 0
    for c, ch in enumerate(checks):
        bylst.setdefault(ch[1], []).append(c)
    for c0, ch0 in enumerate(checks):
        if len(ch0[1]) != 1 or c0 in done:
            continue
        seq, chain = [ch0[0]] + list(ch0[1]), [c0]
        while True:
            nx = [c for c in bylst.get(tuple(seq), []) if c not in done and c not in chain]
            if not nx:
                break
            chain.append(nx[0])
            seq = [checks[nx[0]][0]] + seq
        done.update(chain)
        best = max(best, len(seq))
    return best


def segments(steps, seg=4096):
    n, acc = 1, 0
    for u in steps:
        if acc + u > seg and acc > 0:
            n, acc = n + 1, 0
        acc += u
    return n


print('%-24s %5s %6s %5s %6s %6s %6s %6s %5s %5s %5s' % ('design', 'steps', 'sites', 'tasks', 'step1', 'step2', 'step3',
                                                           'segs', 'users', 'group', 'pairs'))
TRUE = ("1'b1", "1'd1")
for d in sys.argv[4:]:
    dump = os.path.join(root, d, 'dump.txt')
    ba = os.path.join(data, d + '.ba.txt')
    txt = open(dump).read()
    if '(hint: used when' in txt:
        # the bsc-fix-all dumps (sites/modes/) write a read's condition as a hint; same count
        dump = os.path.join(os.environ['GATE_TMP'], d + '.dump.txt')
        open(dump, 'w').write(re.sub(r'^(       reads \S+)  \(hint: used when (.*)\)$', r'\1 when \2', txt, flags=re.M))
    D = design.Design(dump, ba if os.path.exists(ba) else '')
    rows, ports, order, allp = [], {}, [], {}
    hasm = any(s['kind'] == 'method' for s in D.steps)
    for s in D.steps:
        n = len(s['reads']) + len(s['calls'])
        acts = D.A.rules.get(s['name'], (None, []))[1] if (s['kind'] == 'rule' and D.A is not None) else []
        nt = sum(1 for a in acts if a and a[0] == 'task')
        rows.append(n + nt)
        order.append(n + nt)
        for c in s['calls']:
            ports.setdefault((c[0], c[1]), set()).add(s['name'])
        for c in s['calls']:
            u_ = allp.setdefault((c[0], c[1]), [set(), False])
            u_[0].add(s['name'])
            u_[1] = u_[1] or c[2].strip() not in TRUE
    rows.sort(reverse=True)
    top = (rows + [0, 0, 0])[:3]
    print('%-24s %5d %6d %5d %6d %6d %6d %6d %5d %5d %5d' % (d, len(rows), sum(rows),
          sum(r for r in rows) - sum(len(s['reads']) + len(s['calls']) for s in D.steps),
          top[0], top[1], top[2], segments(order), max((len(v) for v in ports.values()), default=0), largest_group(dump),
          max((len(v[0]) for v in allp.values() if hasm or v[1]), default=0)))

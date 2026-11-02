"""Callers of one child port that may fire together (action shadowing), on the emitter's model, with
last caller (round 8): rules and methods; a pair is excluded by a recorded exclusion or bsc's exported
C between two methods; each pair the solver says may both call is classified by its port (L last
caller, U self-SBR outside the key, I identical calls of a self-CF port, I-fail where not idempotent,
X otherwise), as sites/tools/shadow.py.  The emitter's model has primitive children only.
usage: python3 -I shadow.py DESIGN_DIR WORKDIR"""
import os
import re
import sys
HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
sys.path.insert(0, os.path.join(HERE, '..', '..', 'sites', 'tools'))
import design, model, condsites  # noqa: E402,E401
from satc import Enc  # noqa: E402

d, work = sys.argv[1], sys.argv[2]
name = os.path.basename(d.rstrip('/'))
cs, res = condsites.conditional_sites(d, work)
ctx = res['_ctx']
D = design.Design(os.path.join(d, 'dump.txt'), os.path.join(HERE, '..', '..', 'sites', 'data', name + '.ba.txt'))
M = model.Model(D, None, cs)
SD = ctx['D']
ap = ctx['ap']
nR = M.nR
expC, expO = set(), {}
for ln in open(os.path.join(d, 'dump.txt')).read().split('\n'):
    if ln.startswith('exported C:'):
        for a, b in re.findall(r'\(([^,()]+), ([^,()]+)\)', ln):
            expC |= {(a, b), (b, a)}
    if ln.startswith('exported SB/SBR:'):
        for a, r, b in re.findall(r'(\S+) (SBR|SB) (\S+)', ln.split(':', 1)[1]):
            expO[(a, b)] = r


def meth_of(k):
    return M.rules[k].get('meth') or (M.rules[k]['name'] if k >= nR else None)


def wf(k):
    n = M.rules[k]['name']
    for x in ('WILL_FIRE_' + n, 'CAN_FIRE_' + n):
        if x in SD.defs:
            return ('id', x)
    return ('id', 'EN_' + (meth_of(k) or n))


def ccond(n, inst, meth):
    cs_ = [a[3] for a in ap.rules.get(n, (None, []))[1] if a and a[0] == 'call' and a[1] == inst and a[2] == meth]
    return [c if c is not None else ('lit', "1'd1") for c in cs_] or [('lit', "1'd1")]


def klass(kd, m, same, order_ok):
    K = M.K[kd]
    if K.sbr(m, m):
        if any(q != m and K.prec(m, q) and not K.sbr(m, q) for q in K.ports):
            return 'U'
        return 'L' if order_ok else 'L!order'
    if not K.conf(m, m):
        if not same:
            return 'X'
        return 'I-fail' if (kd, m) in (('ctr', 'addA'), ('ctr', 'addB')) else 'I'
    return 'X'


POS = {st['name']: i for i, st in enumerate(D.steps)}
U = M.users()
tot = sat = 0
out, byc = [], {}
for p, us in sorted(U.items()):
    kd, i, m = p
    if kd == 'tasks' or not M.K[kd].has_en(m):
        continue
    ks = sorted(set(k for k, _ in us))
    if len(ks) < 2:
        continue
    args = {k: repr(M.rules[k]['sites'][si]['arg']) for k, si in us}
    inst = M.byk[kd][i]
    for a in ks:
        for b in ks:
            if a >= b or M.excl_src(a, b) is not None:
                continue
            ma, mb = (meth_of(a) if a >= nR else None), (meth_of(b) if b >= nR else None)
            if ma and mb and (ma, mb) in expC:
                continue
            na, nb = M.rules[a]['name'], M.rules[b]['name']
            # last caller keeps the scan's order: methods before rules, two methods exported in order
            first, second = (a, b) if POS[na] < POS[nb] else (b, a)
            fm, sm = first >= nR, second >= nR
            order_ok = not (sm and not fm) and (not (fm and sm) or (meth_of(first), meth_of(second)) in expO)
            E = Enc(SD)
            ca = E.OR([E.b(x) for x in ccond(na, inst, m)])
            cb = E.OR([E.b(x) for x in ccond(nb, inst, m)])
            tot += 1
            if E.sat([E.b(wf(a)), E.b(wf(b)), ca, cb], work) is not False:
                sat += 1
                c = klass(kd, m, args[a] == args[b], order_ok)
                byc[c] = byc.get(c, 0) + 1
                out.append('%s %s.%s: %s and %s' % (c, inst, m, na, nb))
print('%s: caller pairs without a static exclusion %d: exclusive (abstract UNSAT) %d, may both call %d [%s]'
      % (name, tot, tot - sat, sat, ', '.join('%s %d' % (k, byc[k]) for k in sorted(byc)) or '-'))
for x in out:
    print('  ' + x)

"""Writers of one child port that may fire together (action shadowing, design open issue): for each
pair of a port's callers with different arguments and no static exclusion, ask the solver (cadical over
the abstract skeleton, as the site tool) whether both can call in one cycle.
usage: python3 -I shadow.py DESIGN_DIR WORKDIR"""
import os
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


def wf(n):
    x = 'WILL_FIRE_' + n
    return ('id', x) if x in SD.defs else ('id', 'CAN_FIRE_' + n)


def ccond(n, inst, meth):
    cs_ = [a[3] for a in ap.rules.get(n, (None, []))[1] if a and a[0] == 'call' and a[1] == inst and a[2] == meth]
    return [c if c is not None else ('lit', "1'd1") for c in cs_] or [('lit', "1'd1")]


U = M.users()
tot = sat = unsat = 0
out = []
for p, us in sorted(U.items()):
    kd, i, m = p
    if kd == 'tasks' or not M.K[kd].has_en(m) or not M.K[kd].hasarg(m):
        continue
    ks = sorted(set(k for k, _ in us))
    if len(ks) < 2:
        continue
    args = {k: repr(M.rules[k]['sites'][si]['arg']) for k, si in us}
    if len(set(args.values())) < 2:
        continue
    inst = M.byk[kd][i]
    for a in ks:
        for b in ks:
            if a >= b or M.excl_src(a, b) is not None:
                continue
            na, nb = M.rules[a]['name'], M.rules[b]['name']
            E = Enc(SD)
            ca = E.OR([E.b(x) for x in ccond(na, inst, m)])
            cb = E.OR([E.b(x) for x in ccond(nb, inst, m)])
            r = E.sat([E.b(wf(na)), E.b(wf(nb)), ca, cb], work)
            tot += 1
            if r is False:
                unsat += 1
            else:
                sat += 1
                out.append('MAY SHADOW %s.%s: %s and %s' % (inst, m, na, nb))
print('%s: writer pairs without a static exclusion %d: callers exclusive (abstract UNSAT) %d, may both call %d' % (name, tot, unsat, sat))
for x in out:
    print('  ' + x)

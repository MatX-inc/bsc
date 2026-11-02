"""Action shadowing (two writers of one action port in one cycle, design open issue 6): for every action
port with an argument called by two steps with different argument expressions and no recorded
exclusion (disjoint guards, urgency, mutually_exclusive), ask the solver (cadical over the abstract
skeleton, as sitecond.py) whether both can call in one cycle.
usage: python3 -I shadow.py WORKDIR DESIGN_DIR...   (SITES_DATA as for sitecond.py)"""
import os
import sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import sitecond  # noqa: E402
from satc import Enc  # noqa: E402
from sitecond import canon_expr  # noqa: E402


def main():
    work = sys.argv[1]
    os.makedirs(work, exist_ok=True)
    for d in sys.argv[2:]:
        if not os.path.exists(os.path.join(d, 'dump.txt')):
            continue
        name = os.path.basename(d.rstrip('/'))
        try:
            res = sitecond.analyse(d, work, work)
        except Exception as ex:
            print('%-24s ERROR %s' % (name, ex))
            continue
        ctx = res['_ctx']
        SD, ap, steps = ctx['D'], ctx['ap'], ctx['steps']
        dump = sitecond.parse_dump(os.path.join(d, 'dump.txt'))
        exc = set()
        for (a, b) in dump['disj'] + dump['me'] + dump['urg']:
            exc.add((a, b))
            exc.add((b, a))
        conf = set()      # bsc's confirmed use-level facts (b, p, q): b's call q against the earlier p
        for fct in dump['facts']:
            if fct['ok']:
                conf.add((fct['b'], fct['p'], fct['q']))

        def wf(n):
            x = 'WILL_FIRE_' + n
            return ('id', x) if x in SD.defs else ('id', 'CAN_FIRE_' + n)
        calls = {}
        for st in steps:
            if st['kind'] != 'rule':
                continue
            for a in ap.rules.get(st['name'], (None, []))[1]:
                if a and a[0] == 'call' and a[4]:
                    calls.setdefault((a[1], a[2]), []).append((st['name'], a[3], tuple(canon_expr(x) for x in a[4])))
        tot = sat = 0
        out = []
        for port, cl in sorted(calls.items()):
            for x in range(len(cl)):
                for y in range(x + 1, len(cl)):
                    (na, ca, aa), (nb, cb, ab) = cl[x], cl[y]
                    if na == nb or aa == ab or (na, nb) in exc:
                        continue
                    lab = '%s.%s' % port
                    if (nb, lab, lab) in conf or (na, lab, lab) in conf:
                        continue
                    E = Enc(SD)
                    roots = [E.b(wf(na)), E.b(wf(nb))]
                    if ca is not None:
                        roots.append(E.b(ca))
                    if cb is not None:
                        roots.append(E.b(cb))
                    tot += 1
                    if E.sat(roots, work) is not False:
                        sat += 1
                        out.append('%s.%s: %s, %s' % (port[0], port[1], na, nb))
        print('%-24s writer pairs (no recorded exclusion) %4d, callers exclusive %4d, may both call %3d %s'
              % (name, tot, tot - sat, sat, '; '.join(out[:4])), flush=True)


if __name__ == '__main__':
    sys.setrecursionlimit(1000000)
    import threading
    threading.stack_size(512 * 1024 * 1024)
    t = threading.Thread(target=main)
    t.start()
    t.join()

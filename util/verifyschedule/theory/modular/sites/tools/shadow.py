"""Action shadowing (two callers of one child action port in one cycle) with last caller (round 8).

For every child action port called by two steps (rules and methods), drop the pairs that a recorded
exclusion covers (disjoint guards, urgency, mutually_exclusive, bsc's exported C between two methods),
then ask the solver (cadical over the abstract skeleton, as sitecond.py) whether both can call in one
cycle.  Each pair that may both call is classified by the child:

  L   primitive, self-SBR, in the last-caller key: the port carries the last caller's argument
      (bsc's priority mux); two methods must be exported in execution order (SB/SBR), and a method
      must not execute after a rule on the port (the scan's order), else `L!order`
  U   primitive, self-SBR, outside the key (an Unsafe wire: an observer after `m` in one caller)
  I   primitive, self-CF/SB, identical calls: needs the port's idempotence (`I-fail` where it fails:
      Counter.bs addA/addB, modelled C)
  S   synthesized child: off path A until its own RealizesU proves the law (bsc's SBR is no guarantee)
  X   anything else (self-C port both called: an exclusion this tool does not know)

usage: python3 -I shadow.py WORKDIR DESIGN_DIR...   (SITES_DATA as for sitecond.py; SHADOW_VERBOSE=1 lists pairs)"""
import os
import re
import sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import sitecond  # noqa: E402
from satc import Enc  # noqa: E402
from sitecond import canon_expr  # noqa: E402

# bsc's library Verilog modules (src/Verilog): a child of one of these is a primitive
VLIB = '/home/ravi/bluespec/lean/bsc/src/Verilog'
PRIMS = set(f[:-2] for f in os.listdir(VLIB) if f.endswith('.v')) if os.path.isdir(VLIB) else set()
# self-CF action ports whose identical calls are not idempotent (the child must be modelled C)
NOT_IDEMPOTENT = {('Counter', 'addA'), ('Counter', 'addB')}


def lc_key(ann, ports, m):
    """LastCaller.lcKey on bsc's annotation: m SBR m, and no port that observes m after it shares a
    caller with it (prec m q without SBR)."""
    rel = lambda p, q: ann.get((p, q))
    if rel(m, m) != 'SBR':
        return False
    for q in ports:
        if q == m:
            continue
        r = rel(m, q)
        if r == 'SB':          # m before q, one caller may do both: q observes m's write
            return False
    return True


def classify(kid, m, args_same, order_ok):
    kind, ann = kid['kind'], kid['ann']
    ports = list(kid['meths'].keys())
    self_rel = ann.get((m, m))
    if kind not in PRIMS:
        return 'S'
    if self_rel == 'SBR':
        if not lc_key(ann, ports, m):
            return 'U'
        return 'L' if order_ok else 'L!order'
    if self_rel in ('CF', 'SB'):
        if not args_same:
            return 'X'
        return 'I-fail' if (kind, m) in NOT_IDEMPOTENT else 'I'
    return 'X'


def exported(dump_path):
    """bsc's exported method relations: C pairs, and SB/SBR (ordered) pairs."""
    C, ordered = set(), {}
    for ln in open(dump_path).read().split('\n'):
        if ln.startswith('exported C:'):
            for a, b in re.findall(r'\(([^,()]+), ([^,()]+)\)', ln):
                C.add((a, b))
                C.add((b, a))
        if ln.startswith('exported SB/SBR:'):
            for a, r, b in re.findall(r'(\S+) (SBR|SB) (\S+)', ln.split(':', 1)[1]):
                ordered[(a, b)] = r
    return C, ordered


def main():
    work = sys.argv[1]
    verbose = os.environ.get('SHADOW_VERBOSE') == '1'
    os.makedirs(work, exist_ok=True)
    tot_all = {}
    for d in sys.argv[2:]:
        if not os.path.exists(os.path.join(d, 'dump.txt')):
            continue
        name = os.path.basename(d.rstrip('/'))
        try:
            res = sitecond.analyse(d, work, work)
        except Exception as ex:
            print('%-24s ERROR %s' % (name, ex), flush=True)
            continue
        ctx = res['_ctx']
        SD, ap, steps = ctx['D'], ctx['ap'], ctx['steps']
        dump = sitecond.parse_dump(os.path.join(d, 'dump.txt'))
        expC, expO = exported(os.path.join(d, 'dump.txt'))
        exc = set()
        for (a, b) in dump['disj'] + dump['me'] + dump['urg']:
            exc.add((a, b))
            exc.add((b, a))
        pos = {st['name']: i for i, st in enumerate(steps)}
        mname = {st['name']: st.get('meth') for st in steps if st['kind'] == 'method'}

        def wf(n):
            for x in ('WILL_FIRE_' + n, 'CAN_FIRE_' + n, 'EN_' + (mname.get(n) or '')):
                if x in SD.defs:
                    return ('id', x)
            if mname.get(n):
                return ('id', 'EN_' + mname[n])
            return ('id', 'CAN_FIRE_' + n)
        calls = {}
        for st in steps:
            if st['kind'] not in ('rule', 'method'):
                continue
            for a in ap.rules.get(st['name'], (None, []))[1]:
                if a and a[0] == 'call' and a[4]:
                    calls.setdefault((a[1], a[2]), []).append((st['name'], a[3], tuple(canon_expr(x) for x in a[4])))
        tot = sat = 0
        byc = {}
        out = []
        for port, cl in sorted(calls.items()):
            kid = dump['kids'].get(port[0])
            if kid is None:
                continue
            for x in range(len(cl)):
                for y in range(x + 1, len(cl)):
                    (na, ca, aa), (nb, cb, ab) = cl[x], cl[y]
                    if na == nb or (na, nb) in exc:
                        continue
                    ma, mb = mname.get(na), mname.get(nb)
                    if ma and mb and (ma, mb) in expC:
                        continue        # bsc's exported C: never both called
                    # last caller's order: two methods must be exported in execution order
                    first, second = (na, nb) if pos[na] < pos[nb] else (nb, na)
                    fm, sm = mname.get(first), mname.get(second)
                    order_ok = (not (fm and sm) or (fm, sm) in expO) and not (sm and not fm)
                    E = Enc(SD)
                    roots = [E.b(wf(na)), E.b(wf(nb))]
                    if ca is not None:
                        roots.append(E.b(ca))
                    if cb is not None:
                        roots.append(E.b(cb))
                    tot += 1
                    if E.sat(roots, work) is not False:
                        sat += 1
                        c = classify(kid, port[1], aa == ab, order_ok)
                        byc[c] = byc.get(c, 0) + 1
                        out.append('%s %s.%s [%s]: %s, %s%s' % (c, port[0], port[1], kid['kind'], first, second,
                                                                 '' if aa != ab else ' (identical call)'))
        cls = ', '.join('%s %d' % (k, byc[k]) for k in sorted(byc))
        blocked = sum(v for k, v in byc.items() if k not in ('L', 'I'))
        print('%-24s caller pairs (no recorded exclusion) %4d, exclusive %4d, may both call %3d [%s]; %s'
              % (name, tot, tot - sat, sat, cls or '-', 'last caller admits all' if sat and not blocked else
                 ('no shadowing' if not sat else 'still blocked: %d' % blocked)), flush=True)
        for k, v in byc.items():
            tot_all[k] = tot_all.get(k, 0) + v
        if verbose:
            for o in out:
                print('    ' + o, flush=True)
    print('TOTAL by class: %s' % ', '.join('%s %d' % (k, tot_all[k]) for k in sorted(tot_all)), flush=True)


if __name__ == '__main__':
    sys.setrecursionlimit(1000000)
    import threading
    threading.stack_size(512 * 1024 * 1024)
    t = threading.Thread(target=main)
    t.start()
    t.join()

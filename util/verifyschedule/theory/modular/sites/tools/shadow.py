"""Action shadowing (two callers of one child action port in one cycle) with last caller (round 8).

For every child action port called by two steps (rules and methods), drop the pairs that a recorded
exclusion covers (disjoint guards, urgency, mutually_exclusive, bsc's exported C between two methods),
then ask the solver (cadical over the abstract skeleton, as sitecond.py) whether two callers can call in
one cycle.  Each port is classified once, by the child (review r8-1: soundness minor 3, cost minor 2):

  L   a decided (primitive, port) with a proved last-caller law (DECIDED), m SBR m and lcKey on the
      instance's schedule: the port carries the last caller's argument (bsc's priority mux).  One pair
      that may both call makes it a last-caller port of all its users; then only the static order
      condition is checked per pair: two methods exported SBR in execution order, and no method executing
      after a rule on the port (the scan's order), else `L!order` for a pair that may both call
  U   self-SBR outside DECIDED, or failing lcKey on the instance's schedule (an Unsafe wire)
  I   self-CF/SB, identical calls (no Lean model has such a port: blocked); `I-fail` for Counter addA/addB
  S   synthesized child: off path A until its own RealizesU proves the law (bsc's SBR is no guarantee);
      a user BVI import is an assumed BVI leaf with its declared schedule (review r8-2 minor 3), so its
      self-SBR ports are U and its other ports are classed as a primitive's
  X   anything else (self-C port both called: an exclusion this tool does not know)
  ?   the solver timed out on the port's first pair (unknown, blocked)

Each port costs one solver call (`two_call`: recorded exclusions as clauses, at least two callers by a
sequential counter), plus one per pair with a bad static order on an L port and two where an assertion is
needed (cost review r8-2 minor 4: the per-pair search was quadratic where every pair is exclusive).  The solver sees every port as its own
atom (sitecond's exact design, as the emitter's caller-exclusivity facts), and no bsc fact is used.
User assertions (Ravi's decision): bsc's mutually_exclusive pairs are exclusions, and each conflict_free check
line is assumed in every solver call naming its rules (`!(WILL_FIRE_R && line)`, an instrumentation wire's
wget read as its one writer's argument), as in the emitter's gate run; pairs that need either are counted.

usage: python3 -I shadow.py WORKDIR DESIGN_DIR...   (SITES_DATA as for sitecond.py; SHADOW_VERBOSE=1 lists ports)"""
import os
import re
import sys
import time
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import sitecond  # noqa: E402
from satc import Enc  # noqa: E402
from sitecond import canon_expr  # noqa: E402
import mecheck  # noqa: E402

# the decided last-caller list (Ravi): (bsc Verilog primitive, port) with its law proved in LastCaller.lean;
# WIRED: the ones the prototype emitter certifies (LC_KEY), the others are proved, not wired
DECIDED = {}
for t_ in ('RegN', 'RegUN', 'RegA'):
    DECIDED[(t_, 'write')] = 'creg_lastWins 1 0'
for t_ in ('ConfigRegN', 'ConfigRegUN', 'ConfigRegA'):
    DECIDED[(t_, 'write')] = 'cfg_lastWins'
for t_ in ('CRegN5', 'CRegUN5', 'CRegA5'):
    for k_ in range(5):
        DECIDED[(t_, 'port%d__write' % k_)] = 'creg_lastWins 5 %d' % k_
DECIDED[('RWire', 'wset')] = 'rwire_lastWins (RWireSBR)'
DECIDED[('RWire0', 'wset')] = 'pulseWireOR_lastWins (mkPulseWireOR)'
DECIDED[('FIFO2', 'clear')] = 'fifo2_clear_lastWins'
DECIDED[('Counter', 'setF')] = 'counter_setF_lastWins'
WIRED = {k for k, v in DECIDED.items() if k[0] in ('RegN', 'RegUN', 'RegA', 'ConfigRegN')}   # the emitter's kind_of
# bsc's library Verilog modules (src/Verilog): a child of one of these is a primitive
VLIB = '/home/ravi/bluespec/lean/bsc/src/Verilog'
PRIMS = set(f[:-2] for f in os.listdir(VLIB) if f.endswith('.v')) if os.path.isdir(VLIB) else set()
# self-CF action ports whose identical calls are not idempotent (the child must be modelled C)
NOT_IDEMPOTENT = {('Counter', 'addA'), ('Counter', 'addB')}


def lc_key(ann, ports, m):
    """LastCaller.lcKey on the instance's schedule: m SBR m, and no port that observes m after it shares a
    caller with it (prec m q without SBR)."""
    rel = lambda p, q: ann.get((p, q))
    if rel(m, m) != 'SBR':
        return False
    return all(rel(m, q) != 'SB' for q in ports if q != m)


def ba_synth(txt):
    """Synthesized instances: the APackage prints a synthesized module's interface type with a trailing soft
    hyphen (U+00AD); a library primitive's or a user BVI import's has none (as e2e/tools/design.py)."""
    return set(m.group(1) for m in re.finditer(r'^\s+(\S+) :: ABSTRACT:\s*(\S+) = (\S+)', txt, re.M)
               if m.group(2).endswith('\u00ad'))


BSC_LIB = '/home/ravi/bluespec/lean/bsc/src/Libraries'
BSC_PKGS = set(re.sub(r'\.bsv?$', '', f) for _, _, fs in os.walk(BSC_LIB) for f in fs
               if f.endswith('.bs') or f.endswith('.bsv'))


def ba_bvi(txt):
    """User BVI imports: neither synthesized nor declared in a bsc library package (as design.py)."""
    return set(m.group(1) for m in re.finditer(r'^\s+(\S+) :: ABSTRACT:\s*(\S+) = (\S+)', txt, re.M)
               if not m.group(2).endswith('\u00ad') and m.group(2).split('.')[0] not in BSC_PKGS)


def check_lines(dump_path):
    """Each step's run-time check lines, as the dump prints them."""
    out, cur = [], None
    for ln in open(dump_path).read().split('\n'):
        m = re.match(r'^  \d+\. (\S+)\s+\[', ln)
        if m:
            cur = m.group(1)
        m = re.match(r'^       check (.*)$', ln)
        if m and cur:
            out.append((cur, m.group(1)))
    return out


def me_line(c):
    """A mutually_exclusive check line, read by its structure (mecheck, as the emitter's model)."""
    return mecheck.me_parse(c) is not None


def port_class(kid, m, synth, bvi=False):
    """The port's class before any pair: 'L' (decided, schedule checked), 'S', 'U', 'CF' (self-CF/SB), 'X'."""
    kind, ann = kid['kind'], kid['ann']
    ports = list(kid['meths'].keys())
    if synth:
        return 'S'
    self_rel = ann.get((m, m))
    if self_rel == 'SBR':
        return 'L' if not bvi and kind in PRIMS and (kind, m) in DECIDED and lc_key(ann, ports, m) else 'U'
    if self_rel in ('CF', 'SB'):
        return 'CF'
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
        t0 = time.time()
        try:
            res = sitecond.analyse(d, work, work)
        except Exception as ex:
            print('%-24s ERROR %s' % (name, ex), flush=True)
            continue
        ctx = res['_ctx']
        t1 = time.time()
        SD, ap, steps = ctx['DX'], ctx['ap'], ctx['steps']     # every port its own atom, as the emitter's cx facts
        dump = sitecond.parse_dump(os.path.join(d, 'dump.txt'))
        expC, expO = exported(os.path.join(d, 'dump.txt'))
        batxt = open(os.path.join(sitecond.DATA, name + '.ba.txt')).read()
        synth, bvi = ba_synth(batxt), ba_bvi(batxt)
        exc, exc_me = set(), set()
        for (a, b) in dump['disj'] + dump['urg']:
            exc.add((a, b))
            exc.add((b, a))
        # the user's mutually_exclusive exclusions from the check lines by structure, as the emitter's model reads
        # them (soundness review r8b-1 M1: items 1 and 4 from one source), asserted equal to the dumper's list
        clines = check_lines(os.path.join(d, 'dump.txt'))
        try:
            melines = [ln for (_, ln) in clines if me_line(ln)]
            mecheck.same_pairs(melines, dump['me'])
        except (ValueError, AssertionError) as ex:
            print('%-24s ERROR %s' % (name, ex), flush=True)
            continue
        # bsc's mutually_exclusive groups (suffix chains of lines, as the emitter's GROUPS): each is one at-most-one
        # constraint in the solver call (cost review r8b-1 minor 1); the lines' other pairs stay pairwise
        groups, gpairs = mecheck.suffix_groups(melines)
        grp_of = {}
        for g, mem in enumerate(groups):
            for x in mem:
                grp_of.setdefault(x, set()).add(g)
        for (a, b) in gpairs:
            exc_me.add((a, b))
            exc_me.add((b, a))
        exc_me -= exc
        # conflict_free check lines, assumed (the emitter's gate run does the same: mke2e.py cf_roots)
        wr = {}
        for rn, (_, acts) in ap.rules.items():
            for a in acts:
                if a and a[0] == 'call' and a[2] == 'wset':
                    wr.setdefault(a[1], []).append((rn, a[3], a[4]))

        def res(e):
            if isinstance(e, tuple) and len(e) == 4 and e[0] == 'call' and not e[3] and e[2] in ('wget', 'whas'):
                ws = wr.get(e[1], [])
                if len(ws) == 1 and (ws[0][1] is None or (ws[0][1][0] == 'lit' and ws[0][1][1] in ("1'd1", "1'b1"))) and ws[0][2]:
                    return ws[0][2][0] if e[2] == 'wget' else ('id', 'WILL_FIRE_' + ws[0][0])
                return e
            if isinstance(e, tuple):
                return tuple(res(x) if isinstance(x, (tuple, list)) else x for x in e)
            if isinstance(e, list):
                return [res(x) for x in e]
            return e
        cfa = [(rn, set(re.findall(r'WILL_FIRE_(\S+?)(?=[\s)&|]|$)', ln)) | {rn}, res(ctx['parse'](ln)))
               for (rn, ln) in clines if not me_line(ln)]
        rely = {'me': 0, 'cf': 0}
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
        ncalls = [0]

        def may_both(x, y, assume=True):
            (na, ca, aa), (nb, cb, ab) = x, y
            E = Enc(SD)
            roots = [E.b(wf(na)), E.b(wf(nb))]
            if ca is not None:
                roots.append(E.b(ca))
            if cb is not None:
                roots.append(E.b(cb))
            if assume:
                roots += [-E.AND([E.b(wf(rn)), E.b(ex)]) for (rn, nm, ex) in cfa if nm & {na, nb}]
            ncalls[0] += 1
            return E.sat(roots, work)          # True, False, or None (timeout: unknown)

        def two_call(cl, users, me=True, cf=True):
            """One solver call per port (cost review r8-2 minor 4): may two of its callers with no recorded
            exclusion between them both call?  Recorded exclusions are facts (bsc's disjoint guards and urgency,
            exported C, an asserted mutually_exclusive pair), so they enter as clauses; `at least two callers`
            is a sequential counter, linear in callers.  Returns (result, a witness pair of call entries)."""
            E = Enc(SD)
            ent = {}
            for c in cl:
                ent.setdefault(c[0], []).append((c, E.AND([E.b(wf(c[0]))] + ([E.b(c[1])] if c[1] is not None else []))))
            ys = {n: E.OR([l for _, l in ent[n]]) for n in users}
            roots = []
            # the recorded exclusions among the users, from each user's adjacency (cost review r8b-1 minor 1): the
            # work and the clauses are linear in the exclusions recorded among the callers, which a
            # mutually_exclusive group of n callers makes n(n-1)/2 (the solver's input is quadratic in callers there)
            uset = set(users)
            for na in users:
                for nb in adj_of(na, me):
                    if nb in uset and pos[na] < pos[nb]:
                        roots.append(-E.AND([ys[na], ys[nb]]))
            if me:      # a mutually_exclusive group among the callers: at most one, a sequential counter
                for g in sorted(set(g for n in users for g in grp_of.get(n, ()))):
                    s1 = -1
                    for n in users:
                        if g in grp_of.get(n, ()):
                            roots.append(-E.AND([s1, ys[n]]))
                            s1 = E.OR([s1, ys[n]])
            s_, t_ = -1, -1
            for n in users:
                t_, s_ = E.OR([t_, E.AND([s_, ys[n]])]), E.OR([s_, ys[n]])
            roots.append(t_)
            if cf:
                roots += [-E.AND([E.b(wf(rn)), E.b(ex)]) for (rn, nm, ex) in cfa if nm & set(users)]
            ncalls[0] += 1
            r, val = E.sat_model(roots, work)
            if not r:
                return r, None
            lv = lambda l: val.get(abs(l), False) == (l > 0)
            on = [n for n in users if lv(ys[n])]
            return True, tuple(next(c for c, l in ent[n] if lv(l)) for n in on[:2])

        def order_ok(na, nb):
            first, second = (na, nb) if pos[na] < pos[nb] else (nb, na)
            fm, sm = mname.get(first), mname.get(second)
            if fm and sm:
                return expO.get((fm, sm)) == 'SBR'      # CertL needs SBR (or C, never both called)
            return not (sm and not fm)                  # a method after a rule: not in the scan's order
        # recorded exclusions as adjacency: bsc's disjoint guards and urgency, the asserted mutually_exclusive pairs,
        # and bsc's exported C between two methods
        adj_x, adj_m = {}, {}
        for (a, b) in exc:
            adj_x.setdefault(a, set()).add(b)
        for (a, b) in exc_me:
            adj_m.setdefault(a, set()).add(b)
        msteps = [n for n in mname if mname[n]]
        for a in msteps:
            for b in msteps:
                if a != b and (mname[a], mname[b]) in expC:
                    adj_x.setdefault(a, set()).add(b)

        def adj_of(n, me=True):
            return (adj_x.get(n, set()) | adj_m.get(n, set())) if me else adj_x.get(n, set())

        def excluded(na, nb):
            return nb in adj_of(na) or bool(grp_of.get(na, set()) & grp_of.get(nb, set()))

        def n_excl(n, uset, mem_g):
            """How many other callers n has a recorded exclusion with (adjacency and groups), in O(its degree)."""
            gs = grp_of.get(n, ())
            if len(gs) <= 1:
                M = mem_g[next(iter(gs))] if gs else set()
                return max(len(M) - 1, 0) + sum(1 for x in adj_of(n) if x in uset and x not in M)
            return len((set().union(*(mem_g[g] for g in gs)) | (adj_of(n) & uset)) - {n})
        byc, out, nport, lcports = {}, [], 0, {}
        for port, cl in sorted(calls.items()):
            kid = dump['kids'].get(port[0])
            if kid is None:
                continue
            users = sorted(set(c[0] for c in cl), key=lambda n: pos[n])
            if len(users) < 2:
                continue
            nport += 1
            uset = set(users)
            mem_g = {}
            for n in users:
                for g in grp_of.get(n, ()):
                    mem_g.setdefault(g, set()).add(n)
            # some pair of callers with no recorded exclusion (from the adjacency and the groups, linear in them)
            free = any(n_excl(n, uset, mem_g) < len(users) - 1 for n in users)
            pc = port_class(kid, port[1], port[0] in synth, port[0] in bvi)
            r, wp = two_call(cl, users)
            if r is False:
                # no two callers: does the port rely on an assertion (a mutually_exclusive pair, a conflict_free check)?
                if any((adj_m.get(a, set()) & uset) or any(len(mem_g[g]) > 1 for g in grp_of.get(a, ()))
                       for a in users if a in adj_m or a in grp_of) and two_call(cl, users, me=False)[0] is not False:
                    rely['me'] += 1
                if any(nm & set(users) for (_, nm, _) in cfa) and two_call(cl, users, cf=False)[0] is not False:
                    rely['cf'] += 1
                continue
            if not free:
                continue
            if r:
                x, y = wp
            else:       # a timeout: the first pair with no recorded exclusion, for the report
                x, y = next((a, b) for i, a in enumerate(cl) for b in cl[i + 1:]
                            if a[0] != b[0] and not excluded(a[0], b[0]))
            pr = lambda a, b: '%s, %s' % ((a[0], b[0]) if pos[a[0]] < pos[b[0]] else (b[0], a[0]))
            if pc == 'L':
                lcports['%s.%s' % port] = len(users)
                byc['L'] = byc.get('L', 0) + 1
                # a bad static order needs a method caller (order_ok): method calls against every call, not every pair
                mcl = [c for c in cl if mname.get(c[0])]
                bad, seen_ = [], set()
                for a in mcl:
                    for b in cl:
                        k_ = (id(a), id(b)) if id(a) < id(b) else (id(b), id(a))
                        if a[0] != b[0] and k_ not in seen_ and not excluded(a[0], b[0]) and not order_ok(a[0], b[0]):
                            seen_.add(k_)
                            bad.append((a, b))
                badsat = [(a, b) for (a, b) in bad if may_both(a, b) is not False]
                out.append('L %s.%s [%s] callers %d%s: first pair %s%s' % (
                    port[0], port[1], kid['kind'], len(users), '' if (kid['kind'], port[1]) in WIRED else ' (law proved, not wired)',
                    pr(x, y), ' (timeout: unknown, taken as may both call)' if r is None else ''))
                for (a, b) in badsat:
                    byc['L!order'] = byc.get('L!order', 0) + 1
                    out.append('L!order %s.%s [%s]: %s' % (port[0], port[1], kid['kind'], pr(a, b)))
                continue
            if r is None:
                c = '?'
            elif pc in ('S', 'U', 'X'):
                c = pc
            else:
                c = ('I-fail' if (kid['kind'], port[1]) in NOT_IDEMPOTENT else 'I') if x[2] == y[2] else 'X'
            byc[c] = byc.get(c, 0) + 1
            out.append('%s %s.%s [%s]: %s%s' % (c, port[0], port[1], kid['kind'], pr(x, y),
                                                 ' (identical call)' if x[2] == y[2] else ''))
        cls = ', '.join('%s %d' % (k, byc[k]) for k in sorted(byc))
        blocked = sum(v for k, v in byc.items() if k != 'L')
        print('%-24s ports with two callers %4d, solver calls %5d, shadowing ports %3d [%s]; lc callers max %d; '
              'ports relying on mutually_exclusive %d, conflict_free %d; %s'
              % (name, nport, ncalls[0], sum(byc.values()), cls or '-', max(lcports.values(), default=0),
                 rely['me'], rely['cf'],
                 'last caller admits all' if byc and not blocked else
                 ('no shadowing' if not byc else 'still blocked: %d' % blocked)) +
              '; wall %.2f s (site analysis %.2f s)' % (time.time() - t0, t1 - t0), flush=True)
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

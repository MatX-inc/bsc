"""Emit one corpus module's modular obligation file (emit/conventions.md, sites/rule.md).

usage: python3 -I mke2e_min.py DUMP BA OUT.lean NAME [NSTEPS]

mke2e_min.py: mke2e.py with round 7's soundness minors fixed (review-soundness-r7-2.md): every
solver-labelled fact is checked with each port its own atom (the file's facts carry no AliasOK);
bsc's confirmed facts keyed by both steps and both uses, used only where each stated condition
implies bsc's; each fact labelled by its source.

Structure (children, kinds, annotations, steps, sites, users, exclusions) is
the design's; data and guards are opaque operators over their real fan-in.
Round 7: the family statement (ParentU, CertU, OblU, RealizesU); exported
methods; exact call conditions; conditional reads under schedlean's conditions
(sites/tools/sitecond.py: conservative reach, and readiness under the ready
literal with the use-level fact over R_seq); escapes with their dynamic facts;
several urgency blockers; FIFO2 (library), SizedFIFO and RegFile (assumed).
"""
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import design  # noqa: E402
import model   # noqa: E402

dump, ba, out, NAME = sys.argv[1:5]
NST = int(sys.argv[5]) if len(sys.argv) > 5 else None
D = design.Design(dump, ba)
CS = None
if os.environ.get('E2E_SITES', '1') == '1':
    import condsites  # noqa: E402
    CS, CSRES = condsites.conditional_sites(os.path.dirname(os.path.abspath(dump)),
                                            os.path.join(os.path.dirname(os.path.abspath(out)), 'sitework_' + NAME))
M = model.Model(D, NST, CS)
K = M.K
nR, nM, nS = M.nR, M.nM, M.nS
MET = nM > 0


class MAnn:
    """The module's own exported annotations: methods pairwise CF where the dump exports CF,
    each conflicting with itself."""
    cf = set()
    for a_, b_ in D.info.get('CF', []):
        cf.add((a_, b_))
        cf.add((b_, a_))
    names = [s['name'] for s in M.msteps]

    @classmethod
    def prec(cls, a, b):
        return False

    @classmethod
    def sbr(cls, a, b):
        return False

    @classmethod
    def conf(cls, a, b):
        return a == b or (cls.names[a], cls.names[b]) not in cls.cf


if MET:
    assert all(not MAnn.conf(a, b) for a in range(nM) for b in range(nM) if a != b), 'non-CF method pairs'
# bsc's mutually_exclusive checks as suffix groups (cost review r7-1 M1): a group [x0, .., xn] has
# the checks (x_j, [x_j+1 .. xn]) for every j < n; the file states it once (SufAsm), O(n) data
GROUPS, CHK_G = [], {}
_bylst = {}
for c_, ch_ in enumerate(M.checks):
    _bylst.setdefault(tuple(ch_['lst']), []).append(c_)
for c0_, ch0_ in enumerate(M.checks):
    if len(ch0_['lst']) != 1 or c0_ in CHK_G:
        continue
    seq_, chain_ = [ch0_['head']] + ch0_['lst'], [c0_]
    while True:
        nx_ = [c_ for c_ in _bylst.get(tuple(seq_), []) if c_ not in CHK_G and c_ not in chain_]
        if not nx_:
            break
        chain_.append(nx_[0])
        seq_ = [M.checks[nx_[0]]['head']] + seq_
    g_ = len(GROUPS)
    GROUPS.append(seq_)
    for c_ in chain_:
        j_ = seq_.index(M.checks[c_]['head'])
        assert M.checks[c_]['lst'] == seq_[j_ + 1:], 'a check is not its group suffix'   # generator check
        CHK_G[c_] = (g_, j_)
LITC = [c_ for c_ in range(len(M.checks)) if c_ not in CHK_G]     # checks stated one by one
LITI = {c_: n_ for n_, c_ in enumerate(LITC)}
need, esc, Dn = M.scan_masks(MAnn if MET else None)
ESC = M.esc_pairs(esc)
U = M.users()
nT = M.nT

L = []
w = L.append
MARK = os.environ.get('MARK') == '1'


def mark(label):
    if MARK:
        w('\n#eval show IO Unit from do let t ← IO.monoMsNow; let h ← IO.getNumHeartbeats; '
          'IO.println s!"@@ %s {t} {h}"\n' % label)


def tab(items, ind='  '):
    def go(lo, hi):
        if hi - lo == 1:
            return '(.leaf (%s))' % items[lo]
        mid = (lo + hi) // 2
        return '(.node %d %s\n%s%s)' % (mid, go(lo, mid), ind, go(mid, hi))
    return go(0, len(items))


SPLITW = int(os.environ.get('E2E_SPLITW', '20000'))   # source characters per table definition
SPLITS = {}


def tab_range(items, lo, hi, ind='  '):
    if hi - lo == 1:
        return '(.leaf (%s))' % items[lo]
    mid = (lo + hi) // 2
    return '(.node %d %s\n%s%s)' % (mid, tab_range(items, lo, mid, ind), ind, tab_range(items, mid, hi, ind))


def split_struct(items, W):
    pre = [0]
    for x in items:
        pre.append(pre[-1] + len(x))

    def go(lo, hi, top):
        if pre[hi] - pre[lo] <= W or hi - lo == 1:
            return ('whole' if top else 'chunk', lo, hi)
        mid = (lo + hi) // 2
        return ('node', mid, go(lo, mid, False), go(mid, hi, False))
    return go(0, len(items), True)


def stab(name, ty, items, W=None):
    """`def name : ty` as `tab(items)`, its subtrees of at most W characters their own definitions."""
    st = split_struct(items, SPLITW if W is None else W)
    SPLITS[name] = st

    def render(x):
        if x[0] == 'whole':
            return tab_range(items, x[1], x[2])
        if x[0] == 'chunk':
            nm = '%s_%d_%d' % (name, x[1], x[2])
            w('def %s : %s :=\n  %s' % (nm, ty, tab_range(items, x[1], x[2])))
            return nm
        return '(.node %d %s\n  %s)' % (x[1], render(x[2]), render(x[3]))
    top = render(st)
    w('def %s : %s :=\n  %s' % (name, ty, top))


def stabproof(name, thm, params, args, stmt, leaves, one=True):
    st = SPLITS[name]
    fmt = tabproof if one else tabpair

    def render(x):
        if x[0] == 'whole':
            return fmt(leaves[x[1]:x[2]])
        if x[0] == 'chunk':
            nm = '%s_%d_%d' % (thm, x[1], x[2])
            w('theorem %s %s : %s :=\n  %s' % (nm, params, stmt('%s_%d_%d' % (name, x[1], x[2]), x[1], x[2]),
                                               fmt(leaves[x[1]:x[2]])))
            return '%s %s' % (nm, args)
        return '⟨%s,\n  %s⟩' % (render(x[2]), render(x[3]))
    return render(st)


def tabpair(items, ind='  '):
    def go(lo, hi):
        if hi - lo == 1:
            return items[lo]
        mid = (lo + hi) // 2
        return '⟨%s,\n%s%s⟩' % (go(lo, mid), ind, go(mid, hi))
    return go(0, len(items))


def tabproof(items, ind='  '):
    def go(lo, hi):
        if hi - lo == 1:
            return 'Tab.one (%s)' % items[lo]
        mid = (lo + hi) // 2
        return '⟨%s,\n%s%s⟩' % (go(lo, mid), ind, go(mid, hi))
    return go(0, len(items))


def nat(x):
    return '%d' % x


def allL(items):
    if not items:
        return 'trivial'
    return '⟨' + ', '.join(items + ['trivial']) + '⟩'


def tup(xs, z='ops.z'):
    if not xs:
        return z
    if len(xs) == 1:
        return xs[0]
    return 'ops.tup (%s) (%s)' % (xs[0], tup(xs[1:]))


def tupcong(cs):
    if not cs:
        return 'rfl'
    if len(cs) == 1:
        return cs[0]
    return 'congr2 ops.tup (%s) (%s)' % (cs[0], tupcong(cs[1:]))


def andc(cs):
    """Congruence for a right-nested && over equalities."""
    if len(cs) == 1:
        return cs[0]
    return 'congr2 and (%s) (%s)' % (cs[0], andc(cs[1:]))


def andget(h, n, i):
    """From h : (b0 && (b1 && ...)) = true with n conjuncts, the i-th."""
    t = h
    for _ in range(i):
        t = '((Bool.and_eq_true _ _).mp %s).2' % t
    if i < n - 1:
        t = '((Bool.and_eq_true _ _).mp %s).1' % t
    return t


def ocread(p, oc='oc'):
    kd, i, m = p
    return '(%s (%s %d)).ret %s' % (oc, M.ihelper(kd), i, K[kd].lports[K[kd].code(m)])


def site_port(k, i):
    return M.rules[k]['sites'][i]['port']


def stepL(k):
    return '.inr (rR %d)' % k if k < nR else '.inl (mM %d)' % (k - nR)


def vterm(k):
    return '(u (%s))' % stepL(k)


# ---- expressions: at the step's environment (rho), at the hardware (oc), and the value proofs ----

def Rterm(k, i, at):
    """The reach term of conditional read site i of step k."""
    R = M.rules[k]['sites'][i]['R']
    return 'ops.g %d (%s)' % (R[1], tup([at(j) for j in R[2]]))


def is_cond(k, j):
    return M.rules[k]['sites'][j].get('R') is not None


def sv_hw(k, j, t):
    """Site j's value in the model, over the hardware: its port where its condition holds, else t."""
    return '(if %s = true then %s else %s)' % (cond_hw(k, M.rules[k]['sites'][j]), ocread(site_port(k, j)), t)


def Yof(k, i):
    return M.rules[k]['sites'][i].get('Ysv') or []


def Rsv(k, i, tn):
    """R over the site values of its conditional reads, the skipped value of read j named tn(j)."""
    R = M.rules[k]['sites'][i]['R']
    return 'ops.g %d (%s)' % (R[1], tup([sv_hw(k, j, tn(j)) if j in Yof(k, i) else ocread(site_port(k, j)) for j in R[2]]))


def fsrc(n_, c):
    return c.hy if FACTS[n_]['ok'] else c.hr


def r_inv(k, i, c):
    """R at the site values (skipped values c.v) equals R at the ports, under c.hf; None if R reads
    no conditional read."""
    s = M.rules[k]['sites'][i]
    Y = Yof(k, i)
    if not Y:
        return None
    vj = lambda j: '(%s %d)' % (c.v, j)
    if s['inv'] == 'self':
        R = s['R']
        G = 'fun y => ops.g %d (%s)' % (R[1], tup(['y' if j == i else ocread(site_port(k, j)) for j in R[2]]))
        return 'inv_self (G := %s) (fun t => %s.rdy_%d oc t %s) %s' % (G, fsrc(s['fact'], c), s['fact'], c.hf, vj(i))
    return '%s.inv_%d oc %s %s' % (fsrc(s['inv'], c), s['inv'], ' '.join(vj(j) for j in Y), c.hf)


def rval(k, i, c):
    """R at the step's environment equals R at the ports."""
    s = M.rules[k]['sites'][i]
    Y = Yof(k, i)
    base = 'congrArg (ops.g %d) (%s)' % (s['R'][1], tupcong([evst(k, j, c) if j in Y else evt(k, j, c) for j in s['R'][2]]))
    iv = r_inv(k, i, c)
    return base if iv is None else '(%s).trans (%s)' % (base, iv)


def ex(k, e, at, xarg, boolean=False):
    """Render e over `at` (a function from a read site index to its term)."""
    t = e[0]
    if t == 'z':
        return 'ops.z'
    if t == 'k':
        return 'ops.k %d' % e[1]
    if t == 'x':
        return xarg
    if t == 'true':
        return 'true'
    if t == 'gk':
        return 'ops.g %d ops.z' % e[1]
    if t == 'lit':
        return 'ops.toB (%s)' % at(e[1])
    if t in ('f', 'g'):
        return 'ops.%s %d (%s)' % (t, e[1], tup([at(j) for j in e[2]]))
    if t == 'split':
        rs = ' && '.join('(%s)' % Rterm(k, i, at) for i in e[1]) if len(e[1]) > 1 else Rterm(k, e[1][0], at)
        if len(e[1]) > 2:
            rs = andterm([Rterm(k, i, at) for i in e[1]])
        return '(bif %s then %s else %s)' % (rs, ex(k, e[2], at, xarg, boolean), ex(k, e[3], at, xarg, boolean))
    raise ValueError(e)


def andterm(ts):
    if len(ts) == 1:
        return ts[0]
    return '(%s) && (%s)' % (ts[0], andterm(ts[1:]))


def rho(j):
    return 'ρ %d' % j


def hw(k):
    return lambda j: ocread(site_port(k, j))


def xhw(k):
    return 'w.arg (mM %d)' % (k - nR) if k >= nR else 'ops.z'


class Ctx:
    """The names in scope where a value proof is rendered: the step's skipped values, its
    argument, a proof that it fires (`WF … = true`), the solver facts."""

    def __init__(self, v, x='ops.z', hf='hf', hy='hy', hr='hr'):
        self.v, self.x, self.hf, self.hy, self.hr = v, x, hf, hy, hr


def evt(k, j, c):
    return 'ev_%d_%d ops E %s oc %s' % (k, j, c.v, c.x)


def fact_term(k, i, c, hR):
    """The ready literal from R, under hf: site i's readiness fact (over the site values)."""
    s = M.rules[k]['sites'][i]
    src = fsrc(s['fact'], c)
    Y = Yof(k, i)
    if not Y:
        return '%s.rdy_%d oc %s %s' % (src, s['fact'], c.hf, hR)
    return '%s.rdy_%d oc %s %s ((%s).trans %s)' % (src, s['fact'], ' '.join('(%s %d)' % (c.v, j) for j in Y), c.hf,
                                                   r_inv(k, i, c), hR)


def evst(k, j, c):
    return 'evs_%d_%d ops E %s oc %s' % (k, j, c.v, c.x)


def evct(k, j, c, hcond):
    return 'evc_%d_%d ops E %s oc %s (%s)' % (k, j, c.v, c.x, hcond)


def val(k, e, c, cond_ok=None):
    """A proof that e at the step's environment equals e at the hardware.  cond_ok maps a
    conditional read site to a proof of its condition (inside a split's then-branch)."""
    t = e[0]
    if t in ('z', 'k', 'x', 'true', 'gk'):
        return 'rfl'

    def rd(j):
        s = M.rules[k]['sites'][j]
        if s.get('R') is not None:
            assert cond_ok is not None and j in cond_ok, 'conditional read %d of step %d used outside its reach' % (j, k)
            return evct(k, j, c, cond_ok[j])
        return evt(k, j, c)
    if t == 'lit':
        return 'congrArg ops.toB (%s)' % rd(e[1])
    if t in ('f', 'g'):
        return 'congrArg (ops.%s %d) (%s)' % (t, e[1], tupcong([rd(j) for j in e[2]]))
    if t == 'split':
        crs = e[1]
        rv = andc([rval(k, i, c) for i in crs])
        ok = dict(cond_ok or {})
        for n_, i in enumerate(crs):
            hR = andget('hR', len(crs), n_)
            s = M.rules[k]['sites'][i]
            ok[i] = hR if s['ckind'] == 'reach' else fact_term(k, i, c, hR)
        return 'split_val (%s) (fun hR => %s) (%s)' % (rv, val(k, e[2], c, ok), val(k, e[3], c, cond_ok))
    raise ValueError(e)


# ---- per-site renderings ----

def sf_leaf(k, st):
    if st['kind'] == 'task':
        return 'fun V (ops : Ops V) (E : EqC V) (x : V) => ((fun ρ => %s, fun ρ => %s) : SF V)' % (
            ex(k, st['cond'], rho, 'x', True), tup([ex(k, a, rho, 'x') for a in st['args']]))
    c, a = st['cond'], st['arg']
    if c[0] == 'true' and a[0] == 'z':
        return 'rdSF'
    return 'fun V (ops : Ops V) (E : EqC V) (x : V) => ((fun ρ => %s, fun ρ => %s) : SF V)' % (
        ex(k, c, rho, 'x', True), ex(k, a, rho, 'x'))


def cond_hw(k, st):
    return ex(k, st['cond'], hw(k), xhw(k), True)


def arg_hw(k, st):
    if st['kind'] == 'task':
        return tup([ex(k, a, hw(k), xhw(k)) for a in st['args']])
    return ex(k, st['arg'], hw(k), xhw(k))


def cond_val(k, st, c):
    return val(k, st['cond'], c)


def arg_val(k, st, c):
    if st['kind'] == 'task':
        return tupcong([val(k, a, c) for a in st['args']])
    return val(k, st['arg'], c)


def is_uncond(st):
    return st['cond'][0] == 'true'


def guard_hw(k):
    return ex(k, M.rules[k]['guard'], hw(k), xhw(k), True)


def guard_rho(k):
    return ex(k, M.rules[k]['guard'], rho, 'x', True)


RDC = {}
for k_, r_ in enumerate(M.rules):
    RDC[k_] = [i for i, s in enumerate(r_['sites'])
               if s['kind'] == 'call' and not is_uncond(s) and M.is_rdy_guarded(s['port'])]
CONDRD = any(RDC.values())


def rdy_hw(p):
    kd, i, m = p
    return '(oc (%s %d)).rdy %s' % (M.ihelper(kd), i, K[kd].lports[K[kd].code(m)])


def G_def(k):
    """CAN_FIRE's guard part: the explicit guard, and bsc's lifted readiness of conditional
    guarded calls (`!c || RDY`)."""
    base = guard_hw(k)
    if not RDC[k]:
        return base
    parts = ['(!(%s) || %s)' % (cond_hw(k, M.rules[k]['sites'][i]), rdy_hw(site_port(k, i))) for i in RDC[k]]
    return '(%s) && (%s)' % (base, andterm(parts + ['true']))


evneed = set()     # (k, i): unconditional read sites whose value lemma is used
evcneed = set()    # (k, i): conditional read sites


def collect(k, e):
    t = e[0]
    if t in ('f', 'g'):
        for j in e[2]:
            (evcneed if M.rules[k]['sites'][j].get('R') is not None else evneed).add((k, j))
    elif t == 'lit':
        evneed.add((k, e[1]))
    elif t == 'split':
        for i in e[1]:
            evcneed.add((k, i))
        collect(k, e[2])
        collect(k, e[3])


for k_, r_ in enumerate(M.rules):
    for st_ in r_['sites']:
        if st_['kind'] == 'task':
            collect(k_, st_['cond'])
            for a_ in st_['args']:
                collect(k_, a_)
        else:
            collect(k_, st_['cond'])
            collect(k_, st_['arg'])
    if r_['guard'] and r_['guard'][0] not in ('check', 'false'):
        collect(k_, r_['guard'])
    collect(k_, r_.get('res', ('z',)))
evsneed = set()
for (k_, i_) in list(evcneed):
    s_ = M.rules[k_]['sites'][i_]
    for j_ in s_['R'][2]:
        if j_ in (s_.get('Ysv') or []):
            evsneed.add((k_, j_))
            c_ = M.rules[k_]['sites'][j_]['cond']
            if c_[0] == 'lit':
                evneed.add((k_, c_[1]))
            elif c_[0] == 'g':
                for j2_ in c_[2]:
                    evneed.add((k_, j2_))
        else:
            evneed.add((k_, j_))
    if s_['cond'][0] == 'lit':
        evneed.add((k_, s_['cond'][1]))
    elif s_['cond'][0] == 'g':
        for j_ in s_['cond'][2]:
            evneed.add((k_, j_))
for f_ in M.facts:
    pass

FACTS = M.facts
UNCONF = [f for f in FACTS if not f['ok']]
HASF2 = 'f2' in M.kinds or 'sf' in M.kinds

for k_, r_ in enumerate(M.rules):
    for i_, s_ in enumerate(r_['sites']):
        if s_.get('fact') is not None:
            FACTS[s_['fact']]['k'] = k_
        if isinstance(s_.get('inv'), int):
            FACTS[s_['inv']]['k'] = k_
assert M.fan_outside == 0, 'an R literal reads a port that is not a read site of its step'

# escape facts: per escaped pair, per statically failing site pair, the dynamic exclusion
EFACTS = []        # dict(r1, r2, i1, i2, ok, how, bsc)


def sitecond_facts(path):
    import sitecond as _sc
    return _sc.parse_dump(path)['facts']
if CS is not None:
    sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), '..', '..', 'sites', 'tools'))
    from satc import Enc as EncE  # noqa: E402
    # every port its own atom (the site tool's exact view): the facts quantify over every child output
    SDE, APE = CSRES['_ctx']['DX'], CSRES['_ctx']['ap']
    BFE, PARSE = CSRES['_ctx']['bsc_facts'], CSRES['_ctx']['parse']
    WORKE = os.path.join(os.path.dirname(os.path.abspath(out)), 'sitework_' + NAME)
    CSL = {(k_, e_['label']): e_ for k_, v_ in CS.items() for e_ in v_}

    def wfe(n):
        x_ = 'WILL_FIRE_' + n
        return ('id', x_) if x_ in SDE.defs else ('id', 'CAN_FIRE_' + n)

    def site_lit(E, k, st):
        """The site's condition as the file states it, as a literal of E."""
        if st['kind'] == 'read':
            if is_uncond(st):
                return 1
            e_ = CSL[(M.rules[k]['name'], st['label'])]
            if e_['kind'] == 'rdy':
                return E.OR([E.atom(('port', E.D.canon(('call', p_[0], p_[1], [])))) for p_ in e_['rdy']])
            return e_['enc'](E)
        if st['kind'] == 'task':
            return 1    # a task statement's condition: true is the weaker cover
        inst_, meth_ = st['label'].rsplit('.', 1)
        meth_ = 'sub' if meth_.startswith('sub') else meth_
        cs_ = [a[3] for a in APE.rules.get(M.rules[k]['name'], (None, []))[1]
               if a and a[0] == 'call' and a[1] == inst_ and a[2] == meth_]
        cs_ = [c if c is not None else ('lit', "1'd1") for c in cs_] or [('lit', "1'd1")]
        return E.OR([E.b(c) for c in cs_])

    def bsc_fact(k1, s1, lit1, k2, s2, lit2):
        """bsc's confirmed fact for these two sites (keyed by both steps and both uses, r7 review s2
        minor 3), provided each site's stated condition (with WF) implies bsc's condition (with the
        step's guard); else None."""
        if s1['kind'] == 'task' or s2['kind'] == 'task':
            return None
        n1, n2 = M.rules[k1]['name'], M.rules[k2]['name']
        text = BFE.covers(n1, s1['label'], n2, s2['label'])
        if text is None:
            return None
        for (n, s, litf) in ((n1, s1, lit1), (n2, s2, lit2)):
            E = EncE(SDE)
            try:
                cs_ = [PARSE(t) for t in BFE.use_cond(n, s['label'])]
                g_ = BFE.use_guard(n)
                g_ = PARSE(g_) if g_ else ('lit', "1'd1")
            except Exception:
                return None
            if not cs_:
                return None
            if E.sat([E.b(wfe(n)), litf(E), -E.AND([E.b(g_), E.OR([E.b(c) for c in cs_])])], WORKE) is not False:
                return None
        return text
for (r1, r2, bad) in ESC:
    for (i1, i2) in bad:
        s1, s2 = M.rules[r1]['sites'][i1], M.rules[r2]['sites'][i2]
        how, btext = None, None
        if CS is not None:
            # this fact, for this site pair and the conditions this file states (r7 review s1 minor 3)
            E_ = EncE(SDE)
            roots_ = [E_.b(wfe(M.rules[r1]['name'])), E_.b(wfe(M.rules[r2]['name'])),
                      site_lit(E_, r1, s1), site_lit(E_, r2, s2)]
            if E_.sat(roots_, WORKE) is False:
                how = 'solver'
            else:
                btext = bsc_fact(r1, s1, lambda E, k=r1, s=s1: site_lit(E, k, s),
                                 r2, s2, lambda E, k=r2, s=s2: site_lit(E, k, s))
                how = 'bsc' if btext else None
        EFACTS.append(dict(r1=r1, r2=r2, i1=i1, i2=i2, ok=how is not None, how=how, bsc=btext))
UNCONF_E = [f for f in EFACTS if not f['ok']]
HAS_UNCONF = bool(UNCONF or UNCONF_E)
HR_ARG = ' (hr : Unconfirmed ops E)' if HAS_UNCONF else ''
HR_USE = ' hr' if HAS_UNCONF else ''

# ---------------------------------------------------------------- header
w('''/- %s: the modular obligation of one corpus module, emitted by e2e/tools/mke2e.py.
   Structure (children, steps, sites, users, exclusions) is the design's; data and
   guards are opaque operators over their real fan-in (see e2e/README.md). -/
import Scan
open Modular Emit Scan
open scoped List

set_option autoImplicit false
set_option linter.unusedVariables false
set_option linter.unusedSimpArgs false
set_option linter.deprecated false
set_option maxRecDepth 100000
set_option maxHeartbeats 0

namespace %s
noncomputable section

/-- The module's operators: constants, opaque data and Boolean operators (by number), tuples,
    and the design's Bool reading of a one-bit value. -/
structure Ops (V : Type) where
  z : V
  tup : V → V → V
  k : Nat → V
  f : Nat → V → V
  g : Nat → V → Bool
  ofBool : Bool → V
  toB : V → Bool := fun _ => false
''' % (NAME, NAME))
w('''section
variable {V : Type}
/-- A split use of conditional reads: equal selections, and equal values where selected. -/
theorem split_val {α : Type} {b b' : Bool} {x1 x2 y1 y2 : α} (hb : b = b') (h1 : b' = true → x1 = y1)
    (h2 : x2 = y2) : (bif b then x1 else x2) = (bif b' then y1 else y2) := by
  subst hb; cases b <;> simp_all
/-- Urgency with several blockers: `a && !(b₁ || …)` excludes each `bᵢ`. -/
theorem blk_excl_or {a b : Bool} {l : List Bool} {p : Nat} (h : (a && !orL l) = true) (hl : l[p]? = some b)
    (hb : b = true) : False := by
  have : orL l = true := orL_true.mpr ⟨b, List.mem_of_getElem? hl, hb⟩
  rw [this] at h; simp at h
theorem iff_of_eqB {a b : Bool} (h : a = b) : a = true ↔ b = true := by rw [h]
/-- A conditional read's site value, its condition rewritten. -/
theorem ite_bcongr {α : Type} {c c' : Bool} (h : c = c') (a b : α) :
    (if c = true then a else b) = (if c' = true then a else b) := by rw [h]
/-- An R term over its own read's site value: the readiness fact gives the invariance. -/
theorem inv_self {G : V → Bool} {c : Bool} {p : V} (h : ∀ t, G (if c = true then p else t) = true → c = true)
    (t : V) : G (if c = true then p else t) = G p := by
  cases c with
  | true => rfl
  | false =>
    have h1 : G t ≠ true := fun e => absurd (h t e) (by decide)
    have h2 : G p ≠ true := fun e => absurd (h p e) (by decide)
    simp only [Bool.false_eq_true, ite_false]
    cases ht : G t <;> cases hp : G p <;> simp_all
theorem cond_congr {α : Type} {b b' : Bool} {x x' y y' : α} (hb : b = b') (hx : x = x') (hy : y = y') :
    (bif b then x else y) = (bif b' then x' else y') := by subst hb hx hy; rfl
/-- One user's enable entry, from its firing and its call condition. -/
theorem en_cnd {a c : Bool} {On C : Prop} (hOn : On ↔ a = true) (hC : a = true → (C ↔ c = true)) :
    (a && c) = true ↔ On ∧ C := by
  rw [Bool.and_eq_true]
  constructor
  · intro h; exact ⟨hOn.mpr h.1, (hC h.1).mpr h.2⟩
  · intro h; exact ⟨hOn.mp h.1, (hC (hOn.mp h.1)).mp h.2⟩
theorem en_unc {a : Bool} {On C : Prop} (hOn : On ↔ a = true) (hC : a = true → C) : a = true ↔ On ∧ C :=
  ⟨fun h => ⟨hOn.mpr h, hC h⟩, fun h => hOn.mp h.1⟩
/-- A port's enable over explicit per-user entries. -/
theorem en_pairs (P : Parent V) (fire : ((j : P.I) → Outs (P.CI j).M V) → Wires P.X.M V → P.PR → Bool)
    (oc : (j : P.I) → Outs (P.CI j).M V) (w : Wires P.X.M V) (j : P.I) (cm : (P.CI j).M) :
    ∀ (l : List (Step P × Bool)), AllL (fun e => e.2 = true ↔ On P fire oc w e.1 ∧ Calls P (trS P oc w e.1) j cm) l →
      (orL (l.map Prod.snd) = true ↔ ∃ s ∈ l.map Prod.fst, On P fire oc w s ∧ Calls P (trS P oc w s) j cm)
  | [], _ => by simp [orL]
  | e :: l, ⟨h1, h2⟩ => by
    have h1' : e.2 = true ↔ On P fire oc w e.1 ∧ Calls P (trS P oc w e.1) j cm := h1
    simp only [List.map_cons, orL, Bool.or_eq_true, List.mem_cons, exists_eq_or_imp]
    rw [h1', en_pairs P fire oc w j cm l h2]
theorem mem_callsOf_inv {I : Type} {CM : I → Type} (ρ : Nat → V) : ∀ (ss : List (Site I CM V)) (c : Σ j, CM j × V),
    c ∈ callsOf ρ ss → ∃ s ∈ ss, s.cond ρ = true ∧ c = ⟨s.j, (s.m, s.arg ρ)⟩
  | [], c, h => by simp [callsOf] at h
  | s :: ss, c, h => by
    simp only [callsOf, List.mem_append] at h
    rcases h with h | h
    · by_cases hc : s.cond ρ = true
      · rw [if_pos hc] at h; simp only [List.mem_singleton] at h; exact ⟨s, List.mem_cons_self .., hc, h⟩
      · rw [if_neg hc] at h; simp at h
    · obtain ⟨s', hs', h1, h2⟩ := mem_callsOf_inv ρ ss c h
      exact ⟨s', List.mem_cons_of_mem _ hs', h1, h2⟩
end
''')

# ---------------------------------------------------------------- interfaces


def sched_lean(sl, ctor):
    rel = {'CF': '.CF', 'SB': '.SB', 'SBR': '.SBR', 'C': '.C'}
    items = []
    for ps, r, qs in sl:
        items.append('([%s], %s, [%s])' % (', '.join(ctor + p for p in ps), rel[r], ', '.join(ctor + q for q in qs)))
    return '[' + ',\n   '.join(items) + ']'


w('/-! ## Child interfaces: the library\'s, and assumed kinds from the dump\'s schedules -/\n')
if 'fifo' in M.kinds:
    w('''/-- FIFOL1's ports. -/
inductive FL where
  | enq | deq | first | notFull | i_notFull | notEmpty | i_notEmpty | clear
  deriving DecidableEq

/-- `FIFOL1`'s schedule, as the dump prints it. -/
def flSched : List (List FL × Rel × List FL) :=
  %s

/-- FIFOL1 (assumed leaf): its schedule; `deq` has paths to `enq`'s ready and the full flags. -/
def flAnn : Ann FL :=
  { Ann.ofSched flSched (fun m => match m with | .enq | .deq | .clear => false | _ => true)
      (fun m => match m with | .enq | .deq | .clear => true | _ => false) (fun _ => false)
      (fun m => match m with | .enq | .deq | .first => false | _ => true) with
    path := fun p q => match p, q with
      | .deq, .enq | .deq, .notFull | .deq, .i_notFull | .deq, .clear | .enq, .clear => true
      | _, _ => false }

abbrev FL.X : Iface := ⟨FL, inferInstance, flAnn⟩
''' % sched_lean(M.fifo_sched, '.'))
if 'cfg' in M.kinds:
    w('''/-- ConfigRegN's ports. -/
inductive CfgM where
  | read | write
  deriving DecidableEq

/-- ConfigRegN (assumed leaf): `read` returns the cycle's start value. -/
def cfgAnn : Ann CfgM :=
  Ann.ofSched %s (fun m => m == .read) (fun m => m == .write) (fun _ => false) (fun _ => true)

abbrev Cfg.X : Iface := ⟨CfgM, inferInstance, cfgAnn⟩
''' % sched_lean(M.cfg_sched, '.'))
if 'rf' in M.kinds:
    w('''/-- RegFile's ports: `upd`, and one port per copy of `sub` (bsc's port copies). -/
inductive RFM where
  | upd | s0 | s1 | s2 | s3 | s4
  deriving DecidableEq
def RFM.isS : RFM → Bool
  | .upd => false
  | _ => true
/-- RegFile (assumed leaf): `sub CF sub, sub SB upd, upd C upd`; a copy's calls share their
    address; the address reaches the copy's result combinationally. -/
def rfAnn : Ann RFM where
  prec p q := p.isS && !q.isS
  sbr _ _ := false
  conf p q := !p.isS && !q.isS
  share m := m.isS
  hasEn m := !m.isS
  alwaysEn _ := false
  alwaysRdy _ := true
  path p q := p.isS && decide (p = q)
abbrev RF.X : Iface := ⟨RFM, inferInstance, rfAnn⟩
''')
if HASF2:
    w('/-- The dump\'s FIFO2 / SizedFIFO schedule is the library\'s `fifoAnn` (checked). -/\n'
      'theorem f2Sched : ∀ p q : FifoM, Ann.ofSched fifo2Sched (fun m => fifoAnn.share m) (fun m => fifoAnn.hasEn m)\n'
      '    (fun m => fifoAnn.alwaysEn m) (fun m => fifoAnn.alwaysRdy m) |>.prec p q = fifoAnn.prec p q := by\n'
      '  intro p q; cases p <;> cases q <;> rfl\n' if False else '')

if nT:
    w('/-- The module\'s task statements, numbered in rank order (trust point: bsc\'s Verilog order). -/\n'
      'abbrev nT : Nat := %d\ndef tP (t : Nat) : Fin nT := ⟨t %% nT, Nat.mod_lt _ (by decide)⟩\n' % nT)
w('inductive Kind where\n  | ' + ' | '.join(M.kinds) + '\n  deriving DecidableEq\n')
w('abbrev nk : Kind → Nat\n' + ''.join('  | .%s => %d\n' % (k, len(M.byk[k])) for k in M.kinds))
w('abbrev KI : Kind → Iface\n' + ''.join('  | .%s => %s\n' % (k, K[k].lean_iface) for k in M.kinds))
w('''abbrev I := Σ κ : Kind, Fin (nk κ)
abbrev CI : I → Iface := fun j => KI j.1
abbrev OC (V : Type) := (j : I) → Outs (CI j).M V
''')
for k in M.kinds:
    w('def %s (i : Nat) : I := ⟨.%s, ⟨i %% nk .%s, Nat.mod_lt _ (by decide)⟩⟩' % (M.ihelper(k), k, k))
if not MET:
    XM = 'Empty'
    w('''
def xAnn : Ann Empty where
  prec e _ := e.elim
  sbr e _ := e.elim
  conf e _ := e.elim
  share e := e.elim
  hasEn e := e.elim
  alwaysEn e := e.elim
  alwaysRdy e := e.elim
abbrev X : Iface := ⟨Empty, inferInstance, xAnn⟩

abbrev nR : Nat := %d
abbrev nS : Nat := %d
abbrev St := Empty ⊕ Fin nR
/-- Rule `k` (rules are numbered in schedule order). -/
def rR (k : Nat) : Fin nR := ⟨k %% nR, Nat.mod_lt _ (by decide)⟩
theorem rR_val (r : Fin nR) : rR r.val = r := Fin.ext (Nat.mod_eq_of_lt r.isLt)
theorem rlt (r : Fin nR) : r.val < nS := Nat.lt_of_lt_of_le r.isLt (by decide)
''' % (nR, nS))
else:
    XM = '(Fin nM)'
    w('''
abbrev nM : Nat := %d
/-- The module's exported methods (dump order): pairwise CF, each conflicting with itself; enabled,
    with RDY outputs. -/
def xAnn : Ann (Fin nM) where
  prec _ _ := false
  sbr _ _ := false
  conf p q := decide (p = q)
  share _ := false
  hasEn _ := true
  alwaysEn _ := false
  alwaysRdy _ := false
abbrev X : Iface := ⟨Fin nM, inferInstance, xAnn⟩

abbrev nR : Nat := %d
abbrev nS : Nat := %d
abbrev St := Fin nM ⊕ Fin nR
/-- Rule `k` (rules are numbered in schedule order); method `m` is step `nR + m`. -/
def rR (k : Nat) : Fin nR := ⟨k %% nR, Nat.mod_lt _ (by decide)⟩
theorem rR_val (r : Fin nR) : rR r.val = r := Fin.ext (Nat.mod_eq_of_lt r.isLt)
def mM (k : Nat) : Fin nM := ⟨k %% nM, Nat.mod_lt _ (by decide)⟩
theorem mM_val (m : Fin nM) : mM m.val = m := Fin.ext (Nat.mod_eq_of_lt m.isLt)
theorem rlt (r : Fin nR) : r.val < nS := Nat.lt_of_lt_of_le r.isLt (by decide)
theorem mlt (m : Fin nM) : nR + m.val < nS := Nat.lt_of_lt_of_le (Nat.add_lt_add_left m.isLt nR) (by decide)
/-- A step's table index: rules first, then methods. -/
def tix : St → Nat
  | .inl m => nR + m.val
  | .inr r => r.val
def stOf (t : Nat) : St := bif Nat.blt t nR then .inr (rR t) else .inl (mM (t - nR))
theorem stOf_tix (s : St) : stOf (tix s) = s := by
  cases s with
  | inl m =>
    have h : Nat.blt (nR + m.val) nR = false := Bool.eq_false_iff.mpr (fun h' => absurd (Nat.blt_eq.mp h') (Nat.not_lt.mpr (Nat.le_add_right nR m.val)))
    simp only [stOf, tix, h, cond_false, Nat.add_sub_cancel_left, mM_val]
  | inr r =>
    have h : Nat.blt r.val nR = true := Nat.blt_eq.mpr r.isLt
    simp only [stOf, tix, h, cond_true, rR_val]
/-- A method is called: enabled (as `called`, before the parent exists). -/
def calledM {V : Type} (w : Wires (Fin nM) V) (m : Fin nM) : Bool := (xAnn.hasEn m && w.en m) || xAnn.alwaysEn m
''' % (nM, nR, nS))

w('''/-! ## Per-kind builders, port splits, uses -/

def regW {V : Type} (en : Bool) (d z : V) : Wires (CRegM 1) V :=
  ⟨fun m => match m with | .wr _ => en | .rd _ => false, fun m => match m with | .wr _ => d | .rd _ => z⟩
def wireW {V : Type} (en : Bool) (d z : V) : Wires WireM V :=
  ⟨fun m => match m with | .wset => en | _ => false, fun m => match m with | .wset => d | _ => z⟩
def wire0W {V : Type} (en : Bool) (z : V) : Wires Wire0M V :=
  ⟨fun m => match m with | .wset => en | _ => false, fun _ => z⟩
theorem fin1 (i : Fin 1) : i = 0 := by obtain ⟨k, hk⟩ := i; congr; omega
theorem regAll {Q : CRegM 1 → Prop} (hr : Q Reg.read) (hw : Q Reg.write) : ∀ cm, Q cm := by
  intro cm; cases cm with
  | rd i => rw [fin1 i]; exact hr
  | wr i => rw [fin1 i]; exact hw
theorem wireAll {Q : WireM → Prop} (hs : Q .wset) (hg : Q .wget) (hh : Q .whas) : ∀ cm, Q cm := by
  intro cm; cases cm <;> assumption
theorem reg_nopath (p q : CRegM 1) : (cregAnn 1).path p q = false := by
  cases p <;> cases q <;> rename_i a b <;> rw [fin1 a, fin1 b] <;> rfl
theorem wire0All {Q : Wire0M → Prop} (hs : Q .wset) (hh : Q .whas) : ∀ cm, Q cm := by
  intro cm; cases cm <;> assumption

/-- A port's uses: a table keyed by step (its sites per step), and its users as a list. -/
structure Uses (M : Type) where
  uses : M → Tab (Nat × List Nat)
  us : M → List Nat
def regU (rd wr : Tab (Nat × List Nat)) (rd' wr' : List Nat) : Uses (CRegM 1) :=
  ⟨fun m => match m with | .rd _ => rd | .wr _ => wr, fun m => match m with | .rd _ => rd' | .wr _ => wr'⟩
def wireU (s g h : Tab (Nat × List Nat)) (s' g' h' : List Nat) : Uses WireM :=
  ⟨fun m => match m with | .wset => s | .wget => g | .whas => h, fun m => match m with | .wset => s' | .wget => g' | .whas => h'⟩
def wire0U (s h : Tab (Nat × List Nat)) (s' h' : List Nat) : Uses Wire0M :=
  ⟨fun m => match m with | .wset => s | .whas => h, fun m => match m with | .wset => s' | .whas => h'⟩
''')
if 'cfg' in M.kinds:
    w('''def cfgW {V : Type} (en : Bool) (d z : V) : Wires CfgM V :=
  ⟨fun m => match m with | .write => en | .read => false, fun m => match m with | .write => d | .read => z⟩
theorem cfgAll {Q : CfgM → Prop} (hr : Q .read) (hw : Q .write) : ∀ cm, Q cm := by
  intro cm; cases cm <;> assumption
def cfgU (rd wr : Tab (Nat × List Nat)) (rd' wr' : List Nat) : Uses CfgM :=
  ⟨fun m => match m with | .read => rd | .write => wr, fun m => match m with | .read => rd' | .write => wr'⟩
''')
if 'fifo' in M.kinds:
    w('''def fifoW {V : Type} (eE dE cE : Bool) (d z : V) : Wires FL V :=
  ⟨fun m => match m with | .enq => eE | .deq => dE | .clear => cE | _ => false,
   fun m => match m with | .enq => d | _ => z⟩
theorem flAll {Q : FL → Prop} (h1 : Q .enq) (h2 : Q .deq) (h3 : Q .first) (h4 : Q .notFull) (h5 : Q .i_notFull)
    (h6 : Q .notEmpty) (h7 : Q .i_notEmpty) (h8 : Q .clear) : ∀ cm, Q cm := by
  intro cm; cases cm <;> assumption
def fifoU (enq deq first nf inf ne ine clr : Tab (Nat × List Nat)) (enq' deq' first' nf' inf' ne' ine' clr' : List Nat) : Uses FL :=
  ⟨fun m => match m with
    | .enq => enq | .deq => deq | .first => first | .notFull => nf | .i_notFull => inf
    | .notEmpty => ne | .i_notEmpty => ine | .clear => clr,
   fun m => match m with
    | .enq => enq' | .deq => deq' | .first => first' | .notFull => nf' | .i_notFull => inf'
    | .notEmpty => ne' | .i_notEmpty => ine' | .clear => clr'⟩
''')
if HASF2:
    w('''def f2W {V : Type} (eE dE cE : Bool) (d z : V) : Wires FifoM V :=
  ⟨fun m => match m with | .enq => eE | .deq => dE | .clear => cE | _ => false,
   fun m => match m with | .enq => d | _ => z⟩
theorem f2All {Q : FifoM → Prop} (h1 : Q .enq) (h2 : Q .deq) (h3 : Q .first) (h4 : Q .notEmpty) (h5 : Q .notFull)
    (h6 : Q .i_notEmpty) (h7 : Q .i_notFull) (h8 : Q .clear) : ∀ cm, Q cm := by
  intro cm; cases cm <;> assumption
def f2U (enq deq first ne nf ine inf clr : Tab (Nat × List Nat)) (enq' deq' first' ne' nf' ine' inf' clr' : List Nat) : Uses FifoM :=
  ⟨fun m => match m with
    | .enq => enq | .deq => deq | .first => first | .notEmpty => ne | .notFull => nf
    | .i_notEmpty => ine | .i_notFull => inf | .clear => clr,
   fun m => match m with
    | .enq => enq' | .deq => deq' | .first => first' | .notEmpty => ne' | .notFull => nf'
    | .i_notEmpty => ine' | .i_notFull => inf' | .clear => clr'⟩
''')
if 'rf' in M.kinds:
    w('''def rfW {V : Type} (eU : Bool) (dU a0 a1 a2 a3 a4 : V) : Wires RFM V :=
  ⟨fun m => match m with | .upd => eU | _ => false,
   fun m => match m with | .upd => dU | .s0 => a0 | .s1 => a1 | .s2 => a2 | .s3 => a3 | .s4 => a4⟩
theorem rfAll {Q : RFM → Prop} (h0 : Q .upd) (h1 : Q .s0) (h2 : Q .s1) (h3 : Q .s2) (h4 : Q .s3) (h5 : Q .s4) :
    ∀ cm, Q cm := by
  intro cm; cases cm <;> assumption
def rfU (upd a0 a1 a2 a3 a4 : Tab (Nat × List Nat)) (upd' a0' a1' a2' a3' a4' : List Nat) : Uses RFM :=
  ⟨fun m => match m with | .upd => upd | .s0 => a0 | .s1 => a1 | .s2 => a2 | .s3 => a3 | .s4 => a4,
   fun m => match m with | .upd => upd' | .s0 => a0' | .s1 => a1' | .s2 => a2' | .s3 => a3' | .s4 => a4'⟩
''')


def uses_tab(p):
    by = {}
    for k, i in U.get(p, []):
        by.setdefault(k, []).append(i)
    ks = sorted(by)
    if not ks:
        return '(.leaf (0, []))'

    def go(lo, hi):
        if hi - lo == 1:
            return '(.leaf (%d, [%s]))' % (ks[lo], ', '.join(map(str, by[ks[lo]])))
        mid = (lo + hi) // 2
        return '(.node %d %s %s)' % (ks[mid], go(lo, mid), go(mid, hi))
    return go(0, len(ks))


def uses_us(p):
    return '[%s]' % ', '.join(str(k) for k in sorted(set(k for k, _ in U.get(p, []))))


for kd in M.kinds:
    if kd == 'tasks':
        stab('tusesT', 'Tab (Tab (Nat × List Nat))', [uses_tab(('tasks', 0, t)) for t in range(nT)])
        stab('tusT', 'Tab (List Nat)', [uses_us(('tasks', 0, t)) for t in range(nT)])
        w('def uses_tasks : Tab (Uses (KI .tasks).M) := .leaf ⟨fun t => tusesT.get t.val, fun t => tusT.get t.val⟩')
        continue
    items = []
    for i in range(len(M.byk[kd])):
        args = ' '.join(uses_tab((kd, i, m)) for m in K[kd].ports)
        args2 = ' '.join(uses_us((kd, i, m)) for m in K[kd].ports)
        items.append('%s %s %s' % (K[kd].usesc, args, args2))
    stab('uses_%s' % kd, 'Tab (Uses (KI .%s).M)' % kd, items)
w('def usesK : (κ : Kind) → Tab (Uses (KI κ).M)\n' + ''.join('  | .%s => uses_%s\n' % (k, k) for k in M.kinds))
if not MET:
    w('''/-- A port's uses: a table keyed by rule, so a rule's sites on the port are a lookup (no walk). -/
def usesAt (p : Σ j : I, (CI j).M) : Tab (Nat × List Nat) := ((usesK p.1.1).get p.1.2.val).uses p.2
def usL (p : Σ j : I, (CI j).M) : List Nat := ((usesK p.1.1).get p.1.2.val).us p.2
def users (j : I) (cm : (CI j).M) : List St := (usL ⟨j, cm⟩).map (fun k => .inr (rR k))
def idx (s : St) (p : Σ j : I, (CI j).M) : List Nat := match s with
  | .inl e => e.elim
  | .inr r => idxT (usesAt p) r.val
''')
else:
    w('''/-- A port's uses: a table keyed by step index, so a step's sites on the port are a lookup. -/
def usesAt (p : Σ j : I, (CI j).M) : Tab (Nat × List Nat) := ((usesK p.1.1).get p.1.2.val).uses p.2
def usL (p : Σ j : I, (CI j).M) : List Nat := ((usesK p.1.1).get p.1.2.val).us p.2
def users (j : I) (cm : (CI j).M) : List St := (usL ⟨j, cm⟩).map stOf
def idx (s : St) (p : Σ j : I, (CI j).M) : List Nat := idxT (usesAt p) (tix s)
''')
for kd in M.kinds:
    if kd == 'tasks':
        w('theorem usOK_tasks\' : Tab.allB (fun t T => usersT T == tusT.get t) tusesT 0 nT = true := by decide +kernel')
        continue
    w('theorem usOK_%s : Tab.allB (fun _ U => ([%s] : List (KI .%s).M).all (fun m => usersT (U.uses m) == U.us m)) uses_%s 0 (nk .%s) = true := by\n  decide +kernel'
      % (kd, ', '.join(K[kd].lports), kd, kd, kd))
    if kd == 'reg':
        w('theorem portsU_reg : ∀ p : CRegM 1, p ∈ [Reg.read, Reg.write]\n  | .rd ⟨0, _⟩ => by simp\n  | .wr ⟨0, _⟩ => by simp')
    else:
        w('theorem portsU_%s : ∀ p : (KI .%s).M, p ∈ [%s] := by intro p; cases p <;> simp' % (kd, kd, ', '.join(K[kd].lports)))
w('''/-- The users list is the uses table's (decided per kind). -/
theorem usersT_eq (p : Σ j : I, (CI j).M) : usersT (usesAt p) = usL p := by
  obtain ⟨⟨κ, i, hi⟩, m⟩ := p
  cases κ
''' + ''.join(('  · have h := Tab.allB_spec _ tusesT 0 nT usOK_tasks\' m.val (Nat.zero_le _) m.isLt\n'
                 '    have hi0 : i = 0 := by simp at hi; omega\n'
                 '    subst hi0\n'
                 '    exact beq_iff_eq.mp h\n') if kd == 'tasks' else
               ('  · have h := List.all_eq_true.mp (Tab.allB_spec _ uses_%s 0 (nk .%s) usOK_%s i (Nat.zero_le _) hi) m (portsU_%s m)\n'
                '    exact beq_iff_eq.mp h\n') % (kd, kd, kd, kd) for kd in M.kinds))
if not MET:
    w('''theorem mem_users {r : Fin nR} {p : Σ j : I, (CI j).M} {i : Nat} (h : i ∈ idx (.inr r) p) : (.inr r : St) ∈ users p.1 p.2 := by
  have h1 := mem_usersT h
  rw [usersT_eq] at h1
  have h' := List.mem_map_of_mem (f := fun k => (.inr (rR k) : St)) h1
  rw [rR_val] at h'
  exact h'
/-- A fact for each user of a port, from a balanced proof over its uses table (no list proof). -/
theorem users_all {j : I} {cm : (CI j).M} {Q : St → Prop}
    (h : Tab.All (fun e => e.2.isEmpty = false → Q (.inr (rR e.1))) (usesAt ⟨j, cm⟩)) : ∀ s ∈ users j cm, Q s := by
  intro s hs
  obtain ⟨k, hk, rfl⟩ := List.mem_map.mp hs
  rw [← usersT_eq] at hk
  exact usersT_all (Q := fun k => Q (.inr (rR k))) h k hk
''')
else:
    w('''theorem mem_users {s : St} {p : Σ j : I, (CI j).M} {i : Nat} (h : i ∈ idx s p) : s ∈ users p.1 p.2 := by
  have h1 := mem_usersT h
  rw [usersT_eq] at h1
  have h' := List.mem_map_of_mem (f := stOf) h1
  rw [stOf_tix] at h'
  exact h'
theorem users_all {j : I} {cm : (CI j).M} {Q : St → Prop}
    (h : Tab.All (fun e => e.2.isEmpty = false → Q (stOf e.1)) (usesAt ⟨j, cm⟩)) : ∀ s ∈ users j cm, Q s := by
  intro s hs
  obtain ⟨k, hk, rfl⟩ := List.mem_map.mp hs
  rw [← usersT_eq] at hk
  exact usersT_all (Q := fun k => Q (stOf k)) h k hk
''')
mark('uses')

# ---------------------------------------------------------------- site tables
w('/-! ## Steps as numbered site tables (rules, then methods) -/\n')
for k, r in enumerate(M.rules):
    keys = [M.lport(st['port']) for st in r['sites']] or ['⟨%s 0, %s⟩' % (M.ihelper(M.kinds[0]), K[M.kinds[0]].lports[0])]
    stab('key_%d' % k, 'Tab (Σ j : I, (CI j).M)', keys)
stab('keyT', 'Tab (Tab (Σ j : I, (CI j).M))', ['key_%d' % k for k in range(nS)])
stab('kT', 'Tab Nat', [nat(len(r['sites'])) for r in M.rules])
w('abbrev kR (k : Nat) : Nat := kT.get k\n')
w('''section
variable {V : Type}
/-- An unconditional site that passes the dummy argument (reads, calls without data). -/
def rdSF (V : Type) (ops : Ops V) (E : EqC V) (x : V) : SF V := (fun _ => true, fun _ => ops.z)
end
''')
for k, r in enumerate(M.rules):
    leaves = [sf_leaf(k, st) for st in r['sites']] or ['rdSF']
    stab('sf_%d' % k, 'Tab ((V : Type) → Ops V → EqC V → V → SF V)', leaves)
stab('sfT', 'Tab (Tab ((V : Type) → Ops V → EqC V → V → SF V))', ['sf_%d' % k for k in range(nS)])
w('''
section
variable {V : Type}
/-- Step `k`'s sites at argument `x` (a method's argument; rules pass `z`). -/
def siteS (ops : Ops V) (E : EqC V) (k : Nat) (x : V) : Nat → Site I (fun j => (CI j).M) V :=
  mkSite (keyT.get k).get (fun i => (sfT.get k).get i V ops E x)
abbrev siteR (ops : Ops V) (E : EqC V) (k : Nat) : Nat → Site I (fun j => (CI j).M) V := siteS ops E k ops.z
end
''')


def res_leaf(k):
    r = M.rules[k]
    g = r['guard']
    resv = ex(k, r.get('res', ('z',)), rho, 'x')
    if g and g[0] not in ('check', 'false'):
        return ('fun V (ops : Ops V) (E : EqC V) (x : V) (ρ : Nat → V) => bif %s then some (%s) else none'
                % (guard_rho(k), resv))
    return 'fun V (ops : Ops V) (E : EqC V) (x : V) (ρ : Nat → V) => some (%s)' % resv


w('/-- Each step\'s result: its explicit guard, read at its position (run-time check rules: `some z`);\n'
  '    a method returns its result expression. -/')
stab('resT', 'Tab ((V : Type) → Ops V → EqC V → V → (Nat → V) → Option V)', [res_leaf(k) for k in range(nS)])
mark('sites')

# ---------------------------------------------------------------- guards and firing
w('/-! ## The hardware: guards, firing, run-time checks -/\n')
UC = [sum(1 << i for i, s in enumerate(r['sites']) if not is_uncond(s) and M.is_rdy_guarded(s['port'])) for r in M.rules]
NEEDUC = any(UC)
MULTIBLK = any(len(v) > 1 for v in M.blk.values())
WXM = 'Wires %s V' % XM
if NEEDUC:
    stab('ucT', 'Tab Nat', [nat(x) for x in UC])
w('''def alwaysRdyB (p : Σ j : I, (CI j).M) : Bool := (CI p.1).ann.alwaysRdy p.2

section
variable {V : Type}
''' + ('''/-- The ready signals of a step's unconditional guarded sites, ANDed into its CAN_FIRE (a conditional
    guarded call's readiness is in its guard `G`, a readiness read's comes from the child's alias). -/
def RD (oc : OC V) (k : Nat) : Bool :=
  (List.range' 0 (kR k)).all (fun i => alwaysRdyB ((keyT.get k).get i) || (ucT.get k).testBit i ||
    (oc ((keyT.get k).get i).1).rdy ((keyT.get k).get i).2)
''' if NEEDUC else '''/-- The ready signals of a rule's guarded calls, ANDed into its CAN_FIRE (calls are unconditional). -/
def RD (oc : OC V) (k : Nat) : Bool :=
  (List.range' 0 (kR k)).all (fun i => alwaysRdyB ((keyT.get k).get i) || (oc ((keyT.get k).get i).1).rdy ((keyT.get k).get i).2)
'''))
for k, r in enumerate(M.rules):
    g = r['guard']
    if g[0] not in ('check', 'false'):
        w('def G_%d (V : Type) (ops : Ops V) (E : EqC V) (oc : OC V) : Bool := %s' % (k, G_def(k)))
w('def bTrue (V : Type) (ops : Ops V) (E : EqC V) (oc : OC V) : Bool := true')
done, order = set(), []


def visit(k):
    if k in done:
        return
    for b in M.blk.get(k, []):
        visit(b)
    done.add(k)
    order.append(k)


for k in range(nS):
    if M.rules[k]['guard'][0] not in ('check', 'false'):
        visit(k)
for k in order:
    if k in M.blk:
        if MULTIBLK:
            w('def B_%d (V : Type) (ops : Ops V) (E : EqC V) (oc : OC V) : Bool := !(orL [%s])'
              % (k, ', '.join('WFd_%d V ops E oc' % b for b in M.blk[k])))
        else:
            w('def B_%d (V : Type) (ops : Ops V) (E : EqC V) (oc : OC V) : Bool := !(WFd_%d V ops E oc)' % (k, M.blk[k][0]))
    bname = ('B_%d' % k) if k in M.blk else 'bTrue'
    w('def WFd_%d (V : Type) (ops : Ops V) (E : EqC V) (oc : OC V) : Bool := (RD oc %d && G_%d V ops E oc) && %s V ops E oc'
      % (k, k, k, bname))
w('end\n')
w('/-- bsc\'s `mutually_exclusive` groups whose checks are suffixes (`SufAsm`), in bsc\'s order. -/')
stab('grpT', 'Tab (List Nat)', ['[' + ', '.join(nat(x) for x in g) + ']' for g in GROUPS] or ['[]'])
w('section\nvariable {V : Type}\n')
if CHK_G:
    w('def bFalse (V : Type) (ops : Ops V) (E : EqC V) (oc : OC V) : Bool := false')
    stab('wfdT', 'Tab ((V : Type) → Ops V → EqC V → OC V → Bool)',
         [('WFd_%d' % k) if M.rules[k]['guard'][0] not in ('check', 'false') else 'bFalse' for k in range(nS)])
for k, r in enumerate(M.rules):
    g = r['guard']
    if g[0] == 'check' and g[1] in CHK_G:
        ch = M.checks[g[1]]
        g_, j_ = CHK_G[g[1]]
        assert all(M.rules[y]['guard'][0] not in ('check', 'false') for y in ch['lst'])
        w('def G_%d (V : Type) (ops : Ops V) (E : EqC V) (oc : OC V) : Bool := WFd_%d V ops E oc && '
          'orL (((grpT.get %d).drop %d).map (fun y => (wfdT.get y) V ops E oc))' % (k, ch['head'], g_, j_ + 1))
    elif g[0] == 'check':
        ch = M.checks[g[1]]
        w('def G_%d (V : Type) (ops : Ops V) (E : EqC V) (oc : OC V) : Bool := WFd_%d V ops E oc && orL [%s]'
          % (k, ch['head'], ', '.join('WFd_%d V ops E oc' % y for y in ch['lst'])))
    elif g[0] == 'false':
        w('def G_%d (V : Type) (ops : Ops V) (E : EqC V) (oc : OC V) : Bool := false' % k)
w('end\n')
stab('gT', 'Tab ((V : Type) → Ops V → EqC V → OC V → Bool)', ['G_%d' % k for k in range(nS)])
w('def bT : Tab ((V : Type) → Ops V → EqC V → OC V → Bool) :=\n  %s'
  % tab([('B_%d' % k) if k in M.blk else 'bTrue' for k in range(nS)]))
nC = len(LITC)
nG = len(GROUPS)
nD = len(M.disj)
stab('chkHead', 'Tab Nat', [nat(M.checks[c]['head']) for c in LITC] or ['0'])
stab('chkLst', 'Tab (List Nat)', ['[' + ', '.join(map(str, M.checks[c]['lst'])) + ']' for c in LITC] or ['[]'])
w('abbrev nC : Nat := %d' % nC)
w('abbrev nG : Nat := %d' % nG)
stab('dAT', 'Tab Nat', [nat(a) for a, b in M.disj] or ['0'])
stab('dBT', 'Tab Nat', [nat(b) for a, b in M.disj] or ['0'])
w('abbrev nD : Nat := %d' % nD)


def fact_stmt(f):
    """Readiness: R over the site values -> the read's ready literal.  Invariance: R over the site
    values equals R over the ports.  Each skipped value is bound as t<j>."""
    k, i = f['k'], f['site']
    s = M.rules[k]['sites'][i]
    Y = f['Y']
    bind = '(oc : OC V)' + ''.join(' (t%d : V)' % j for j in Y)
    lhs = Rsv(k, i, lambda j: 't%d' % j)
    if f['kind'] == 'inv':
        return '∀ %s, WF ops E oc %d = true → %s = %s' % (bind, k, lhs, Rterm(k, i, hw(k)))
    return ('∀ %s, WF ops E oc %d = true → %s = true → ops.toB (%s) = true'
            % (bind, k, lhs, ocread(site_port(k, s['cond'][1]))))


def efact_stmt(f):
    s1, s2 = M.rules[f['r1']]['sites'][f['i1']], M.rules[f['r2']]['sites'][f['i2']]
    hs = ['WF ops E oc %d = true' % f['r1'], 'WF ops E oc %d = true' % f['r2']]
    for (r, s) in ((f['r1'], s1), (f['r2'], s2)):
        if not is_uncond(s):
            hs.append('%s = true' % cond_hw(r, s))
    return '∀ oc : OC V, ' + ' → '.join(hs + ['False'])


hy_fields = ['  disj : ∀ (oc : OC V) (d : Nat), Nat.blt d nD = true → (CF ops E oc (dAT.get d) && CF ops E oc (dBT.get d)) = false']
if HASF2:
    hy_fields.append('  tob : ∀ b, ops.toB (ops.ofBool b) = b')
un_fields = []
for n_, f in enumerate(FACTS):
    what = 'readiness' if f['kind'] == 'rdy' else 'invariance of R'
    (hy_fields if f['ok'] else un_fields).append(
        '  /-- %s, %s / %s: this formula %s by the site tool (site-value form%s) -/\n  %s_%d : %s'
        % (what, f['step'], f['label'], 'confirmed' if f['ok'] else 'not confirmed',
           (', %d later-site literals dropped' % f['late']) if f['late'] else '', f['kind'], n_, fact_stmt(f)))
for n_, f in enumerate(EFACTS):
    (hy_fields if f['ok'] else un_fields).append(
        '  /-- escape %s → %s: %s then %s (%s) -/\n  esc_%d : %s'
        % (M.rules[f['r1']]['name'], M.rules[f['r2']]['name'], M.rules[f['r1']]['sites'][f['i1']]['label'],
           M.rules[f['r2']]['sites'][f['i2']]['label'],
           {'solver': 'this fact confirmed by the solver over these conditions',
            'bsc': 'bsc: %s; these conditions imply bsc\'s' % f['bsc'],
            None: 'not confirmed'}[f['how']], n_, efact_stmt(f)))
w('''
section
variable {V : Type}
/-- CAN_FIRE: the guarded calls' ready signals and the step's guard. -/
def CF (ops : Ops V) (E : EqC V) (oc : OC V) (k : Nat) : Bool := RD oc k && (gT.get k) V ops E oc
/-- WILL_FIRE: CAN_FIRE and not blocked by a more urgent rule (a method's is its CAN_FIRE). -/
def WF (ops : Ops V) (E : EqC V) (oc : OC V) (k : Nat) : Bool := CF ops E oc k && (bT.get k) V ops E oc
def fire (ops : Ops V) (E : EqC V) : OC V → %s → Fin nR → Bool := fun oc _ r => WF ops E oc r.val
''' % WXM + ('''def stepOn (ops : Ops V) (E : EqC V) (oc : OC V) : St → Bool
  | .inl e => e.elim
  | .inr r => WF ops E oc r.val
''' if not MET else '''def stepOn (ops : Ops V) (E : EqC V) (oc : OC V) (w : Wires (Fin nM) V) : St → Bool
  | .inl m => calledM w m
  | .inr r => WF ops E oc r.val
''') + '''/-- The module's assertions: bsc's run-time checks of `mutually_exclusive` (never `WF h && (WF y || …)`). -/
def asmP (ops : Ops V) (E : EqC V) (oc : OC V) : Prop :=
  (∀ g, Nat.blt g nG = true → SufAsm (WF ops E oc) (grpT.get g)) ∧
  ∀ c, Nat.blt c nC = true → (WF ops E oc (chkHead.get c) && orL ((chkLst.get c).map (WF ops E oc))) = false
theorem cf_of_wf {ops : Ops V} {E : EqC V} {oc : OC V} {k : Nat} (h : WF ops E oc k = true) : CF ops E oc k = true :=
  ((Bool.and_eq_true _ _).mp h).1
theorem rd_of_cf {ops : Ops V} {E : EqC V} {oc : OC V} {k : Nat} (h : CF ops E oc k = true) : RD oc k = true :=
  ((Bool.and_eq_true _ _).mp h).1
theorem g_of_cf {ops : Ops V} {E : EqC V} {oc : OC V} {k : Nat} (h : CF ops E oc k = true) : (gT.get k) V ops E oc = true :=
  ((Bool.and_eq_true _ _).mp h).2

end
''')


def excl(x, y, hA='hA', hy='hy', h1='h1', h2='h2'):
    """Proof of False from h1 : WF x = true, h2 : WF y = true."""
    s = M.excl_src(x, y)
    if s[0] == 'blk':
        a, b = (h1, h2) if not s[1] else (h2, h1)
        if MULTIBLK:
            return 'blk_excl_or %s (p := %d) rfl %s' % (a, s[2], b)
        return 'blk_excl %s %s' % (a, b)
    if s[0] == 'd':
        a, b = (h1, h2) if not s[2] else (h2, h1)
        return 'dX ops E %s oc %d rfl %s %s' % (hy, s[1], a, b)
    a, b = (h1, h2) if not s[3] else (h2, h1)
    if s[1] in CHK_G:
        g_, j_ = CHK_G[s[1]]
        return 'meG ops E oc %s %d %d %d rfl rfl rfl rfl %s %s' % (hA, g_, j_, j_ + 1 + s[2], a, b)
    return 'meX ops E oc %s %d %d rfl rfl %s %s' % (hA, LITI[s[1]], s[2], a, b)


mark('guards')

# ---------------------------------------------------------------- arms
w('/-! ## The hardware: per-port definitions, per-kind arm tables -/\n')
w('section\nvariable {V : Type}\n')
muxes = []
chk_heads = {}
for c_, ch_ in enumerate(M.checks):
    if c_ not in CHK_G:
        chk_heads.setdefault(ch_['head'], []).append(c_)
MGRP = {}          # mux -> (group, its members of the group in group order), placed last


def order_mux(us):
    mem_ = set(k for k, _ in us)
    best_g = max(range(len(GROUPS)), key=lambda g: len(mem_ & set(GROUPS[g])), default=None)
    gs_ = [x for x in GROUPS[best_g] if x in mem_] if best_g is not None else []
    if len(gs_) < 2:
        gs_ = []
    rem = mem_ - set(gs_)
    order_, plan = [], {}
    if gs_:
        MGRP[len(muxes)] = (best_g, gs_)
    while rem:
        best = None
        for x in sorted(rem):
            others = rem - {x}
            cb, cov = None, set()
            for c in chk_heads.get(x, []):
                cs = set(M.checks[c]['lst']) & others
                if len(cs) > len(cov):
                    cb, cov = c, cs
            key = (len(others - cov), x)
            if best is None or key < best[0]:
                best = (key, x, cb, others - cov)
        _, x, cb, unc = best
        order_.append(x)
        plan[x] = (cb, unc | set(gs_))
        rem.discard(x)
    for x in gs_:
        order_.append(x)
        plan[x] = ('grp', None)
    return order_, plan


def entry(k, st):
    """A user's enable entry: it fires (a method: is called), and its call condition."""
    on = 'calledM w (mM %d)' % (k - nR) if k >= nR else 'WF ops E oc %d' % k
    if is_uncond(st):
        return on
    return '(%s && %s)' % (on, cond_hw(k, st))


def argsite(k, si):
    return M.rules[k]['sites'][si]


portinfo = {}
SELMUX = {}
for kd in M.kinds:
    if kd == 'tasks':
        continue
    for i in range(len(M.byk[kd])):
        for m in K[kd].ports:
            p = (kd, i, m)
            if not (K[kd].has_en(m) or (K[kd].hasarg(m) and not K[kd].fx(m))):
                continue
            us = U.get(p, [])
            nm = '%s_%d_%s' % (kd, i, m)
            if not us:
                portinfo[p] = dict(users=[], argk='none')
                continue
            if any(k >= nR for k, _ in us):
                assert all(k >= nR for k, _ in us), 'a port with method and rule users: %s' % (p,)
            if K[kd].has_en(m):
                w('def en_%s (ops : Ops V) (E : EqC V) (oc : OC V) (w : %s) : Bool := orL [%s]'
                  % (nm, WXM, ', '.join(entry(k, argsite(k, si)) for k, si in us)))
            args = [argsite(k, si)['arg'] for k, si in us]
            hwargs = [arg_hw(k, argsite(k, si)) for k, si in us]
            if not K[kd].hasarg(m):
                argk = 'none'
            elif len(set(hwargs)) == 1:
                argk = 'const' if args[0][0] in ('z', 'k') else ('single' if len(us) == 1 else 'mux')
            else:
                argk = 'mux'
            if args and args[0][0] == 'x' and argk != 'none':
                argk = 'single' if len(us) == 1 else 'mux'
            if argk == 'mux':
                q = len(muxes)
                order_, plan = order_mux(us)
                assert len(order_) == len(us), 'a step uses a mux port twice'
                mus = sorted(us, key=lambda e: order_.index(e[0]))
                margs = [argsite(k, si) for k, si in mus]
                muxes.append((p, mus, plan))
                SELMUX[q] = any(not is_uncond(st) for st in margs) or any(
                    M.excl_src(a_, b_) is None for (a_, _) in mus for (b_, _) in mus if a_ < b_)
                stab('mT_%d' % q, 'Tab ((V : Type) → Ops V → EqC V → OC V → Bool × V)',
                     ['fun V (ops : Ops V) (E : EqC V) (oc : OC V) => ((%s, %s) : Bool × V)'
                      % (entry(k, st) if SELMUX[q] else 'WF ops E oc %d' % k, arg_hw(k, st))
                      for (k, _), st in zip(mus, margs)])
                w('def mlist_%d (V : Type) (ops : Ops V) (E : EqC V) (oc : OC V) : List (Bool × V) :=\n'
                  '  (List.range\' 0 %d).map (fun t => (mT_%d.get t) V ops E oc)' % (q, len(mus), q))
                w('def arg_%s (ops : Ops V) (E : EqC V) (oc : OC V) (w : %s) : V := pmux (mlist_%d V ops E oc) ops.z'
                  % (nm, WXM, q))
                portinfo[p] = dict(users=us, argk='mux', mux=q, args=args, mus=mus, margs=margs,
                                   mpos={k: t for t, (k, _) in enumerate(mus)})
            else:
                if argk != 'none':
                    w('def arg_%s (ops : Ops V) (E : EqC V) (oc : OC V) (w : %s) : V := %s'
                      % (nm, WXM, arg_hw(us[0][0], argsite(*us[0]))))
                portinfo[p] = dict(users=us, argk=argk, args=args)
w('end\n')

tuser = {}
for k, r in enumerate(M.rules):
    for i, st in enumerate(r['sites']):
        if st['kind'] == 'task':
            tuser[st['port'][2]] = (k, i, st)


def tcond_hw_t(k, st):
    if st['hw'][0] == 'chk':
        return '(gT.get %d) V ops E oc' % k
    return cond_hw(k, st)


if nT:
    w('/-- Task statement `t`: its enable (the statement\'s condition, `WILL_FIRE` included) and arguments. -/')
    stab('tenT', 'Tab ((V : Type) → Ops V → EqC V → OC V → Bool)', [
        'fun V (ops : Ops V) (E : EqC V) (oc : OC V) => WF ops E oc %d && %s' % (tuser[t][0], tcond_hw_t(tuser[t][0], tuser[t][2]))
        for t in range(nT)])
    stab('targT', 'Tab ((V : Type) → Ops V → EqC V → OC V → V)', [
        'fun V (ops : Ops V) (E : EqC V) (oc : OC V) => %s' % arg_hw(tuser[t][0], tuser[t][2])
        for t in range(nT)])


def en_term(p):
    kd, i, m = p
    return ('en_%s_%d_%s ops E oc w' % (kd, i, m)) if portinfo.get(p, {}).get('users') else 'false'


def arg_term(p):
    kd, i, m = p
    pi = portinfo.get(p, {})
    return ('arg_%s_%d_%s ops E oc w' % (kd, i, m)) if pi.get('users') and pi.get('argk') != 'none' else 'ops.z'


def arm_body(kd, i):
    P_ = lambda m: (kd, i, m)
    if kd == 'reg':
        return 'regW (%s) (%s) ops.z' % (en_term(P_('write')), arg_term(P_('write')))
    if kd == 'cfg':
        return 'cfgW (%s) (%s) ops.z' % (en_term(P_('write')), arg_term(P_('write')))
    if kd == 'fifo':
        return 'fifoW (%s) (%s) (%s) (%s) ops.z' % (en_term(P_('enq')), en_term(P_('deq')), en_term(P_('clear')),
                                                    arg_term(P_('enq')))
    if kd in ('f2', 'sf'):
        return 'f2W (%s) (%s) (%s) (%s) ops.z' % (en_term(P_('enq')), en_term(P_('deq')), en_term(P_('clear')),
                                                  arg_term(P_('enq')))
    if kd == 'rf':
        return 'rfW (%s) (%s) %s' % (en_term(P_('upd')), arg_term(P_('upd')),
                                     ' '.join('(%s)' % arg_term(P_('s%d' % c)) for c in range(model.RF_COPIES)))
    if kd in ('rwS', 'rwC'):
        return 'wireW (%s) (%s) ops.z' % (en_term(P_('wset')), arg_term(P_('wset')))
    if kd in ('rw0S', 'rw0C'):
        return 'wire0W (%s) ops.z' % en_term(P_('wset'))
    if kd == 'tasks':
        return '⟨fun t => (tenT.get t.val) V ops E oc, fun t => (targT.get t.val) V ops E oc⟩'
    raise ValueError(kd)


for kd in M.kinds:
    for i in range(len(M.byk[kd])):
        w('def arm_%s_%d (V : Type) (ops : Ops V) (E : EqC V) (oc : OC V) (w : %s) : Wires (KI .%s).M V :=\n  %s'
          % (kd, i, WXM, kd, arm_body(kd, i)))
for kd in M.kinds:
    w('def arms_%s : Tab ((V : Type) → Ops V → EqC V → OC V → %s → Wires (KI .%s).M V) :=\n  %s'
      % (kd, WXM, kd, tab(['arm_%s_%d' % (kd, i) for i in range(len(M.byk[kd]))])))
if MET:
    stab('retT', 'Tab ((V : Type) → Ops V → EqC V → OC V → V)',
         ['fun V (ops : Ops V) (E : EqC V) (oc : OC V) => %s' % ex(nR + m, M.rules[nR + m].get('res', ('z',)), hw(nR + m), 'ops.z')
          for m in range(nM)])
outs = ('⟨fun e => e.elim, fun e => e.elim⟩' if not MET else
        '⟨fun m => CF ops E oc (nR + m.val), fun m => (retT.get m.val) V ops E oc⟩')
w('''section
variable {V : Type}
def arms : (κ : Kind) → Tab ((V : Type) → Ops V → EqC V → OC V → %s → Wires (KI κ).M V)
''' % WXM + ''.join('  | .%s => arms_%s\n' % (k, k) for k in M.kinds) + '''
/-- The parent's combinational logic: children through the arm tables; a method's RDY is its
    CAN_FIRE, its result the result expression. -/
def comb (ops : Ops V) (E : EqC V) (oc : OC V) (w : %s) : ((j : I) → Wires (CI j).M V) × Outs %s V :=
  (fun j => (arms j.1).get j.2.val V ops E oc w, %s)
/-- The parallel muxes' selectors are one-hot. -/
def onehotP (ops : Ops V) (E : EqC V) (oc : OC V) : Prop :=
  ''' % (WXM, XM, outs) + ' ∧ '.join(['amoChain false ((mlist_%d V ops E oc).map Prod.fst)' % q for q in range(len(muxes))] + ['True']) + '''

/-- **The family**: each step's program is `runSites v … v` of its own skipped values `v`. -/
abbrev F (ops : Ops V) (E : EqC V) : ParentU V where
  I := I
  decI := inferInstance
  kids := allKids [''' + ', '.join('.' + k for k in M.kinds) + '''] nk
  kids_nodup := allKids_nodup nk _ (by decide)
  kids_all := mem_allKids _ nk (fun κ => by cases κ <;> simp)
  CI := CI
  X := X
  PR := Fin nR
''' + ('''  body := fun m _ _ => m.elim
''' if not MET else '''  body := fun m x v => runSites v ((resT.get (nR + m.val)) V ops E x) (sitesF (siteS ops E (nR + m.val) x) (kR (nR + m.val))) 0 v
''') + '''  rbody := fun r v => runSites v ((resT.get r.val) V ops E ops.z) (sitesF (siteR ops E r.val) (kR r.val)) 0 v
  comb := comb ops E
  asm := fun oc _ => asmP ops E oc
  onehot := fun oc _ => onehotP ops E oc
  toB := ops.toB

/-- The parent at skipped values `u` (step `s` sees only `u s`). -/
abbrev P (ops : Ops V) (E : EqC V) (u : St → Nat → V) : Parent V := (F ops E).at u
''' + ('''/-- Each step's program takes its skipped values only through `runSites`; its sites and result
    do not receive them (design §18 trust point 7, checked by `rfl`). -/
theorem F_sites (ops : Ops V) (E : EqC V) : F ops E = withRuleSites (F ops E)
    (fun r => sitesF (siteR ops E r.val) (kR r.val)) (fun r => (resT.get r.val) V ops E ops.z) := rfl
''' if not MET else '''/-- Each step's program takes its skipped values only through `runSites`; its sites and result
    receive the method's argument, never the skipped values (design §18 trust point 7, by `rfl`). -/
theorem F_sites (ops : Ops V) (E : EqC V) : F ops E = withSitesX (F ops E)
    (fun m x => sitesF (siteS ops E (nR + m.val) x) (kR (nR + m.val))) (fun m x => (resT.get (nR + m.val)) V ops E x)
    (fun r => sitesF (siteR ops E r.val) (kR r.val)) (fun r => (resT.get r.val) V ops E ops.z) := rfl
''') + '''end
''')
mark('arms')

# caller exclusivity of mux users that no recorded exclusion covers (the solver over schedlean's
# conditions, as sites/tools/shadow.py); a pair that may both call is action shadowing (unconfirmed)
CFACTS = []
if CS is not None:
    sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), '..', '..', 'sites', 'tools'))
    from satc import Enc  # noqa: E402
    SDX, APX = CSRES['_ctx']['DX'], CSRES['_ctx']['ap']     # every port its own atom
    WORK = os.path.join(os.path.dirname(os.path.abspath(out)), 'sitework_' + NAME)

    def wfx(n):
        x_ = 'WILL_FIRE_' + n
        return ('id', x_) if x_ in SDX.defs else ('id', 'CAN_FIRE_' + n)

    def ccx(n, inst, meth):
        cs_ = [a[3] for a in APX.rules.get(n, (None, []))[1] if a and a[0] == 'call' and a[1] == inst and a[2] == meth]
        return [c if c is not None else ('lit', "1'd1") for c in cs_] or [('lit', "1'd1")]
for q, (p_, mus_, plan_) in enumerate(muxes):
    if not SELMUX.get(q):
        continue
    ks_ = [k for k, _ in mus_]
    for a_ in range(len(ks_)):
        for b_ in range(a_ + 1, len(ks_)):
            x_, y_ = ks_[a_], ks_[b_]
            if M.excl_src(x_, y_) is not None:
                continue
            how_, btext_ = None, None
            if CS is not None:
                inst_ = M.byk[p_[0]][p_[1]]
                meth_ = 'sub' if p_[0] == 'rf' and p_[2] != 'upd' else p_[2]
                E_ = Enc(SDX)
                roots_ = [E_.b(wfx(M.rules[x_]['name'])), E_.b(wfx(M.rules[y_]['name'])),
                          E_.OR([E_.b(c) for c in ccx(M.rules[x_]['name'], inst_, meth_)]),
                          E_.OR([E_.b(c) for c in ccx(M.rules[y_]['name'], inst_, meth_)])]
                if E_.sat(roots_, WORK) is False:
                    how_ = 'solver'
                else:
                    sx_, sy_ = M.rules[x_]['sites'][dict(mus_)[x_]], M.rules[y_]['sites'][dict(mus_)[y_]]
                    btext_ = bsc_fact(x_, sx_, lambda E, n=M.rules[x_]['name']: E.OR([E.b(c) for c in ccx(n, inst_, meth_)]),
                                      y_, sy_, lambda E, n=M.rules[y_]['name']: E.OR([E.b(c) for c in ccx(n, inst_, meth_)]))
                    how_ = 'bsc' if btext_ else None
            CFACTS.append(dict(q=q, x=x_, y=y_, sx=dict(mus_)[x_], sy=dict(mus_)[y_], ok=how_ is not None,
                               how=how_, bsc=btext_))
CFIND = {(f['q'], f['x'], f['y']): n_ for n_, f in enumerate(CFACTS)}
if CS is not None and os.environ.get('E2E_BSC_STATS'):
    # measurement only: bsc's coverage of every fact, exact keys with the condition check, against
    # the round-7 key (later step, p, q), which dropped the first step
    _old = set((f_['b'], f_['p'], f_['q']) for f_ in sitecond_facts(dump) if f_['ok'])

    def _oldkey(k1, s1, k2, s2):
        if s1['kind'] == 'task' or s2['kind'] == 'task':
            return False
        return (M.rules[k2]['name'], s1['label'], s2['label']) in _old
    _ne = sum(1 for f_ in EFACTS if bsc_fact(f_['r1'], M.rules[f_['r1']]['sites'][f_['i1']],
                                             lambda E, k=f_['r1'], s=M.rules[f_['r1']]['sites'][f_['i1']]: site_lit(E, k, s),
                                             f_['r2'], M.rules[f_['r2']]['sites'][f_['i2']],
                                             lambda E, k=f_['r2'], s=M.rules[f_['r2']]['sites'][f_['i2']]: site_lit(E, k, s)))
    _oe = sum(1 for f_ in EFACTS if _oldkey(f_['r1'], M.rules[f_['r1']]['sites'][f_['i1']], f_['r2'],
                                            M.rules[f_['r2']]['sites'][f_['i2']]))
    _nk = sum(1 for f_ in EFACTS if f_['how'] != 'task' and M.rules[f_['r1']]['sites'][f_['i1']]['kind'] != 'task'
              and M.rules[f_['r2']]['sites'][f_['i2']]['kind'] != 'task'
              and BFE.covers(M.rules[f_['r1']]['name'], M.rules[f_['r1']]['sites'][f_['i1']]['label'],
                             M.rules[f_['r2']]['name'], M.rules[f_['r2']]['sites'][f_['i2']]['label']))
    print('bsc coverage of escape facts: round-7 key %d, exact key %d, exact key and conditions implied %d (of %d); '
          'solver %d' % (_oe, _nk, _ne, len(EFACTS), sum(1 for f_ in EFACTS if f_['how'] == 'solver')))


def cfact_stmt(f):
    hs = []
    for (k, si) in ((f['x'], f['sx']), (f['y'], f['sy'])):
        hs.append('WF ops E oc %d = true' % k)
        st = M.rules[k]['sites'][si]
        if not is_uncond(st):
            hs.append('%s = true' % cond_hw(k, st))
    return '∀ oc : OC V, ' + ' → '.join(hs + ['False'])


for n_, f in enumerate(CFACTS):
    (hy_fields if f['ok'] else un_fields).append(
        '  /-- callers of one port never both call: %s, %s (%s) -/\n  cx_%d : %s'
        % (M.rules[f['x']]['name'], M.rules[f['y']]['name'],
           {'solver': 'solver: exclusive', 'bsc': 'bsc: %s' % f['bsc'], None: 'solver: may both call'}[f['how']],
           n_, cfact_stmt(f)))
HAS_UNCONF = HAS_UNCONF or any(not f['ok'] for f in CFACTS)
HR_ARG = ' (hr : Unconfirmed ops E)' if HAS_UNCONF else ''
HR_USE = ' hr' if HAS_UNCONF else ''
w("""section
variable {V : Type}
/-- bsc's solver verdicts, hypotheses as today: disjointness (CAN_FIRE of each pair); the
    use-level readiness facts over R_seq, each escape's dynamic exclusion and the callers'
    exclusivity at a mux, as confirmed by the solver over schedlean's conditions. -/
structure Hyps (ops : Ops V) (E : EqC V) : Prop where
""" + '\n'.join(hy_fields) + '\n' + (("""/-- Facts the site tool refutes (or bsc did not confirm): a module that needs one is off path A
    (fail safe); this file states them so that its cost and structure can be measured. -/
structure Unconfirmed (ops : Ops V) (E : EqC V) : Prop where
""" + '\n'.join(un_fields) + '\n') if HAS_UNCONF else '') + """
theorem meX (ops : Ops V) (E : EqC V) (oc : OC V) (hA : asmP ops E oc) (c p : Nat) {y : Nat}
    (hc : Nat.blt c nC = true) (hp : (chkLst.get c)[p]? = some y)
    (h1 : WF ops E oc (chkHead.get c) = true) (h2 : WF ops E oc y = true) : False :=
  me_excl (hA.2 c hc) h1 (p := p) (by rw [List.getElem?_map, hp]; rfl) h2
theorem meG (ops : Ops V) (E : EqC V) (oc : OC V) (hA : asmP ops E oc) (g a b : Nat) {x y : Nat}
    (hg : Nat.blt g nG = true) (hx : (grpT.get g)[a]? = some x) (hy : (grpT.get g)[b]? = some y)
    (hab : Nat.blt a b = true) (h1 : WF ops E oc x = true) (h2 : WF ops E oc y = true) : False :=
  sufAsm_excl (hA.1 g hg) hx hy hab h1 h2
theorem dX (ops : Ops V) (E : EqC V) (hy : Hyps ops E) (oc : OC V) (d : Nat) (hd : Nat.blt d nD = true)
    (h1 : WF ops E oc (dAT.get d) = true) (h2 : WF ops E oc (dBT.get d) = true) : False :=
  disj_excl (hy.disj oc d hd) (cf_of_wf h1) (cf_of_wf h2)
end
""")

# ---------------------------------------------------------------- site proofs
w('/-! ## Site facts: reads before, dummy arguments -/\n')
w('''def fxF (κ c : Nat) : Bool := match κ, c with
''' + ''.join(
    ''.join('  | %d, %d => true\n' % (M.kn[kd], K[kd].code(m)) for m in K[kd].ports if K[kd].fx(m))
    for kd in M.kinds) + '''  | _, _ => false
def kindN : Kind → Nat
''' + ''.join('  | .%s => %d\n' % (k, M.kn[k]) for k in M.kinds) + '''def codeK : (κ : Kind) → (KI κ).M → Nat
''')
for kd in M.kinds:
    if kd == 'reg':
        w('  | .reg => fun m => match m with | .rd _ => 0 | .wr _ => 1')
    elif kd == 'tasks':
        w('  | .tasks => fun _ => 0')
    else:
        w('  | .%s => fun m => match m with %s' % (kd, ' '.join('| .%s => %d' % (p, K[kd].code(p)) for p in K[kd].ports)))
w('''def npK : Kind → Nat
''' + ''.join('  | .%s => %d\n' % (k, 1 if k == 'tasks' else len(K[k].ports)) for k in M.kinds) + '''def offK : Kind → Nat
''' + ''.join('  | .%s => %d\n' % (k, M.off[k]) for k in M.kinds) + '''def baseOf (j : I) : Nat := offK j.1 + j.2.val * npK j.1
def fxP (p : Σ j : I, (CI j).M) : Bool := fxF (kindN p.1.1) (codeK p.1.1 p.2)

section
variable {V : Type}
/-- Site `i` of step `k`: it reads only earlier sites, and a flagged port's call carries `z`. -/
def SQ (ops : Ops V) (E : EqC V) (k i : Nat) (F : (V : Type) → Ops V → EqC V → V → SF V) : Prop :=
  ∀ x : V, SFReads i (F V ops E x) ∧ (fxP ((keyT.get k).get i) = true → ∀ ρ, (F V ops E x).2 ρ = ops.z)
def SQT (ops : Ops V) (E : EqC V) (k : Nat) (t : Tab ((V : Type) → Ops V → EqC V → V → SF V)) : Prop :=
  Tab.AllR (SQ ops E k) t 0 (kT.get k)
''')


def rcong(e):
    """e at ρ equals e at ρ', from h : ∀ i' < i, ρ i' = ρ' i'."""
    t = e[0]
    if t in ('z', 'k', 'x', 'true', 'gk'):
        return 'rfl'
    if t == 'lit':
        return 'congrArg ops.toB (h %d (by decide))' % e[1]
    if t in ('f', 'g'):
        return 'congrArg (ops.%s %d) (%s)' % (t, e[1], tupcong(['h %d (by decide)' % j for j in e[2]]))
    raise ValueError(e)


def rcong_k(k, e):
    if e[0] == 'split':
        rs = andc(['congrArg (ops.g %d) (%s)' % (M.rules[k]['sites'][i]['R'][1],
                                                 tupcong(['h %d (by decide)' % j for j in M.rules[k]['sites'][i]['R'][2]]))
                   for i in e[1]])
        return 'cond_congr (%s) (%s) (%s)' % (rs, rcong_k(k, e[2]), rcong_k(k, e[3]))
    return rcong(e)


def sq_leaf(k, i, st):
    nofx = 'fun h => absurd h (by decide)'
    if st['kind'] == 'task':
        return "fun x => ⟨fun ρ ρ' h => ⟨%s, %s⟩, %s⟩" % (rcong_k(k, st['cond']), tupcong([rcong_k(k, a) for a in st['args']]), nofx)
    c, a = st['cond'], st['arg']
    if c[0] == 'true' and a[0] == 'z':
        return 'fun x => ⟨sfr_const _ _, fun _ _ => rfl⟩'
    fxp = 'fun _ _ => rfl' if a[0] == 'z' else nofx
    return "fun x => ⟨fun ρ ρ' h => ⟨%s, %s⟩, %s⟩" % (rcong_k(k, c), rcong_k(k, a), fxp)


for k, r in enumerate(M.rules):
    if r['sites']:
        body = stabproof('sf_%d' % k, 'sq_%d' % k, '(ops : Ops V) (E : EqC V)', 'ops E',
                         lambda nm, lo, hi, k=k: 'Tab.AllR (SQ ops E %d) %s %d %d' % (k, nm, lo, hi),
                         [sq_leaf(k, i, st) for i, st in enumerate(r['sites'])])
    else:
        body = 'fun _ _ h => absurd h (Nat.not_lt_zero _)'
    w('theorem sq_%d (ops : Ops V) (E : EqC V) : SQT ops E %d sf_%d :=\n  %s' % (k, k, k, body))
w('theorem sqT (ops : Ops V) (E : EqC V) : Tab.AllR (SQT ops E) sfT 0 nS :=\n  %s'
  % stabproof('sfT', 'sqT', '(ops : Ops V) (E : EqC V)', 'ops E',
              lambda nm, lo, hi: 'Tab.AllR (SQT ops E) %s %d %d' % (nm, lo, hi), ['sq_%d ops E' % k for k in range(nS)]))
w('''theorem sqR (ops : Ops V) (E : EqC V) (k : Nat) (hk : k < nS) : Tab.AllR (SQ ops E k) (sfT.get k) 0 (kR k) :=
  Tab.get_allR _ 0 nS (sqT ops E) k (Nat.zero_le _) hk
theorem hOK (ops : Ops V) (E : EqC V) (k : Nat) (hk : k < nS) (x : V) : ∀ i, i < kR k → ReadsBefore i (siteS ops E k x i) :=
  fun i hi => ((Tab.get_allR _ 0 (kR k) (sqR ops E k hk) i (Nat.zero_le _) hi) x).1

/-- The environment of step `k`'s sites at argument `x`, the hardware's values. -/
abbrev envS (ops : Ops V) (E : EqC V) (v : Nat → V) (oc : OC V) (k : Nat) (x : V) : Nat → V :=
  envHw v oc (sitesF (siteS ops E k x) (kR k)) 0 v
abbrev envR (ops : Ops V) (E : EqC V) (v : Nat → V) (oc : OC V) (k : Nat) : Nat → V := envS ops E v oc k ops.z

theorem trR_eq (ops : Ops V) (E : EqC V) (u : St → Nat → V) (oc : OC V) (r : Fin nR) :
    trR (P ops E u) oc r = (callsOf (envR ops E (u (.inr r)) oc r.val) (sitesF (siteR ops E r.val) (kR r.val)),
      (resT.get r.val) V ops E ops.z (envR ops E (u (.inr r)) oc r.val)) :=
  trace_fn (u (.inr r)) oc _ (siteR ops E r.val) (kR r.val) (hOK ops E r.val (rlt r) ops.z)
theorem trS_eq (ops : Ops V) (E : EqC V) (u : St → Nat → V) (oc : OC V) (w : %s) (r : Fin nR) :
    trS (P ops E u) oc w (.inr r) = callsOf (envR ops E (u (.inr r)) oc r.val) (sitesF (siteR ops E r.val) (kR r.val)) :=
  congrArg Prod.fst (trR_eq ops E u oc r)
theorem envS_at (ops : Ops V) (E : EqC V) (v : Nat → V) (oc : OC V) (k : Nat) (hk : k < nS) (x : V) (i : Nat) (hi : i < kR k) :
    envS ops E v oc k x i = if (siteS ops E k x i).cond (envS ops E v oc k x) then (oc (siteS ops E k x i).j).ret (siteS ops E k x i).m else v i :=
  envHw_fn v oc (siteS ops E k x) (kR k) (hOK ops E k hk x) i hi
theorem envR_at (ops : Ops V) (E : EqC V) (v : Nat → V) (oc : OC V) (k : Nat) (hk : k < nS) (i : Nat) (hi : i < kR k) :
    envR ops E v oc k i = if (siteR ops E k i).cond (envR ops E v oc k) then (oc (siteR ops E k i).j).ret (siteR ops E k i).m else v i :=
  envS_at ops E v oc k hk ops.z i hi
''' % WXM + ('''theorem trM_eq (ops : Ops V) (E : EqC V) (u : St → Nat → V) (oc : OC V) (w : Wires (Fin nM) V) (m : Fin nM) :
    trM (P ops E u) oc w m = (callsOf (envS ops E (u (.inl m)) oc (nR + m.val) (w.arg m)) (sitesF (siteS ops E (nR + m.val) (w.arg m)) (kR (nR + m.val))),
      (resT.get (nR + m.val)) V ops E (w.arg m) (envS ops E (u (.inl m)) oc (nR + m.val) (w.arg m))) :=
  trace_fn (u (.inl m)) oc _ (siteS ops E (nR + m.val) (w.arg m)) (kR (nR + m.val)) (hOK ops E (nR + m.val) (mlt m) (w.arg m))
theorem trSM_eq (ops : Ops V) (E : EqC V) (u : St → Nat → V) (oc : OC V) (w : Wires (Fin nM) V) (m : Fin nM) :
    trS (P ops E u) oc w (.inl m) = callsOf (envS ops E (u (.inl m)) oc (nR + m.val) (w.arg m)) (sitesF (siteS ops E (nR + m.val) (w.arg m)) (kR (nR + m.val))) :=
  congrArg Prod.fst (trM_eq ops E u oc w m)
''' if MET else '') + 'end\n')
mark('siteproofs')

# value lemmas
w('section\nvariable {V : Type}\n')
for (k, i) in sorted(evneed):
    w('theorem ev_%d_%d (ops : Ops V) (E : EqC V) (v : Nat → V) (oc : OC V) (x : V) : envS ops E v oc %d x %d = %s :=\n'
      '  (envS_at ops E v oc %d (by decide) x %d (by decide)).trans rfl' % (k, i, k, i, ocread(site_port(k, i)), k, i))
for (k, i) in sorted(evcneed):
    s = M.rules[k]['sites'][i]
    c = Ctx('v', 'x')
    cv = val(k, s['cond'], c)
    chw = cond_hw(k, s)
    w('theorem evc_%d_%d (ops : Ops V) (E : EqC V) (v : Nat → V) (oc : OC V) (x : V) (h : %s = true) : envS ops E v oc %d x %d = %s :=\n'
      '  (envS_at ops E v oc %d (by decide) x %d (by decide)).trans (if_pos ((%s).trans h))'
      % (k, i, chw, k, i, ocread(site_port(k, i)), k, i, cv))
for (k, i) in sorted(evsneed):
    s = M.rules[k]['sites'][i]
    cv = val(k, s['cond'], Ctx('v', 'x'))
    w('theorem evs_%d_%d (ops : Ops V) (E : EqC V) (v : Nat → V) (oc : OC V) (x : V) : envS ops E v oc %d x %d = %s :=\n'
      '  (envS_at ops E v oc %d (by decide) x %d (by decide)).trans (ite_bcongr (%s) _ _)'
      % (k, i, k, i, sv_hw(k, i, 'v %d' % i), k, i, cv))
w('end\n')

HYP = '(hy : Hyps ops E)' + HR_ARG
HYA = 'hy' + HR_USE
w('''section
variable {V : Type}
theorem res_some (z : V) {b : Bool} (h : b = true) : (bif b then some z else none).isSome = true := by
  rw [h]; rfl
theorem res_eq {b : Bool} {r r' : V} (hb : b = true) (hr : r = r') : (bif b then some r else none) = some r' := by
  subst hb hr; rfl
/-- A step's run-time precondition: a rule fires, a method can fire. -/
def PRE (ops : Ops V) (E : EqC V) (oc : OC V) (k : Nat) : Bool := bif Nat.blt k nR then WF ops E oc k else CF ops E oc k
theorem pre_r {ops : Ops V} {E : EqC V} {oc : OC V} {r : Fin nR} (h : WF ops E oc r.val = true) : PRE ops E oc r.val = true := by
  unfold PRE; rw [show Nat.blt r.val nR = true from Nat.blt_eq.mpr r.isLt]; exact h
''' + ('''theorem pre_m {ops : Ops V} {E : EqC V} {oc : OC V} {m : Fin nM} (h : CF ops E oc (nR + m.val) = true) :
    PRE ops E oc (nR + m.val) = true := by
  unfold PRE; rw [show Nat.blt (nR + m.val) nR = false from Bool.eq_false_iff.mpr (fun h' => absurd (Nat.blt_eq.mp h') (Nat.not_lt.mpr (Nat.le_add_right nR m.val)))]; exact h
''' if MET else '') + '''/-- Rule `k`'s result is defined when it fires (its guard read at its position). -/
def RQ (ops : Ops V) (E : EqC V) %s (k : Nat) (g : (V : Type) → Ops V → EqC V → OC V → Bool) : Prop :=
  ∀ (v : Nat → V) (oc : OC V), PRE ops E oc k = true → ((resT.get k) V ops E ops.z (envR ops E v oc k)).isSome = true
''' % HYP)


def hg_term(k, hf):
    """guard_hw = true from hf : PRE (rule) — the guard part of CAN_FIRE."""
    g = 'g_of_cf (cf_of_wf %s)' % hf if k < nR else 'g_of_cf %s' % hf
    if RDC[k]:
        g = '((Bool.and_eq_true _ _).mp (%s)).1' % g
    return g


def rq_leaf(k):
    g = M.rules[k]['guard']
    if g and g[0] not in ('check', 'false'):
        c = Ctx('v', 'ops.z', 'hf')
        return ('fun v oc hf => res_some _ ((%s).trans (%s))' % (val(k, g, c), hg_term(k, 'hf')))
    return 'fun _ _ _ => rfl'


w('theorem rqT (ops : Ops V) (E : EqC V) %s : Tab.AllR (RQ ops E %s) gT 0 nS :=\n  %s'
  % (HYP, HYA, stabproof('gT', 'rqT', '(ops : Ops V) (E : EqC V) ' + HYP, 'ops E ' + HYA,
                         lambda nm, lo, hi: 'Tab.AllR (RQ ops E %s) %s %d %d' % (HYA, nm, lo, hi),
                         ['(%s : RQ ops E %s %d G_%d)' % (rq_leaf(k), HYA, k, k)
                          for k in range(nS)])))
w('''theorem resR_ok (ops : Ops V) (E : EqC V) %s (u : St → Nat → V) (oc : OC V) (w : %s) (r : Fin nR)
    (hf : fire ops E oc w r = true) : (trR (P ops E u) oc r).2.isSome = true := by
  rw [trR_eq]
  exact Tab.get_allR _ 0 nS (rqT ops E %s) r.val (Nat.zero_le _) (rlt r) (u (.inr r)) oc (pre_r hf)
''' % (HYP, WXM, HYA))
if MET:
    w('''/-- Method `m`'s result: its result expression, when it can fire. -/
def RQM (ops : Ops V) (E : EqC V) %s (m : Nat) (_n : Nat) : Prop :=
  ∀ (v : Nat → V) (oc : OC V) (x : V), CF ops E oc (nR + m) = true →
    (resT.get (nR + m)) V ops E x (envS ops E v oc (nR + m) x) = some ((retT.get m) V ops E oc)
''' % HYP)
    stab('mIdx', 'Tab Nat', [nat(m) for m in range(nM)])

    def rqm_leaf(m):
        k = nR + m
        c = Ctx('v', 'x', 'hf')
        return ('fun v oc x hf => res_eq ((%s).trans (%s)) (%s)'
                % (val(k, M.rules[k]['guard'], c), hg_term(k, 'hf'), val(k, M.rules[k].get('res', ('z',)), c)))
    w('theorem rqM (ops : Ops V) (E : EqC V) %s : Tab.AllR (RQM ops E %s) mIdx 0 nM :=\n  %s'
      % (HYP, HYA, tabproof(['(%s : RQM ops E %s %d %d)' % (rqm_leaf(m), HYA, m, m) for m in range(nM)])))
    w('''theorem cf_of_avail {ops : Ops V} {E : EqC V} {u : St → Nat → V} {oc : OC V} {w : Wires (Fin nM) V} {m : Fin nM}
    (h : Avail (P ops E u) oc w m) : CF ops E oc (nR + m.val) = true :=
  h.1.resolve_left (fun h' => by cases h')
theorem resM_ok (ops : Ops V) (E : EqC V) %s (u : St → Nat → V) (oc : OC V) (w : Wires (Fin nM) V) (m : Fin nM)
    (h : Avail (P ops E u) oc w m) : (trM (P ops E u) oc w m).2 = some (((P ops E u).comb oc w).2.ret m) := by
  rw [trM_eq]
  exact Tab.get_allR _ 0 nM (rqM ops E %s) m.val (Nat.zero_le _) m.isLt (u (.inl m)) oc (w.arg m) (cf_of_avail h)
''' % (HYP, HYA))
w('end\n')
mark('values')

# ---------------------------------------------------------------- calls at every skipped value
w('''/-! ## Each step's calls at every skipped value (`CertU.trU`) -/

section
variable {V : Type}
/-- Site `i` of step `k`, when the step may run, agrees at any two skipped values of its step:
    its condition, and its argument where taken. -/
def HQ (ops : Ops V) (E : EqC V) %s (k i : Nat) (F : (V : Type) → Ops V → EqC V → V → SF V) : Prop :=
  ∀ (oc : OC V) (v v' : Nat → V) (x : V), PRE ops E oc k = true →
    (F V ops E x).1 (envS ops E v oc k x) = (F V ops E x).1 (envS ops E v' oc k x) ∧
    ((F V ops E x).1 (envS ops E v oc k x) = true → (F V ops E x).2 (envS ops E v oc k x) = (F V ops E x).2 (envS ops E v' oc k x))
def HQT (ops : Ops V) (E : EqC V) %s (k : Nat) (t : Tab ((V : Type) → Ops V → EqC V → V → SF V)) : Prop :=
  Tab.AllR (HQ ops E %s k) t 0 (kT.get k)
''' % (HYP, HYP, HYA))


def both(f):
    a = f(Ctx('v', 'x', 'hf'))
    b = f(Ctx("v'", 'x', 'hf'))
    if a == 'rfl':
        return 'rfl'
    return '(%s).trans (%s).symm' % (a, b)


def hq_leaf(k, i, st):
    c0 = st['cond'] if st['kind'] != 'task' else st['cond']
    if is_uncond(st) and st['kind'] != 'task' and st['arg'][0] in ('z', 'k', 'x'):
        return "fun _ _ _ _ _ => ⟨rfl, fun _ => rfl⟩"
    return "fun oc v v' x hf => ⟨%s, fun _ => %s⟩" % (both(lambda c: cond_val(k, st, c)), both(lambda c: arg_val(k, st, c)))


for k, r in enumerate(M.rules):
    if r['sites']:
        body = stabproof('sf_%d' % k, 'hq_%d' % k, '(ops : Ops V) (E : EqC V) ' + HYP, 'ops E ' + HYA,
                         lambda nm, lo, hi, k=k: 'Tab.AllR (HQ ops E %s %d) %s %d %d' % (HYA, k, nm, lo, hi),
                         [hq_leaf(k, i, st) for i, st in enumerate(r['sites'])])
    else:
        body = 'fun _ _ h => absurd h (Nat.not_lt_zero _)'
    w('theorem hq_%d (ops : Ops V) (E : EqC V) %s : HQT ops E %s %d sf_%d :=\n  %s' % (k, HYP, HYA, k, k, body))
w('theorem hqT (ops : Ops V) (E : EqC V) %s : Tab.AllR (HQT ops E %s) sfT 0 nS :=\n  %s'
  % (HYP, HYA, stabproof('sfT', 'hqT', '(ops : Ops V) (E : EqC V) ' + HYP, 'ops E ' + HYA,
                         lambda nm, lo, hi: 'Tab.AllR (HQT ops E %s) %s %d %d' % (HYA, nm, lo, hi),
                         ['hq_%d ops E %s' % (k, HYA) for k in range(nS)])))
w('''/-- A step that may run makes the same calls at any two skipped values of its step. -/
theorem trUS (ops : Ops V) (E : EqC V) %s (oc : OC V) (k : Nat) (hk : k < nS) (v v' : Nat → V) (x : V)
    (hf : PRE ops E oc k = true) :
    callsOf (envS ops E v oc k x) (sitesF (siteS ops E k x) (kR k)) = callsOf (envS ops E v' oc k x) (sitesF (siteS ops E k x) (kR k)) :=
  callsOf_fn_congr _ _ _ _ (fun i hi =>
    Tab.get_allR _ 0 (kR k) (Tab.get_allR _ 0 nS (hqT ops E %s) k (Nat.zero_le _) hk) i (Nat.zero_le _) hi oc v v' x hf)
end
''' % (HYP, HYA))
mark('hq')

# readiness of each taken guarded site (only where some guarded site is conditional)
if NEEDUC:
    w('''section
variable {V : Type}
theorem rd_at {oc : OC V} {k : Nat} (h : RD oc k = true) (i : Nat) (hi : i < kR k)
    (ha : alwaysRdyB ((keyT.get k).get i) = false) (hu : (ucT.get k).testBit i = false) :
    (oc ((keyT.get k).get i).1).rdy ((keyT.get k).get i).2 = true := by
  have h2 := List.all_eq_true.mp h i (List.mem_range'.mpr ⟨i, hi, by omega⟩)
  simp only [ha, hu, Bool.false_or] at h2
  exact h2
theorem rdy_of_rdc {c c' r : Bool} (h : (!c || r) = true) (hc : c' = true) (e : c' = c) : r = true := by
  subst e; rw [hc] at h; simpa using h
/-- A taken guarded site of step `k` is ready: from CAN_FIRE (unconditional), from the guard's
    lifted readiness (a conditional call), or from the child's alias (a readiness read). -/
def RY (ops : Ops V) (E : EqC V) %s (k i : Nat) (F : (V : Type) → Ops V → EqC V → V → SF V) : Prop :=
  ∀ (oc : OC V) (v : Nat → V) (x : V), (∀ j, AliasOK ops.toB (CI j).ann (oc j)) → PRE ops E oc k = true →
    (F V ops E x).1 (envS ops E v oc k x) = true → alwaysRdyB ((keyT.get k).get i) = false →
    (oc ((keyT.get k).get i).1).rdy ((keyT.get k).get i).2 = true
def RYT (ops : Ops V) (E : EqC V) %s (k : Nat) (t : Tab ((V : Type) → Ops V → EqC V → V → SF V)) : Prop :=
  Tab.AllR (RY ops E %s k) t 0 (kT.get k)
''' % (HYP, HYP, HYA))

    def ry_leaf(k, i, st):
        if not M.is_rdy_guarded(st['port']):
            return 'fun _ _ _ _ _ _ ha => absurd ha (by decide)'
        hrd = 'rd_of_cf (cf_of_wf hf)' if k < nR else 'rd_of_cf hf'
        if is_uncond(st):
            return 'fun oc v x _ hf _ ha => rd_at (%s) %d (by decide) ha (by decide)' % (hrd, i)
        if st['kind'] == 'read':
            lsite = st['cond'][1]
            kd, ii, m = st['port']
            lp = M.rules[k]['sites'][lsite]['port']
            evl = evt(k, lsite, Ctx('v', 'x'))
            return ('fun oc v x hal hf hc ha => ((hal (%s %d)).2 %s %s rfl).trans ((congrArg ops.toB (%s)).symm.trans hc)'
                    % (M.ihelper(kd), ii, K[kd].lports[K[kd].code(m)], K[lp[0]].lports[K[lp[0]].code(lp[2])], evl))
        n_ = RDC[k].index(i)
        gpart = '((Bool.and_eq_true _ _).mp (g_of_cf %s)).2' % ('(cf_of_wf hf)' if k < nR else 'hf')
        return ('fun oc v x _ hf hc ha => rdy_of_rdc (%s) hc (%s)'
                % (andget(gpart, len(RDC[k]) + 1, n_), cond_val(k, st, Ctx('v', 'x', 'hf'))))

    for k, r in enumerate(M.rules):
        if r['sites']:
            body = stabproof('sf_%d' % k, 'ry_%d' % k, '(ops : Ops V) (E : EqC V) ' + HYP, 'ops E ' + HYA,
                             lambda nm, lo, hi, k=k: 'Tab.AllR (RY ops E %s %d) %s %d %d' % (HYA, k, nm, lo, hi),
                             [ry_leaf(k, i, st) for i, st in enumerate(r['sites'])])
        else:
            body = 'fun _ _ h => absurd h (Nat.not_lt_zero _)'
        w('theorem ry_%d (ops : Ops V) (E : EqC V) %s : RYT ops E %s %d sf_%d :=\n  %s' % (k, HYP, HYA, k, k, body))
    w('theorem ryT (ops : Ops V) (E : EqC V) %s : Tab.AllR (RYT ops E %s) sfT 0 nS :=\n  %s'
      % (HYP, HYA, stabproof('sfT', 'ryT', '(ops : Ops V) (E : EqC V) ' + HYP, 'ops E ' + HYA,
                             lambda nm, lo, hi: 'Tab.AllR (RYT ops E %s) %s %d %d' % (HYA, nm, lo, hi),
                             ['ry_%d ops E %s' % (k, HYA) for k in range(nS)])))
    w('''theorem rdyS (ops : Ops V) (E : EqC V) %s (oc : OC V) (k : Nat) (hk : k < nS) (v : Nat → V) (x : V)
    (hal : ∀ j, AliasOK ops.toB (CI j).ann (oc j)) (hf : PRE ops E oc k = true) :
    ∀ c ∈ callsOf (envS ops E v oc k x) (sitesF (siteS ops E k x) (kR k)),
      (CI c.1).ann.alwaysRdy c.2.1 = false → (oc c.1).rdy c.2.1 = true := by
  intro c hc ha
  obtain ⟨s, hs, hcs, rfl⟩ := mem_callsOf_inv _ _ c hc
  obtain ⟨i, hi, rfl⟩ := mem_sitesF hs
  exact Tab.get_allR _ 0 (kR k) (Tab.get_allR _ 0 nS (ryT ops E %s) k (Nat.zero_le _) hk) i (Nat.zero_le _) hi oc v x hal hf hcs ha
end
''' % (HYP, HYA))
mark('ry')

# ---------------------------------------------------------------- checks (kernel)
w('/-! ## Decided checks over closed tables -/\n')
w('''/-- A check over each step's sites (steps without sites pass). -/
theorem sitesB_spec (f : Nat → Nat → (Σ j : I, (CI j).M) → Bool)
    (h : Tab.allB (fun k keys => Nat.beq (kT.get k) 0 || Tab.allB (f k) keys 0 (kT.get k)) keyT 0 nS = true)
    (k : Nat) (hk : k < nS) : ∀ i, i < kR k → f k i ((keyT.get k).get i) = true := by
  intro i hi
  have h1 := Tab.allB_spec _ keyT 0 nS h k (Nat.zero_le _) hk
  simp only [Bool.or_eq_true] at h1
  rcases h1 with h1 | h1
  · have := Nat.eq_of_beq_eq_true h1; simp only [kR] at hi; omega
  · exact Tab.allB_spec _ _ 0 (kT.get k) h1 i (Nat.zero_le _) hi
theorem chkAll : Tab.allB (fun k keys => Nat.beq (kT.get k) 0 ||
    Tab.allB (fun i p => (idxT (usesAt p) k).contains i) keys 0 (kT.get k)) keyT 0 nS = true := by
  decide +kernel
theorem hchkT (k : Nat) (hk : k < nS) : ∀ i, i < kR k → i ∈ idxT (usesAt ((keyT.get k).get i)) k := by
  intro i hi
  have h := sitesB_spec (fun k i p => (idxT (usesAt p) k).contains i) chkAll k hk i hi
  simpa using h
theorem hchk (r : Fin nR) : ∀ i, i < kR r.val → i ∈ idx (.inr r) ((keyT.get r.val).get i) :=
  hchkT r.val (rlt r)
''' + ('''theorem hchkM (m : Fin nM) : ∀ i, i < kR (nR + m.val) → i ∈ idx (.inl m) ((keyT.get (nR + m.val)).get i) :=
  hchkT (nR + m.val) (mlt m)
''' if MET else ''))

w('''section
variable {V : Type}

theorem onIff (ops : Ops V) (E : EqC V) (u : St → Nat → V) (oc : OC V) (w : %s) :
    ∀ s, On (P ops E u) (fire ops E) oc w s ↔ %s = true := by
  intro s; cases s with
  | inl e => %s
  | inr r => exact Iff.rfl
''' % (WXM, 'stepOn ops E oc s' if not MET else 'stepOn ops E oc w s', 'exact e.elim' if not MET else 'exact Iff.rfl') + '''
/-- A rule's site on a port: the rule calls the port when the site's condition holds. -/
theorem cR (ops : Ops V) (E : EqC V) (u : St → Nat → V) (oc : OC V) (w : %s) (r : Fin nR) (j : I) (cm : (CI j).M)
    (i0 : Nat) (hidx : idx (.inr r) ⟨j, cm⟩ = [i0]) (hi0 : Nat.blt i0 (kR r.val) = true)
    (hk0 : (keyT.get r.val).get i0 = ⟨j, cm⟩) (hc : (siteR ops E r.val i0).cond (envR ops E (u (.inr r)) oc r.val) = true) :
    Calls (P ops E u) (trS (P ops E u) oc w (.inr r)) j cm := by
  show ∃ x, (cm, x) ∈ proj j (trS (P ops E u) oc w (.inr r))
  rw [trS_eq]
  exact (calls_one _ (siteR ops E r.val) (kR r.val) (keyT.get r.val).get (fun _ => rfl) (idx (.inr r)) (hchk r) j cm i0
    hidx hi0 hk0).mpr hc
theorem cRk (ops : Ops V) (E : EqC V) (u : St → Nat → V) (oc : OC V) (w : %s) (r : Fin nR) (κ : Kind) (i : Nat)
    (hi : i < nk κ) (cm : (KI κ).M) (i0 : Nat) (hidx : idxT (((usesK κ).get i).uses cm) r.val = [i0])
    (hi0 : Nat.blt i0 (kR r.val) = true) (hk0 : (keyT.get r.val).get i0 = ⟨⟨κ, ⟨i, hi⟩⟩, cm⟩)
    (hc : (siteR ops E r.val i0).cond (envR ops E (u (.inr r)) oc r.val) = true) :
    Calls (P ops E u) (trS (P ops E u) oc w (.inr r)) ⟨κ, ⟨i, hi⟩⟩ cm :=
  cR ops E u oc w r ⟨κ, ⟨i, hi⟩⟩ cm i0 hidx hi0 hk0 hc
theorem aR (ops : Ops V) (E : EqC V) (u : St → Nat → V) (oc : OC V) (w : %s) (r : Fin nR) (j : I) (cm : (CI j).M)
    (i0 : Nat) (hidx : idx (.inr r) ⟨j, cm⟩ = [i0]) (v : V)
    (hv : (siteR ops E r.val i0).cond (envR ops E (u (.inr r)) oc r.val) = true → (siteR ops E r.val i0).arg (envR ops E (u (.inr r)) oc r.val) = v)
    (x : V) (hx : (cm, x) ∈ proj j (trS (P ops E u) oc w (.inr r))) : v = x := by
  rw [trS_eq] at hx
  exact arg_one _ (siteR ops E r.val) (kR r.val) (keyT.get r.val).get (fun _ => rfl) (idx (.inr r)) (hchk r) j cm i0 hidx v hv x hx
theorem aRk (ops : Ops V) (E : EqC V) (u : St → Nat → V) (oc : OC V) (w : %s) (r : Fin nR) (κ : Kind) (i : Nat)
    (hi : i < nk κ) (cm : (KI κ).M) (i0 : Nat) (hidx : idxT (((usesK κ).get i).uses cm) r.val = [i0]) (v : V)
    (hv : (siteR ops E r.val i0).cond (envR ops E (u (.inr r)) oc r.val) = true → (siteR ops E r.val i0).arg (envR ops E (u (.inr r)) oc r.val) = v)
    (x : V) (hx : (cm, x) ∈ proj (⟨κ, ⟨i, hi⟩⟩ : I) (trS (P ops E u) oc w (.inr r))) : v = x :=
  aR ops E u oc w r ⟨κ, ⟨i, hi⟩⟩ cm i0 hidx v hv x hx
theorem cRkIff (ops : Ops V) (E : EqC V) (u : St → Nat → V) (oc : OC V) (w : %s) (r : Fin nR) (κ : Kind)
    (i : Nat) (hi : i < nk κ) (cm : (KI κ).M) (i0 : Nat) (hidx : idxT (((usesK κ).get i).uses cm) r.val = [i0])
    (hi0 : Nat.blt i0 (kR r.val) = true) (hk0 : (keyT.get r.val).get i0 = ⟨⟨κ, ⟨i, hi⟩⟩, cm⟩) :
    Calls (P ops E u) (trS (P ops E u) oc w (.inr r)) ⟨κ, ⟨i, hi⟩⟩ cm ↔
      (siteR ops E r.val i0).cond (envR ops E (u (.inr r)) oc r.val) = true := by
  show (∃ x, (cm, x) ∈ proj (⟨κ, ⟨i, hi⟩⟩ : I) (trS (P ops E u) oc w (.inr r))) ↔ _
  rw [trS_eq]
  exact calls_one _ (siteR ops E r.val) (kR r.val) (keyT.get r.val).get (fun _ => rfl) (idx (.inr r)) (hchk r)
    (⟨κ, ⟨i, hi⟩⟩ : I) cm i0 hidx hi0 hk0
/-- A rule's call on a port was made: its site's condition held. -/
theorem aRkC (ops : Ops V) (E : EqC V) (u : St → Nat → V) (oc : OC V) (w : %s) (r : Fin nR) (κ : Kind) (i : Nat)
    (hi : i < nk κ) (cm : (KI κ).M) (i0 : Nat) (hidx : idxT (((usesK κ).get i).uses cm) r.val = [i0])
    (x : V) (hx : (cm, x) ∈ proj (⟨κ, ⟨i, hi⟩⟩ : I) (trS (P ops E u) oc w (.inr r))) :
    (siteR ops E r.val i0).cond (envR ops E (u (.inr r)) oc r.val) = true := by
  rw [trS_eq] at hx
  obtain ⟨i', hi', hc, -⟩ := mem_proj_fn _ (siteR ops E r.val) (kR r.val) (keyT.get r.val).get (fun _ => rfl) (idx (.inr r))
    (hchk r) (⟨κ, ⟨i, hi⟩⟩ : I) cm x hx
  have e : idx (.inr r) ⟨⟨κ, ⟨i, hi⟩⟩, cm⟩ = [i0] := hidx
  rw [e, List.mem_singleton] at hi'
  subst hi'
  exact hc
''' % (WXM, WXM, WXM, WXM, WXM, WXM) + ('''/-- A method's site on a port, and its argument (the method's argument is `w.arg m`). -/
theorem cMk (ops : Ops V) (E : EqC V) (u : St → Nat → V) (oc : OC V) (w : Wires (Fin nM) V) (m : Fin nM) (κ : Kind) (i : Nat)
    (hi : i < nk κ) (cm : (KI κ).M) (i0 : Nat) (hidx : idxT (((usesK κ).get i).uses cm) (nR + m.val) = [i0])
    (hi0 : Nat.blt i0 (kR (nR + m.val)) = true) (hk0 : (keyT.get (nR + m.val)).get i0 = ⟨⟨κ, ⟨i, hi⟩⟩, cm⟩)
    (hc : (siteS ops E (nR + m.val) (w.arg m) i0).cond (envS ops E (u (.inl m)) oc (nR + m.val) (w.arg m)) = true) :
    Calls (P ops E u) (trS (P ops E u) oc w (.inl m)) ⟨κ, ⟨i, hi⟩⟩ cm := by
  show ∃ x, (cm, x) ∈ proj (⟨κ, ⟨i, hi⟩⟩ : I) (trS (P ops E u) oc w (.inl m))
  rw [trSM_eq]
  exact (calls_one _ (siteS ops E (nR + m.val) (w.arg m)) (kR (nR + m.val)) (keyT.get (nR + m.val)).get (fun _ => rfl)
    (idx (.inl m)) (hchkM m) (⟨κ, ⟨i, hi⟩⟩ : I) cm i0 hidx hi0 hk0).mpr hc
theorem aMk (ops : Ops V) (E : EqC V) (u : St → Nat → V) (oc : OC V) (w : Wires (Fin nM) V) (m : Fin nM) (κ : Kind) (i : Nat)
    (hi : i < nk κ) (cm : (KI κ).M) (i0 : Nat) (hidx : idxT (((usesK κ).get i).uses cm) (nR + m.val) = [i0]) (v : V)
    (hv : (siteS ops E (nR + m.val) (w.arg m) i0).cond (envS ops E (u (.inl m)) oc (nR + m.val) (w.arg m)) = true →
      (siteS ops E (nR + m.val) (w.arg m) i0).arg (envS ops E (u (.inl m)) oc (nR + m.val) (w.arg m)) = v)
    (x : V) (hx : (cm, x) ∈ proj (⟨κ, ⟨i, hi⟩⟩ : I) (trS (P ops E u) oc w (.inl m))) : v = x := by
  rw [trSM_eq] at hx
  exact arg_one _ (siteS ops E (nR + m.val) (w.arg m)) (kR (nR + m.val)) (keyT.get (nR + m.val)).get (fun _ => rfl)
    (idx (.inl m)) (hchkM m) (⟨κ, ⟨i, hi⟩⟩ : I) cm i0 hidx v hv x hx
''' if MET else '') + 'end\n')

# ---------------------------------------------------------------- mux chains
w('/-! ## The muxes\' one-hot chains, from the exclusions -/\n')
w('section\nvariable {V : Type}\n')
w('''/-- A run-time check excludes its head with every rule of its list. -/
theorem chkC (ops : Ops V) (E : EqC V) (oc : OC V) (hA : asmP ops E oc) (c : Nat) (hc : Nat.blt c nC = true) :
    ∀ y ∈ chkLst.get c, WF ops E oc (chkHead.get c) = true → WF ops E oc y = true → False :=
  forall_of_check (hA.2 c hc)
''')
nchk_pairs = nuncov = 0
CVQ = 'fun e => ∀ y, e.2.testBit y = true → WF ops E oc e.1 = true → WF ops E oc y = true → False'
def wfx_of(k, si, h):
    st = M.rules[k]['sites'][si]
    return h if is_uncond(st) else '((Bool.and_eq_true _ _).mp %s).1' % h


def cx_of(k, si, h):
    st = M.rules[k]['sites'][si]
    return [] if is_uncond(st) else ['((Bool.and_eq_true _ _).mp %s).2' % h]


for q, (p, us, plan) in enumerate(muxes):
    ks = [k for k, _ in us]
    assert all(k < nR for k in ks), 'a mux with method users'
    if SELMUX.get(q):
        sis = dict(us)
        w('def selq_%d (V : Type) (ops : Ops V) (E : EqC V) (oc : OC V) (k : Nat) : Bool :=\n  %s'
          % (q, ''.join('bif Nat.beq k %d then %s else ' % (k, entry(k, M.rules[k]['sites'][sis[k]])) for k in ks) + 'false'))
        cvs, pfs = [], []
        for a, x in enumerate(ks):
            tail = ks[a + 1:]
            nuncov += len(tail)
            prs = []
            for y in tail:
                if M.excl_src(x, y) is not None:
                    prs.append('fun h1 h2 => %s' % excl(x, y, h1=wfx_of(x, sis[x], 'h1'), h2=wfx_of(y, sis[y], 'h2')))
                else:
                    n_ = CFIND[(q, x, y)]
                    src = 'hy' if CFACTS[n_]['ok'] else 'hr'
                    prs.append('fun h1 h2 => %s.cx_%d oc %s' % (src, n_, ' '.join(
                        [wfx_of(x, sis[x], 'h1')] + cx_of(x, sis[x], 'h1') + [wfx_of(y, sis[y], 'h2')] + cx_of(y, sis[y], 'h2'))))
            cvs.append('(%d, cvS [] [%s])' % (x, ', '.join(map(str, tail))))
            pfs.append('cover_of (fun y hy => absurd hy (List.not_mem_nil)) %s' % allL(prs))
        stab('msT_%d' % q, 'Tab (Nat × Nat)', cvs)
        CVS = 'fun e => ∀ y, e.2.testBit y = true → selq_%d V ops E oc e.1 = true → selq_%d V ops E oc y = true → False' % (q, q)
        prf = stabproof('msT_%d' % q, 'cv_%d' % q, '(ops : Ops V) (E : EqC V) %s (oc : OC V) (hA : asmP ops E oc)' % HYP,
                        'ops E %s oc hA' % HYA, lambda nm, lo, hi: 'Tab.All (%s) %s' % (CVS, nm), pfs, one=False)
        w('theorem chain_%d (ops : Ops V) (E : EqC V) %s (oc : OC V) (hA : asmP ops E oc) :\n'
          '    amoChain false ((mlist_%d V ops E oc).map Prod.fst) :=\n'
          '  amoChain_of_cover (fun k => selq_%d V ops E oc k) (fun x y => selq_%d V ops E oc x = true → selq_%d V ops E oc y = true → False)\n'
          '    (fun _ _ h => h) msT_%d [%s] (by decide +kernel) (by decide +kernel)\n    %s'
          % (q, HYP, q, q, q, q, q, ', '.join(map(str, ks)), prf))
        continue
    gq = MGRP.get(q)
    gs = gq[1] if gq else []
    ksr = ks[:len(ks) - len(gs)]
    assert ks[len(ks) - len(gs):] == gs
    nchk_pairs += len(gs) * (len(gs) - 1) // 2
    if gq and not ksr:
        # every member in one group: the group's checks, no per-member data
        w('def gsm_%d : List Nat := [%s]' % (q, ', '.join(nat(x) for x in gs)))
        w('theorem chain_%d (ops : Ops V) (E : EqC V) %s (oc : OC V) (hA : asmP ops E oc) :\n'
          '    amoChain false ((mlist_%d V ops E oc).map Prod.fst) :=\n'
          '  amoChain_of_grp (fun k => WF ops E oc k) gsm_%d (grpT.get %d) (by decide +kernel) (hA.1 %d rfl)'
          % (q, HYP, q, q, gq[0], gq[0]))
        continue
    cvs, pfs = [], []
    for a, x in enumerate(ksr):
        tail = ks[a + 1:]
        cb, unc = plan[x]
        uncl = [y for y in tail if y in unc]
        nuncov += len(uncl)
        nchk_pairs += len(tail) - len(uncl)
        pw = allL(['fun h1 h2 => %s' % excl(x, y) for y in uncl])
        ul = '[%s]' % ', '.join(map(str, uncl))
        if cb is None:
            cvs.append('(%d, cvS [] %s)' % (x, ul))
            pfs.append('cover_of (fun y hy => absurd hy (List.not_mem_nil)) %s' % pw)
        else:
            cvs.append('(%d, cvS (chkLst.get %d) %s)' % (x, LITI[cb], ul))
            pfs.append('cover_of (chkC ops E oc hA %d rfl) %s' % (LITI[cb], pw))
    stab('msT_%d' % q, 'Tab (Nat × Nat)', cvs)
    if gq:
        prf = stabproof('msT_%d' % q, 'cv_%d' % q, '(ops : Ops V) (E : EqC V) %s (oc : OC V) (hA : asmP ops E oc)' % HYP,
                        'ops E %s oc hA' % HYA, lambda nm, lo, hi: 'Tab.All (%s) %s' % (CVQ, nm), pfs, one=False)
        w('def gsm_%d : List Nat := [%s]' % (q, ', '.join(nat(x) for x in gs)))
        w('theorem chain_%d (ops : Ops V) (E : EqC V) %s (oc : OC V) (hA : asmP ops E oc) :\n'
          '    amoChain false ((mlist_%d V ops E oc).map Prod.fst) :=\n'
          '  amoChain_of_cover_grp (fun k => WF ops E oc k) msT_%d [%s] gsm_%d (grpT.get %d) (by decide +kernel) (by decide +kernel)\n'
          '    (%s) (by decide +kernel) (hA.1 %d rfl)'
          % (q, HYP, q, q, ', '.join(map(str, ksr)), q, gq[0], prf, gq[0]))
        continue
    prf = stabproof('msT_%d' % q, 'cv_%d' % q, '(ops : Ops V) (E : EqC V) %s (oc : OC V) (hA : asmP ops E oc)' % HYP,
                    'ops E %s oc hA' % HYA, lambda nm, lo, hi: 'Tab.All (%s) %s' % (CVQ, nm), pfs, one=False)
    w('theorem chain_%d (ops : Ops V) (E : EqC V) %s (oc : OC V) (hA : asmP ops E oc) :\n'
      '    amoChain false ((mlist_%d V ops E oc).map Prod.fst) :=\n'
      '  amoChain_of_cover (fun k => WF ops E oc k) (fun x y => WF ops E oc x = true → WF ops E oc y = true → False)\n'
      '    (fun _ _ h => h) msT_%d [%s] (by decide +kernel) (by decide +kernel)\n    %s'
      % (q, HYP, q, q, ', '.join(map(str, ks)), prf))
w('end\n')
mark('chains')

# ---------------------------------------------------------------- Glue and scan
w('/-! ## The scan: footprints, kind tables, exception masks, escapes -/\n')


def ents(l):
    return '[' + ', '.join('(%d, %d, %d)' % e for e in l) + ']'


def kc(t):
    return '⟨[%s], [%s], [%s], [%s]⟩' % tuple(', '.join(map(str, x)) for x in t)


kt = M.kind_tables()
stab('fpRT', 'Tab (List Ent)', [ents(l) for l in Dn.fpR])
if MET:
    stab('fpMT', 'Tab (List Ent)', [ents(l) for l in Dn.fpM])
stab('ktT', 'Tab (Tab KC)', [tab([kc(t) for t in kd]) for kd in kt])
GMUX = [q for q in range(len(muxes)) if not SELMUX.get(q)]
groups = [[k for k, _ in muxes[q][1]] for q in GMUX]
gsets = [set(g) for g in groups]
for i in range(nR):
    for j in range(nR):
        if need[i] >> j & 1 and not any(i in g and j in g for g in gsets):
            groups.append([j, i])
            gsets.append({i, j})
gid = [[g for g, gs in enumerate(gsets) if i in gs] for i in range(nR)]
EPAIRS = [(r1, r2) for (r1, r2, bad) in ESC]
members = sorted(set(x for g in groups for x in g) | set(r1 for r1, _ in EPAIRS))
rcode = [0] * nR
for c_, x in enumerate(members):
    rcode[x] = c_ + 1
rinv = [0] + members


def code_mask(m):
    r = 0
    for j in range(nR):
        if m >> j & 1:
            assert rcode[j], 'exception bit of a rule in no group'
            r |= 1 << rcode[j]
    return r


exRc = [code_mask(x) for x in need]
escRc = [code_mask(x) for x in esc]
Dc = M.design(exRc, escRc, MAnn if MET else None)
Dc.rc = rcode
okc, _, infoc = model.scanlib.run_scan(Dc)
assert okc, infoc
stab('exRT', 'Tab Nat', [nat(x) for x in exRc])
stab('escRT', 'Tab Nat', [nat(x) for x in escRc])
stab('rcT', 'Tab Nat', [nat(x) for x in rcode])
stab('rinvT', 'Tab Nat', [nat(x) for x in rinv])
stab('memT', 'Tab (List Nat)', ['[' + ', '.join(map(str, g)) + ']' for g in groups] or ['[]'])
stab('gidT', 'Tab (List Nat)', ['[' + ', '.join(map(str, g)) + ']' for g in gid])
w('abbrev ng : Nat := %d' % len(groups))
if MET:
    nfI, nfO, ngI, ngO, h3ok = M.meth_tables(MAnn)
    assert not any(h3ok), 'h3ok entries'
    for nm_, tb in [('nfIT', nfI), ('nfOT', nfO), ('ngIT', ngI), ('ngOT', ngO)]:
        stab(nm_, 'Tab Nat', [nat(x) for x in tb])
w('''def D : Design where
  nM := %s
  nR := nR
  d := %d
  fpM := %s
  fpR := fpRT
  kt := ktT
  nfI := %s
  nfO := %s
  ngI := %s
  ngO := %s
  h3ok := .leaf 0
  exM := .leaf 0
  exR := exRT
  exMR := .leaf 0
  escR := escRT
  rc := fun i => rcT.get i
''' % (('nM', M.d, 'fpMT', 'nfIT', 'nfOT', 'ngIT', 'ngOT') if MET else ('0', M.d, '.leaf []', '.leaf 0', '.leaf 0', '.leaf 0', '.leaf 0')))
mark('design')
# the scan in segments (cost review r7-1 M2): a cut before the step that would take a segment past
# SEG_USES footprint entries; each checkpoint is the full state there (design.md §10's gate)
SEG_USES = int(os.environ.get('E2E_SEG_USES', '4096'))
# the gate's numeric bounds (design.md §10 item 6), as generator checks
assert SEG_USES <= 4096, 'segments of at most 4,096 uses'
assert M.nslots <= 8192, 'a state of at most 8,192 slots'
assert len(rinv) <= 4096, 'at most 4,096 rule codes'
assert max(len(r_['sites']) for r_ in M.rules) <= int(os.environ.get('E2E_STEP_SITES', '3072')), 'sites in one step'
uses_ = [len(Dc.fpM[k_]) for k_ in range(Dc.nM)] + [len(Dc.fpR[i_]) for i_ in range(Dc.nR)]
assert max(uses_ + [0]) <= SEG_USES, 'a step with more uses than a segment (gate: uses per step)'
cuts_, acc_ = [], 0
for g_, n_ in enumerate(uses_):
    if acc_ + n_ > SEG_USES and acc_ > 0:
        cuts_.append(g_)
        acc_ = 0
    acc_ += n_
_, states_, _ = model.scanlib.run_scan(Dc)
for j_, c_ in enumerate(cuts_):
    w(model.scanlib.emit_tree(states_[c_ - 1], Dc.d, 'cp%d' % (j_ + 1)))
prev_, prevn_, names_ = 0, 'ST.init', []
for j_, c_ in enumerate(cuts_):
    w('theorem seg%d : D.seg %d %d %s cp%d = true := by decide +kernel' % (j_, prev_, c_ - prev_, prevn_, j_ + 1))
    names_.append((c_ - prev_, 'cp%d' % (j_ + 1), 'seg%d' % j_))
    prev_, prevn_ = c_, 'cp%d' % (j_ + 1)
w('theorem segZ : D.segLast %d %s = true := by decide +kernel' % (prev_, prevn_))
w('theorem scanOK : D.Ok := D.ok_of_chain [%s] %s\n' % (', '.join('(%d, %s)' % (m_, cp_) for (m_, cp_, _) in names_),
                                                       ('⟨' + ', '.join(nm_ for (_, _, nm_) in names_) + ', segZ⟩') if names_ else 'segZ'))
NSEG = len(cuts_) + 1
mark('scan')
w('def chB (j : I) : Bool := decide (j.1 = .tasks)' if nT else 'def chB (j : I) : Bool := false')
w('def posK : (κ : Kind) → (KI κ).M → Nat\n' + ''.join(
    ('  | .tasks => fun t => t.val\n' if kd == 'tasks' else '  | .%s => fun _ => 0\n' % kd) for kd in M.kinds))
stab('stT', 'Tab Nat', [nat(M.tstart[k]) for k in range(nR)])
w('def stF (g : Nat) : Nat := bif Nat.blt g nR then stT.get g else %d' % nT)
w('''/-- Chain intervals by the scan's step position (methods first, then rules by rank). -/
def cloG (g : Nat) : Nat := if g < D.nM then 0 else stF (g - D.nM)
def chiG (g : Nat) : Nat := if g < D.nM then 0 else stF (g - D.nM + 1)
theorem cloG_r (r : Nat) : cloG (D.nM + r) = stF r := by
  unfold cloG; rw [if_neg (Nat.not_lt.mpr (Nat.le_add_right _ _)), Nat.add_sub_cancel_left]
theorem chiG_r (r : Nat) : chiG (D.nM + r) = stF (r + 1) := by
  unfold chiG; rw [if_neg (Nat.not_lt.mpr (Nat.le_add_right _ _)), Nat.add_sub_cancel_left]
theorem stMono : Tab.allB (fun g x => Nat.ble x %d && Nat.ble x (bif Nat.blt (g + 1) nR then stT.get (g + 1) else %d))
    stT 0 nR = true := by decide +kernel
theorem chainAll : Tab.allB (fun k keys =>
    incB ((((List.range' 0 (kT.get k)).map keys.get).filter (fun p => chB p.1)).map (fun p => posK p.1.1 p.2)) &&
    ((((List.range' 0 (kT.get k)).map keys.get).filter (fun p => chB p.1)).map (fun p => posK p.1.1 p.2)).all
      (fun x => Nat.ble (bif Nat.blt k nR then stF k else 0) x && Nat.blt x (bif Nat.blt k nR then stF (k + 1) else 0))) keyT 0 nS = true := by
  decide +kernel
theorem cmonoG (g1 g2 : Nat) (h : g1 < g2) : chiG g1 ≤ cloG g2 := by
  have hm := mono_of_succ stF (stF_mono stT nR %d stMono)
  unfold chiG cloG
  split <;> split
  · exact Nat.le_refl _
  · exact Nat.zero_le _
  · omega
  · exact hm _ _ (by omega)
''' % (nT, nT, nT))
w('''section
variable {V : Type}
def G (ops : Ops V) (E : EqC V) (u : St → Nat → V) : Glue (P ops E u) D where
  mi := fun m => %s
  ruleAt := rR
  rk := fun r => r.val
  base := baseOf
  kind := fun j => kindN j.1
  code := fun j m => codeK j.1 m
  fx := fxF
  z := ops.z
  ch := chB
  pos := fun j m => posK j.1 m
  clo := cloG
  chi := chiG
end

def entK (p : Σ j : I, (CI j).M) : Ent := (baseOf p.1, kindN p.1.1, codeK p.1.1 p.2)
theorem fpAll : Tab.allB (fun k l => l == (List.range' 0 (kT.get k)).map (fun i => entK ((keyT.get k).get i))) fpRT 0 nR = true := by
  decide +kernel
''' % ('m.val' if MET else 'm.elim') + ('''theorem fpAllM : Tab.allB (fun m l => l == (List.range' 0 (kT.get (nR + m))).map (fun i => entK ((keyT.get (nR + m)).get i))) fpMT 0 nM = true := by
  decide +kernel
''' if MET else '') + '''theorem ghg : Tab.allB (fun x l => l.all (fun g => Nat.blt g ng && (memT.get g).contains x)) gidT 0 nR = true := by
  decide +kernel
theorem ghT : Tab.allB (fun y n => subB n (orMask (gidT.get y) (fun g => bits ((memT.get g).map D.rc)))) exRT 0 nR = true := by
  decide +kernel
theorem hinj_ok : ∀ g, g < ng → ∀ x ∈ memT.get g, ∀ x1, x1 < D.nR → D.rc x1 = D.rc x → x1 = x :=
  chains_inj D rinvT.get memT ng (by decide +kernel) (by decide +kernel)
theorem hg_ok : ∀ x, x < nR → ∀ g ∈ gidT.get x, g < ng ∧ x ∈ memT.get g := by
  intro x hx g hg
  have h := List.all_eq_true.mp (Tab.allB_spec _ gidT 0 nR ghg x (Nat.zero_le _) hx) g hg
  simp only [Bool.and_eq_true, List.contains_iff_mem] at h
  exact ⟨Nat.blt_eq.mp h.1, h.2⟩
theorem hT_ok : ∀ y, y < nR → Sub (exRT.get y) (orMask (gidT.get y) (fun g => bits ((memT.get g).map D.rc))) :=
  fun y hy => sub_of_subB (Tab.allB_spec _ exRT 0 nR ghT y (Nat.zero_le _) hy)
''')
for kd in M.kinds:
    if kd == 'tasks':
        w('theorem ports_tasks : ∀ p : (KI .tasks).M, p ∈ List.finRange nT := List.mem_finRange')
        continue
    ports = ', '.join(K[kd].lports)
    if kd == 'reg':
        w('theorem ports_reg : ∀ p : CRegM 1, p ∈ [Reg.read, Reg.write]\n  | .rd ⟨0, _⟩ => by simp\n  | .wr ⟨0, _⟩ => by simp')
    else:
        w('theorem ports_%s : ∀ p : (KI .%s).M, p ∈ [%s] := by intro p; cases p <;> simp' % (kd, kd, ports))
w('''def portsK : (κ : Kind) → List (KI κ).M
''' + ''.join(('  | .tasks => List.finRange nT\n' if kd == 'tasks' else '  | .%s => [%s]\n' % (kd, ', '.join(K[kd].lports))) for kd in M.kinds) + '''def portsJ (j : I) : List (CI j).M := portsK j.1
theorem portsJ_ok (j : I) (m : (CI j).M) : m ∈ portsJ j := by
  obtain ⟨κ, i⟩ := j
  cases κ
''' + ''.join('  · exact ports_%s m\n' % kd for kd in M.kinds))
w('''section
variable {V : Type}
theorem testBit_leaf0 (k i : Nat) : ((Tab.leaf 0 : Tab Nat).get k).testBit i = false := by simp [Tab.get]

theorem fpR_ok (ops : Ops V) (E : EqC V) (u : St → Nat → V) (oc : OC V) (r : Fin nR) :
    (trR (P ops E u) oc r).1.map (G ops E u).ent <+ D.fpR.get ((G ops E u).rk r) := by
  rw [trR_eq]
  refine (fp_of_sites (G ops E u) _ _).trans ?_
  have h := Tab.allB_spec _ fpRT 0 nR fpAll r.val (Nat.zero_le _) r.isLt
  rw [beq_iff_eq] at h
  show (sitesF (siteR ops E r.val) (kR r.val)).map (G ops E u).entS <+ fpRT.get r.val
  rw [h, sitesF_map]
  exact List.Sublist.refl _

theorem zR_ok (ops : Ops V) (E : EqC V) (u : St → Nat → V) (oc : OC V) (r : Fin nR) :
    ∀ c ∈ (trR (P ops E u) oc r).1, (G ops E u).fx ((G ops E u).kind c.1) ((G ops E u).code c.1 c.2.1) = true → c.2.2 = (G ops E u).z := by
  rw [trR_eq]
  apply z_of_sites (G ops E u)
  intro s hs hf ρ'
  obtain ⟨i, hi, rfl⟩ := mem_sitesF hs
  exact ((Tab.get_allR _ 0 (kR r.val) (sqR ops E r.val (rlt r)) i (Nat.zero_le _) hi) ops.z).2 hf ρ'
''' + ('''theorem fpM_ok (ops : Ops V) (E : EqC V) (u : St → Nat → V) (oc : OC V) (w : Wires (Fin nM) V) (m : Fin nM) :
    (trM (P ops E u) oc w m).1.map (G ops E u).ent <+ D.fpM.get ((G ops E u).mi m) := by
  rw [trM_eq]
  refine (fp_of_sites (G ops E u) _ _).trans ?_
  have h := Tab.allB_spec _ fpMT 0 nM fpAllM m.val (Nat.zero_le _) m.isLt
  rw [beq_iff_eq] at h
  show (sitesF (siteS ops E (nR + m.val) (w.arg m)) (kR (nR + m.val))).map (G ops E u).entS <+ fpMT.get m.val
  rw [h, sitesF_map]
  exact List.Sublist.refl _
theorem zM_ok (ops : Ops V) (E : EqC V) (u : St → Nat → V) (oc : OC V) (w : Wires (Fin nM) V) (m : Fin nM) :
    ∀ c ∈ (trM (P ops E u) oc w m).1, (G ops E u).fx ((G ops E u).kind c.1) ((G ops E u).code c.1 c.2.1) = true → c.2.2 = (G ops E u).z := by
  rw [trM_eq]
  apply z_of_sites (G ops E u)
  intro s hs hf ρ'
  obtain ⟨i, hi, rfl⟩ := mem_sitesF hs
  exact ((Tab.get_allR _ 0 (kR (nR + m.val)) (sqR ops E (nR + m.val) (mlt m)) i (Nat.zero_le _) hi) (w.arg m)).2 hf ρ'
''' if MET else ''))
for kd in M.kinds:
    if kd == 'tasks':
        continue
    w('theorem kind_%s (z : V) : KindOK (V := V) (KI .%s).ann (codeK .%s) (D.kc %d) (fxF %d) z :=\n'
      '  kindOK_of_chk ports_%s (by decide +kernel) z' % (kd, kd, kd, M.kn[kd], M.kn[kd], kd))
w('''theorem chainS (ops : Ops V) (E : EqC V) (u : St → Nat → V) (k : Nat) (hk : k < nS) (x : V) :
    (∀ p ∈ (sitesF (siteS ops E k x) (kR k)).map (fun s => (⟨s.j, s.m⟩ : Σ j : I, (CI j).M)),
      (G ops E u).ch p.1 = true → (bif Nat.blt k nR then stF k else 0) ≤ (G ops E u).pos p.1 p.2 ∧
        (G ops E u).pos p.1 p.2 < (bif Nat.blt k nR then stF (k + 1) else 0)) ∧
    incB ((((sitesF (siteS ops E k x) (kR k)).map (fun s => (⟨s.j, s.m⟩ : Σ j : I, (CI j).M))).filter
      (fun p => (G ops E u).ch p.1)).map (fun p => (G ops E u).pos p.1 p.2)) = true := by
  rw [sitesF_map]
  exact chain_keys (G := G ops E u) _ _ _ _ (Tab.allB_spec _ keyT 0 nS chainAll k (Nat.zero_le _) hk)
''')


def kcase(non, tk):
    return ''.join(('    · %s\n' % tk) if kd == 'tasks' else ('    · %s\n' % non(kd)) for kd in M.kinds)


ch_items = []
for g, mem in enumerate(groups):
    if g < len(GMUX):
        ch_items.append('(fun oc w hE => chain_%d ops E %s oc hE.1 : CHQ ops E u %d [%s])' % (GMUX[g], HYA, g, ', '.join(map(str, mem))))
    else:
        a, b = mem
        ch_items.append('(fun oc w hE => amoChain_of_pairsL (fun k => WF ops E oc k) '
                        '(fun x y => WF ops E oc x = true → WF ops E oc y = true → False) (fun _ _ h => h) [%d, %d] '
                        '⟨⟨fun h1 h2 => %s, trivial⟩, trivial, trivial⟩ : CHQ ops E u %d [%d, %d])'
                        % (a, b, excl(a, b, hA='hE.1'), g, a, b))
w('/-- Exception group `l`: its rules never two fire. -/\n'
  'def CHQ (ops : Ops V) (E : EqC V) (u : St → Nat → V) (g : Nat) (l : List Nat) : Prop :=\n'
  '  ∀ oc w, Env (P ops E u) oc w → amoChain false (l.map (fun i => fire ops E oc w (rR i)))')
w('theorem chT (ops : Ops V) (E : EqC V) (u : St → Nat → V) %s : Tab.AllR (CHQ ops E u) memT 0 ng :=\n  %s'
  % (HYP, tabproof(ch_items)))

# escapes: one dynamic proof per escaped pair
w('''
/-- Ports' static `Follow`: a flagged (argument-less) port's calls carry `z`. -/
def okP (j : I) (a b : (CI j).M) : Bool :=
  folP (CI j).ann a b && (!decide (a = b) || !(CI j).ann.share a || fxP ⟨j, a⟩)
theorem okP_follow (z : V) : ∀ j (a b : (CI j).M × V), okP j a.1 b.1 = true →
    (fxP ⟨j, a.1⟩ = true → a.2 = z) → (fxP ⟨j, b.1⟩ = true → b.2 = z) → (CI j).ann.Follow a b := by
  intro j a b h ha hb
  simp only [okP, Bool.and_eq_true, Bool.or_eq_true, Bool.not_eq_true', decide_eq_false_iff_not] at h
  obtain ⟨hf, hs⟩ := h
  simp only [folP, Bool.and_eq_true, Bool.or_eq_true, decide_eq_true_eq, Bool.not_eq_true'] at hf
  obtain ⟨⟨h11, h12⟩, h13⟩ := hf
  refine ⟨fun hne => ?_, h12, h13, fun he hsh => ?_⟩
  · rcases h11 with h | h
    · exact absurd h hne
    · exact h
  · rcases hs with (h | h) | h
    · exact absurd he h
    · rw [hsh] at h; cases h
    · have e2 : fxP ⟨j, b.1⟩ = true := by rw [← he]; exact h
      rw [ha h, hb e2]
/-- An escaped pair over numbered site tables, with flagged ports' calls carrying `z`. -/
theorem okT_exceptZ (z : V) (ρ1 ρ2 : Nat → V) (site1 site2 : Nat → Site I (fun j => (CI j).M) V) (k1 k2 : Nat)
    (key1 : Tab (Σ j, (CI j).M)) (key2 : Nat → Σ j, (CI j).M)
    (hkey1 : ∀ i, (⟨(site1 i).j, (site1 i).m⟩ : Σ j, (CI j).M) = key1.get i)
    (hkey2 : ∀ i, (⟨(site2 i).j, (site2 i).m⟩ : Σ j, (CI j).M) = key2 i)
    (idx2 : (Σ j, (CI j).M) → List Nat) (hchk2 : ∀ i, i < k2 → i ∈ idx2 (key2 i)) (bad : List (Nat × Nat))
    (hz1 : ∀ c ∈ callsOf ρ1 (sitesF site1 k1), fxP ⟨c.1, c.2.1⟩ = true → c.2.2 = z)
    (hz2 : ∀ c ∈ callsOf ρ2 (sitesF site2 k2), fxP ⟨c.1, c.2.1⟩ = true → c.2.2 = z)
    (hc : Tab.allB (fun i p => (portsJ p.1).all (fun b => okP p.1 p.2 b ||
      (idx2 ⟨p.1, b⟩).all (fun i2 => bad.contains (i, i2)))) key1 0 k1 = true)
    (hbad : AllL (fun q => (site1 q.1).cond ρ1 = true → (site2 q.2).cond ρ2 = true → False) bad) :
    ∀ j, ∀ a ∈ proj j (callsOf ρ1 (sitesF site1 k1)), ∀ b ∈ proj j (callsOf ρ2 (sitesF site2 k2)), (CI j).ann.Follow a b := by
  intro j ⟨a1, a2⟩ ha b hb
  obtain ⟨s, hs, hc1, ha2⟩ := mem_proj_callsOf (I := I) (CM := fun j => (CI j).M) ρ1 j a1 a2 (sitesF site1 k1) ha
  rw [List.mem_filter] at hs
  obtain ⟨i1, hi1, rfl⟩ := mem_sitesF hs.1
  have e1 : key1.get i1 = ⟨j, a1⟩ := (hkey1 i1).symm.trans ((onPort_iff (I := I) (CM := fun j => (CI j).M) (V := V) j a1 _).mp hs.2)
  obtain ⟨i2, hi2, hc2, -⟩ := mem_proj_fn (I := I) (CM := fun j => (CI j).M) ρ2 site2 k2 key2 hkey2 idx2 hchk2 j b.1 b.2 hb
  have h := Tab.allB_spec _ key1 0 k1 hc i1 (Nat.zero_le _) hi1
  rw [e1] at h
  have h' := List.all_eq_true.mp h b.1 (portsJ_ok j b.1)
  rw [Bool.or_eq_true] at h'
  rcases h' with h' | h'
  · exact okP_follow z j (a1, a2) b h' (fun hf => hz1 ⟨j, (a1, a2)⟩ (mem_proj j _ _ ha) hf)
      (fun hf => hz2 ⟨j, b⟩ (mem_proj j _ _ hb) hf)
  · have hm := List.contains_iff_mem.mp (List.all_eq_true.mp h' i2 hi2)
    exact (AllL.mem hbad _ hm hc1 hc2).elim
/-- `esc_of_pairs` with its table check needed only below `nR` (the escape table's last leaf may
    be a rule's). -/
theorem esc_of_pairsB (u : St → Nat → V) (ops : Ops V) (E : EqC V) (L : List (Nat × Nat))
    (hL : ∀ p ∈ L, ∀ oc w, Env (P ops E u) oc w → fire ops E oc w (rR p.1) = true → fire ops E oc w (rR p.2) = true →
      OkT (P ops E u) (trR (P ops E u) oc (rR p.1)).1 (trR (P ops E u) oc (rR p.2)).1)
    (hinj : ∀ p ∈ L, ∀ x1, x1 < D.nR → D.rc x1 = D.rc p.1 → x1 = p.1)
    (hT : ∀ i, i < D.nR → Sub (D.escR.get i) (bits ((L.filter (fun p => p.2 == i)).map (fun p => D.rc p.1)))) :
    ∀ oc w r1 r2, Env (P ops E u) oc w → (D.escR.get ((G ops E u).rk r2)).testBit (D.rc ((G ops E u).rk r1)) = true →
      fire ops E oc w r1 = true → fire ops E oc w r2 = true → OkT (P ops E u) (trR (P ops E u) oc r1).1 (trR (P ops E u) oc r2).1 := by
  intro oc w r1 r2 hE hb f1 f2
  obtain ⟨p, hp, e⟩ := List.mem_map.mp (mem_of_testBit_bits (hT _ r2.isLt _ hb))
  have hp' := List.mem_filter.mp hp
  have e2 : p.2 = r2.val := beq_iff_eq.mp hp'.2
  have e1 : r1.val = p.1 := hinj p hp'.1 _ r1.isLt e.symm
  have h := hL p hp'.1 oc w hE (by rw [← e1, rR_val]; exact f1) (by rw [e2, rR_val]; exact f2)
  rw [← e1, e2, rR_val, rR_val] at h
  exact h
''')
for n_e, (r1, r2, bad) in enumerate(ESC):
    hb = []
    for (i1, i2) in bad:
        fi = [f['r1'] == r1 and f['r2'] == r2 and f['i1'] == i1 and f['i2'] == i2 for f in EFACTS].index(True)
        f = EFACTS[fi]
        s1, s2 = M.rules[r1]['sites'][i1], M.rules[r2]['sites'][i2]
        args = ['f1', 'f2']
        c1 = Ctx('(u (.inr (rR %d)))' % r1, hf='f1')
        c2 = Ctx('(u (.inr (rR %d)))' % r2, hf='f2')
        if not is_uncond(s1):
            args.append('((%s).symm.trans h1)' % cond_val(r1, s1, c1))
        if not is_uncond(s2):
            args.append('((%s).symm.trans h2)' % cond_val(r2, s2, c2))
        src = 'hy' if f['ok'] else 'hr'
        hb.append('fun h1 h2 => %s.esc_%d oc %s' % (src, fi, ' '.join(args)))
    w('''theorem escChk_%d : Tab.allB (fun i p => (portsJ p.1).all (fun b => okP p.1 p.2 b ||
    (idxT (usesAt ⟨p.1, b⟩) %d).all (fun i2 => [%s].contains (i, i2)))) (keyT.get %d) 0 (kT.get %d) = true := by
  decide +kernel''' % (n_e, r2, ', '.join('(%d, %d)' % b for b in bad), r1, r1))
    w('''theorem esc_%d (ops : Ops V) (E : EqC V) %s (u : St → Nat → V) (oc : OC V) (w : %s)
    (hE : Env (P ops E u) oc w) (f1 : fire ops E oc w (rR %d) = true) (f2 : fire ops E oc w (rR %d) = true) :
    OkT (P ops E u) (trR (P ops E u) oc (rR %d)).1 (trR (P ops E u) oc (rR %d)).1 := by
  have hz1 := zR_ok ops E u oc (rR %d)
  have hz2 := zR_ok ops E u oc (rR %d)
  rw [trR_eq] at hz1 hz2 ⊢
  rw [trR_eq]
  exact okT_exceptZ ops.z _ _ (siteR ops E (rR %d).val) (siteR ops E (rR %d).val)
    (kR (rR %d).val) (kR (rR %d).val) (keyT.get (rR %d).val) (keyT.get (rR %d).val).get (fun _ => rfl) (fun _ => rfl)
    (idx (.inr (rR %d))) (hchk (rR %d)) [%s] hz1 hz2 escChk_%d
    %s''' % (n_e, HYP, WXM, r1, r2, r1, r2, r1, r2, r1, r2, r1, r2, r1, r2, r2, r2,
             ', '.join('(%d, %d)' % b for b in bad), n_e, allL(hb)))
if ESC:
    w('def escL : List (Nat × Nat) := [%s]' % ', '.join('(%d, %d)' % e for e in EPAIRS))
    stab('escMemT', 'Tab (List Nat)', ['[' + ', '.join(str(r1) for r1, _ in EPAIRS) + ']'])
    w('''theorem escInj : ∀ p ∈ escL, ∀ x1, x1 < D.nR → D.rc x1 = D.rc p.1 → x1 = p.1 := by
  have h := chains_inj D rinvT.get escMemT 1 (by decide +kernel) (by decide +kernel)
  intro p hp
  exact h 0 (by decide) p.1 (by revert p hp; decide)
theorem escTab : Tab.allB (fun i n => subB n (bits ((escL.filter (fun p => p.2 == i)).map (fun p => D.rc p.1)))) escRT 0 nR = true := by
  decide +kernel
theorem escT_ok : ∀ i, i < D.nR → Sub (D.escR.get i) (bits ((escL.filter (fun p => p.2 == i)).map (fun p => D.rc p.1))) :=
  fun i hi => sub_of_subB (Tab.allB_spec _ escRT 0 nR escTab i (Nat.zero_le _) hi)
''')
    esc_field = ('esc_of_pairsB u ops E escL (fun p hp => AllL.mem (show AllL (fun p : Nat × Nat => ∀ oc w, Env (P ops E u) oc w → '
                 'fire ops E oc w (rR p.1) = true → fire ops E oc w (rR p.2) = true → '
                 'OkT (P ops E u) (trR (P ops E u) oc (rR p.1)).1 (trR (P ops E u) oc (rR p.2)).1) escL from %s) p hp) escInj escT_ok'
                 % allL(['esc_%d ops E %s u' % (n_e, HYA) for n_e in range(len(ESC))]))
else:
    esc_field = '''by
    intro _ _ r1 r2 _ h
    have e : (D.escR.get ((G ops E u).rk r2)).testBit (D.rc ((G ops E u).rk r1)) = false := by
      show (escRT.get r2.val).testBit (rcT.get r1.val) = false
      exact Nat.testBit_zero _ _ ▸ (by decide +kernel : ∀ k, k < nR → escRT.get k = 0) _ r2.isLt ▸ rfl
    rw [e] at h; cases h'''
    esc_field = "fun _ _ r1 r2 _ h => by\n    have e : (D.escR.get ((G ops E u).rk r2)).testBit (D.rc ((G ops E u).rk r1)) = false := by\n      rw [show D.escR.get ((G ops E u).rk r2) = 0 from Tab.allB_spec (fun _ n => Nat.beq n 0) escRT 0 nR (by decide +kernel) r2.val (Nat.zero_le _) r2.isLt |> Nat.eq_of_beq_eq_true]\n      exact Nat.zero_testBit _\n    rw [e] at h; cases h"

meth_fields = ('''  mi_lt m := m.isLt
''' if MET else '''  mi_lt m := m.elim
''')
if MET:
    nf = '''  nfI m1 m2 h x y := by have e := nf_self nfIT nfOK_I m1 m2 h; subst e; exact notFollow_self m1 x y
  nfO m1 m2 h x y := by have e := nf_self nfOT nfOK_O m2 m1 h; subst e; exact notFollow_self _ x y
  ngI m1 m2 h x y := by have e := nf_self ngIT ngOK_I m1 m2 h; subst e; exact fun hg => notFollow_self _ x y hg.1
  ngO m1 m2 h x y := by have e := nf_self ngOT ngOK_O m2 m1 h; subst e; exact fun hg => notFollow_self _ x y hg.1
  h3ok m m' h := by simp [D, Tab.get] at h
  exM _ _ m1 m2 _ h := by simp [D, Tab.get] at h
'''
    w('''theorem nf_self (T : Tab Nat) (hT : Tab.allB (fun m2 n => (List.range nM).all (fun m1 => !n.testBit m1 || Nat.beq m1 m2)) T 0 nM = true)
    (m1 m2 : Fin nM) (h : (T.get m2.val).testBit m1.val = true) : m1 = m2 := by
  have h1 := List.all_eq_true.mp (Tab.allB_spec _ T 0 nM hT m2.val (Nat.zero_le _) m2.isLt) m1.val (List.mem_range.mpr m1.isLt)
  rw [h] at h1
  exact Fin.ext (Nat.eq_of_beq_eq_true (by simpa using h1))
theorem notFollow_self (m : Fin nM) (x y : V) : ¬ X.ann.Follow (m, x) (m, y) := fun hf => by
  have h := hf.2.1; simp [X, xAnn] at h
theorem nfOK_I : Tab.allB (fun m2 n => (List.range nM).all (fun m1 => !n.testBit m1 || Nat.beq m1 m2)) nfIT 0 nM = true := by decide +kernel
theorem nfOK_O : Tab.allB (fun m2 n => (List.range nM).all (fun m1 => !n.testBit m1 || Nat.beq m1 m2)) nfOT 0 nM = true := by decide +kernel
theorem ngOK_I : Tab.allB (fun m2 n => (List.range nM).all (fun m1 => !n.testBit m1 || Nat.beq m1 m2)) ngIT 0 nM = true := by decide +kernel
theorem ngOK_O : Tab.allB (fun m2 n => (List.range nM).all (fun m1 => !n.testBit m1 || Nat.beq m1 m2)) ngOT 0 nM = true := by decide +kernel
/-- No method calls a chain (task) port. -/
theorem cinM_ok (ops : Ops V) (E : EqC V) (u : St → Nat → V) (oc : OC V) (w : Wires (Fin nM) V) (m : Fin nM) :
    ∀ c ∈ (trM (P ops E u) oc w m).1, (G ops E u).ch c.1 = true →
      (G ops E u).clo ((G ops E u).mi m) ≤ (G ops E u).pos c.1 c.2.1 ∧ (G ops E u).pos c.1 c.2.1 < (G ops E u).chi ((G ops E u).mi m) := by
  rw [trM_eq]
  have hc := chain_in_of_sites (G ops E u) (envS ops E (u (.inl m)) oc (nR + m.val) (w.arg m)) _ _ _ (chainS ops E u (nR + m.val) (mlt m) (w.arg m)).1
  have e1 : (bif Nat.blt (nR + m.val) nR then stF (nR + m.val) else 0) = 0 := by
    rw [show Nat.blt (nR + m.val) nR = false from Bool.eq_false_iff.mpr (fun h' => absurd (Nat.blt_eq.mp h') (Nat.not_lt.mpr (Nat.le_add_right nR m.val)))]; rfl
  have e2 : (bif Nat.blt (nR + m.val) nR then stF (nR + m.val + 1) else 0) = 0 := by
    rw [show Nat.blt (nR + m.val) nR = false from Bool.eq_false_iff.mpr (fun h' => absurd (Nat.blt_eq.mp h') (Nat.not_lt.mpr (Nat.le_add_right nR m.val)))]; rfl
  intro c hcm hch
  have := hc c hcm hch
  rw [e1, e2] at this
  omega
theorem cincM_ok (ops : Ops V) (E : EqC V) (u : St → Nat → V) (oc : OC V) (w : Wires (Fin nM) V) (m : Fin nM) (j : I)
    (a b : (CI j).M × V) (hj : (G ops E u).ch j = true) (hab : [a, b] <+ proj j (trM (P ops E u) oc w m).1) :
    (G ops E u).pos j a.1 < (G ops E u).pos j b.1 := by
  have ha := mem_proj j _ a (hab.subset (List.mem_cons_self ..))
  have := (cinM_ok ops E u oc w m ⟨j, a⟩ ha hj).2
  simp only [G, chiG, show m.val < D.nM from m.isLt, if_true] at this
  omega
''')
else:
    nf = '''  nfI m := m.elim
  nfO m := m.elim
  ngI m := m.elim
  ngO m := m.elim
  h3ok m := m.elim
  exM _ _ m := m.elim
'''

w('''
theorem glueOK (ops : Ops V) (E : EqC V) %s (u : St → Nat → V) : GlueOK (P ops E u) D (G ops E u) (fire ops E) where
''' % HYP + meth_fields + '''  rk_lt r := r.isLt
  rk_at r := rR_val r
  at_rk i hi := Nat.mod_eq_of_lt hi
  kind j h := by
    obtain ⟨κ, i⟩ := j
    cases κ
''' + kcase(lambda kd: 'exact kind_%s _' % kd, 'simp [G, chB] at h') + '''  kindC j h := by
    obtain ⟨κ, i⟩ := j
    cases κ
''' + kcase(lambda kd: 'simp [G, chB] at h',
              'exact kindOut_chain (fun _ => rfl) (by show (0 : Nat) ∈ [0]; simp) (by show (0 : Nat) ∈ [0]; simp)') + '''  chAnn j h a b hlt := by
    obtain ⟨κ, i⟩ := j
    cases κ
''' + kcase(lambda kd: 'simp [G, chB] at h', 'exact taskAnn_group hlt') + '''  cmono g1 g2 h := cmonoG g1 g2 h
  cinM ''' + ('oc w m := cinM_ok ops E u oc w m' if MET else '_ _ m := m.elim') + '''
  cinR oc r := by
    rw [trR_eq]
    have hc := chain_in_of_sites (G ops E u) (envR ops E (u (.inr r)) oc r.val) _ _ _ (chainS ops E u r.val (rlt r) ops.z).1
    have e0 : Nat.blt r.val nR = true := Nat.blt_eq.mpr r.isLt
    simp only [e0, cond_true] at hc
    intro c hcm hch
    have := hc c hcm hch
    show cloG (D.nM + r.val) ≤ _ ∧ _ < chiG (D.nM + r.val)
    rw [cloG_r, chiG_r]
    exact this
  cincM ''' + ('oc w m j a b hj hab := cincM_ok ops E u oc w m j a b hj hab' if MET else '_ _ m := m.elim') + '''
  cincR oc r := by
    rw [trR_eq]
    exact chain_inc_of_sites (G ops E u) _ _ (chainS ops E u r.val (rlt r) ops.z).2
  chCode j h p := by
    obtain ⟨κ, i⟩ := j
    cases κ
''' + kcase(lambda kd: 'simp [G, chB] at h', 'rfl') + '''  fpM ''' + ('oc w m := fpM_ok ops E u oc w m' if MET else '_ _ m := m.elim') + '''
  fpR oc r := fpR_ok ops E u oc r
  zM ''' + ('oc w m := zM_ok ops E u oc w m' if MET else '_ _ m := m.elim') + '''
  zR oc r := zR_ok ops E u oc r
''' + nf + '''  exR := exR_of_chains (P ops E u) (G ops E u) (fire ops E) (fun r => r.isLt) rR_val memT gidT ng
    (chT ops E u ''' + HYA + ''') hg_ok hinj_ok hT_ok
  exMR ''' + ('_ _ m r _ h := by simp [D, Tab.get] at h' if MET else '_ _ m := m.elim') + '''
  esc := ''' + esc_field + '''

/-- **The pair fields, from the scan.** -/
def pairs (ops : Ops V) (E : EqC V) %s (u : St → Nat → V) : PairFacts (P ops E u) (G ops E u).rules (fire ops E) :=
  pairFacts_of_scan (P ops E u) scanOK (G ops E u) (fire ops E) (glueOK ops E %s u)
end
''' % (HYP, HYA))
mark('glue')

# ---------------------------------------------------------------- EAQ per instance
w('/-! ## Enables and arguments: one lemma per child instance, dispatched by table -/\n')
w('''section
variable {V : Type}
/-- What `Cert.en` and `Cert.arg` say about child `⟨κ, i⟩` whose arm is `A`. -/
def EAQ (ops : Ops V) (E : EqC V) %s (u : St → Nat → V) (κ : Kind) (i : Nat)
    (A : (V : Type) → Ops V → EqC V → OC V → %s → Wires (KI κ).M V) : Prop :=
  ∀ (h : i < nk κ) (oc : OC V) (w : %s), Env (P ops E u) oc w →
    (∀ cm : (KI κ).M, (KI κ).ann.hasEn cm = true →
      ((A V ops E oc w).en cm = true ↔ ∃ s ∈ users (⟨κ, ⟨i, h⟩⟩ : I) cm, On (P ops E u) (fire ops E) oc w s ∧
        Calls (P ops E u) (trS (P ops E u) oc w s) (⟨κ, ⟨i, h⟩⟩ : I) cm)) ∧
    (∀ cm : (KI κ).M, ∀ s ∈ users (⟨κ, ⟨i, h⟩⟩ : I) cm, Av (P ops E u) (fire ops E) oc w s →
      ∀ x, (cm, x) ∈ proj (⟨κ, ⟨i, h⟩⟩ : I) (trS (P ops E u) oc w s) → (A V ops E oc w).arg cm = x)

/-- A flagged (argument-less value) port's calls carry `z`. -/
theorem zarg (ops : Ops V) (E : EqC V) (u : St → Nat → V) (oc : OC V) (w : %s) (j : I) (cm : (CI j).M)
    (hf : fxP ⟨j, cm⟩ = true) :
    ∀ s ∈ users j cm, Av (P ops E u) (fire ops E) oc w s → ∀ x, (cm, x) ∈ proj j (trS (P ops E u) oc w s) → ops.z = x := by
  intro s _ _ x hx
  cases s with
  | inl e => %s
  | inr r =>
    have hc := mem_proj j _ _ hx
    exact (zR_ok ops E u oc r ⟨j, (cm, x)⟩ hc hf).symm
''' % (HYP, WXM, WXM, WXM, 'exact e.elim' if not MET else 'exact (zM_ok ops E u oc w e ⟨j, (cm, x)⟩ (mem_proj j _ _ hx) hf).symm'))

HXS = {}


def hx(kd, i, m, lp, k, si):
    nm = 'hx_%d_%d_%s' % (k, si, m)
    if k < nR:
        HXS[nm] = 'idxT (((usesK .%s).get %d).uses %s) (rR %d).val = [%d]' % (kd, i, lp, k, si)
    else:
        HXS[nm] = 'idxT (((usesK .%s).get %d).uses %s) (nR + (mM %d).val) = [%d]' % (kd, i, lp, k - nR, si)
    return nm


def uses_leaves(p):
    by = {}
    for k, i in U.get(p, []):
        by.setdefault(k, []).append(i)
    return [(k, by[k]) for k in sorted(by)]


BIGPORT = int(os.environ.get('E2E_BIGPORT', '8'))


def users_proof(p, leaf):
    lv = uses_leaves(p)
    return 'users_all\n      %s' % tabpair(['fun _ => %s' % leaf(k, sts) for k, sts in lv], '      ')


def en_proof(p, inst):
    kd, i, m = p
    lp = K[kd].lports[K[kd].code(m)]
    lv = uses_leaves(p)
    if not lv:
        return 'en_none (P ops E u) (fire ops E) oc w _ _ _ rfl _ rfl'
    simple = (not MET) and all(is_uncond(M.rules[k]['sites'][sts[0]]) for k, sts in lv)
    if simple:
        leaf = lambda k, sts: ('cRk ops E u oc w (rR %d) .%s %d h %s %d %s rfl rfl rfl'
                               % (k, kd, i, lp, sts[0], hx(kd, i, m, lp, k, sts[0])))
        if len(lv) <= BIGPORT:
            return ('en_list (P ops E u) (fire ops E) oc w %s %s (stepOn ops E oc) (onIff ops E u oc w)\n'
                    '      (users %s %s) %s' % (inst, lp, inst, lp, allL([leaf(k, sts) for k, sts in lv])))
        return ('en_lit (P ops E u) (fire ops E) oc w %s %s (stepOn ops E oc) (onIff ops E u oc w)\n'
                '      (users %s %s) _ rfl (%s)' % (inst, lp, inst, lp, users_proof(p, leaf)))
    ents_, pfs = [], []
    for k, sts in lv:
        si = sts[0]
        st = M.rules[k]['sites'][si]
        ents_.append('(%s, %s)' % ('.inr (rR %d)' % k if k < nR else '.inl (mM %d)' % (k - nR), entry(k, st)))
        h = hx(kd, i, m, lp, k, si)
        if k < nR:
            if is_uncond(st):
                pfs.append('en_unc Iff.rfl (fun hf => cRk ops E u oc w (rR %d) .%s %d h %s %d %s rfl rfl rfl)' % (k, kd, i, lp, si, h))
            else:
                cv = cond_val(k, st, Ctx('(u (.inr (rR %d)))' % k, hf='hf'))
                pfs.append('en_cnd Iff.rfl (fun hf => (cRkIff ops E u oc w (rR %d) .%s %d h %s %d %s rfl rfl).trans (iff_of_eqB (%s)))'
                           % (k, kd, i, lp, si, h, cv))
        else:
            assert is_uncond(st), 'conditional method call'
            pfs.append('en_unc Iff.rfl (fun hf => cMk ops E u oc w (mM %d) .%s %d h %s %d %s rfl rfl rfl)' % (k - nR, kd, i, lp, si, h))
    us_ = '[%s]' % ', '.join(e.split(',')[0][1:] for e in ents_)
    return ('by\n      rw [show users %s %s = %s from rfl]\n      exact en_pairs (P ops E u) (fire ops E) oc w %s %s [%s]\n        %s'
            % (inst, lp, us_, inst, lp, ', '.join(ents_), allL(pfs)))


def arg_proof(p, inst):
    pi = portinfo.get(p, {})
    kd, i, m = p
    lp = K[kd].lports[K[kd].code(m)]
    if K[kd].fx(m):
        return 'zarg ops E u oc w _ _ rfl'
    lv = uses_leaves(p)
    if not lv:
        return 'fun s hs => by cases hs'
    argof = {k: si for k, si in pi.get('users', [])}

    def leaf(k, sts):
        si = sts[0]
        st = M.rules[k]['sites'][si]
        h = hx(kd, i, m, lp, k, si)
        if k < nR:
            av = arg_val(k, st, Ctx('(u (.inr (rR %d)))' % k, hf='hav'))
            core = 'aRk ops E u oc w (rR %d) .%s %d h %s %d %s _ (fun _ => %s) x hx' % (k, kd, i, lp, si, h, av)
        else:
            av = arg_val(k, st, Ctx('(u (.inl (mM %d)))' % (k - nR), 'w.arg (mM %d)' % (k - nR), '(cf_of_avail hav)'))
            core = 'aMk ops E u oc w (mM %d) .%s %d h %s %d %s _ (fun _ => %s) x hx' % (k - nR, kd, i, lp, si, h, av)
        if pi.get('argk') == 'mux':
            sel = 'hav'
            if SELMUX.get(pi['mux']) and not is_uncond(st):
                cv = cond_val(k, st, Ctx('(u (.inr (rR %d)))' % k, hf='hav'))
                sel = ('((Bool.and_eq_true _ _).mpr ⟨hav, (%s).symm.trans (aRkC ops E u oc w (rR %d) .%s %d h %s %d %s x hx)⟩)'
                       % (cv, k, kd, i, lp, si, h))
            return ('fun hav x hx => (arg_muxT ops.z (chain_%d ops E %s oc hE.1) (p := %d) rfl %s).trans\n        (%s)'
                    % (pi['mux'], HYA, pi['mpos'][k], sel, core))
        return 'fun hav x hx => %s' % core
    if len(lv) <= BIGPORT:
        return ('arg_users (P ops E u) (fire ops E) oc w %s %s _ (users %s %s)\n      %s'
                % (inst, lp, inst, lp, allL([leaf(k, sts) for k, sts in lv])))
    return users_proof(p, leaf)


if nT:
    w('/-- The task statements\' child. -/\nabbrev jT : I := ⟨.tasks, ⟨0, by decide⟩⟩')
    w('''/-- Task statement `t`'s enable and argument facts (its one user). -/
def TQ (ops : Ops V) (E : EqC V) %s (u : St → Nat → V) (t : Nat) (f : (V : Type) → Ops V → EqC V → OC V → Bool) : Prop :=
  ∀ (oc : OC V) (w : %s), Env (P ops E u) oc w →
    (f V ops E oc = true ↔ ∃ s ∈ users jT (tP t), On (P ops E u) (fire ops E) oc w s ∧
      Calls (P ops E u) (trS (P ops E u) oc w s) jT (tP t)) ∧
    (∀ s ∈ users jT (tP t), Av (P ops E u) (fire ops E) oc w s →
      ∀ x, (tP t, x) ∈ proj jT (trS (P ops E u) oc w s) → (targT.get t) V ops E oc = x)
''' % (HYP, WXM))

    def tq_leaf(t):
        k, i0, st = tuser[t]
        if st['hw'][0] == 'chk':
            enp = 'en_tchk (fun hw => ((Bool.and_eq_true _ _).mp (cf_of_wf hw)).2)'
        elif is_uncond(st):
            enp = 'en_tk rfl'
        else:
            enp = 'en_cnd Iff.rfl (fun hf => iff_of_eqB (%s))' % cond_val(k, st, Ctx('(u (.inr (rR %d)))' % k, hf='hf'))
        av = arg_val(k, st, Ctx('(u (.inr (rR %d)))' % k, hf='hav'))
        return ('(have hx : idxT (((usesK .tasks).get 0).uses (tP %d)) (rR %d).val = [%d] := by decide +kernel\n'
                '    have hu : users jT (tP %d) = [.inr (rR %d)] := by decide +kernel\n'
                '    fun oc w hE => ⟨en_one (P ops E u) (fire ops E) oc w jT (tP %d) (.inr (rR %d)) _ hu _\n'
                '      (cRkIff ops E u oc w (rR %d) .tasks 0 (by decide) (tP %d) %d hx rfl rfl) _ (%s),\n'
                '    arg_users (P ops E u) (fire ops E) oc w jT (tP %d) _ _ ⟨fun hav x hx\' =>\n'
                '      aRk ops E u oc w (rR %d) .tasks 0 (by decide) (tP %d) %d hx _ (fun _ => %s) x hx\', trivial⟩⟩)'
                % (t, k, i0, t, k, t, k, k, t, i0, enp, t, k, t, i0, av))
    w('theorem tqAll (ops : Ops V) (E : EqC V) %s (u : St → Nat → V) : Tab.AllR (TQ ops E %s u) tenT 0 nT :=\n  %s'
      % (HYP, HYA, stabproof('tenT', 'tqAll', '(ops : Ops V) (E : EqC V) %s (u : St → Nat → V)' % HYP, 'ops E %s u' % HYA,
                             lambda nm, lo, hi: 'Tab.AllR (TQ ops E %s u) %s %d %d' % (HYA, nm, lo, hi), [tq_leaf(t) for t in range(nT)])))
    w('''theorem ea_tasks_0 (ops : Ops V) (E : EqC V) %s (u : St → Nat → V) : EAQ ops E %s u .tasks 0 arm_tasks_0 := by
  intro h oc w hE
  refine ⟨fun cm _ => ?_, fun cm => ?_⟩
  · obtain ⟨t, ht⟩ := cm
    have e : (⟨t, ht⟩ : Fin nT) = tP t := Fin.ext (Nat.mod_eq_of_lt ht).symm
    have := (Tab.get_allR _ 0 nT (tqAll ops E %s u) t (Nat.zero_le _) ht oc w hE).1
    rw [← e] at this
    exact this
  · obtain ⟨t, ht⟩ := cm
    have e : (⟨t, ht⟩ : Fin nT) = tP t := Fin.ext (Nat.mod_eq_of_lt ht).symm
    have := (Tab.get_allR _ 0 nT (tqAll ops E %s u) t (Nat.zero_le _) ht oc w hE).2
    rw [← e] at this
    exact this
''' % (HYP, HYA, HYA, HYA))

ALL = {'reg': 'regAll', 'cfg': 'cfgAll', 'fifo': 'flAll', 'f2': 'f2All', 'sf': 'f2All', 'rf': 'rfAll',
       'rwS': 'wireAll', 'rwC': 'wireAll', 'rw0S': 'wire0All', 'rw0C': 'wire0All'}
for kd in M.kinds:
    if kd == 'tasks':
        continue
    for i in range(len(M.byk[kd])):
        inst = '⟨.%s, ⟨%d, h⟩⟩' % (kd, i)
        ens, args = [], []
        HXS.clear()
        for m in K[kd].ports:
            p = (kd, i, m)
            if K[kd].has_en(m):
                ens.append('fun _ => %s' % en_proof(p, inst))
            else:
                ens.append('fun hh => by cases hh')
            args.append(arg_proof(p, inst))
        w('theorem ea_%s_%d (ops : Ops V) (E : EqC V) %s (u : St → Nat → V) : EAQ ops E %s u .%s %d arm_%s_%d := by\n'
          % (kd, i, HYP, HYA, kd, i, kd, i) + ''.join('  have %s : %s := by decide +kernel\n' % (nm, st_) for nm, st_ in HXS.items()) +
          '  intro h oc w hE\n'
          '  refine ⟨%s (%s), %s (%s)⟩'
          % (ALL[kd], ')\n    ('.join(ens), ALL[kd], ')\n    ('.join(args)))
for kd in M.kinds:
    n = len(M.byk[kd])
    if kd == 'tasks':
        w('theorem eaAll_tasks (ops : Ops V) (E : EqC V) %s (u : St → Nat → V) : Tab.AllR (EAQ ops E %s u .tasks) arms_tasks 0 1 :=\n'
          '  Tab.one (ea_tasks_0 ops E %s u)' % (HYP, HYA, HYA))
        continue
    w('theorem eaAll_%s (ops : Ops V) (E : EqC V) %s (u : St → Nat → V) : Tab.AllR (EAQ ops E %s u .%s) arms_%s 0 %d :=\n  %s'
      % (kd, HYP, HYA, kd, kd, n, tabproof(['ea_%s_%d ops E %s u' % (kd, i, HYA) for i in range(n)])))
w('''theorem eaAll (ops : Ops V) (E : EqC V) %s (u : St → Nat → V) (κ : Kind) (i : Nat) (h : i < nk κ) :
    EAQ ops E %s u κ i ((arms κ).get i) := by
  cases κ
''' % (HYP, HYA) + ''.join('  · exact Tab.get_allR _ 0 _ (eaAll_%s ops E %s u) i (Nat.zero_le _) h\n' % (kd, HYA) for kd in M.kinds) + 'end\n')
mark('ea')

# ---------------------------------------------------------------- the certificate
if NEEDUC:
    rdyR_body = '''intro oc w r hE hf c hc ha
  rw [trR_eq] at hc
  exact rdyS ops E %s oc r.val (rlt r) _ ops.z hE.2.1 (pre_r hf) c hc ha''' % HYA
    rdyM_body = '''intro oc w m hE hav c hc ha
  rw [trM_eq] at hc
  exact rdyS ops E %s oc (nR + m.val) (mlt m) _ (w.arg m) hE.2.1 (pre_m (cf_of_avail hav)) c hc ha''' % HYA
else:
    rdyR_body = '''intro oc w r _ hf c hc ha
  rw [trR_eq] at hc
  obtain ⟨i, hi, e⟩ := mem_keys (callsOf_fn_ports _ _ _ c hc)
  have hrd := rd_of_cf (cf_of_wf hf)
  have h2 := List.all_eq_true.mp hrd i (List.mem_range'.mpr ⟨i, hi, by omega⟩)
  rw [e] at h2
  simp only [alwaysRdyB, Bool.or_eq_true] at h2
  rcases h2 with h2 | h2
  · rw [ha] at h2; cases h2
  · exact h2'''
    rdyM_body = '''intro oc w m hE hav c hc ha
  rw [trM_eq] at hc
  obtain ⟨i, hi, e⟩ := mem_keys (callsOf_fn_ports _ _ _ c hc)
  have hrd := rd_of_cf (cf_of_avail hav)
  have h2 := List.all_eq_true.mp hrd i (List.mem_range'.mpr ⟨i, hi, by omega⟩)
  rw [e] at h2
  simp only [alwaysRdyB, Bool.or_eq_true] at h2
  rcases h2 with h2 | h2
  · rw [ha] at h2; cases h2
  · exact h2'''
w('''/-! ## The certificate and the obligation -/

section
variable {V : Type}
def fp (ops : Ops V) (E : EqC V) : St → List (Σ j : I, (CI j).M)
''' + ('''  | .inl e => e.elim
''' if not MET else '''  | .inl m => (sitesF (siteS ops E (nR + m.val) ops.z) (kR (nR + m.val))).map (fun s => ⟨s.j, s.m⟩)
''') + '''  | .inr r => (sitesF (siteR ops E r.val) (kR r.val)).map (fun s => ⟨s.j, s.m⟩)

theorem usersOk (ops : Ops V) (E : EqC V) : ∀ (s : St), ∀ p ∈ fp ops E s, s ∈ users p.1 p.2 := by
  intro s p hp
  cases s with
''' + ('''  | inl e => exact e.elim
''' if not MET else '''  | inl m =>
    obtain ⟨i, hi, rfl⟩ := mem_keys hp
    exact mem_users (hchkM m i hi)
''') + '''  | inr r =>
    obtain ⟨i, hi, rfl⟩ := mem_keys hp
    exact mem_users (hchk r i hi)

theorem fpOk (ops : Ops V) (E : EqC V) (u : St → Nat → V) : ∀ (oc : OC V) (w : %s) (s : St),
    ∀ c ∈ trS (P ops E u) oc w s, (⟨c.1, c.2.1⟩ : Σ j : I, (CI j).M) ∈ fp ops E s := by
  intro oc w s c hc
  cases s with
''' % WXM + ('''  | inl e => exact e.elim
''' if not MET else '''  | inl m =>
    rw [trSM_eq] at hc
    have h := callsOf_fn_ports _ _ _ c hc
    rw [sitesF_map] at h
    show _ ∈ (sitesF (siteS ops E (nR + m.val) ops.z) (kR (nR + m.val))).map (fun s => (⟨s.j, s.m⟩ : Σ j : I, (CI j).M))
    rw [sitesF_map]
    exact h
''') + '''  | inr r => rw [trS_eq] at hc; exact callsOf_fn_ports _ _ _ c hc

theorem rdyR_ok (ops : Ops V) (E : EqC V) %s (u : St → Nat → V) : ∀ (oc : OC V) (w : %s) (r : Fin nR),
    Env (P ops E u) oc w → fire ops E oc w r = true → ∀ c ∈ (trR (P ops E u) oc r).1,
      ((P ops E u).CI c.1).ann.alwaysRdy c.2.1 = false → (oc c.1).rdy c.2.1 = true := by
  %s
''' % (HYP, WXM, rdyR_body) + ('''theorem rdyM_ok (ops : Ops V) (E : EqC V) %s (u : St → Nat → V) : ∀ (oc : OC V) (w : Wires (Fin nM) V) (m : Fin nM),
    Env (P ops E u) oc w → Avail (P ops E u) oc w m → ∀ c ∈ (trM (P ops E u) oc w m).1,
      ((P ops E u).CI c.1).ann.alwaysRdy c.2.1 = false → (oc c.1).rdy c.2.1 = true := by
  %s
''' % (HYP, rdyM_body) if MET else '') + '''
theorem onehot_ok (ops : Ops V) (E : EqC V) %s (u : St → Nat → V) (oc : OC V) (w : %s)
    (hC : EnvC (P ops E u) oc w) : onehotP ops E oc :=
  ''' % (HYP, WXM) + allL(['chain_%d ops E %s oc hC.1' % (q, HYA) for q in range(len(muxes))]) + '''

def cert (ops : Ops V) (E : EqC V) %s (u : St → Nat → V) : Cert (P ops E u) :=
  PairFacts.toCert (P ops E u) (pairs ops E %s u) (fp ops E) users (usersOk ops E) (fpOk ops E u)
''' % (HYP, HYA) + ('''    (fun m => m.elim) (fun _ _ m => m.elim)
''' if not MET else '''    (fun m => Or.inl (decide_eq_true rfl)) (fun oc w m _ hav => resM_ok ops E %s u oc w m hav)
''' % HYA) + '''    (fun oc w r _ hf => resR_ok ops E %s u oc w r hf)
''' % HYA + ('''    (fun _ _ m => m.elim) (rdyR_ok ops E %s u)
''' % HYA if not MET else '''    (rdyM_ok ops E %s u) (rdyR_ok ops E %s u)
''' % (HYA, HYA)) + '''    (fun oc w j cm hE hcm => by obtain ⟨κ, i, hi⟩ := j; exact (eaAll ops E %s u κ i hi hi oc w hE).1 cm hcm)
''' % HYA + ('''    (fun _ _ m => m.elim)
''' if not MET else '''    (fun _ _ m _ h => by simp [X, xAnn] at h)
''') + '''    (fun oc w j cm hE s hs hav x hx => by obtain ⟨κ, i, hi⟩ := j; exact (eaAll ops E %s u κ i hi hi oc w hE).2 cm s hs hav x hx)
    (fun oc w j cm _ h => by obtain ⟨κ, i⟩ := j; cases κ <;> cases cm <;> cases h)
    (onehot_ok ops E %s u)
''' % (HYA, HYA) + ('''theorem cf_of_availB {ops : Ops V} {E : EqC V} {oc : OC V} {w : Wires (Fin nM) V} {m : Fin nM}
    (h : Avail (F ops E).base oc w m) : CF ops E oc (nR + m.val) = true :=
  h.1.resolve_left (fun h' => by cases h')
''' if MET else '') + '''
/-- **The certificate fixed before the skipped values**: rules, firing and hits do not mention
    `u`, and a step's calls are the same at every `u` (`trUS`). -/
def certU (ops : Ops V) (E : EqC V) %s : CertU (F ops E) where
  rules := (G ops E (fun _ _ => ops.z)).rules
  fire := fire ops E
  hit := (pairs ops E %s (fun _ _ => ops.z)).hit
  cert u := cert ops E %s u
  rules_eq _ := rfl
  fire_eq _ := rfl
  hit_eq _ := rfl
  trU oc w s u u' _ hav := by
    cases s with
''' % (HYP, HYA, HYA) + ('''    | inl e => exact e.elim
''' if not MET else '''    | inl m => exact (trSM_eq ops E u oc w m).trans ((trUS ops E %s oc (nR + m.val) (mlt m) _ _ (w.arg m)
        (pre_m (cf_of_availB hav))).trans (trSM_eq ops E u' oc w m).symm)
''' % HYA) + '''    | inr r => exact (trS_eq ops E u oc w r).trans ((trUS ops E %s oc r.val (rlt r) _ _ ops.z (pre_r hav)).trans
        (trS_eq ops E u' oc w r).symm)

/-- **The module's obligation**: the order and every child's calls are fixed before the skipped
    values; for every value type, operators, constants and solver verdicts. -/
theorem obl [Inhabited V] (ops : Ops V) (E : EqC V) %s : OblU (F ops E) :=
  oblU_of_cert (F ops E) (certU ops E %s)
end
''' % (HYA, HYP, HYA))
mark('cert')

# ---------------------------------------------------------------- Acyc
w('/-! ## Acyclicity: read lists as filters of allOuts -/\n')
path_inputs = {'fifo': ['enq', 'deq'], 'rwS': ['wset'], 'rwC': ['wset'], 'rw0S': ['wset'], 'rw0C': ['wset'],
               'rf': ['s%d' % c for c in range(model.RF_COPIES)]}
path_outs = {'fifo': ['enq', 'notFull', 'i_notFull', 'clear'], 'rwS': ['wget', 'whas'], 'rwC': ['wget', 'whas'],
             'rw0S': ['whas'], 'rw0C': ['whas'], 'rf': ['s%d' % c for c in range(model.RF_COPIES)]}
rank = {}
NOACYC = False
agneed = set()


def prk(p):
    kd, i, m = p
    if kd == 'fifo':
        b1, b2 = rank.get((kd, i), (0, 0))
        return b2 if m == 'clear' else (b1 if m in ('enq', 'notFull', 'i_notFull') else 0)
    if kd in path_outs and m in path_outs[kd]:
        return rank.get((kd, i), 0)
    return 0


def bterm(kd, m):
    if kd == 'fifo':
        return '(bnd_fifo.get i).2' if m == 'enq' else '(bnd_fifo.get i).1'
    return 'bnd_%s.get i' % kd


def ereads(k, e):
    t = e[0]
    if t in ('f', 'g'):
        return [site_port(k, j) for j in e[2]]
    if t == 'lit':
        return [site_port(k, e[1])]
    if t == 'split':
        out = []
        for i in e[1]:
            out += [site_port(k, j) for j in M.rules[k]['sites'][i]['R'][2]]
        return out + ereads(k, e[2]) + ereads(k, e[3])
    return []


def wf_reads(k):
    r = M.rules[k]
    out = [st['port'] for st in r['sites'] if M.is_rdy_guarded(st['port'])]
    if r['guard'][0] not in ('check', 'false'):
        out += ereads(k, r['guard'])
    for i in RDC[k]:
        out += ereads(k, r['sites'][i]['cond'])
    for b in M.blk.get(k, []):
        out += wf_reads(b)
    return out


def input_reads(p):
    pi = portinfo.get(p)
    if not pi or not pi['users']:
        return []
    out = []
    for t, (k, si) in enumerate(pi['users']):
        if K[p[0]].has_en(p[2]) or pi['argk'] == 'mux':
            out += wf_reads(k)
        st = M.rules[k]['sites'][si]
        if pi['argk'] != 'none':
            out += ereads(k, st['arg'])
        if (K[p[0]].has_en(p[2]) or pi['argk'] == 'mux') and not is_uncond(st):
            out += ereads(k, st['cond'])
    return out


for it in range(1000):
    changed = False
    for kd in path_inputs:
        if kd not in M.kinds:
            continue
        for i in range(len(M.byk[kd])):
            fan = {}
            for m in path_inputs[kd]:
                fan[m] = max([-1] + [prk(y) for y in input_reads((kd, i, m))])
            if kd == 'fifo':
                b1 = fan['deq'] + 1
                b = (b1, max(b1, fan['enq'] + 1))
            else:
                b = max(fan.values()) + 1
            if rank.get((kd, i), (0, 0) if kd == 'fifo' else 0) != b:
                rank[(kd, i)] = b
                changed = True
    if not changed:
        break
else:
    rank = {}
    NOACYC = True
NN = max([0] + [max(v) if isinstance(v, tuple) else v for v in rank.values()]) + 1


if NOACYC:
    w('end\nend %s\n\n#print axioms %s.obl\n' % (NAME, NAME))
    L_KEEP = L
    L = []
    w = L.append
def mxW(k):
    return max([0] + [prk(y) for y in wf_reads(k)])


def mxRD(k):
    return max([0] + [prk(st['port']) for st in M.rules[k]['sites'] if M.is_rdy_guarded(st['port'])])


w(('''def fifoRk (b : Nat × Nat) : FL → Nat := fun m => match m with
  | .enq | .notFull | .i_notFull => b.1
  | .clear => b.2
  | _ => 0
''' if 'fifo' in M.kinds else '') + ('''def rfRk (b : Nat) : RFM → Nat := fun m => match m with
  | .upd => 0
  | _ => b
''' if 'rf' in M.kinds else '') + '''def wireRk (b : Nat) : WireM → Nat := fun m => match m with
  | .wget | .whas => b
  | _ => 0
def wire0Rk (b : Nat) : Wire0M → Nat := fun m => match m with
  | .whas => b
  | _ => 0
''')
RKF = {'fifo': 'fifoRk', 'rwS': 'wireRk', 'rwC': 'wireRk', 'rw0S': 'wire0Rk', 'rw0C': 'wire0Rk', 'rf': 'rfRk'}
for kd in M.kinds:
    if kd in RKF:
        if kd == 'fifo':
            w('def bnd_fifo : Tab (Nat × Nat) :=\n  %s' % tab(['(%d, %d)' % rank.get(('fifo', i), (0, 0)) for i in range(len(M.byk[kd]))]))
        else:
            w('def bnd_%s : Tab Nat :=\n  %s' % (kd, tab([nat(rank.get((kd, i), 0)) for i in range(len(M.byk[kd]))])))
w('def rkK : (κ : Kind) → Nat → (KI κ).M → Nat\n' + ''.join(
    ('  | .%s => fun i m => %s (bnd_%s.get i) m\n' % (kd, RKF[kd], kd)) if kd in RKF else ('  | .%s => fun _ _ => 0\n' % kd)
    for kd in M.kinds))
w('def rk (y : Σ j : I, (CI j).M) : Nat := rkK y.1.1 y.1.2.val y.2')
w('def mxRDT : Tab Nat :=\n  %s' % tab([nat(mxRD(k)) for k in range(nS)]))
w('''def allOuts : List (Σ j : I, (CI j).M) := allPorts [''' + ', '.join('.' + k for k in M.kinds) + '''] nk (fun κ => (KI κ).M) portsK
theorem mem_allOuts (y : Σ j : I, (CI j).M) : y ∈ allOuts :=
  mem_allPorts _ nk _ portsK (fun κ => by cases κ <;> simp) (fun κ m => by
    cases κ
''' + ''.join(('    · exact ports_%s m\n' % kd) for kd in M.kinds) + '''  ) y
''' + ('''def allIns : List Empty := []
theorem mem_allIns (p : Empty) : p ∈ allIns := p.elim
''' if not MET else '''def allIns : List (Fin nM) := List.finRange nM
theorem mem_allIns (p : Fin nM) : p ∈ allIns := List.mem_finRange p
''') + '''def outsBelow (r : Nat) : List (Σ j : I, (CI j).M) := allOuts.filter (fun y => Nat.blt (rk y) r)
def DK : (κ : Kind) → Nat → (KI κ).M → List (Σ j : I, (CI j).M)
''')
for kd in M.kinds:
    if kd == 'fifo':
        w('  | .fifo => fun i m => match m with | .enq => outsBelow (bnd_fifo.get i).2 | .deq => outsBelow (bnd_fifo.get i).1 | _ => allOuts')
    elif kd in ('rwS', 'rwC', 'rw0S', 'rw0C'):
        w('  | .%s => fun i m => match m with | .wset => outsBelow (bnd_%s.get i) | _ => allOuts' % (kd, kd))
    elif kd == 'rf':
        w('  | .rf => fun i m => match m with | .upd => allOuts | _ => outsBelow (bnd_rf.get i)')
    else:
        w('  | .%s => fun _ _ => allOuts' % kd)
if MET:
    w('''/-- A child input reached by one of its paths reads no parent input (no method drives one). -/
def pinK : (κ : Kind) → (KI κ).M → Bool
''' + ''.join(('  | .rf => fun m => m.isS\n' if kd == 'rf' else
               '  | .fifo => fun m => match m with | .enq | .deq => true | _ => false\n' if kd == 'fifo' else
               '  | .%s => fun m => match m with | .wset => true | _ => false\n' % kd if kd in ('rwS', 'rwC', 'rw0S', 'rw0C') else
               '  | .%s => fun _ => false\n' % kd) for kd in M.kinds) + '''def DwK (κ : Kind) (m : (KI κ).M) : List (Fin nM) := bif pinK κ m then [] else allIns
theorem pin_all (κ : Kind) (p q : (KI κ).M) (h : (KI κ).ann.path p q = true) : pinK κ p = true := by
  cases κ
''' + ''.join(('  · rw [reg_nopath] at h; cases h\n' if kd == 'reg' else
               '  · cases p <;> cases q <;> first | rfl | cases h\n') for kd in M.kinds))
w('''
theorem rdBnd : Tab.allB (fun k keys => Nat.beq (kT.get k) 0 ||
    Tab.allB (fun i p => alwaysRdyB p || Nat.ble (rk p) (mxRDT.get k)) keys 0 (kT.get k)) keyT 0 nS = true := by
  decide +kernel
theorem rdB (k : Nat) (hk : k < nS) : ∀ i, i < kR k → alwaysRdyB ((keyT.get k).get i) = false →
    rk ((keyT.get k).get i) ≤ mxRDT.get k := by
  intro i hi ha
  have h := sitesB_spec (fun k i p => alwaysRdyB p || Nat.ble (rk p) (mxRDT.get k)) rdBnd k hk i hi
  simp only [ha, Bool.false_or] at h
  exact Nat.le_of_ble_eq_true h

theorem all_congr' {α : Type} {f g : α → Bool} : ∀ (l : List α), (∀ a ∈ l, f a = g a) → l.all f = l.all g
  | [], _ => rfl
  | a :: l, h => by
    simp only [List.all_cons]
    rw [h a (List.mem_cons_self ..), all_congr' l (fun b hb => h b (List.mem_cons_of_mem _ hb))]

section
variable {V : Type}
theorem agO {oc oc' : OC V} {b : Nat} (hR : ∀ y, Nat.blt (rk y) b = true → AgreeO (oc y.1) (oc' y.1) y.2)
    (j : I) (m : (CI j).M) (h : Nat.blt (rk ⟨j, m⟩) b = true) : (oc j).ret m = (oc' j).ret m := (hR ⟨j, m⟩ h).2
theorem agR {oc oc' : OC V} {b : Nat} (hR : ∀ y, Nat.blt (rk y) b = true → AgreeO (oc y.1) (oc' y.1) y.2)
    (j : I) (m : (CI j).M) (h : Nat.blt (rk ⟨j, m⟩) b = true) : (oc j).rdy m = (oc' j).rdy m := (hR ⟨j, m⟩ h).1

theorem agRD {oc oc' : OC V} {b : Nat} (hR : ∀ y, Nat.blt (rk y) b = true → AgreeO (oc y.1) (oc' y.1) y.2)
    (k : Nat) (hk : k < nS) (hb : Nat.blt (mxRDT.get k) b = true) : RD oc k = RD oc' k := by
  unfold RD
  apply all_congr'
  intro i hi
  have hi' : i < kR k := by simp at hi; omega
  cases ha : alwaysRdyB ((keyT.get k).get i)
  · rw [(hR _ (blt_of_le_blt (rdB k hk i hi' ha) hb)).1]
  · rfl
''')
agneed = set()
for kd in path_inputs:
    if kd not in M.kinds:
        continue
    for i in range(len(M.byk[kd])):
        for m in path_inputs[kd]:
            pi = portinfo.get((kd, i, m))
            if pi and pi['users'] and (K[kd].has_en(m) or pi['argk'] == 'mux'):
                for k, _ in pi['users']:
                    agneed.add(k)
                    st_ = [k]
                    while st_:
                        kk = st_.pop()
                        for b in M.blk.get(kk, []):
                            if b not in agneed:
                                agneed.add(b)
                                st_.append(b)


def ag_order():
    seen, out_ = set(), []

    def go(k):
        if k in seen:
            return
        for b in M.blk.get(k, []):
            go(b)
        seen.add(k)
        out_.append(k)
    for k in sorted(agneed):
        go(k)
    return out_


def oread_ag(p, hb):
    kd, i, m = p
    return 'agO hR (%s %d) %s (blt_of_le_blt (Nat.le_of_ble_eq_true rfl) %s)' % (
        M.ihelper(kd), i, K[kd].lports[K[kd].code(m)], hb)


def rdy_ag(p, hb):
    kd, i, m = p
    return 'agR hR (%s %d) %s (blt_of_le_blt (Nat.le_of_ble_eq_true rfl) %s)' % (
        M.ihelper(kd), i, K[kd].lports[K[kd].code(m)], hb)


def agx(k, e, rd):
    t = e[0]
    if t in ('z', 'k', 'x', 'true', 'gk'):
        return 'rfl'
    if t == 'lit':
        return 'congrArg ops.toB (%s)' % rd(site_port(k, e[1]))
    if t in ('f', 'g'):
        return 'congrArg (ops.%s %d) (%s)' % (t, e[1], tupcong([rd(site_port(k, j)) for j in e[2]]))
    if t == 'split':
        rs = andc(['congrArg (ops.g %d) (%s)' % (M.rules[k]['sites'][i]['R'][1],
                                                 tupcong([rd(site_port(k, j)) for j in M.rules[k]['sites'][i]['R'][2]]))
                   for i in e[1]])
        return 'cond_congr (%s) (%s) (%s)' % (rs, agx(k, e[2], rd), agx(k, e[3], rd))
    raise ValueError(e)


def list_cong(terms):
    if not terms:
        return 'rfl'
    return 'congr2 List.cons (%s) (%s)' % (terms[0], list_cong(terms[1:]))


for k in ag_order():
    mk = mxW(k)
    rd = lambda p: oread_ag(p, 'hb')
    gag = agx(k, M.rules[k]['guard'], rd)
    if RDC[k]:
        parts = ['congr2 or (congrArg not (%s)) (%s)' % (agx(k, M.rules[k]['sites'][i]['cond'], rd), rdy_ag(site_port(k, i), 'hb'))
                 for i in RDC[k]]
        gag = 'congr2 and (%s) (%s)' % (gag, andc(parts + ['rfl']))
    if k in M.blk:
        bl = ['agWF_%d ops E hR (blt_of_le_blt (Nat.le_of_ble_eq_true rfl) hb)' % b for b in M.blk[k]]
        bag = ('congrArg (fun l => !orL l) (%s)' % list_cong(bl)) if MULTIBLK else 'congrArg not (%s)' % bl[0]
    else:
        bag = 'rfl'
    w('theorem agWF_%d (ops : Ops V) (E : EqC V) {oc oc\' : OC V} {b : Nat}\n'
      '    (hR : ∀ y, Nat.blt (rk y) b = true → AgreeO (oc y.1) (oc\' y.1) y.2) (hb : Nat.blt %d b = true) :\n'
      '    WF ops E oc %d = WF ops E oc\' %d :=\n'
      '  congr2 and (congr2 and (agRD hR %d (by decide) (blt_of_le_blt (Nat.le_of_ble_eq_true rfl) hb)) (%s)) (%s)'
      % (k, mk, k, k, k, gag, bag))


def oread_dq(p):
    kd, i, m = p
    return 'agO hR (%s %d) %s rfl' % (M.ihelper(kd), i, K[kd].lports[K[kd].code(m)])


def dq_port(p):
    pi = portinfo.get(p)
    kd, i, m = p
    if not pi or not pi['users']:
        return '⟨rfl, rfl⟩'
    if K[kd].has_en(m):
        ens = []
        for (k, si) in pi['users']:
            st = M.rules[k]['sites'][si]
            if is_uncond(st):
                ens.append('agWF_%d ops E hR rfl' % k)
            else:
                ens.append('congr2 and (agWF_%d ops E hR rfl) (%s)' % (k, agx(k, st['cond'], oread_dq)))
        en = 'congrArg orL (%s)' % list_cong(ens)
    else:
        en = 'rfl'
    if pi['argk'] == 'mux':
        def selag(k, st):
            if SELMUX.get(pi['mux']) and not is_uncond(st):
                return 'congr2 and (agWF_%d ops E hR rfl) (%s)' % (k, agx(k, st['cond'], oread_dq))
            return 'agWF_%d ops E hR rfl' % k
        ents_ = ['congr2 Prod.mk (%s) (%s)' % (selag(k, st), agx(k, st['arg'], oread_dq))
                 for (k, _), st in zip(pi['mus'], pi['margs'])]
        arg = 'congrArg (fun l => pmux l ops.z) (%s)' % list_cong(ents_)
    elif pi['argk'] in ('single', 'const'):
        k, si = pi['users'][0]
        arg = agx(k, M.rules[k]['sites'][si]['arg'], oread_dq)
    else:
        arg = 'rfl'
    return '⟨%s, %s⟩' % (en, arg)


for kd in M.kinds:
    if kd not in path_inputs:
        continue
    n = len(M.byk[kd])
    pins = path_inputs[kd]
    hyp = "(∀ y, Nat.blt (rk y) (%s) = true → AgreeO (oc y.1) (oc' y.1) y.2) → AgreeW (A V ops E oc w) (A V ops E oc' w') %s"
    w('def DQ_%s (ops : Ops V) (E : EqC V) (i : Nat) (A : (V : Type) → Ops V → EqC V → OC V → %s → Wires (KI .%s).M V) : Prop :=\n'
      '  ∀ (h : i < nk .%s) (oc oc\' : OC V) (w w\' : %s),\n    %s'
      % (kd, WXM, kd, kd, WXM, ' ∧ '.join('(%s)' % (hyp % (bterm(kd, m), K[kd].lports[K[kd].code(m)])) for m in pins)))
    for i in range(n):
        parts = ['fun hR => %s' % dq_port((kd, i, m)) for m in pins]
        w('theorem dq_%s_%d (ops : Ops V) (E : EqC V) : DQ_%s ops E %d arm_%s_%d := fun h oc oc\' w w\' =>\n  %s'
          % (kd, i, kd, i, kd, i, ('⟨' + ', '.join(parts) + '⟩') if len(parts) > 1 else parts[0]))
    w('theorem dqAll_%s (ops : Ops V) (E : EqC V) : Tab.AllR (DQ_%s ops E) arms_%s 0 %d :=\n  %s'
      % (kd, kd, kd, n, tabproof(['dq_%s_%d ops E' % (kd, i) for i in range(n)])))
w('end\n')


def proj_sel(pins, m):
    """The conjunct of m in a right-nested ∧ of len(pins) parts."""
    i = pins.index(m)
    t = ''
    for _ in range(i):
        t += '.2'
    if i < len(pins) - 1:
        t += '.1'
    return t


def dep_case(kd):
    da = 'dep_of_all (F ops E).base allOuts mem_allOuts allIns mem_allIns oc oc\' w w\' hD hW _ _'
    if MET:
        da = 'dep_of_all (F ops E).base allOuts mem_allOuts allIns mem_allIns oc oc\' w w\' hD (by simpa [DwK, pinK] using hW) _ _'
    if kd not in path_inputs:
        return '    | %s => exact %s\n' % (kd, da)
    pins = path_inputs[kd]
    s = '    | %s =>\n      cases p with\n' % kd
    for m in K[kd].ports:
        if m in pins:
            s += ('      | %s => exact (Tab.get_allR _ 0 _ (dqAll_%s ops E) i (Nat.zero_le _) hi hi oc oc\' w w\')%s (forall_filter allOuts mem_allOuts _ hD)\n'
                  % (m, kd, proj_sel(pins, m) if len(pins) > 1 else ''))
        else:
            s += '      | %s => exact %s\n' % (m, da)
    return s


def rkok_case(kd):
    if kd not in path_inputs:
        if kd == 'reg':
            return '    | reg => rw [reg_nopath] at hpq; cases hpq\n'
        return '    | %s => cases hpq\n' % kd
    if kd == 'fifo':
        return ('    | fifo => cases p <;> cases q <;> first | exact blt_true (List.mem_filter.mp hy).2 | exact Nat.lt_of_lt_of_le (blt_true (List.mem_filter.mp hy).2) (fifoLe i hi) | cases hpq\n')
    return ('    | %s => cases p <;> cases q <;> first | exact blt_true (List.mem_filter.mp hy).2 | cases hpq\n' % kd)


def rklt_case(kd):
    if kd not in path_inputs:
        return '    | %s => exact Nat.zero_lt_succ _\n' % kd
    if kd == 'fifo':
        return ('    | fifo =>\n      have hb := Tab.allB_spec _ bnd_fifo 0 (nk .fifo) rkB_fifo i (Nat.zero_le _) hi\n'
                '      simp only [Bool.and_eq_true] at hb\n'
                '      cases m <;> first | exact Nat.zero_lt_succ _ | exact Nat.blt_eq.mp hb.1 | exact Nat.blt_eq.mp hb.2\n')
    return ('    | %s =>\n      have hb := Tab.allB_spec _ bnd_%s 0 (nk .%s) rkB_%s i (Nat.zero_le _) hi\n'
            '      cases m <;> first | exact Nat.zero_lt_succ _ | exact Nat.blt_eq.mp hb\n'
            % (kd, kd, kd, kd))


for kd in M.kinds:
    if kd in path_inputs:
        if kd == 'fifo':
            w('theorem rkB_fifo : Tab.allB (fun _ b => Nat.blt b.1 %d && Nat.blt b.2 %d) bnd_fifo 0 (nk .fifo) = true := by decide +kernel' % (NN, NN))
            w('theorem fifoLeB : Tab.allB (fun _ b => Nat.ble b.1 b.2) bnd_fifo 0 (nk .fifo) = true := by decide +kernel')
            w('theorem fifoLe (i : Nat) (hi : i < nk .fifo) : (bnd_fifo.get i).1 ≤ (bnd_fifo.get i).2 :=\n'
              '  Nat.le_of_ble_eq_true (Tab.allB_spec _ bnd_fifo 0 (nk .fifo) fifoLeB i (Nat.zero_le _) hi)')
        else:
            w('theorem rkB_%s : Tab.allB (fun _ b => Nat.blt b %d) bnd_%s 0 (nk .%s) = true := by decide +kernel' % (kd, NN, kd, kd))
w('''
section
variable {V : Type}
def acyc (ops : Ops V) (E : EqC V) : Acyc (F ops E).base where
  D j p := DK j.1 j.2.val p
  Dw ''' + ('_ _ := []' if not MET else 'j p := DwK j.1 p') + '''
  Do ''' + ('e := e.elim' if not MET else '_ := allOuts') + '''
  Dwo ''' + ('e := e.elim' if not MET else '_ := []') + '''
  dep oc oc' w w' j p hD hW := by
    obtain ⟨κ, i, hi⟩ := j
    cases κ with
''' + ''.join(dep_case(kd) for kd in M.kinds) + '''  depo ''' + ('_ _ _ _ q := q.elim' if not MET else '''oc oc' w w' q hD _ := by
    have e : oc = oc' := funext fun j => Outs.ext' fun q => hD ⟨j, q⟩ (mem_allOuts _)
    subst e; exact ⟨rfl, rfl⟩''') + '''
  rk := rk
  N := %d
  rk_lt y := by
    obtain ⟨⟨κ, i, hi⟩, m⟩ := y
    cases κ with
''' % NN + ''.join(rklt_case(kd) for kd in M.kinds) + '''  rk_ok j q p hpq y hy := by
    obtain ⟨κ, i, hi⟩ := j
    cases κ with
''' + ''.join(rkok_case(kd) for kd in M.kinds) + '''  reach _ := []
  reach_w ''' + ('_ _ _ _ _ hp := by cases hp' if not MET else '''j q p hpq p' hp := by
    have h := pin_all j.1 p q hpq
    simp only [DwK, h, cond_true] at hp
    cases hp''') + '''
  reach_o _ _ _ _ _ _ _ hp := by cases hp
  path_w ''' + ('q := q.elim' if not MET else '_ _ hp := by cases hp') + '''
  path_o ''' + ('q := q.elim' if not MET else '_ _ _ _ hp := by cases hp') + '''
end
''')
mark('acyc')

# ---------------------------------------------------------------- Built and top
w('''/-! ## The built module and its top theorem -/

/-- The assumed leaves (library contracts not yet proved): FIFOL1, ConfigRegN, SizedFIFO, RegFile. -/
structure Leaves (V : Type) (toB : V → Bool) where
''' + ('''  fifo : Module V FL.X
  fifoR : Realizes fifo
  fifoC : Causal fifo
''' if 'fifo' in M.kinds else '') + ('''  cfg : Module V Cfg.X
  cfgR : Realizes cfg
  cfgC : Causal cfg
''' if 'cfg' in M.kinds else '') + ('''  sf : Module V FIFO2.X
  sfR : Realizes sf
  sfC : Causal sf
  sfA : Aliased toB sf
''' if 'sf' in M.kinds else '') + ('''  rf : Module V RF.X
  rfR : Realizes rf
  rfC : Causal rf
''' if 'rf' in M.kinds else '') + '''  dummy : Unit := ()

section
variable {V : Type}
def implK (ops : Ops V) (lv : Leaves V ops.toB) : (κ : Kind) → Module V (KI κ)
''')
IMPL = {'reg': 'Reg', 'cfg': 'lv.cfg', 'fifo': 'lv.fifo', 'tasks': 'Tasks nT', 'f2': 'FIFO2 ops.ofBool', 'sf': 'lv.sf',
        'rf': 'lv.rf', 'rwS': 'RWire ops.ofBool true true', 'rwC': 'RWire ops.ofBool true false',
        'rw0S': 'RWire0 ops.ofBool true true', 'rw0C': 'RWire0 ops.ofBool true false'}
for kd in M.kinds:
    w('  | .%s => %s' % (kd, IMPL[kd]))


def leaf(kd):
    return {'reg': 'Built.leaf _ reg_realizes (creg_causal 1)', 'cfg': 'Built.leaf _ lv.cfgR lv.cfgC',
            'fifo': 'Built.leaf _ lv.fifoR lv.fifoC', 'tasks': 'Built.leaf _ (tasks_realizes nT) (tasks_causal nT)',
            'f2': 'Built.leaf _ (fifo2_realizes _) (fifo2_causal _)', 'sf': 'Built.leaf _ lv.sfR lv.sfC',
            'rf': 'Built.leaf _ lv.rfR lv.rfC'}.get(kd, 'Built.leaf _ (rwire_realizes _ _ _) (rwire_causal _ _ _)'
                                                    if kd in ('rwS', 'rwC') else
                                                    'Built.leaf _ (rwire0_realizes _ _ _) (rwire0_causal _ _ _)')


def alias(kd):
    if kd == 'f2':
        return '(fifo2_aliased ops.ofBool ops.toB hy.tob).toU'
    if kd == 'sf':
        return 'lv.sfA.toU'
    if kd == 'reg':
        return 'aliased_of_none _ _ (fun p q => rfl) (fun a p => rfl)'
    return 'aliased_of_none _ _ (fun p q => by cases p <;> cases q <;> rfl) (fun a p => by cases a <;> cases p <;> rfl)' if kd != 'tasks' else \
        'aliased_of_none _ _ (fun p q => rfl) (fun a p => rfl)'


TOPM = '((F ops E).toModuleU (fun j => (implK ops lv j.1).toU))'
w('''
theorem built [Inhabited V] (ops : Ops V) (E : EqC V) %s (lv : Leaves V ops.toB) :
    Built X %s :=
  Built.node (F ops E) (fun j => (implK ops lv j.1).toU)
    (fun ⟨κ, _⟩ => by
      cases κ
''' % (HYP, TOPM) + ''.join('      · exact %s\n' % leaf(kd) for kd in M.kinds) + '''    )
    (fun ⟨κ, _⟩ => by
      cases κ
''' + ''.join('      · exact %s\n' % alias(kd) for kd in M.kinds) + '''    )
    (obl ops E %s) (acyc ops E)

/-- **The module's top theorem**: the contract, with the run fixed before every skipped value
    of the tree, and every input has a hardware cycle. -/
theorem top [Inhabited V] (ops : Ops V) (E : EqC V) %s (lv : Leaves V ops.toB) :
    RealizesU %s ∧
    ∀ s w, %s.clean s = s → ∃ o s' g, %s.hw s w o s' g :=
  (built ops E %s lv).top
end

end
end %s

#print axioms %s.obl
#print axioms %s.top
''' % (HYA, HYP, TOPM, TOPM, TOPM, HYA, NAME, NAME, NAME))


if NOACYC:
    L = L_KEEP
open(out, 'w').write('\n'.join(L) + '\n')
nsites = sum(len(r['sites']) for r in M.rules)
print('scan segments %d (at most %d uses each)' % (NSEG, SEG_USES))
print('wrote %s: rules %d, methods %d, children %d, sites %d, task statements %d, muxes %d (member pairs: %d by checks, %d one by one), '
      'exR bits %d, escapes %d (site pairs %d, unconfirmed %d), readiness and invariance facts %d (unconfirmed %d), later-site literals dropped %d, ranks N=%d, ag lemmas %d'
      % (out, nR, nM, len(M.inst), nsites, nT, len(muxes), nchk_pairs, nuncov, sum(bin(x).count('1') for x in need),
         len(ESC), len(EFACTS), len(UNCONF_E), len(FACTS), len(UNCONF), M.later_drops, NN, len(agneed)))

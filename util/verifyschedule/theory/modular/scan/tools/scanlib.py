"""Kind tables, a Python mirror of Scan.lean's pass, and Lean emission.

The mirror follows Design.F exactly (same checks, same state), so that the
checkpoints it writes are the states the kernel computes.
"""


# ---- child kinds: ports (code order), annotations as in Modular.lean ----

class Kind:
    def __init__(self, name, ports, prec, sbr, conf, share, fx):
        self.name, self.ports = name, ports
        self.prec, self.sbr, self.conf, self.share, self.fx = prec, sbr, conf, share, fx

    def code(self, p):
        return self.ports.index(p)

    def folP(self, p, q):
        return (p == q or not self.prec(q, p)) and not self.conf(p, q) and not self.conf(q, p)

    def grpP(self, p, q):
        return self.folP(p, q) and not self.sbr(p, q) and not self.sbr(q, p)

    def argP(self, p, q):
        return p != q or not self.share(p) or self.fx(p)

    def kc(self, c):
        q0 = self.ports[c]
        P = self.ports
        cin = [self.code(p) for p in P if not (self.folP(p, q0) and self.argP(p, q0))]
        cout = [self.code(q) for q in P if not (self.folP(q0, q) and self.argP(q0, q))]
        gin = [self.code(p) for p in P if not (self.grpP(p, q0) and self.argP(p, q0))]
        gout = [self.code(q) for q in P if not (self.grpP(q0, q) and self.argP(q0, q))]
        return (cin, cout, gin, gout)


def creg(n):
    ports = []
    for i in range(n):
        ports += [('rd', i), ('wr', i)]
    isW = lambda p: p[0] == 'wr'
    prec = lambda p, q: p[1] < q[1] or (p[1] == q[1] and not isW(p) and isW(q))
    sbr = lambda p, q: p[1] != q[1] or (isW(p) and isW(q))
    return Kind('creg%d' % n, ports, prec, sbr, lambda p, q: False, lambda m: not isW(m),
                lambda m: not isW(m))


FIFO_P = ['enq', 'deq', 'first', 'notEmpty', 'notFull', 'i_notEmpty', 'i_notFull', 'clear']


def fifo2():
    precs = {('first', 'deq'), ('first', 'clear'), ('notEmpty', 'deq'), ('notEmpty', 'enq'),
             ('notEmpty', 'clear'), ('notFull', 'deq'), ('notFull', 'enq'), ('notFull', 'clear'),
             ('deq', 'clear'), ('enq', 'clear')}
    sbrs = {('deq', 'clear'), ('enq', 'clear'), ('clear', 'clear')}
    confs = {('enq', 'enq'), ('deq', 'deq')}
    share = lambda m: m not in ('enq', 'deq', 'clear')
    fx = lambda m: m not in ('enq',)
    return Kind('fifo2', FIFO_P, lambda p, q: (p, q) in precs, lambda p, q: (p, q) in sbrs,
                lambda p, q: (p, q) in confs, share, fx)


def rwire():
    W = ['wset', 'wget', 'whas']
    return Kind('rwire', W, lambda p, q: p == 'wset' and q != 'wset', lambda p, q: False,
                lambda p, q: p == 'wset' and q == 'wset', lambda m: m != 'wset', lambda m: m != 'wset')


# ---- the scan, mirrored ----

def bit(i):
    return 1 << i


def clr(a, b):
    return a ^ (a & b)


def subB(a, b):
    return a & b == a


class Design:
    """fpM[k], fpR[i]: lists of (base, kind, code); kt[kind][code] = (cin, cout, gin, gout)."""

    def __init__(self, nM, nR, d, fpM, fpR, kt, nfI, nfO, ngI, ngO, h3ok, exM, exR, exMR, escR):
        self.__dict__.update(locals())
        del self.__dict__['self']


def run_scan(D, upto=None, verbose=False):
    """Run the pass; returns (ok, list of states after each step) -- states as dicts."""
    st = {}
    n = D.nM + D.nR
    states = []
    hits = {}
    get = lambda s: st.get(s, (0, 0, 0, 0))
    for g in range(n if upto is None else upto):
        if g < D.nM:
            k = g
            for (b, kd, c) in D.fpM[k]:
                cin, cout, gin, gout = D.kt[kd][c]
                if any(get(b + c2)[0] == k + 1 for c2 in gin):
                    return False, states, ('grpM', k, (b, kd, c))
                o = get(b + c)
                st[b + c] = (k + 1, o[1] | bit(k), o[2], o[3])
                ex = clr(D.exM[k], bit(k))
                orA = lambda cs: _or(get(b + c2)[1] for c2 in cs)
                if not (subB(orA(cin), D.nfI[k] | ex) and subB(orA(cout), D.nfO[k] | ex) and
                        subB(orA(gin), D.ngI[k] | ex) and subB(orA(gout), D.ngO[k] | ex)):
                    return False, states, ('mm', k, (b, kd, c))
        else:
            i = g - D.nM
            dh = ph = bm = 0
            for (b, kd, c) in D.fpR[i]:
                cin, cout, gin, gout = D.kt[kd][c]
                if any(get(b + c2)[0] == g + 1 for c2 in gin):
                    return False, states, ('grpR', i, (b, kd, c))
                o = get(b + c)
                st[b + c] = (g + 1, o[1], o[2] | bit(i), o[3])
                bad = _or(get(b + c2)[2] for c2 in cin)
                if not subB(bad, D.exR[i] | D.escR[i] | bit(i)):
                    return False, states, ('h4', i, (b, kd, c), bad)
                dh |= _or(get(b + c2)[1] for c2 in cout)
                ph |= _or(get(b + c2)[3] for c2 in cout)
                bm |= _or(get(b + c2)[1] for c2 in cin)
            H = clr(dh | ph, D.exMR[i])
            B = clr(bm, D.exMR[i])
            if H != 0:
                for m in range(D.nM):
                    if (B >> m) & 1 and not subB(H, D.h3ok[m] | clr(D.exM[m], bit(m))):
                        return False, states, ('h3', i, m)
            hits[i] = H
            for (b, kd, c) in D.fpR[i]:
                o = get(b + c)
                st[b + c] = (o[0], o[1], o[2], o[3] | H)
        states.append(dict(st))
    return True, states, hits


def _or(it):
    r = 0
    for x in it:
        r |= x
    return r


# ---- Lean emission ----

def nl(x):
    return '(nat_lit %d)' % x


def tab(vals, fmt, lo=0):
    """A balanced Tab literal over vals[0..]."""
    def go(a, b):
        if b - a == 1:
            return '(.leaf %s)' % fmt(vals[a])
        mid = (a + b) // 2
        return '(.node %s %s %s)' % (nl(mid), go(a, mid), go(mid, b))
    if not vals:
        return '(.leaf %s)' % fmt(None)
    return go(0, len(vals))


def fmt_ents(l):
    if l is None:
        return '[]'
    return '[' + ', '.join('(%s, %s, %s)' % tuple(nl(x) for x in e) for e in l) + ']'


def fmt_nat(x):
    return nl(0) if x is None else nl(x)


def fmt_list(l):
    return '[' + ', '.join(nl(x) for x in l) + ']'


def fmt_kc(k):
    if k is None:
        return '⟨[], [], [], []⟩'
    return '⟨%s, %s, %s, %s⟩' % tuple(fmt_list(x) for x in k)


def fmt_kt(kinds):
    return tab([tab(kd, fmt_kc) for kd in kinds], lambda x: x if x is not None else '(.leaf ⟨[], [], [], []⟩)')


def emit_design(D, name='D'):
    out = []
    out.append('def %s : Design where' % name)
    out.append('  nM := %s' % nl(D.nM))
    out.append('  nR := %s' % nl(D.nR))
    out.append('  d := %s' % nl(D.d))
    out.append('  fpM := %s' % tab(D.fpM, fmt_ents))
    out.append('  fpR := %s' % tab(D.fpR, fmt_ents))
    out.append('  kt := %s' % fmt_kt(D.kt))
    for f in ['nfI', 'nfO', 'ngI', 'ngO', 'h3ok', 'exM']:
        out.append('  %s := %s' % (f, tab(getattr(D, f), fmt_nat)))
    for f in ['exR', 'exMR', 'escR']:
        out.append('  %s := %s' % (f, tab(getattr(D, f), fmt_nat)))
    return '\n'.join(out)


def emit_state(st, d, name):
    items = sorted(st.items())
    body = ', '.join('(%d, ⟨%d, %d, %d, %d⟩)' % (s, v[0], v[1], v[2], v[3]) for s, v in items)
    return 'def %s : ST := Tr.ofList SS.z %d [%s]' % (name, d, body)


def emit_tree(st, d, name, leaf_fmt=None, dflt='SS.z'):
    """The state as the tree literal the scan builds (paths to depth d for set keys)."""
    if leaf_fmt is None:
        leaf_fmt = lambda v: '⟨%s, %s, %s, %s⟩' % tuple(nl(x) for x in v)
    keys = {}
    for s, v in st.items():
        keys[s % (1 << d)] = v   # distinct keys only (no aliasing in generated designs)

    def go(depth, ks):
        # ks: dict of remaining key bits -> value, at this subtree
        if not ks:
            return '(.lf %s)' % dflt
        if depth == 0:
            (v,) = ks.values()
            return '(.lf %s)' % leaf_fmt(v)
        L = {k >> 1: v for k, v in ks.items() if k % 2 == 0}
        R = {k >> 1: v for k, v in ks.items() if k % 2 == 1}
        return '(.nd %s %s)' % (go(depth - 1, L), go(depth - 1, R))

    return 'def %s : ST := %s' % (name, go(d, keys))

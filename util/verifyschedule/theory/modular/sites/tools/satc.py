"""Boolean skeletons of bsc expressions, Tseitin CNF, and cadical.

Data values are abstract: every comparison or other non-Boolean term is a
free atom, except that `x == k` for distinct constants `k` of one operand
`x` exclude each other (EqC's law).  UNSAT here is therefore UNSAT for the
real values; SAT may be spurious.
"""
import os
import re
import subprocess
import tempfile

CADICAL = os.path.expanduser('~/.elan/toolchains/leanprover--lean4---v4.35.0/bin/cadical')


def litval(s):
    m = re.match(r"^(\d+)'[sS]?([bdhoBDHO])([0-9a-fA-F_]+)$", s)
    if m:
        base = {'b': 2, 'd': 10, 'h': 16, 'o': 8}[m.group(2).lower()]
        try:
            return int(m.group(3).replace('_', ''), base)
        except ValueError:
            return None
    if re.match(r'^0x[0-9a-fA-F]+$', s):
        return int(s, 16)
    if re.match(r'^\d+$', s):
        return int(s)
    return None


def is_bool_lit(e):
    if e[0] != 'lit':
        return None
    s = e[1]
    if re.match(r"^1'[bdhBDH][01]$", s):
        return s[-1] == '1'
    return None


class Enc:
    """One CNF problem over a design's definitions."""

    def __init__(self, D):
        self.D = D              # design: .defs, .types, .is_bool(name), .canon(expr)
        self.n = 1              # var 1 is TRUE
        self.cl = [[1]]
        self.memo = {}
        self.atoms = {}
        self.eqs = {}           # canon operand -> {value: var}

    def new(self):
        self.n += 1
        return self.n

    def atom(self, key):
        v = self.atoms.get(key)
        if v is None:
            v = self.atoms[key] = self.new()
        return v

    def eqatom(self, x, kexpr):
        k = litval(kexpr[1]) if kexpr[0] == 'lit' else None
        cx = self.D.canon(x)
        if k is None:
            return self.atom(('eqv', cx, self.D.canon(kexpr)))
        d = self.eqs.setdefault(cx, {})
        if k not in d:
            d[k] = self.atom(('eq', cx, k))
        return d[k]

    def AND(self, xs):
        xs = [x for x in xs if x != 1]
        if any(x == -1 for x in xs):
            return -1
        if not xs:
            return 1
        if len(xs) == 1:
            return xs[0]
        v = self.new()
        for x in xs:
            self.cl.append([-v, x])
        self.cl.append([v] + [-x for x in xs])
        return v

    def OR(self, xs):
        return -self.AND([-x for x in xs])

    def ITE(self, c, a, b):
        return self.OR([self.AND([c, a]), self.AND([-c, b])])

    def b(self, e):
        """Literal for a Boolean expression."""
        t = e[0]
        if t == 'lit':
            bl = is_bool_lit(e)
            if bl is not None:
                return 1 if bl else -1
            return self.atom(('lit', e[1]))
        if t == 'id':
            n = e[1]
            if n in self.memo:
                return self.memo[n]
            de = self.D.bool_def(n)
            if de is None:
                v = self.atom(('id', n))
            else:
                self.memo[n] = self.atom(('cyc', n))   # guards against cycles
                v = self.b(de)
            self.memo[n] = v
            return v
        if t == 'un' and e[1] in ('!', '~'):
            return -self.b(e[2])
        if t == 'bin':
            op = e[1]
            if op == '&&' or (op == '&' and self.D.boolish(e[2]) and self.D.boolish(e[3])):
                return self.AND([self.b(e[2]), self.b(e[3])])
            if op == '||' or (op == '|' and self.D.boolish(e[2]) and self.D.boolish(e[3])):
                return self.OR([self.b(e[2]), self.b(e[3])])
            if op in ('==', '!='):
                l, r = e[2], e[3]
                bl = is_bool_lit(r)
                if bl is None:
                    bl, l, r = is_bool_lit(l), r, l
                if bl is not None and self.D.boolish(l):
                    x = self.b(l) if bl else -self.b(l)
                    return x if op == '==' else -x
                if r[0] == 'lit':
                    x = self.eqatom(l, r)
                elif l[0] == 'lit':
                    x = self.eqatom(r, l)
                else:
                    x = self.atom(('eqv',) + tuple(sorted([self.D.canon(l), self.D.canon(r)])))
                return x if op == '==' else -x
            return self.atom(('t', self.D.canon(e)))
        if t == 'if':
            return self.ITE(self.b(e[1]), self.b(e[2]), self.b(e[3]))
        if t == 'case':
            idx = e[1]
            sel, prev = [], []
            for lab, val in e[2]:
                q = self.eqatom(idx, lab)
                sel.append(self.AND([q] + [-p for p in prev] + [self.b(val)]))
                prev.append(q)
            if e[3] is not None:
                sel.append(self.AND([-p for p in prev] + [self.b(e[3])]))
            return self.OR(sel)
        if t == 'call':
            return self.atom(('port', self.D.canon(e)))
        return self.atom(('t', self.D.canon(e)))

    def finish(self):
        for cx, d in self.eqs.items():
            vs = list(d.values())
            # at most one, as a prefix chain (linear)
            if len(vs) > 1:
                pre = vs[0]
                for v in vs[1:]:
                    self.cl.append([-pre, -v])
                    np_ = self.new()
                    self.cl.append([-np_, pre, v])
                    self.cl.append([np_, -pre])
                    self.cl.append([np_, -v])
                    pre = np_

    def sat(self, roots, workdir):
        """Is the conjunction of the literal list `roots` satisfiable?"""
        self.finish()
        cls = self.cl + [[r] for r in roots]
        fd, path = tempfile.mkstemp(suffix='.cnf', dir=workdir)
        with os.fdopen(fd, 'w') as f:
            f.write('p cnf %d %d\n' % (self.n, len(cls)))
            for c in cls:
                f.write(' '.join(map(str, c)) + ' 0\n')
        try:
            r = subprocess.run([CADICAL, '-q', path], stdout=subprocess.PIPE, stderr=subprocess.DEVNULL,
                               timeout=120)
            rc = r.returncode
        finally:
            os.unlink(path)
        if rc == 10:
            return True
        if rc == 20:
            return False
        return None

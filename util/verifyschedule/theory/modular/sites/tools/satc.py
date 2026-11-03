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

    def sat_model(self, roots, workdir):
        """As sat, with cadical's witness: (True, {variable: value}), (False, None) or (None, None)."""
        self.finish()
        cls = self.cl + [[r] for r in roots]
        fd, path = tempfile.mkstemp(suffix='.cnf', dir=workdir)
        with os.fdopen(fd, 'w') as f:
            f.write('p cnf %d %d\n' % (self.n, len(cls)))
            for c in cls:
                f.write(' '.join(map(str, c)) + ' 0\n')
        try:
            r = subprocess.run([CADICAL, '-q', path], stdout=subprocess.PIPE, stderr=subprocess.DEVNULL,
                               timeout=120, text=True)
            rc = r.returncode
        except subprocess.TimeoutExpired:
            rc = None
        finally:
            os.unlink(path)
        if rc == 10:
            val = {}
            for ln in r.stdout.split('\n'):
                if ln.startswith('v '):
                    for x in ln[2:].split():
                        x = int(x)
                        if x:
                            val[abs(x)] = x > 0
            return True, val
        if rc == 20:
            return False, None
        return None, None

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
        except subprocess.TimeoutExpired:
            rc = None      # unknown: every caller treats None as not UNSAT (fails safe)
        finally:
            os.unlink(path)
        if rc == 10:
            return True
        if rc == 20:
            return False
        return None


class EncS(Enc):
    """Enc where, while `on`, an atom over a conditional read y of the step stands for the model's
    site value: ite(cond_y, the atom over y's port, the same atom over a fresh value of y).
    `sub` maps (inst, meth) to cond_y's literal.  Atoms over a fresh value are free (sound for UNSAT).
    Every atom kind is wrapped: b() (ids, comparisons, terms, ports, case arms) and eqatom() ('eq'
    literals of a reach formula)."""

    def __init__(self, D):
        super().__init__(D)
        self.sub = {}
        self.on = False

    def _wrap(self, reads, mk):
        ys = sorted(set(reads) & set(self.sub)) if self.on else []
        if not ys:
            return mk(())
        out = []
        for mask in range(1 << len(ys)):
            S = tuple(y for i, y in enumerate(ys) if mask >> i & 1)
            out.append(self.AND([-self.sub[y] if y in S else self.sub[y] for y in ys] + [mk(S)]))
        return self.OR(out)

    def _atom(self, key, S):
        return self.atom(key + (('skip',) + S,) if S else key)

    def _eqatom(self, x, kexpr, S):
        k = litval(kexpr[1]) if kexpr[0] == 'lit' else None
        cx = self.D.canon(x)
        if S:
            cx = (cx, S)
        if k is None:
            return self.atom(('eqv', cx, self.D.canon(kexpr)))
        d = self.eqs.setdefault(cx, {})
        if k not in d:
            d[k] = self.atom(('eq', cx, k))
        return d[k]

    def eqatom(self, x, kexpr):
        # an 'eq' literal (case arm, array selection) also reads site values while `on` (r7 review s2 minor 1)
        rd = set(self.D.expr_reads(x)) | set(self.D.expr_reads(kexpr))
        return self._wrap(rd, lambda S: self._eqatom(x, kexpr, S))

    def b(self, e):
        D = self.D
        t = e[0]
        if t == 'lit':
            bl = is_bool_lit(e)
            if bl is not None:
                return 1 if bl else -1
            return self.atom(('lit', e[1]))
        if t == 'id':
            n = e[1]
            mk_ = (n, self.on)
            if mk_ in self.memo:
                return self.memo[mk_]
            de = D.bool_def(n)
            if de is None:
                v = self._wrap(D.expr_reads(e), lambda S: self._atom(('id', n), S))
            else:
                self.memo[mk_] = self.atom(('cyc', n, self.on))
                v = self.b(de)
            self.memo[mk_] = v
            return v
        if t == 'un' and e[1] in ('!', '~'):
            return -self.b(e[2])
        if t == 'bin':
            op = e[1]
            if op == '&&' or (op == '&' and D.boolish(e[2]) and D.boolish(e[3])):
                return self.AND([self.b(e[2]), self.b(e[3])])
            if op == '||' or (op == '|' and D.boolish(e[2]) and D.boolish(e[3])):
                return self.OR([self.b(e[2]), self.b(e[3])])
            if op in ('==', '!='):
                l, r = e[2], e[3]
                bl = is_bool_lit(r)
                if bl is None:
                    bl, l, r = is_bool_lit(l), r, l
                if bl is not None and D.boolish(l):
                    x = self.b(l) if bl else -self.b(l)
                    return x if op == '==' else -x
                rd = D.expr_reads(e)
                if r[0] == 'lit':
                    x = self._wrap(rd, lambda S: self._eqatom(l, r, S))
                elif l[0] == 'lit':
                    x = self._wrap(rd, lambda S: self._eqatom(r, l, S))
                else:
                    key = ('eqv',) + tuple(sorted([D.canon(l), D.canon(r)]))
                    x = self._wrap(rd, lambda S: self._atom(key, S))
                return x if op == '==' else -x
            return self._wrap(D.expr_reads(e), lambda S: self._atom(('t', D.canon(e)), S))
        if t == 'if':
            return self.ITE(self.b(e[1]), self.b(e[2]), self.b(e[3]))
        if t == 'case':
            idx = e[1]
            rd = D.expr_reads(idx)
            sel, prev = [], []
            for lab, val in e[2]:
                q = self._wrap(rd, lambda S, lab=lab: self._eqatom(idx, lab, S))
                sel.append(self.AND([q] + [-p for p in prev] + [self.b(val)]))
                prev.append(q)
            if e[3] is not None:
                sel.append(self.AND([-p for p in prev] + [self.b(e[3])]))
            return self.OR(sel)
        if t == 'call':
            return self._wrap(D.expr_reads(e), lambda S: self._atom(('port', D.canon(e)), S))
        return self._wrap(D.expr_reads(e), lambda S: self._atom(('t', D.canon(e)), S))

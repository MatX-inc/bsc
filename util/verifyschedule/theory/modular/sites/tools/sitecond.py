"""Measure decision 1's site-condition rule on schedlean's corpus.

usage: python3 -I sitecond.py OUTDIR DESIGN_DIR...
  each DESIGN_DIR holds dump.txt (schedlean --dump-design); the dumpba text of
  its .ba is read from DATA/<design>.ba.txt (DATA = ../data next to this file).

Per design it writes OUTDIR/<design>.txt (every conditional site) and prints
one summary line.  The rule:

  * action calls: their exact call conditions (never weakened);
  * value reads: unconditional, unless
      (a) the static footprint scan fails for it (a pair of steps in schedule
          order, not in an exception group, whose uses on one child may not
          follow each other; the read member gets a condition),
      (b) it reads a shared value-method copy that two steps that may fire
          together read with different arguments,
      (c) it is the earlier read of a value/value pair that may not follow
          (read flags),
      (r) [checked, not part of decision 1] its port has a ready signal that
          the step's guard does not imply;
  * a conditional read's condition: an exact hardware one if the hardware has
    one the step can compute (the top-priority user of a shared copy's
    argument mux), else the reach condition: && and || pass their operands
    unconditionally, only an arm of an if / case / array selection is
    conditional on its select (schedlean's <def>@reaches@<copy> rule);
  * the site order: data dependence (a call's condition and arguments, a
    read's arguments) and the child's in-step SB order fixed; reach literals
    reading a site not earlier, or a hardware-only signal (another step's
    WILL_FIRE, a method input not the step's own), are dropped.
"""
import os
import re
import sys
import threading

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from bapkg import APkg, parse_expr  # noqa: E402
from satc import Enc, EncS, litval  # noqa: E402

SC = bool(os.environ.get('SITES_SC'))   # the both-way short-circuit option (rule.md section 8)
DATA = os.environ.get('SITES_DATA') or os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(__file__))), 'data')


# ---------------------------------------------------------------- the dump

def split_top(s):
    out, depth, cur = [], 0, ''
    for ch in s:
        if ch == '[':
            depth += 1
        elif ch == ']':
            depth -= 1
        if ch == ',' and depth == 0:
            out.append(cur.strip())
            cur = ''
        else:
            cur += ch
    if cur.strip():
        out.append(cur.strip())
    return out


def names(t):
    t = t.strip()
    if t.startswith('['):
        return [x.strip() for x in t[1:-1].split(',') if x.strip()]
    return [t]


def parse_ann(s):
    rel = {}
    for item in split_top(s):
        m = re.match(r'^(\[[^\]]*\]|\S+)\s+(CF|SB|SBR|C|SBRS|SA|SAR|EXT)\s+(\[[^\]]*\]|\S+)$', item)
        if not m:
            continue
        for p in names(m.group(1)):
            for q in names(m.group(3)):
                rel[(p, q)] = m.group(2)
    return rel


def pairs(line):
    return re.findall(r'\(([^,()]+), ([^,()]+)\)', line)


def port_of(label):
    m = re.match(r'^([^.\s]+)\.([^\[\s]+)(?:\[(\d+)\])?$', label)
    if not m:
        return None
    return m.group(1), m.group(2), (int(m.group(3)) if m.group(3) is not None else None)


def parse_dump(path):
    lines = open(path).read().split('\n')
    D = {'inputs': [], 'kids': {}, 'steps': [], 'disj': [], 'urg': [], 'me': [], 'usecond': {},
         'facts': []}
    sec, cur, kid, meth = None, None, None, None
    for ln in lines:
        if ln.startswith('inputs:'):
            D['inputs'] = re.findall(r'(\S+) ::', ln)
        if ln.startswith('children:'):
            sec = 'kids'
            continue
        if ln.startswith('execution order:'):
            sec = 'steps'
            continue
        if ln.startswith('use conditions (recomputed):'):
            sec = 'uc'
            continue
        if ln.startswith('disjointness bsc relied on'):
            sec = 'facts'
            continue
        if ln and not ln.startswith(' '):
            if not ln.startswith('execution order'):
                sec = None if sec not in ('facts',) else sec
        if sec == 'kids':
            m = re.match(r'^  (\S+) : (\S+)\s+\[(.*)\]\s*$', ln)
            if m:
                kid = m.group(1)
                D['kids'][kid] = {'kind': m.group(2), 'ann': parse_ann(m.group(3)), 'meths': {}}
                continue
            m = re.match(r'^    (\S+) \((action|value|actionvalue)\)', ln)
            if m and kid:
                meth = m.group(1)
                D['kids'][kid]['meths'][meth] = {'kind': m.group(2), 'copies': []}
                continue
            m = re.match(r'^      copy (\S+):', ln)
            if m and kid and meth:
                D['kids'][kid]['meths'][meth]['copies'].append(m.group(1))
            continue
        if sec == 'steps':
            m = re.match(r'^  (\d+)\. (\S+)\s+\[(value method|method|rule)\s*([^\s\]]*)', ln)
            if m:
                kind = {'value method': 'value', 'method': 'method', 'rule': 'rule'}[m.group(3)]
                cur = {'name': m.group(2), 'kind': kind, 'meth': m.group(4) if kind != 'rule' else None,
                       'reads': [], 'calls': [], 'ooo': [], 'guard': None, 'outputs': [], 'tasks': 0}
                D['steps'].append(cur)
                continue
            if cur is None:
                continue
            s = ln.strip()
            m = re.match(r'^reads (\S+) out of order: (.*)$', s)
            if m:
                cur['ooo'].append((m.group(1), m.group(2)))
                continue
            m = re.match(r'^reads (\S+) when (.*)$', s) or re.match(r'^reads (\S+)\s+\(hint: used when (.*)\)$', s)
            if m:
                cur['reads'].append((m.group(1), m.group(2)))
                continue
            m = re.match(r'^calls (\S+) when (.*?) with ?(.*)$', s)
            if m:
                cur['calls'].append((m.group(1), m.group(2), m.group(3)))
                continue
            m = re.match(r'^guard (.*)$', s)
            if m:
                cur['guard'] = m.group(1)
                continue
            m = re.match(r'^(\d+) system task', s)
            if m:
                cur['tasks'] += int(m.group(1))
            continue
        if sec == 'uc':
            m = re.match(r'^  (\S+) = (.*)$', ln)
            if m:
                D['usecond'][m.group(1)] = m.group(2)
            continue
        if ln.startswith('disjoint guards (bsc):'):
            D['disj'] = pairs(ln)
        elif ln.startswith('urgency blocking'):
            D['urg'] = pairs(ln)
        elif ln.startswith('mutually_exclusive (user):'):
            D['me'] = pairs(ln)
        if sec == 'facts':
            m = re.match(r'^  (confirmed|UNCONFIRMED) (\S+) / (\S+): (\S+)(?: \(any of the (\d+) uses before\))? then (\S+)', ln)
            if m:
                D['facts'].append({'ok': m.group(1) == 'confirmed', 'a': m.group(2), 'b': m.group(3),
                                   'p': m.group(4), 'q': m.group(6)})
    return D


# ---------------------------------------------------------------- formulas

class FS:
    """Hash-consed monotone formulas over path literals."""
    T, F = 0, 1

    def __init__(self):
        self.nodes = [('T',), ('F',)]
        self.idx = {('T',): 0, ('F',): 1}

    def mk(self, n):
        i = self.idx.get(n)
        if i is None:
            i = self.idx[n] = len(self.nodes)
            self.nodes.append(n)
        return i

    def lit(self, key, pol):
        return self.mk(('lit', key, pol))

    def AND(self, xs):
        out = set()
        for x in xs:
            if x == self.F:
                return self.F
            if x == self.T:
                continue
            n = self.nodes[x]
            if n[0] == 'and':
                out.update(n[1])
            else:
                out.add(x)
        if not out:
            return self.T
        if len(out) == 1:
            return next(iter(out))
        return self.mk(('and', tuple(sorted(out))))

    def conj(self, x):
        n = self.nodes[x]
        return frozenset(n[1]) if n[0] == 'and' else frozenset([x])

    def OR(self, xs):
        out = set()
        for x in xs:
            if x == self.T:
                return self.T
            if x == self.F:
                continue
            n = self.nodes[x]
            if n[0] == 'or':
                out.update(n[1])
            else:
                out.add(x)
        if not out:
            return self.F
        if len(out) > 1 and len(out) <= 64:
            # absorption, as schedlean's orAll: x || (c && x) is x
            cs = {x: self.conj(x) for x in out}
            out = set(x for x in out if not any(y != x and cs[y] < cs[x] for y in out))
        if len(out) == 1:
            return next(iter(out))
        return self.mk(('or', tuple(sorted(out))))

    def lits(self, f, memo=None):
        """The (key, pol) literals of formula f."""
        if memo is None:
            memo = {}
        if f in memo:
            return memo[f]
        n = self.nodes[f]
        if n[0] == 'lit':
            r = {(n[1], n[2])}
        elif n[0] in ('and', 'or'):
            r = set()
            for x in n[1]:
                r |= self.lits(x, memo)
        else:
            r = set()
        memo[f] = r
        return r

    def subst(self, f, drop, memo=None):
        """f with every literal key in `drop` replaced by true."""
        if memo is None:
            memo = {}
        if f in memo:
            return memo[f]
        n = self.nodes[f]
        if n[0] == 'lit':
            r = self.T if n[1] in drop else f
        elif n[0] == 'and':
            r = self.AND([self.subst(x, drop, memo) for x in n[1]])
        elif n[0] == 'or':
            r = self.OR([self.subst(x, drop, memo) for x in n[1]])
        else:
            r = f
        memo[f] = r
        return r


# ---------------------------------------------------------------- the design

def canon_expr(e):
    t = e[0]
    if t in ('id', 'lit'):
        return e[1]
    if t == 'call':
        return '%s.%s(%s)' % (e[1], e[2], ','.join(canon_expr(a) for a in e[3]))
    if t == 'if':
        return '(if %s %s %s)' % tuple(canon_expr(x) for x in e[1:])
    if t == 'case':
        return '(case %s %s %s)' % (canon_expr(e[1]), ' '.join('%s:%s' % (canon_expr(a), canon_expr(b)) for a, b in e[2]),
                                    canon_expr(e[3]) if e[3] else '')
    if t == 'sel':
        return '%s[%s]' % (canon_expr(e[1]), canon_expr(e[2]))
    if t == 'app':
        return '(%s %s)' % (canon_expr(e[1]), ' '.join(canon_expr(a) for a in e[2]))
    if t == 'bin':
        return '(%s %s %s)' % (canon_expr(e[2]), e[1], canon_expr(e[3]))
    if t == 'un':
        return '(%s%s)' % (e[1], canon_expr(e[2]))
    if t == 'tup':
        return '(%s)' % ','.join(canon_expr(a) for a in e[1])
    return repr(e)


class Design:
    def __init__(self, name, dump, ap):
        self.name, self.dump, self.ap = name, dump, ap
        self.defs = dict(ap.defs)
        self.types = dict(ap.types)
        # bsc's use-condition names: use_cond_k from the dump, bsc_uses_k = __dusesk
        for k, src in dump['usecond'].items():
            try:
                self.defs[k] = parse_expr(src, ap.insts)
                self.types[k] = 'Bit 1'
            except Exception:
                pass
        for k in list(ap.defs):
            if k.startswith('__duses'):
                self.defs['bsc_uses_' + k[len('__duses'):]] = ap.defs[k]
                self.types['bsc_uses_' + k[len('__duses'):]] = 'Bit 1'
        self.inputs = set(dump['inputs'])
        self._reads, self._hw, self._canon, self._calls = {}, {}, {}, {}
        # one output wire, two ports (FIFO2's `same`): notEmpty = i_notEmpty, notFull = i_notFull
        self.alias = {}
        for k, kd in dump['kids'].items():
            ms = kd['meths']
            for a, b in (('notEmpty', 'i_notEmpty'), ('notFull', 'i_notFull')):
                if a in ms and b in ms:
                    self.alias[(k, a)] = b

    # -- Boolean view for the SAT encoder
    def bool_def(self, n):
        if n in self.defs and self.types.get(n, '').strip() in ('Bit 1', 'Bool', 'Prelude.Bool'):
            return self.defs[n]
        return None

    def boolish(self, e):
        if e[0] == 'id':
            return self.types.get(e[1], '').strip() in ('Bit 1', 'Bool', 'Prelude.Bool')
        if e[0] == 'bin' and e[1] in ('&&', '||', '==', '!=', '<', '<=', '>', '>=', '.<', '.<=', '.>', '.>='):
            return True
        if e[0] == 'un' and e[1] == '!':
            return True
        if e[0] == 'lit':
            return bool(re.match(r"^1'", e[1]))
        return False

    def canon(self, e):
        """A canonical string, alias definitions resolved."""
        if e[0] == 'id':
            n = e[1]
            if n in self._canon:
                return self._canon[n]
            self._canon[n] = n
            de = self.defs.get(n)
            if de is not None and (de[0] in ('id', 'call') or (de[0] == 'app' and de[1][0] == 'id'
                                                                and de[1][1] in ('zeroExtend', 'signExtend', 'truncate'))):
                c = self.canon(de) if de[0] in ('id', 'call') else n
            else:
                c = n
            self._canon[n] = c
            return c
        if e[0] == 'call':
            m = self.alias.get((e[1], e[2]), e[2])
            return '%s.%s(%s)' % (e[1], m, ','.join(self.canon(a) for a in e[3]))
        if e[0] in ('lit',):
            return e[1]
        return canon_expr(e)

    # -- the ports a definition or expression reads (through definitions, not WILL_FIRE)
    def def_reads(self, n):
        r = self._reads.get(n)
        if r is not None:
            return r
        self._reads[n] = frozenset()
        de = self.defs.get(n)
        r = frozenset(self.expr_reads(de)) if de is not None else frozenset()
        self._reads[n] = r
        return r

    def expr_reads(self, e, acc=None):
        if acc is None:
            acc = set()
        stack = [e]
        while stack:
            x = stack.pop()
            t = x[0]
            if t == 'id':
                n = x[1]
                if n.startswith('WILL_FIRE_'):
                    continue
                if n in self.defs:
                    acc |= self.def_reads(n)
            elif t == 'call':
                acc.add((x[1], x[2]))
                stack.extend(x[3])
            elif t == 'if':
                stack.extend(x[1:4])
            elif t == 'case':
                stack.append(x[1])
                for a, b in x[2]:
                    stack.extend([a, b])
                if x[3] is not None:
                    stack.append(x[3])
            elif t == 'sel':
                stack.extend([x[1], x[2]])
            elif t == 'app':
                stack.append(x[1])
                stack.extend(x[2])
            elif t == 'bin':
                stack.extend([x[2], x[3]])
            elif t == 'un':
                stack.append(x[2])
            elif t == 'tup':
                stack.extend(x[1])
        return acc

    # -- hardware-only names an expression reaches (WILL_FIRE refs, inputs)
    def def_hw(self, n):
        r = self._hw.get(n)
        if r is not None:
            return r
        self._hw[n] = frozenset()
        de = self.defs.get(n)
        r = frozenset(self.expr_hw(de)) if de is not None else frozenset()
        self._hw[n] = r
        return r

    def expr_hw(self, e):
        acc = set()
        stack = [e]
        while stack:
            x = stack.pop()
            t = x[0]
            if t == 'id':
                n = x[1]
                if n.startswith('WILL_FIRE_'):
                    acc.add(n)
                elif n in self.defs:
                    acc |= self.def_hw(n)
                elif n in self.inputs:
                    acc.add(n)
            elif t == 'call':
                stack.extend(x[3])
            elif t == 'if':
                stack.extend(x[1:4])
            elif t == 'case':
                stack.append(x[1])
                for a, b in x[2]:
                    stack.extend([a, b])
                if x[3] is not None:
                    stack.append(x[3])
            elif t == 'sel':
                stack.extend([x[1], x[2]])
            elif t == 'app':
                stack.append(x[1])
                stack.extend(x[2])
            elif t == 'bin':
                stack.extend([x[2], x[3]])
            elif t == 'un':
                stack.append(x[2])
            elif t == 'tup':
                stack.extend(x[1])
        return acc

    # -- arguments of value-method calls: port -> ports its argument reads
    def def_calls(self, n):
        r = self._calls.get(n)
        if r is not None:
            return r
        self._calls[n] = {}
        de = self.defs.get(n)
        r = self.expr_calls(de) if de is not None else {}
        self._calls[n] = r
        return r

    def expr_calls(self, e):
        acc = {}
        stack = [e]
        while stack:
            x = stack.pop()
            t = x[0]
            if t == 'id':
                if x[1].startswith('WILL_FIRE_'):
                    continue
                if x[1] in self.defs:
                    for k, v in self.def_calls(x[1]).items():
                        acc.setdefault(k, set()).update(v)
            elif t == 'call':
                s = acc.setdefault((x[1], x[2]), set())
                for a in x[3]:
                    s |= self.expr_reads(a)
                stack.extend(x[3])
            elif t == 'if':
                stack.extend(x[1:4])
            elif t == 'case':
                stack.append(x[1])
                for a, b in x[2]:
                    stack.extend([a, b])
                if x[3] is not None:
                    stack.append(x[3])
            elif t == 'sel':
                stack.extend([x[1], x[2]])
            elif t == 'app':
                stack.append(x[1])
                stack.extend(x[2])
            elif t == 'bin':
                stack.extend([x[2], x[3]])
            elif t == 'un':
                stack.append(x[2])
            elif t == 'tup':
                stack.extend(x[1])
        return acc

    def array_elems(self, arr):
        seen = 0
        while arr[0] == 'id' and arr[1] in self.defs and seen < 50:
            arr = self.defs[arr[1]]
            seen += 1
        if arr[0] == 'app' and arr[1] == ('id', 'PrimBuildArray'):
            return arr[2]
        return None

    def width(self, e):
        if e[0] == 'id':
            m = re.match(r'^Bit (\d+)$', self.types.get(e[1], '').strip())
            if m:
                return int(m.group(1))
        if e[0] == 'lit':
            m = re.match(r"^(\d+)'", e[1])
            if m:
                return int(m.group(1))
        return None


class Reach:
    """schedlean's reach rule: && and || unconditional, selection arms conditional."""

    def __init__(self, D, fs, targets, mode=None, isR=None, flips=frozenset()):
        self.D, self.fs, self.targets = D, fs, targets
        self.mode, self.isR, self.flips = mode, isR, flips   # mode 'seq': the readiness rule
        self.lit = {}          # key -> ('c', expr) | ('eq', idx, lab)
        self.memo = {}

    def litkey(self, kind, *xs):
        key = (kind,) + tuple(canon_expr(x) for x in xs)
        if key not in self.lit:
            self.lit[key] = (kind,) + xs
        return key

    def edges(self, e, c, acc):
        D, fs = self.D, self.fs
        stack = [(e, c)]
        while stack:
            x, c = stack.pop()
            if c == fs.F:
                continue
            t = x[0]
            if t == 'id':
                n = x[1]
                if n.startswith('WILL_FIRE_') or n not in D.defs:
                    continue
                if D.def_reads(n) & self.targets:
                    acc.setdefault(('d', n), []).append(c)
            elif t == 'call':
                p = (x[1], x[2])
                if p in self.targets:
                    acc.setdefault(('p', p), []).append(c)
                for a in x[3]:
                    stack.append((a, c))
            elif t == 'if':
                k = self.litkey('c', x[1])
                stack.append((x[1], c))
                stack.append((x[2], fs.AND([c, fs.lit(k, True)])))
                stack.append((x[3], fs.AND([c, fs.lit(k, False)])))
            elif t == 'case':
                stack.append((x[1], c))
                ks = []
                for lab, val in x[2]:
                    k = self.litkey('eq', x[1], lab)
                    ks.append(k)
                    stack.append((val, fs.AND([c, fs.lit(k, True)])))
                if x[3] is not None:
                    stack.append((x[3], fs.AND([c] + [fs.lit(k, False) for k in ks])))
            elif t == 'sel':
                stack.append((x[2], c))
                es = D.array_elems(x[1])
                if es is None:
                    stack.append((x[1], c))
                else:
                    w = D.width(x[2])
                    n = len(es) if (w is None or w >= 20) else min(len(es), 2 ** w)
                    for kk in range(n):
                        k = self.litkey('eq', x[2], ('lit', str(kk)))
                        stack.append((es[kk], fs.AND([c, fs.lit(k, True)])))
            elif t == 'app':
                for a in x[2]:
                    stack.append((a, c))
            elif t == 'bin' and self.mode == 'seq' and x[1] in ('&&', '||'):
                # Ravi's rule (2026-11-02): operands in bsc's expression order; a later operand
                # is reached only under the earlier one's short-circuit outcome
                a, b = x[2], x[3]
                ka = self.litkey('c', a)
                stack.append((a, c))
                stack.append((b, fs.AND([c, fs.lit(ka, x[1] == '&&')])))
            elif t == 'bin' and SC and x[1] in ('&&', '||'):
                # option: both-way short-circuit (each operand selected by the other)
                ka, kb = self.litkey('c', x[2]), self.litkey('c', x[3])
                pol = x[1] == '&&'
                stack.append((x[2], fs.AND([c, fs.lit(kb, pol)])))
                stack.append((x[3], fs.AND([c, fs.lit(ka, pol)])))
            elif t == 'bin':
                stack.append((x[2], c))
                stack.append((x[3], c))
            elif t == 'un':
                stack.append((x[2], c))
            elif t == 'tup':
                for a in x[1]:
                    stack.append((a, c))
        return acc

    def R(self, n):
        """port -> condition under which a read reaches definition n's value."""
        r = self.memo.get(n)
        if r is not None:
            return r
        self.memo[n] = {}
        acc = self.edges(self.D.defs[n], self.fs.T, {})
        out = {}
        for key, cs in acc.items():
            c = self.fs.OR(cs)
            if key[0] == 'p':
                out.setdefault(key[1], []).append(c)
            else:
                for p, f in self.R(key[1]).items():
                    out.setdefault(p, []).append(self.fs.AND([c, f]))
        r = {p: self.fs.OR(v) for p, v in out.items()}
        self.memo[n] = r
        return r

    def top(self, items):
        """items: [(expr, pathcond)] -> port -> reach condition."""
        acc = {}
        for e, c in items:
            self.edges(e, c, acc)
        out = {}
        for key, cs in acc.items():
            c = self.fs.OR(cs)
            if key[0] == 'p':
                out.setdefault(key[1], []).append(c)
            else:
                for p, f in self.R(key[1]).items():
                    out.setdefault(p, []).append(self.fs.AND([c, f]))
        return {p: self.fs.OR(v) for p, v in out.items()}


# ---------------------------------------------------------------- analysis

FIFOISH = ('first', 'i_notEmpty', 'notEmpty')


def analyse(ddir, outdir, workdir):
    name = os.path.basename(ddir.rstrip('/'))
    dump = parse_dump(os.path.join(ddir, 'dump.txt'))
    ap = APkg(open(os.path.join(DATA, name + '.ba.txt')).read())
    D = Design(name, dump, ap)
    kids, steps = dump['kids'], dump['steps']
    log = []
    res = {'design': name, 'steps': len(steps), 'parse_errors': len(ap.errors)}

    # child relation (bsc's default C for unlisted pairs; two copies of one method CF)
    def crel(k, p, q):
        ann = kids[k]['ann']
        if p[0] == q[0] and p[1] != q[1]:
            return 'CF'
        r = ann.get((p[0], q[0]))
        if r is not None:
            return r
        if ann.get((q[0], p[0])) is not None:
            return 'rev'
        return 'C'

    def prec(k, p, q):
        return p != q and crel(k, p, q) in ('SB', 'SBR')

    def conf(k, p, q):
        return crel(k, p, q) == 'C' or crel(k, q, p) == 'C'

    def follow(k, a, b):
        return not prec(k, b, a) and not conf(k, a, b)

    def group(k, a, b):
        return follow(k, a, b) and crel(k, a, b) != 'SBR' and crel(k, b, a) != 'SBR'

    def is_value(k, m):
        mt = kids[k]['meths'].get(m)
        return mt is not None and mt['kind'] == 'value'

    # sites per step: reads then calls, as the dump lists them
    for i, st in enumerate(steps):
        st['idx'] = i
        st['sites'] = []
        for lab, cond in st['reads']:
            pt = port_of(lab)
            if pt and pt[0] in kids:
                st['sites'].append({'kind': 'read', 'label': lab, 'inst': pt[0], 'meth': pt[1],
                                    'copy': pt[2], 'bsc': cond, 'need': set(), 'partners': []})
        for lab, cond, args in st['calls']:
            pt = port_of(lab)
            if pt and pt[0] in kids:
                st['sites'].append({'kind': 'call', 'label': lab, 'inst': pt[0], 'meth': pt[1],
                                    'copy': pt[2], 'cond': cond, 'need': set(), 'partners': []})
    nsites = sum(len(st['sites']) for st in steps)
    nreads = sum(1 for st in steps for s in st['sites'] if s['kind'] == 'read')
    res['sites'], res['reads'] = nsites, nreads

    exc = set()
    for (a, b) in dump['disj'] + dump['me'] + dump['urg']:
        exc.add((a, b))
        exc.add((b, a))
    rules = [st for st in steps if st['kind'] == 'rule']

    def use(s):
        return (s['meth'], '' if s['copy'] is None else '[%d]' % s['copy'])

    # (a)/(c): the h4 scan per use, and grp within a step
    act_pairs = 0
    for x in range(len(rules)):
        for y in range(x + 1, len(rules)):
            r1, r2 = rules[x], rules[y]
            if (r1['name'], r2['name']) in exc:
                continue
            for s1 in r1['sites']:
                for s2 in r2['sites']:
                    if s1['inst'] != s2['inst']:
                        continue
                    k = s1['inst']
                    if follow(k, use(s1), use(s2)):
                        continue
                    if s2['kind'] == 'read' and s1['kind'] == 'call':
                        s2['need'].add('a')
                        s2['partners'].append((r1, s1))
                    elif s1['kind'] == 'read' and s2['kind'] == 'call':
                        s1['need'].add('a')
                        s1['partners'].append((r2, s2))
                    elif s1['kind'] == 'read' and s2['kind'] == 'read':
                        s1['need'].add('c')
                        s1['partners'].append((r2, s2))
                    else:
                        act_pairs += 1
    grp_fail = 0
    for st in steps:
        ss = st['sites']
        for x in range(len(ss)):
            for y in range(x + 1, len(ss)):
                if ss[x]['inst'] == ss[y]['inst'] and not group(ss[x]['inst'], use(ss[x]), use(ss[y])):
                    grp_fail += 1
    res['act_pairs'], res['grp_fail'] = act_pairs, grp_fail

    # The rule rank is the generator's to choose.  Pairs failing both ways need a
    # dynamic fact whatever the rank; the others only fix an order.
    def okT(r1, r2):
        return all(follow(s1['inst'], use(s1), use(s2)) for s1 in r1['sites'] for s2 in r2['sites']
                   if s1['inst'] == s2['inst'])
    nR = len(rules)
    must = [set() for _ in range(nR)]
    two_way = 0
    for x in range(nR):
        for y in range(x + 1, nR):
            if (rules[x]['name'], rules[y]['name']) in exc:
                continue
            fxy, fyx = okT(rules[x], rules[y]), okT(rules[y], rules[x])
            if not fxy and not fyx:
                two_way += 1
            elif not fxy:
                must[y].add(x)          # y must precede x
            elif not fyx:
                must[x].add(y)
    # rank: Kahn on `must`, bsc's order breaking ties and cycles
    import heapq
    indeg = [0] * nR
    out_ = [[] for _ in range(nR)]
    for a in range(nR):
        for b in must[a]:
            out_[a].append(b)
            indeg[b] += 1
    heap = [a for a in range(nR) if indeg[a] == 0]
    heapq.heapify(heap)
    rank, seen = [], set()
    while len(rank) < nR:
        if not heap:
            a = min(x for x in range(nR) if x not in seen)     # a cycle: take bsc's next
            indeg[a] = 0
            heapq.heappush(heap, a)
        a = heapq.heappop(heap)
        if a in seen:
            continue
        seen.add(a)
        rank.append(a)
        for b in out_[a]:
            indeg[b] -= 1
            if indeg[b] == 0 and b not in seen:
                heapq.heappush(heap, b)
    pos = {a: i for i, a in enumerate(rank)}
    rr_fail, rr_reads = 0, set()
    for x in range(nR):
        for y in range(nR):
            if x == y or pos[x] > pos[y] or (rules[x]['name'], rules[y]['name']) in exc:
                continue
            r1, r2 = rules[x], rules[y]
            bad = False
            for s1 in r1['sites']:
                for s2 in r2['sites']:
                    if s1['inst'] == s2['inst'] and not follow(s1['inst'], use(s1), use(s2)):
                        bad = True
                        if s2['kind'] == 'read' and s1['kind'] == 'call':
                            rr_reads.add((r2['name'], s2['label']))
                        elif s1['kind'] == 'read':
                            rr_reads.add((r1['name'], s1['label']))
            rr_fail += bad
    res['two_way'], res['rerank_fail_pairs'], res['rerank_reads'] = two_way, rr_fail, len(rr_reads)
    res['rerank_moved'] = sum(1 for i, a in enumerate(rank) if a != i)
    rerank_reads = rr_reads

    # (b): shared value copies read with different arguments by steps that may fire together
    stepby = {st['name']: st for st in steps}
    shared = []
    for k, kd in kids.items():
        for m, mt in kd['meths'].items():
            if mt['kind'] != 'value' or len(mt['copies']) < 2:
                continue
            al = ap.ralloc.get('%s.%s' % (k, m), [])
            umap = {src: [r for lst in rr for r in lst] for src, rr in ap.uses_map.get('%s.%s' % (k, m), [])}
            byport = {}
            for src, pn in al:
                byport.setdefault(pn, []).append(src)
            for pn, srcs in byport.items():
                srcs = sorted(set(srcs))
                if len(srcs) < 2:
                    continue
                users = [(src, r) for src in srcs for r in umap.get(src, [])]
                clash = [(r1, r2) for (s1, r1) in users for (s2, r2) in users
                         if s1 < s2 and r1 != r2 and (r1, r2) not in exc]
                shared.append((k, m, pn - 1, srcs, users, clash))
                if clash:
                    involved = set(r for c in clash for r in c)
                    order = sorted((stepby[r]['idx'], r) for r in involved if r in stepby)
                    top = order[0][1] if order else None
                    for r in involved:
                        st = stepby.get(r)
                        if not st:
                            continue
                        for s in st['sites']:
                            if s['kind'] == 'read' and s['inst'] == k and s['meth'] == m and s['copy'] == pn - 1:
                                s['need'].add('b')
                                s['exact_hw'] = (r == top)
    res['shared_copies'] = len(shared)
    res['shared_clash'] = sum(1 for x in shared if x[5])

    # (r): readiness of value reads whose port has a ready signal
    def rdy_port(k, m):
        ms = kids[k]['meths']
        if m == 'first' and ('i_notEmpty' in ms or 'notEmpty' in ms):
            if not ap.guarded.get(k, True):
                return None          # an unguarded FIFO: first is always ready
            return [(k, x) for x in ('i_notEmpty', 'notEmpty') if x in ms]
        if 'RDY_' + m in ms:
            return [(k, 'RDY_' + m)]
        return None

    # the step's top expressions
    def step_items(st):
        items, acts = [], []
        r = ap.rules.get(st['name'])
        if r is not None:
            pred, acts = r
            items.append((pred, None))
        elif st['kind'] == 'value':
            g = 'CAN_FIRE_' + st['name']
            if g in D.defs:
                items.append((('id', g), None))
        if st['kind'] in ('value', 'method') and st['meth'] in ap.ifc_vals:
            items.append((ap.ifc_vals[st['meth']], None))
        if st['kind'] == 'value' and st['name'] in ap.ifc_vals:
            items.append((ap.ifc_vals[st['name']], None))
        for a in acts:
            if a is None:
                continue
            if a[0] == 'call':
                cond, args = a[3], a[4]
            elif a[0] == 'task':
                cond, args = a[2], a[3]
            else:
                continue
            if cond is not None:
                items.append((cond, None))
            for x in args:
                items.append((x, ('c', cond) if cond is not None else None))
        return items, acts

    def guard_expr(st):
        r = ap.rules.get(st['name'])
        if r is not None:
            return r[0]
        g = 'CAN_FIRE_' + st['name']
        if g in D.defs:
            return ('id', g)
        return ('lit', "1'd1")

    ready_bad = []
    for st in steps:
        g = guard_expr(st)
        for s in st['sites']:
            if s['kind'] != 'read':
                continue
            rp = rdy_port(s['inst'], s['meth'])
            if not rp:
                continue
            E = Enc(D)
            gl = E.b(g)
            rl = E.OR([E.atom(('port', D.canon(('call', p[0], p[1], [])))) for p in rp])
            sat = E.sat([gl, -rl], workdir)
            if sat:
                s['need'].add('r')
                s['rdy'] = rp
                ready_bad.append((st['name'], s['label']))
    res['ready_cand'] = sum(1 for st in steps for s in st['sites'] if s['kind'] == 'read' and rdy_port(s['inst'], s['meth']))
    res['ready_bad'] = len(ready_bad)

    # reach conditions for the needed sites
    fs = FS()
    targets = set((s['inst'], s['meth']) for st in steps for s in st['sites'] if s['kind'] == 'read' and s['need'])
    RC = Reach(D, fs, frozenset(targets))
    need_sites = []
    for st in steps:
        nd = [s for s in st['sites'] if s['kind'] == 'read' and s['need']]
        if not nd:
            continue
        items, acts = step_items(st)
        top = RC.top([(e, fs.T if c is None else fs.lit(RC.litkey('c', c[1]), True)) for e, c in items])
        st['acts'] = acts
        for s in nd:
            s['reach'] = top.get((s['inst'], s['meth']), fs.F)
            need_sites.append((st, s))
    res['need'] = len(need_sites)
    res['need_by'] = {c: sum(1 for _, s in need_sites if c in s['need']) for c in 'abcr'}

    # literal reads, hardware-only literals
    litcache = {}

    def linfo(RCx, key):
        k2 = (id(RCx), key)
        if k2 not in litcache:
            e = RCx.lit[key][1]
            litcache[k2] = (D.expr_reads(e), D.expr_hw(e))
        return litcache[k2]

    def order_pass(isc, rkey, px, RCx, fsx, first=None):
        """Per step: the site order, cycles before weakening, dropped literals."""
        cyc_steps = cyc_sites = hard_cyc = 0
        dropped = dropped_hw = sites_to_true = 0
        for st in steps:
            nd = [s for s in st['sites'] if isc(s)]
            if not nd:
                continue
            own = set(ap.meth_args.get(st['meth'] or '', [])) | ({'EN_' + st['meth']} if st['meth'] else set())
            ss = st['sites']
            n = len(ss)
            byport = {}
            for i, s in enumerate(ss):
                byport.setdefault((s['inst'], s['meth']), []).append(i)
            hard = [set() for _ in range(n)]
            soft = [set() for _ in range(n)]
            # data dependence: read arguments, call conditions and arguments
            items, acts = step_items(st)
            argreads = {}
            for e, _ in items:
                for p, rds in D.expr_calls(e).items():
                    argreads.setdefault(p, set()).update(rds)
            callacts = {}
            for a in acts:
                if a and a[0] == 'call':
                    callacts.setdefault((a[1], a[2]), []).append(a)
            for i, s in enumerate(ss):
                rds = set()
                if s['kind'] == 'read':
                    rds |= argreads.get((s['inst'], s['meth']), set())
                else:
                    for a in callacts.get((s['inst'], s['meth']), []):
                        if a[3] is not None:
                            rds |= D.expr_reads(a[3])
                        for x in a[4]:
                            rds |= D.expr_reads(x)
                for p in rds:
                    for j in byport.get(p, []):
                        if j != i:
                            hard[i].add(j)
            # the child's in-step SB order
            for i, s in enumerate(ss):
                for j, t in enumerate(ss):
                    if i != j and s['inst'] == t['inst'] and prec(s['inst'], use(s), use(t)):
                        hard[j].add(i)
            # condition literals
            for i, s in enumerate(ss):
                if isc(s):
                    s[px + 'lits'] = fsx.lits(s[rkey])
                    for (key, pol) in s[px + 'lits']:
                        rd, hw = linfo(RCx, key)
                        for p in rd:
                            for j in byport.get(p, []):
                                soft[i].add(j)
            # cycles before weakening (hard + soft)
            def sccs(edges):
                idx, low, on, stk, out, cnt = {}, {}, set(), [], [], [0]

                def strong(v):
                    work = [(v, iter(edges[v]))]
                    idx[v] = low[v] = cnt[0]
                    cnt[0] += 1
                    stk.append(v)
                    on.add(v)
                    while work:
                        u, it = work[-1]
                        adv = False
                        for w in it:
                            if w not in idx:
                                idx[w] = low[w] = cnt[0]
                                cnt[0] += 1
                                stk.append(w)
                                on.add(w)
                                work.append((w, iter(edges[w])))
                                adv = True
                                break
                            elif w in on:
                                low[u] = min(low[u], idx[w])
                        if not adv:
                            work.pop()
                            if work:
                                low[work[-1][0]] = min(low[work[-1][0]], low[u])
                            if low[u] == idx[u]:
                                comp = []
                                while True:
                                    w = stk.pop()
                                    on.discard(w)
                                    comp.append(w)
                                    if w == u:
                                        break
                                out.append(comp)
                for v in range(n):
                    if v not in idx:
                        strong(v)
                return [c for c in out if len(c) > 1 or c[0] in edges[c[0]]]
            both = [hard[i] | soft[i] for i in range(n)]
            cs = sccs(both)
            if cs:
                cyc_steps += 1
                cyc_sites += sum(len(c) for c in cs)
                log.append('%sCYCLE in %s: %s' % (px, st['name'], [[ss[i]['label'] for i in c] for c in cs]))
            hc = sccs(hard)
            if hc:
                hard_cyc += 1
                log.append('HARD CYCLE in %s: %s' % (st['name'], [[ss[i]['label'] for i in c] for c in hc]))
                continue
            # order: Kahn on hard edges, preferring sites whose soft predecessors are placed
            placed, pos = set(), {}
            indeg = [len(hard[i]) for i in range(n)]
            succ = [[] for _ in range(n)]
            for i in range(n):
                for j in hard[i]:
                    succ[j].append(i)
            avail = set(i for i in range(n) if indeg[i] == 0)
            while avail:
                best = min(avail, key=lambda i: (0 if first and first(ss[i]) else 1, len(soft[i] - placed), i))
                avail.discard(best)
                pos[best] = len(placed)
                placed.add(best)
                for k in succ[best]:
                    indeg[k] -= 1
                    if indeg[k] == 0:
                        avail.add(k)
            st[px + 'order'] = sorted(range(n), key=lambda i: pos[i])
            for i, s in enumerate(ss):
                if not isc(s):
                    continue
                drop = set()
                for (key, pol) in s[px + 'lits']:
                    rd, hw = linfo(RCx, key)
                    bad_hw = bool(hw - own)
                    later = any(pos.get(j, n) >= pos[i] for p in rd for j in byport.get(p, [])) or \
                        any(p not in byport for p in rd)
                    if bad_hw or later:
                        drop.add(key)
                        if bad_hw:
                            dropped_hw += 1
                        else:
                            dropped += 1
                s[px + 'drop'] = drop
                if drop:
                    log.append('%sDROP in %s, site %s (position %d of %s): %s' % (
                        px, st['name'], s['label'], pos[i], [ss[j]['label'] for j in st[px + 'order']],
                        sorted(' '.join(k[1:]) for k in drop)))
                s[px + 'reach_d'] = fsx.subst(s[rkey], drop) if drop else s[rkey]
                if s[px + 'reach_d'] == fsx.T and s[rkey] != fsx.T:
                    sites_to_true += 1

        return cyc_steps, cyc_sites, hard_cyc, dropped, dropped_hw, sites_to_true

    (res['cyc_steps'], res['cyc_sites'], res['hard_cyc'], res['dropped'], res['dropped_hw'],
     res['to_true']) = order_pass(lambda s: s['kind'] == 'read' and bool(s['need']), 'reach', '', RC, fs)
    for st, s in need_sites:
        s['cond_used'] = fs.T if s.get('exact_hw') else s['reach_d']

    # stress: every read conditional on its reach condition (cycles and drops only)
    if os.environ.get('SITES_ALL'):
        fs2 = FS()
        tg = set((s['inst'], s['meth']) for st in steps for s in st['sites'] if s['kind'] == 'read')
        RC2 = Reach(D, fs2, frozenset(tg))
        nall = ntrue = 0
        for st in steps:
            rd_ = [s for s in st['sites'] if s['kind'] == 'read']
            if not rd_:
                continue
            items, acts = step_items(st)
            top = RC2.top([(e, fs2.T if c is None else fs2.lit(RC2.litkey('c', c[1]), True)) for e, c in items])
            for s in rd_:
                s['all_reach'] = top.get((s['inst'], s['meth']), fs2.F)
                nall += 1
                ntrue += s['all_reach'] == fs2.T
        r2 = order_pass(lambda s: s['kind'] == 'read' and s.get('all_reach', fs2.T) not in (fs2.T,),
                        'all_reach', 'all_', RC2, fs2)
        res['all'] = {'reads': nall, 'reach_true': ntrue, 'cyc_steps': r2[0], 'cyc_sites': r2[1],
                      'hard_cyc': r2[2], 'dropped': r2[3], 'dropped_hw': r2[4], 'to_true': r2[5]}

    res['exact_hw'] = sum(1 for _, s in need_sites if s.get('exact_hw'))
    res['reach_true'] = sum(1 for _, s in need_sites if s.get('reach') == fs.T)

    # SAT: encode formulas
    def enc_f(E, f, memo):
        if f in memo:
            return memo[f]
        nd_ = fs.nodes[f]
        if nd_[0] == 'T':
            r = 1
        elif nd_[0] == 'F':
            r = -1
        elif nd_[0] == 'lit':
            L = RC.lit[nd_[1]]
            v = E.b(L[1]) if L[0] == 'c' else E.eqatom(L[1], L[2])
            r = v if nd_[2] else -v
        elif nd_[0] == 'and':
            r = E.AND([enc_f(E, x, memo) for x in nd_[1]])
        else:
            r = E.OR([enc_f(E, x, memo) for x in nd_[1]])
        memo[f] = r
        return r

    def bsc_expr(s):
        try:
            return parse_expr(s['bsc'], ap.insts)
        except Exception:
            return None

    # completeness: bsc's use condition against the reach condition (before and after dropping)
    cmp = {'eq': 0, 'bsc_stronger': 0, 'bsc_weaker': 0, 'incomparable': 0, 'unparsed': 0}
    cmp_d = dict(cmp)
    for st, s in need_sites:
        be = bsc_expr(s)
        if be is None:
            cmp['unparsed'] += 1
            cmp_d['unparsed'] += 1
            continue
        for f, tab, tag in ((s['reach'], cmp, 'cmp'), (s.get('cond_used', s['reach']), cmp_d, 'cmp_d')):
            E = Enc(D)
            b = E.b(be)
            r = enc_f(E, f, {})
            b_imp_r = not E.sat([b, -r], workdir)
            E2 = Enc(D)
            b2 = E2.b(be)
            r2 = enc_f(E2, f, {})
            r_imp_b = not E2.sat([r2, -b2], workdir)
            if b_imp_r and r_imp_b:
                k = 'eq'
            elif b_imp_r:
                k = 'bsc_stronger'
            elif r_imp_b:
                k = 'bsc_weaker'
            else:
                k = 'incomparable'
            tab[k] += 1
            s[tag] = k
    res['cmp'], res['cmp_d'] = cmp, cmp_d

    # readiness of the 'r' sites under their condition and under bsc's
    rdy_ok = {'used': 0, 'bsc': 0}
    for st, s in need_sites:
        if 'r' not in s['need']:
            continue
        for tag in ('used', 'bsc'):
            E = Enc(D)
            if tag == 'used':
                c = enc_f(E, s.get('cond_used', s['reach']), {})
            else:
                be = bsc_expr(s)
                c = E.b(be) if be is not None else 1
            rl = E.OR([E.atom(('port', D.canon(('call', p[0], p[1], [])))) for p in s['rdy']])
            if E.sat([E.b(guard_expr(st)), c, -rl], workdir) is False:
                rdy_ok[tag] += 1
                s['rdy_' + tag] = True
    res['rdy_ok'] = rdy_ok

    # (r) under the readiness rule (2026-11-02): short-circuit reach, a readiness operand first and
    # left to right otherwise; later-site literals dropped; where that fails, operator orientations
    # are searched (up to three flipped operators)
    RPORTS = set()
    for k_, kd_ in kids.items():
        for m_ in kd_['meths']:
            if m_ in ('i_notEmpty', 'i_notFull', 'notEmpty', 'notFull') or m_.startswith('RDY_'):
                RPORTS.add((k_, m_))

    def isR(e):
        return bool(D.expr_reads(e) & RPORTS)

    def enc_x(E, f, memo, RCx, fsx):
        if f in memo:
            return memo[f]
        nd_ = fsx.nodes[f]
        if nd_[0] == 'T':
            r = 1
        elif nd_[0] == 'F':
            r = -1
        elif nd_[0] == 'lit':
            L = RCx.lit[nd_[1]]
            v = E.b(L[1]) if L[0] == 'c' else E.eqatom(L[1], L[2])
            r = v if nd_[2] else -v
        elif nd_[0] == 'and':
            r = E.AND([enc_x(E, x, memo, RCx, fsx) for x in nd_[1]])
        else:
            r = E.OR([enc_x(E, x, memo, RCx, fsx) for x in nd_[1]])
        memo[f] = r
        return r

    def rdy_holds(st, s, f, RCx, fsx):
        E = Enc(D)
        c = enc_x(E, f, {}, RCx, fsx)
        rl = E.OR([E.atom(('port', D.canon(('call', p[0], p[1], [])))) for p in s['rdy']])
        return E.sat([E.b(guard_expr(st)), c, -rl], workdir) is False

    def opkeys(e, port, acc, seen):
        stack = [e]
        while stack:
            x = stack.pop()
            t = x[0]
            if t == 'id':
                if x[1] in D.defs and x[1] not in seen and port in D.def_reads(x[1]):
                    seen.add(x[1])
                    stack.append(D.defs[x[1]])
            elif t == 'bin':
                if x[1] in ('&&', '||') and port in D.expr_reads(x):
                    acc.add(canon_expr(x))
                stack.extend([x[2], x[3]])
            elif t == 'call':
                stack.extend(x[3])
            elif t == 'if':
                stack.extend(x[1:4])
            elif t == 'case':
                stack.append(x[1])
                for a_, b_ in x[2]:
                    stack.extend([a_, b_])
                if x[3] is not None:
                    stack.append(x[3])
            elif t == 'sel':
                stack.extend([x[1], x[2]])
            elif t == 'app':
                stack.append(x[1])
                stack.extend(x[2])
            elif t == 'un':
                stack.append(x[2])
            elif t == 'tup':
                stack.extend(x[1])
        return acc

    rsites = [(st, s) for st, s in need_sites if 'r' in s['need']]
    # the readiness rule: short-circuit reach sequenced by expression structure (no bias, no search);
    # the file's condition is the ready literal, so the check is guard & R_seq & !RDY UNSAT
    rb = {'sites': len(rsites), 'cons': sum(1 for _, s in rsites if s.get('rdy_used')), 'seq': 0, 'seq_d': 0,
          'cyc_steps': 0, 'cyc_sites': 0, 'later_lits': 0, 'sites_with_drop': 0, 'left': [], 'left_d': []}
    RCb = fsb = None
    if rsites:
        fsb = FS()
        RCb = Reach(D, fsb, frozenset((s['inst'], s['meth']) for _, s in rsites), mode='seq')
        for st, s in rsites:
            s['rb_mark'] = True
            items, _ = step_items(st)
            top = RCb.top([(e, fsb.T if c is None else fsb.lit(RCb.litkey('c', c[1]), True)) for e, c in items])
            s['reach_b'] = top.get((s['inst'], s['meth']), fsb.F)
        rfirst = lambda x: x['kind'] == 'read' and (x['inst'], x['meth']) in RPORTS
        r2 = order_pass(lambda s: bool(s.get('rb_mark')), 'reach_b', 'b_', RCb, fsb, rfirst)
        rb['cyc_steps'], rb['cyc_sites'], rb['later_lits'] = r2[0], r2[1], r2[3] + r2[4]
        for st, s in rsites:
            ok_b = rdy_holds(st, s, s['reach_b'], RCb, fsb)
            ok_bd = rdy_holds(st, s, s.get('b_reach_d', s['reach_b']), RCb, fsb)
            rb['seq'] += ok_b
            rb['seq_d'] += ok_bd
            rb['sites_with_drop'] += bool(s.get('b_drop'))
            s['rdy_bias'] = 'ok' if ok_b else 'fails'
            s['rdy_bias_d'] = ok_bd
            if not ok_b:
                rb['left'].append((st['name'], s['label']))
            if not ok_bd:
                rb['left_d'].append((st['name'], s['label']))
        # cyclic literals: those reading a conditional read of the same step (the site itself or
        # another one), dropped (weakened to true) for the file's use conditions; the rest stay
        for st, s in rsites:
            cports = set((x['inst'], x['meth']) for x in st['sites'] if x['kind'] == 'read' and x['need'])
            cyc = set(key for (key, pol) in fsb.lits(s['reach_b']) if linfo(RCb, key)[0] & cports)
            s['cyc_drop'] = cyc
            s['reach_c'] = fsb.subst(s['reach_b'], cyc) if cyc else s['reach_b']
            s['rdy_ok_c'] = rdy_holds(st, s, s['reach_c'], RCb, fsb)
            rb['cyc_lits'] = rb.get('cyc_lits', 0) + len(cyc)
            rb['cyc_lit_sites'] = rb.get('cyc_lit_sites', 0) + bool(cyc)
            rb['seq_c'] = rb.get('seq_c', 0) + s['rdy_ok_c']
            if not s['rdy_ok_c']:
                rb.setdefault('left_c', []).append((st['name'], s['label']))
    res['rdy_bias'] = rb

    # (r) in the site-value form (round 7 review s1 M1): a literal over a conditional read y of the
    # step reads y's site value ite(cond_y, port_y, fresh). Checked per site: readiness (guard & R &
    # !RDY UNSAT) and, where R reads another conditional read, invariance (guard & R != R_ports UNSAT)
    def cond_reads(st):
        """The step's conditional reads: (inst, meth) -> ('r', ready ports) | ('a', formula)."""
        out = {}
        for x in st['sites']:
            if x['kind'] != 'read' or not x['need']:
                continue
            if 'r' in x['need']:
                out[(x['inst'], x['meth'])] = ('r', x['rdy'])
            elif x.get('cond_used', fs.T) != fs.T:
                out[(x['inst'], x['meth'])] = ('a', x['cond_used'])
        return out

    def f_reads(f, RCx, fsx):
        rd = set()
        for key, pol in fsx.lits(f):
            rd |= set(D.expr_reads(RCx.lit[key][1]))
        return rd

    def rdy_site_check(st, s, drop=frozenset()):
        """The readiness facts of site s in the site-value form, literals over `drop` removed."""
        f = s['reach_b']
        if drop:
            f = fsb.subst(f, set(key for key, pol in fsb.lits(f) if linfo(RCb, key)[0] & set(drop)))
        crs = cond_reads(st)
        Y = sorted(f_reads(f, RCb, fsb) & set(crs))
        E = EncS(D)
        for y in Y:
            kind, c = crs[y]
            if kind == 'r':
                E.sub[y] = E.OR([E.atom(('port', D.canon(('call', p[0], p[1], [])))) for p in c])
            elif not (f_reads(c, RC, fs) & set(crs)):
                E.sub[y] = enc_f(E, c, {})
            else:
                E.sub[y] = E.atom(('cfree',) + y)      # a condition over conditional reads: free
        g = E.b(guard_expr(st))
        E.on = True
        rS = enc_x(E, f, {}, RCb, fsb)
        E.on = False
        rl = E.OR([E.atom(('port', D.canon(('call', p[0], p[1], [])))) for p in s['rdy']])
        ok_r = E.sat([g, rS, -rl], workdir) is False
        others = [y for y in Y if y != (s['inst'], s['meth'])]
        ok_i = None
        if others:
            rP = enc_x(E, f, {}, RCb, fsb)
            ok_i = E.sat([g, E.OR([E.AND([rS, -rP]), E.AND([-rS, rP])])], workdir) is False
        return dict(f=f, Y=Y, others=others, rdy=ok_r, inv=ok_i, ok=ok_r and ok_i is not False)

    rv = {'sites': len(rsites), 'ok': 0, 'rdy': 0, 'inv_needed': 0, 'inv': 0, 'self_only': 0,
          'none': 0, 'left': []}
    for st, s in rsites:
        r_ = rdy_site_check(st, s)
        s['rdy_sv'] = r_
        rv['ok'] += r_['ok']
        rv['rdy'] += r_['rdy']
        rv['inv_needed'] += bool(r_['others'])
        rv['inv'] += bool(r_['inv'])
        rv['self_only'] += bool(r_['Y']) and not r_['others']
        rv['none'] += not r_['Y']
        if not r_['ok']:
            rv['left'].append((st['name'], s['label'], 'rdy' if not r_['rdy'] else 'inv'))
    res['rdy_sv'] = rv

    # the dynamic facts each conditional read needs, under three conditions
    def wf(st):
        n = 'WILL_FIRE_' + st['name']
        return ('id', n) if n in D.defs else ('id', 'CAN_FIRE_' + st['name'])

    def call_cond(st, s):
        cs = [a[3] if a[3] is not None else ('lit', "1'd1")
              for a in ap.rules.get(st['name'], (None, []))[1]
              if a and a[0] == 'call' and a[1] == s['inst'] and a[2] == s['meth']]
        return cs or [('lit', "1'd1")]

    # bsc states "a / b: p (any of the k uses before) then q": the later step b and its use q
    # against every earlier use of p, so a fact is keyed by (b, p, q)
    confirmed = set()
    for fct in dump['facts']:
        if fct['ok']:
            confirmed.add((fct['b'], fct['p'], fct['q']))

    def is_confirmed(st, s, st2, s2):
        e1, e2 = (st2, s2), (st, s)
        if e1[0]['idx'] > e2[0]['idx']:
            e1, e2 = e2, e1
        return (e2[0]['name'], e1[1]['label'], e2[1]['label']) in confirmed

    def model_atoms(E, roots):
        # the atoms a satisfying assignment sets (for judging a witness by hand)
        import subprocess
        import tempfile
        E.finish()
        cls = E.cl + [[r] for r in roots]
        fd, path = tempfile.mkstemp(suffix='.cnf', dir=workdir)
        with os.fdopen(fd, 'w') as f:
            f.write('p cnf %d %d\n' % (E.n, len(cls)))
            for c in cls:
                f.write(' '.join(map(str, c)) + ' 0\n')
        r = subprocess.run([os.path.expanduser('~/.elan/toolchains/leanprover--lean4---v4.35.0/bin/cadical'),
                            path], stdout=subprocess.PIPE, stderr=subprocess.DEVNULL, timeout=120)
        os.unlink(path)
        val = set()
        for ln in r.stdout.decode().split('\n'):
            if ln.startswith('v '):
                val.update(int(x) for x in ln[2:].split() if int(x) > 0)
        return sorted(str(k) for k, v in E.atoms.items() if v in val and k[0] != 'cyc')

    # trace (SITES_RDY_TRACE=1): each readiness site's short-circuit condition, bsc's use condition,
    # and a witness wherever the readiness rule fails
    if os.environ.get('SITES_RDY_TRACE') and rsites:
        def ftxt(f, RCx, fsx, depth=0):
            nd_ = fsx.nodes[f]
            if nd_[0] in ('T', 'F'):
                return nd_[0]
            if nd_[0] == 'lit':
                L = RCx.lit[nd_[1]]
                t = canon_expr(L[1]) if L[0] == 'c' else '%s==%s' % (canon_expr(L[1]), canon_expr(L[2]))
                return ('' if nd_[2] else '!') + t[:160]
            if depth > 3:
                return '...'
            op = ' & ' if nd_[0] == 'and' else ' | '
            return '(' + op.join(ftxt(x, RCx, fsx, depth + 1) for x in nd_[1][:6]) + (op + '...' if len(nd_[1]) > 6 else '') + ')'
        for st, s in rsites:
            log.append('RDYB %s / %s: readiness rule %s; under bsc\'s use condition %s' % (
                st['name'], s['label'], s.get('rdy_bias'), 'ok' if s.get('rdy_bsc') else 'fails'))
            log.append('    short-circuit condition: ' + ftxt(s['reach_b'], RCb, fsb)[:3000])
            log.append('    bsc use condition: ' + str(s.get('bsc'))[:1500])
            if not str(s.get('rdy_bias')).startswith('ok'):
                E = Enc(D)
                c = enc_x(E, s['reach_b'], {}, RCb, fsb)
                rl = E.OR([E.atom(('port', D.canon(('call', p[0], p[1], [])))) for p in s['rdy']])
                for a in model_atoms(E, [E.b(guard_expr(st)), c, -rl]):
                    log.append('    witness: ' + a[:300])
    facts = {'bsc': [0, 0], 'used': [0, 0], 'true': [0, 0]}
    fact_fail = []
    fact_class = {'abstract_unsat': 0, 'same_as_confirmed': 0, 'at_risk': 0}
    models = []
    for st, s in need_sites:
        if 'a' not in s['need'] and 'c' not in s['need']:
            continue
        for (st2, s2) in s['partners']:
            for tag in ('bsc', 'used', 'true'):
                E = Enc(D)
                if tag == 'bsc':
                    be = bsc_expr(s)
                    c = E.b(be) if be is not None else 1
                elif tag == 'used':
                    c = enc_f(E, s.get('cond_used', s['reach']), {})
                else:
                    c = 1
                if s2['kind'] == 'call':
                    c2 = E.OR([E.b(x) for x in call_cond(st2, s2)])
                elif s2.get('need') and tag == 'used' and 'cond_used' in s2:
                    c2 = enc_f(E, s2['cond_used'], {})
                else:
                    c2 = 1
                roots = [E.b(wf(st)), E.b(wf(st2)), c, c2]
                sat = E.sat(roots, workdir)
                facts[tag][0 if sat is False else 1] += 1
                if tag == 'used':
                    if sat is False:
                        fact_class['abstract_unsat'] += 1
                    elif s.get('cmp_d') == 'eq' and is_confirmed(st, s, st2, s2):
                        fact_class['same_as_confirmed'] += 1
                    else:
                        fact_class['at_risk'] += 1
                        fact_fail.append((st['name'], s['label'], st2['name'], s2['label'], s.get('cmp_d')))
                        if len(models) < 3:
                            E2 = Enc(D)
                            c_ = enc_f(E2, s.get('cond_used', s['reach']), {})
                            c2_ = E2.OR([E2.b(x) for x in call_cond(st2, s2)]) if s2['kind'] == 'call' else 1
                            models.append(((st['name'], s['label'], st2['name'], s2['label']),
                                           model_atoms(E2, [E2.b(wf(st)), E2.b(wf(st2)), c_, c2_])))
    res['facts'] = facts
    res['fact_fail'] = len(fact_fail)
    res['fact_class'] = fact_class

    # bsc's use-level facts, each against the new site model
    atrisk = set((x[0], x[1], x[2], x[3]) for x in fact_fail)
    fclass = {'action': 0, 'not_needed': 0, 'same': 0, 'reask_holds': 0, 'reask_at_risk': 0,
              'unconfirmed': 0, 'unmatched': 0}
    fdetail = []
    for fct in dump['facts']:
        if not fct['ok']:
            fclass['unconfirmed'] += 1
            continue
        sa, sb = stepby.get(fct['a']), stepby.get(fct['b'])
        sp = [x for x in sa['sites'] if x['label'] == fct['p']] if sa else []
        sq = [x for x in sb['sites'] if x['label'] == fct['q']] if sb else []
        if not sp or not sq:
            fclass['unmatched'] += 1
            continue
        p_, q_ = sp[0], sq[0]
        if q_['kind'] == 'read':
            st_r, s_r, st_o, s_o = sb, q_, sa, p_
        elif p_['kind'] == 'read':
            st_r, s_r, st_o, s_o = sa, p_, sb, q_
        else:
            fclass['action'] += 1
            continue
        if not s_r['need'] or not any(o is st_o and x is s_o for o, x in s_r['partners']):
            fclass['not_needed'] += 1
            continue
        if s_r.get('cmp_d') == 'eq':
            fclass['same'] += 1
        elif (st_r['name'], s_r['label'], st_o['name'], s_o['label']) in atrisk:
            fclass['reask_at_risk'] += 1
            fdetail.append('re-ask, at risk: %s / %s: %s then %s' % (fct['a'], fct['b'], fct['p'], fct['q']))
        else:
            fclass['reask_holds'] += 1
            fdetail.append('re-ask, holds (abstract UNSAT): %s / %s: %s then %s' % (fct['a'], fct['b'], fct['p'], fct['q']))
    res['fclass'] = fclass

    # shared copies: the argument-equality fact (only where clashes exist)
    res['shared_detail'] = [(k, m, c, len(srcs), len(users), len(clash)) for (k, m, c, srcs, users, clash) in shared]

    # write the detail file
    with open(os.path.join(outdir, name + '.txt'), 'w') as f:
        f.write('design %s\n' % name)
        for k, v in res.items():
            f.write('  %s: %s\n' % (k, v))
        for x in log:
            f.write(x + '\n')
        for st, s in need_sites:
            f.write('\nsite %s / %s  need=%s exact_hw=%s\n' % (st['name'], s['label'], ''.join(sorted(s['need'])),
                                                             s.get('exact_hw', False)))
            f.write('  bsc use condition: %s\n' % s['bsc'])
            f.write('  reach literals: %d, dropped: %d, reach true: %s, after drop true: %s\n' % (
                len(s.get('lits', ())), len(s.get('drop', ())), s['reach'] == fs.T, s.get('reach_d') == fs.T))
            for key, pol in sorted(s.get('lits', ()), key=repr)[:12]:
                f.write('    %s%s %s\n' % ('' if pol else '!', key[0], ' '.join(key[1:])[:150] +
                                           ('   [dropped]' if key in s.get('drop', ()) else '')))
            f.write('  bsc vs reach: %s; after dropping: %s\n' % (s.get('cmp'), s.get('cmp_d')))
            for st2, s2 in s['partners'][:6]:
                f.write('  partner %s / %s (%s)\n' % (st2['name'], s2['label'], s2['kind']))
        for x in fact_fail:
            f.write('FACT AT RISK (abstract SAT, not the confirmed fact): %s / %s against %s / %s (bsc vs used cond: %s)\n' % x)
        for key, atoms in models:
            f.write('MODEL for %s / %s against %s / %s: true atoms:\n' % key)
            for a in atoms[:80]:
                f.write('    %s\n' % a[:200])
        for x in shared:
            f.write('shared copy %s.%s[%d]: %d argument expressions, users %s, clashing pairs %s\n' % (
                x[0], x[1], x[2], len(x[3]), x[4], x[5]))
        for x in ready_bad:
            f.write('READY not implied by guard: %s / %s\n' % x)
        for x in fdetail:
            f.write('BSC FACT %s\n' % x)
        for x in sorted(rerank_reads):
            f.write('RERANK still conditional: %s / %s\n' % x)
        for st, s in need_sites:
            if 'r' in s['need']:
                f.write('READY %s / %s: under the used condition %s, under bsc\'s %s; readiness rule: %s\n' % (
                    st['name'], s['label'], 'ok' if s.get('rdy_used') else 'FAILS', 'ok' if s.get('rdy_bsc') else 'fails',
                    s.get('rdy_bias')))
                sv = s.get('rdy_sv', {})
                f.write('  site-value form: R reads conditional reads %s; readiness %s; invariance %s\n' % (
                    sv.get('Y'), 'holds' if sv.get('rdy') else 'FAILS',
                    'not needed' if sv.get('inv') is None else ('holds' if sv.get('inv') else 'FAILS')))
    # for the end-to-end emitter: the conditional sites and the formulas behind them
    res['_ctx'] = dict(D=D, need=need_sites, RC=RC, fs=fs, RCb=RCb, fsb=fsb, steps=steps, ap=ap,
                       rdy_site_check=rdy_site_check, linfo=linfo, enc_f=enc_f)
    return res


def fmt(r):
    nb = r['need_by']
    return ('%-24s sites %6d reads %6d | cond %4d (a %d b %d c %d r %d) | exactHW %d reachTrue %d | '
            'cycSteps %d cycSites %d hardCyc %d | dropped %d (hw %d) toTrue %d | '
            'bsc vs reach eq %d strong %d weak %d inc %d (after drop eq %d strong %d weak %d inc %d) | '
            'facts used ok %d notShown %d; bsc ok %d notShown %d; true ok %d notShown %d | '
            'shared %d clash %d | rdy %d/%d (ok under used %d, bsc %d) | actPairs %d grp %d | '
            'facts: unsat %d sameAsConfirmed %d atRisk %d | twoWay %d rerankFailPairs %d rerankReads %d moved %d | '
            'bscFacts %s | all %s | rdyRule %s | rdySiteValue %s | perr %d') % (
        r['design'], r['sites'], r['reads'], r['need'], nb['a'], nb['b'], nb['c'], nb['r'],
        r['exact_hw'], r['reach_true'], r['cyc_steps'], r['cyc_sites'], r['hard_cyc'],
        r['dropped'], r['dropped_hw'], r['to_true'],
        r['cmp']['eq'], r['cmp']['bsc_stronger'], r['cmp']['bsc_weaker'], r['cmp']['incomparable'],
        r['cmp_d']['eq'], r['cmp_d']['bsc_stronger'], r['cmp_d']['bsc_weaker'], r['cmp_d']['incomparable'],
        r['facts']['used'][0], r['facts']['used'][1], r['facts']['bsc'][0], r['facts']['bsc'][1],
        r['facts']['true'][0], r['facts']['true'][1],
        r['shared_copies'], r['shared_clash'], r['ready_bad'], r['ready_cand'], r['rdy_ok']['used'], r['rdy_ok']['bsc'],
        r['act_pairs'], r['grp_fail'],
        r['fact_class']['abstract_unsat'], r['fact_class']['same_as_confirmed'], r['fact_class']['at_risk'],
        r['two_way'], r['rerank_fail_pairs'], r['rerank_reads'], r['rerank_moved'],
        r['fclass'], r.get('all'), r.get('rdy_bias'), r.get('rdy_sv'), r['parse_errors'])


def main():
    outdir = sys.argv[1]
    os.makedirs(outdir, exist_ok=True)
    work = os.path.join(outdir, 'tmp')
    os.makedirs(work, exist_ok=True)
    for d in sys.argv[2:]:
        if not os.path.exists(os.path.join(d, 'dump.txt')):
            continue
        name = os.path.basename(d.rstrip('/'))
        if not os.path.exists(os.path.join(DATA, name + '.ba.txt')):
            print('%-24s (no dumpba text)' % name)
            continue
        try:
            r = analyse(d, outdir, work)
            print(fmt(r), flush=True)
        except Exception as ex:
            import traceback
            traceback.print_exc()
            print('%-24s ERROR %s' % (name, ex), flush=True)


if __name__ == '__main__':
    sys.setrecursionlimit(1000000)
    threading.stack_size(512 * 1024 * 1024)
    t = threading.Thread(target=main)
    t.start()
    t.join()

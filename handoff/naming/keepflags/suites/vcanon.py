# Canonical "sink" form of a bsc-generated Verilog module.
#
# Every internal net defined by an assign or by a combinational always block is
# inlined (hash-consed), so a netlist that only adds named wires for existing
# expressions canonicalises to the same sinks as one without them.  Sinks are:
# output ports, the next-state function of every register (per clock event),
# every instance connection, and every system task call (with its path
# condition).  && and || chains are flattened and sorted; nothing else is
# normalised, so logically equal but differently built expressions still count
# as structural differences.
import re, sys

class ParseError(Exception):
    pass

TOK = re.compile(r'''
  (?P<ws>\s+)
| (?P<lc>//[^\n]*)
| (?P<bc>/\*.*?\*/)
| (?P<str>"(?:\\.|[^"\\])*")
| (?P<num>(?:\d[\d_]*)?\s*'[sS]?[bBoOdDhH]\s*[0-9a-fA-FxXzZ_?]+|\d[\d_]*(?:\.\d+)?(?:[eE][-+]?\d+)?)
| (?P<id>[A-Za-z_][A-Za-z0-9_$]*|\\\S+)
| (?P<sys>\$[A-Za-z_][A-Za-z0-9_$]*)
| (?P<mac>`[A-Za-z_][A-Za-z0-9_]*)
| (?P<op><<<|>>>|===|!==|==|!=|<=|>=|&&|\|\||<<|>>|~&|~\||~\^|\^~|\*\*|\+:|-:|[-+*/%<>!~&|^?:;,.()\[\]{}#=@])
''', re.S | re.X)

def preprocess(text):
    defines = {}
    out = []
    stack = []  # list of (active, taken)
    active = True
    for line in text.split('\n'):
        s = line.strip()
        m = re.match(r'`(ifdef|ifndef|else|elsif|endif|define|undef|timescale|include|resetall|default_nettype)\b\s*(.*)$', s)
        if m:
            d, arg = m.group(1), m.group(2).strip()
            if d in ('ifdef', 'ifndef'):
                name = arg.split()[0] if arg else ''
                cond = (name in defines) if d == 'ifdef' else (name not in defines)
                stack.append((active, cond))
                active = active and cond
            elif d == 'elsif':
                pa, taken = stack[-1]
                name = arg.split()[0]
                cond = (not taken) and (name in defines)
                stack[-1] = (pa, taken or cond)
                active = pa and cond
            elif d == 'else':
                pa, taken = stack[-1]
                active = pa and not taken
                stack[-1] = (pa, True)
            elif d == 'endif':
                active, _ = stack.pop()
            elif d == 'define' and active:
                parts = arg.split(None, 1)
                defines[parts[0]] = parts[1] if len(parts) > 1 else ''
            elif d == 'undef' and active:
                defines.pop(arg.split()[0], None)
            out.append('')
            continue
        out.append(line if active else '')
    return '\n'.join(out), defines

def tokenize(text, defines):
    toks = []
    pos = 0
    n = len(text)
    while pos < n:
        m = TOK.match(text, pos)
        if not m:
            raise ParseError('bad char %r at %d' % (text[pos], pos))
        pos = m.end()
        k = m.lastgroup
        if k in ('ws', 'lc', 'bc'):
            continue
        v = m.group(k)
        if k == 'mac':
            name = v[1:]
            if name in defines:
                rep = defines[name]
                if rep.strip():
                    toks.extend(tokenize(rep, defines))
                continue
            toks.append(('mac', v))
            continue
        if k == 'num':
            v = re.sub(r'\s+', '', v)
        toks.append((k, v))
    return toks

def norm_num(v):
    v = v.replace('_', '')
    m = re.match(r"^(\d*)'([sS]?)([bBoOdDhH])([0-9a-fA-FxXzZ?]+)$", v)
    if m:
        w = m.group(1) or '32'
        base = {'b': 2, 'o': 8, 'd': 10, 'h': 16}[m.group(3).lower()]
        digits = m.group(4).lower()
        if re.search('[xz?]', digits):
            return ('num', int(w), m.group(2).lower(), m.group(3).lower() + digits)
        return ('num', int(w), m.group(2).lower(), int(digits, base))
    if re.match(r'^\d+$', v):
        return ('num', 0, '', int(v))
    return ('real', v)

BINPREC = [
    ['||'], ['&&'], ['|'], ['^', '~^', '^~'], ['&'], ['==', '!=', '===', '!=='],
    ['<', '<=', '>', '>='], ['<<', '>>', '<<<', '>>>'], ['+', '-'], ['*', '/', '%'], ['**'],
]
PREC = {}
for i, ops in enumerate(BINPREC):
    for o in ops:
        PREC[o] = i + 1
UNARY = {'+', '-', '!', '~', '&', '~&', '|', '~|', '^', '~^', '^~'}

class Parser:
    def __init__(self, toks):
        self.t = toks
        self.i = 0

    def peek(self, k=0):
        j = self.i + k
        return self.t[j] if j < len(self.t) else ('eof', '')

    def next(self):
        tok = self.peek()
        self.i += 1
        return tok

    def accept(self, v):
        if self.peek()[1] == v and self.peek()[0] in ('op', 'id'):
            self.i += 1
            return True
        return False

    def expect(self, v):
        tok = self.next()
        if tok[1] != v:
            raise ParseError('expected %r got %r at token %d' % (v, tok, self.i))
        return tok

    # ---- expressions (raw AST as tuples) ----
    def expr(self):
        c = self.binexpr(1)
        if self.peek() == ('op', '?'):
            self.next()
            a = self.expr()
            self.expect(':')
            b = self.expr()
            return ('?:', c, a, b)
        return c

    def binexpr(self, minp):
        lhs = self.unary()
        while True:
            tok = self.peek()
            if tok[0] != 'op' or tok[1] not in PREC or PREC[tok[1]] < minp:
                return lhs
            op = tok[1]
            self.next()
            p = PREC[op]
            rhs = self.binexpr(p + 1)
            lhs = ('bin', op, lhs, rhs)

    def unary(self):
        tok = self.peek()
        if tok[0] == 'op' and tok[1] in UNARY:
            self.next()
            return ('un', tok[1], self.unary())
        return self.postfix(self.primary())

    def postfix(self, e):
        while self.peek() == ('op', '['):
            self.next()
            a = self.expr()
            if self.accept(':'):
                b = self.expr()
                self.expect(']')
                e = ('slice', e, a, b)
            elif self.peek()[1] in ('+:', '-:'):
                op = self.next()[1]
                b = self.expr()
                self.expect(']')
                e = ('islice', op, e, a, b)
            else:
                self.expect(']')
                e = ('index', e, a)
        return e

    def primary(self):
        tok = self.next()
        k, v = tok
        if k == 'num':
            return norm_num(v)
        if k == 'str':
            return ('str', v)
        if k == 'id':
            return ('id', v)
        if k == 'mac':
            return ('mac', v)
        if k == 'sys':
            args = []
            if self.peek() == ('op', '('):
                self.next()
                if not self.accept(')'):
                    args.append(self.expr())
                    while self.accept(','):
                        args.append(self.expr())
                    self.expect(')')
            return ('call', v, tuple(args))
        if v == '(':
            e = self.expr()
            self.expect(')')
            return e
        if v == '{':
            first = self.expr()
            if self.peek() == ('op', '{'):
                self.next()
                items = [self.expr()]
                while self.accept(','):
                    items.append(self.expr())
                self.expect('}')
                self.expect('}')
                return ('repl', first, ('cat', tuple(items)))
            items = [first]
            while self.accept(','):
                items.append(self.expr())
            self.expect('}')
            return ('cat', tuple(items))
        raise ParseError('unexpected %r at token %d' % (tok, self.i))

    # ---- statements ----
    def stmt(self):
        tok = self.peek()
        v = tok[1]
        if tok[0] == 'id' and v == 'begin':
            self.next()
            if self.accept(':'):
                self.next()
            body = []
            while not (self.peek()[0] == 'id' and self.peek()[1] == 'end'):
                if self.peek()[0] == 'eof':
                    raise ParseError('eof in begin')
                body.append(self.stmt())
            self.next()
            return ('block', body)
        if tok[0] == 'id' and v == 'if':
            self.next()
            self.expect('(')
            c = self.expr()
            self.expect(')')
            a = self.stmt()
            b = None
            if self.peek()[0] == 'id' and self.peek()[1] == 'else':
                self.next()
                b = self.stmt()
            return ('if', c, a, b)
        if tok[0] == 'id' and v in ('case', 'casez', 'casex'):
            self.next()
            self.expect('(')
            sel = self.expr()
            self.expect(')')
            items = []
            while not (self.peek()[0] == 'id' and self.peek()[1] == 'endcase'):
                if self.peek()[0] == 'id' and self.peek()[1] == 'default':
                    self.next()
                    self.accept(':')
                    items.append((None, self.stmt()))
                    continue
                labs = [self.expr()]
                while self.accept(','):
                    labs.append(self.expr())
                self.expect(':')
                items.append((tuple(labs), self.stmt()))
            self.next()
            return ('case', v, sel, items)
        if tok[0] == 'op' and v == ';':
            self.next()
            return ('nop',)
        if tok[0] == 'op' and v == '#':
            self.next()
            self.next()
            if self.peek() == ('op', ';'):
                self.next()
                return ('nop',)
            return self.stmt()
        if tok[0] == 'sys':
            self.next()
            args = []
            if self.accept('('):
                if not self.accept(')'):
                    args.append(self.expr())
                    while self.accept(','):
                        args.append(self.expr())
                    self.expect(')')
            self.expect(';')
            return ('task', v, tuple(args))
        # assignment
        lhs = self.postfix(self.primary())
        op = self.next()[1]
        if op not in ('=', '<='):
            raise ParseError('bad assignment op %r at token %d' % (op, self.i))
        if self.peek() == ('op', '#'):
            self.next()
            self.next()
        rhs = self.expr()
        self.expect(';')
        return ('asg', op, lhs, rhs)

def parse_module(toks):
    p = Parser(toks)
    mods = []
    while p.peek()[0] != 'eof':
        tok = p.next()
        if tok == ('id', 'module'):
            mods.append(parse_module_body(p))
    return mods

def skip_to_semi(p):
    while p.next()[1] != ';':
        pass

def parse_module_body(p):
    name = p.next()[1]
    M = {'name': name, 'ports': [], 'decls': {}, 'assigns': [], 'always': [], 'insts': [], 'other': []}
    if p.accept('#'):
        depth = 0
        while True:
            t = p.next()[1]
            if t == '(':
                depth += 1
            elif t == ')':
                depth -= 1
                if depth == 0:
                    break
    if p.accept('('):
        while not p.accept(')'):
            t = p.next()
            if t[0] == 'id':
                M['ports'].append(t[1])
    p.expect(';')
    while True:
        tok = p.peek()
        if tok[0] == 'eof':
            raise ParseError('eof in module')
        v = tok[1]
        if tok[0] == 'id' and v == 'endmodule':
            p.next()
            return M
        if tok[0] == 'id' and v in ('input', 'output', 'inout', 'wire', 'reg', 'integer', 'parameter', 'localparam', 'tri', 'wand', 'wor'):
            p.next()
            rng = None
            while p.peek()[1] in ('signed', 'reg', 'wire'):
                p.next()
            if p.peek() == ('op', '['):
                p.next()
                a = p.expr()
                p.expect(':')
                b = p.expr()
                p.expect(']')
                rng = (a, b)
            while True:
                nm = p.next()[1]
                init = None
                if p.peek() == ('op', '['):  # memory
                    p.next(); p.expr(); p.expect(':'); p.expr(); p.expect(']')
                    v = 'memory'
                if p.accept('='):
                    init = p.expr()
                M['decls'].setdefault(nm, []).append((v, rng, init))
                if p.accept(','):
                    continue
                p.expect(';')
                break
            continue
        if tok[0] == 'id' and v == 'assign':
            p.next()
            lhs = p.postfix(p.primary())
            p.expect('=')
            rhs = p.expr()
            p.expect(';')
            M['assigns'].append((lhs, rhs))
            continue
        if tok[0] == 'id' and v in ('always', 'initial'):
            p.next()
            ev = None
            if v == 'always':
                p.expect('@')
                if p.accept('*'):
                    ev = ('*',)
                else:
                    p.expect('(')
                    evs = []
                    depth = 1
                    cur = []
                    while True:
                        t = p.next()
                        if t[1] == '(':
                            depth += 1
                        elif t[1] == ')':
                            depth -= 1
                            if depth == 0:
                                break
                        cur.append(t[1])
                    ev = tuple(cur)
            body = p.stmt()
            M['always'].append((v, ev, body))
            continue
        if tok[0] == 'id' and v in ('function', 'task', 'generate', 'genvar'):
            raise ParseError('unsupported item ' + v)
        if tok[0] == 'id':
            # instance
            mod = p.next()[1]
            params = []
            if p.accept('#'):
                p.expect('(')
                if not p.accept(')'):
                    while True:
                        if p.accept('.'):
                            pn = p.next()[1]
                            p.expect('(')
                            pv = p.expr()
                            p.expect(')')
                            params.append((pn, pv))
                        else:
                            params.append((None, p.expr()))
                        if p.accept(','):
                            continue
                        p.expect(')')
                        break
            iname = p.next()[1]
            conns = []
            p.expect('(')
            if not p.accept(')'):
                while True:
                    if p.accept('.'):
                        pn = p.next()[1]
                        p.expect('(')
                        if p.accept(')'):
                            pv = None
                        else:
                            pv = p.expr()
                            p.expect(')')
                        conns.append((pn, pv))
                    else:
                        conns.append((None, p.expr()))
                    if p.accept(','):
                        continue
                    p.expect(')')
                    break
            p.expect(';')
            M['insts'].append((mod, iname, params, conns))
            continue
        raise ParseError('unexpected module item %r' % (tok,))

ASSOC = {'&&', '||'}

class Canon:
    """hash-consing of canonical expressions; ids are small ints"""
    def __init__(self, M):
        self.M = M
        self.tab = {}
        self.rev = []
        self.defs = {}       # net -> raw expr (from assign)
        self.combdefs = {}   # net -> canonical id (from comb always)
        self.memo = {}
        self.active = set()
        for lhs, rhs in M['assigns']:
            if lhs[0] == 'id':
                self.defs[lhs[1]] = rhs
            else:
                raise ParseError('partial assign')
        self.kinds = {}
        for nm, ds in M['decls'].items():
            self.kinds[nm] = set(d[0] for d in ds)

    def mk(self, node):
        h = self.tab.get(node)
        if h is None:
            h = len(self.rev)
            self.tab[node] = h
            self.rev.append(node)
        return h

    def net(self, name):
        if name in self.memo:
            return self.memo[name]
        if name in self.active:
            raise ParseError('combinational cycle at ' + name)
        if name in self.defs:
            self.active.add(name)
            h = self.ex(self.defs[name], {})
            self.active.discard(name)
        elif name in self.combdefs:
            h = self.combdefs[name]
        else:
            h = self.mk(('leaf', name))
        self.memo[name] = h
        return h

    def flat(self, op, e, env, acc):
        if e[0] == 'bin' and e[1] == op:
            self.flat(op, e[2], env, acc)
            self.flat(op, e[3], env, acc)
        else:
            h = self.ex(e, env)
            n = self.rev[h]
            if n[0] == 'ac' and n[1] == op:
                acc.extend(n[2])
            else:
                acc.append(h)

    def ex(self, e, env):
        k = e[0]
        if k == 'id':
            if e[1] in env:
                return env[e[1]]
            return self.net(e[1])
        if k in ('num', 'real', 'str', 'mac'):
            return self.mk(e)
        if k == 'bin':
            if e[1] in ASSOC:
                acc = []
                self.flat(e[1], e, env, acc)
                return self.mk(('ac', e[1], tuple(sorted(acc))))
            return self.mk(('bin', e[1], self.ex(e[2], env), self.ex(e[3], env)))
        if k == 'un':
            return self.mk(('un', e[1], self.ex(e[2], env)))
        if k == '?:':
            return self.mk(('?:', self.ex(e[1], env), self.ex(e[2], env), self.ex(e[3], env)))
        if k == 'slice':
            return self.mk(('slice', self.ex(e[1], env), self.ex(e[2], env), self.ex(e[3], env)))
        if k == 'islice':
            return self.mk(('islice', e[1], self.ex(e[2], env), self.ex(e[3], env), self.ex(e[4], env)))
        if k == 'index':
            return self.mk(('index', self.ex(e[1], env), self.ex(e[2], env)))
        if k == 'cat':
            return self.mk(('cat', tuple(self.ex(x, env) for x in e[1])))
        if k == 'repl':
            return self.mk(('repl', self.ex(e[1], env), self.ex(e[2], env)))
        if k == 'call':
            return self.mk(('call', e[1], tuple(self.ex(x, env) for x in e[1 + 1])))
        raise ParseError('expr kind ' + k)

    def andc(self, a, b):
        if a is None:
            return b
        acc = []
        for h in (a, b):
            n = self.rev[h]
            if n[0] == 'ac' and n[1] == '&&':
                acc.extend(n[2])
            else:
                acc.append(h)
        return self.mk(('ac', '&&', tuple(sorted(acc))))

    def notc(self, a):
        return self.mk(('un', '!', a))

    # symbolic execution of a statement; env maps var -> id (current value),
    # tasks collects (cond, taskname, args)
    def run(self, s, env, cond, tasks, nb):
        k = s[0]
        if k == 'block':
            for x in s[1]:
                env = self.run(x, env, cond, tasks, nb)
            return env
        if k == 'nop':
            return env
        if k == 'asg':
            lhs = s[2]
            if lhs[0] != 'id':
                raise ParseError('partial lhs in always')
            v = self.ex(s[3], env if s[1] == '=' else {})
            env = dict(env)
            env[lhs[1]] = v
            return env
        if k == 'task':
            args = tuple(self.ex(a, env) for a in s[2])
            tasks.append((cond, s[1], args))
            return env
        if k == 'if':
            c = self.ex(s[1], env)
            e1 = self.run(s[2], env, self.andc(cond, c), tasks, nb)
            e2 = self.run(s[3], env, self.andc(cond, self.notc(c)), tasks, nb) if s[3] else env
            return self.merge(c, e1, e2, env)
        if k == 'case':
            sel = self.ex(s[2], env)
            # convert to an if-chain, in order
            def chain(items, cond0):
                if not items:
                    return env
                labs, st = items[0]
                if labs is None:
                    return self.run(st, env, cond0, tasks, nb)
                c = None
                for l in labs:
                    t = self.mk(('caseq', s[1], sel, self.ex(l, env)))
                    c = t if c is None else self.mk(('ac', '||', tuple(sorted(
                        (list(self.rev[c][2]) if self.rev[c][0] == 'ac' else [c]) + [t]))))
                e1 = self.run(st, env, self.andc(cond0, c), tasks, nb)
                e2 = chain(items[1:], self.andc(cond0, self.notc(c)))
                return self.merge(c, e1, e2, env)
            return chain(list(s[3]), cond)
        raise ParseError('stmt kind ' + k)

    def merge(self, c, e1, e2, env):
        out = dict(env)
        for v in set(e1) | set(e2):
            a = e1.get(v, env.get(v))
            b = e2.get(v, env.get(v))
            if a is None:
                a = self.mk(('hold', v))
            if b is None:
                b = self.mk(('hold', v))
            out[v] = a if a == b else self.mk(('?:', c, a, b))
        return out

    def show(self, h, depth=0, maxd=6):
        n = self.rev[h]
        if depth > maxd:
            return '...'
        k = n[0]
        if k == 'leaf':
            return n[1]
        if k == 'num':
            return "%s'%s%s" % (n[1], n[2], n[3])
        if k in ('str', 'mac', 'real'):
            return str(n[1])
        if k == 'hold':
            return 'HOLD(%s)' % n[1]
        if k == 'ac':
            return '(' + (' %s ' % n[1]).join(self.show(x, depth + 1, maxd) for x in n[2]) + ')'
        if k == 'bin':
            return '(%s %s %s)' % (self.show(n[2], depth + 1, maxd), n[1], self.show(n[3], depth + 1, maxd))
        if k == 'un':
            return '%s%s' % (n[1], self.show(n[2], depth + 1, maxd))
        if k == '?:':
            return '(%s ? %s : %s)' % tuple(self.show(x, depth + 1, maxd) for x in n[1:])
        if k == 'slice':
            return '%s[%s:%s]' % tuple(self.show(x, depth + 1, maxd) for x in n[1:])
        if k == 'index':
            return '%s[%s]' % tuple(self.show(x, depth + 1, maxd) for x in n[1:])
        if k == 'cat':
            return '{' + ', '.join(self.show(x, depth + 1, maxd) for x in n[1]) + '}'
        if k == 'repl':
            return '{%s{%s}}' % (self.show(n[1], depth + 1, maxd), self.show(n[2], depth + 1, maxd))
        if k == 'call':
            return '%s(%s)' % (n[1], ', '.join(self.show(x, depth + 1, maxd) for x in n[2]))
        if k == 'caseq':
            return '(%s %s== %s)' % (self.show(n[2], depth + 1, maxd), n[1], self.show(n[3], depth + 1, maxd))
        return str(n)

def canon_module(M):
    C = Canon(M)
    # combinational always blocks define nets
    comb = []
    seq = []
    for kind, ev, body in M['always']:
        if kind == 'initial':
            continue
        if ev == ('*',) or not any(x in ('posedge', 'negedge') for x in ev):
            comb.append(body)
        else:
            seq.append((ev, body))
    # comb blocks may depend on each other and on assigns; evaluate lazily
    pending = {}
    for body in comb:
        targets = set()
        def collect(s):
            if s[0] == 'block':
                for x in s[1]:
                    collect(x)
            elif s[0] == 'if':
                collect(s[2])
                if s[3]:
                    collect(s[3])
            elif s[0] == 'case':
                for _, st in s[3]:
                    collect(st)
            elif s[0] == 'asg':
                targets.add(s[2][1])
        collect(body)
        for t in targets:
            pending[t] = body
    done_bodies = {}
    orig_net = C.net
    def net(name):
        if name in pending and name not in C.combdefs:
            body = pending[name]
            key = id(body)
            if key not in done_bodies:
                if key in C.active:
                    raise ParseError('comb cycle')
                C.active.add(key)
                env = C.run(body, {}, None, [], False)
                C.active.discard(key)
                done_bodies[key] = env
                for t, h in env.items():
                    C.combdefs[t] = h
        return orig_net(name)
    C.net = net
    sinks = {}
    ports = {}
    for p in M['ports']:
        ds = M['decls'].get(p, [])
        dirs = [d for d in ds if d[0] in ('input', 'output', 'inout')]
        if dirs:
            d = dirs[0]
            ports[p] = (d[0], None if d[1] is None else (C.ex(d[1][0], {}), C.ex(d[1][1], {})))
    for p, (d, _) in ports.items():
        if d == 'output':
            sinks[('out', p)] = C.net(p)
    tasks = []
    for ev, body in seq:
        env = C.run(body, {}, None, tasks, True)
        for v, h in env.items():
            sinks[('next', ' '.join(ev), v)] = h
        # tasks keyed by event
    for mod, iname, params, conns in M['insts']:
        sinks[('inst', iname)] = (mod, tuple((pn, C.ex(pv, {}) if pv is not None else None) for pn, pv in params))
        for pn, pv in conns:
            sinks[('conn', iname, pn)] = C.ex(pv, {}) if pv is not None else None
    for i, (cond, tn, args) in enumerate(tasks):
        pass
    tset = sorted((c if c is not None else -1, tn, args) for c, tn, args in tasks)
    sinks[('tasks',)] = tuple(tset)
    regs = {}
    for nm, ds in M['decls'].items():
        for d in ds:
            if d[0] == 'reg' and nm not in ports:
                regs[nm] = None if d[1] is None else (C.ex(d[1][0], {}), C.ex(d[1][1], {}))
    return C, sinks, ports

def load(path):
    text = open(path, errors='replace').read()
    pp, defines = preprocess(text)
    toks = tokenize(pp, defines)
    return parse_module(toks)

def strhash(C, h, cache):
    """structural string key independent of hash-cons numbering"""
    if h is None:
        return 'None'
    if isinstance(h, tuple):
        return '(' + ','.join(strhash(C, x, cache) if isinstance(x, (int, tuple)) or x is None else repr(x) for x in h) + ')'
    if h in cache:
        return cache[h]
    n = C.rev[h]
    import hashlib
    parts = []
    for x in n:
        if isinstance(x, int) and not isinstance(x, bool) and x is not n[0]:
            parts.append(None)
        parts.append(x)
    def enc(x):
        if isinstance(x, tuple):
            return '[' + ','.join(enc(y) for y in x) + ']'
        return repr(x)
    # children ids appear as ints in specific positions; resolve by kind
    k = n[0]
    if k in ('leaf', 'num', 'real', 'str', 'mac', 'hold'):
        s = repr(n)
    else:
        def r(x):
            if isinstance(x, int) and not isinstance(x, bool):
                return strhash(C, x, cache)
            if isinstance(x, tuple):
                return '[' + ','.join(r(y) for y in x) + ']'
            return repr(x)
        if k in ('bin', 'un', 'ac', 'islice', 'call', 'caseq'):
            # first field after kind is an operator/name string
            s = '(' + k + ',' + repr(n[1]) + ',' + ','.join(r(x) for x in n[2:]) + ')'
        else:
            s = '(' + k + ',' + ','.join(r(x) for x in n[1:]) + ')'
    hv = hashlib.sha1(s.encode()).hexdigest()[:20]
    cache[h] = hv
    return hv

def module_keys(M):
    C, sinks, ports = canon_module(M)
    cache = {}
    keys = {}
    for k, h in sinks.items():
        if k[0] == 'inst':
            mod, params = h
            keys[k] = (mod, tuple((pn, strhash(C, pv, cache) if pv is not None else None) for pn, pv in params))
        elif k[0] == 'tasks':
            keys[k] = tuple(sorted((strhash(C, c, cache) if c != -1 else 'T', tn, tuple(strhash(C, a, cache) for a in args)) for c, tn, args in h))
        else:
            keys[k] = strhash(C, h, cache) if h is not None else None
    sig = tuple(sorted((p, d, None if r is None else (strhash(C, r[0], cache), strhash(C, r[1], cache))) for p, (d, r) in ports.items()))
    return keys, sig, C, sinks

if __name__ == '__main__':
    for M in load(sys.argv[1]):
        keys, sig, C, sinks = module_keys(M)
        print(M['name'], len(keys), 'sinks')
        for k, v in sorted(keys.items(), key=lambda x: str(x[0])):
            print('  ', k, v if not isinstance(v, tuple) or len(str(v)) < 200 else str(v)[:200])

"""Parse `dumpba` output (bsc's APackage, pretty printed) into expression trees.

Only what the site-condition measurement needs: local definitions, rules (their
predicate and actions), interface values, method arguments, the method uses map
and the resource allocation table.

Expression nodes are tuples:
  ('id', name)            identifier (a definition, an input, a function name)
  ('lit', text)           sized or plain literal, or a string
  ('call', inst, meth, args)   value-method read (inst.meth args)
  ('if', c, a, b)         _if_
  ('case', idx, [(label, val)], default)
  ('sel', arr, idx)       PrimArrayDynSelect
  ('app', head, args)     any other application (head is a node)
  ('bin', op, a, b)       infix operator
  ('un', op, a)           prefix operator
  ('tup', [xs])
"""
import re

TOK = re.compile(r'''
   (?P<ws>\s+)
 | (?P<str>"(?:[^"\\]|\\.)*")
 | (?P<lit>\d+'[sS]?[bdhoBDHO][0-9a-fA-FxXzZ_?]+|0x[0-9a-fA-F]+|\d+(?:\.\d+)?)
 | (?P<id>[A-Za-z_$][A-Za-z0-9_$']*(?:\.[A-Za-z_$][A-Za-z0-9_$']*)*)
 | (?P<op>\.<=|\.>=|\.<|\.>|&&|\|\||==|!=|<=|>=|<<|>>|\+\+|->|[-+*/%<>&|^!~()\[\]{},;?:=])
''', re.X)

BIN = {'||': 1, '&&': 2, '|': 3, '^': 4, '&': 5, '==': 6, '!=': 6,
       '<': 7, '<=': 7, '>': 7, '>=': 7, '.<': 7, '.<=': 7, '.>': 7, '.>=': 7, '<<': 8, '>>': 8, '++': 8,
       '+': 9, '-': 9, '*': 10, '/': 10, '%': 10}


def tokenize(s):
    out, i = [], 0
    while i < len(s):
        m = TOK.match(s, i)
        if not m:
            raise ValueError('cannot tokenize at %r' % s[i:i + 40])
        i = m.end()
        k = m.lastgroup
        if k == 'ws':
            continue
        out.append((k, m.group(k)))
    return out


class Parser:
    def __init__(self, toks, insts):
        self.t, self.i, self.insts = toks, 0, insts

    def peek(self, k=0):
        j = self.i + k
        return self.t[j] if j < len(self.t) else ('eof', '')

    def eat(self, v=None):
        tok = self.peek()
        if v is not None and tok[1] != v:
            raise ValueError('expected %r got %r at %d' % (v, tok, self.i))
        self.i += 1
        return tok

    def expr(self, minbp=0):
        lhs = self.unary()
        while True:
            k, v = self.peek()
            if k == 'op' and v in BIN and BIN[v] >= minbp + (0 if minbp == 0 else 0):
                if BIN[v] < minbp:
                    break
                self.eat()
                rhs = self.expr(BIN[v] + 1)
                lhs = ('bin', v, lhs, rhs)
            else:
                break
        return lhs

    def unary(self):
        k, v = self.peek()
        if k == 'op' and v in ('!', '~', '-'):
            self.eat()
            return ('un', v, self.unary())
        return self.app()

    def starts_primary(self):
        k, v = self.peek()
        if k in ('lit', 'str'):
            return self.peek(1)[1] != '->'
        if k == 'id':
            if v in ('of', 'then', 'else', '_'):
                return False
            return self.peek(1)[1] != '->'
        return k == 'op' and v == '('

    def app(self):
        head = self.primary()
        if head[0] in ('if', 'case', 'sel'):
            return head
        args = []
        while self.starts_primary():
            args.append(self.primary())
        if head[0] == 'call' and not head[3] and args:
            return ('call', head[1], head[2], args)
        if head[0] == 'id':
            return self.mk(head, args)
        return ('app', head, args) if args else head

    def mk(self, head, args):
        if head[0] == 'id':
            n = head[1]
            if '.' in n:
                inst, meth = n.split('.', 1)
                if inst in self.insts:
                    return ('call', inst, meth, args)
            if n == 'PrimArrayDynSelect' and len(args) == 2:
                return ('sel', args[0], args[1])
        if not args:
            return head
        return ('app', head, args)

    def primary(self):
        k, v = self.peek()
        if k == 'op' and v == '(':
            self.eat()
            xs = [self.expr()]
            while self.peek()[1] == ',':
                self.eat()
                xs.append(self.expr())
            self.eat(')')
            return xs[0] if len(xs) == 1 else ('tup', xs)
        if k in ('lit', 'str'):
            self.eat()
            return ('lit', v)
        if k == 'id':
            if v == '_if_':
                self.eat()
                c = self.primary()
                a = self.primary()
                b = self.primary()
                return ('if', c, a, b)
            if v == 'case':
                self.eat()
                idx = self.expr()
                self.eat('of')
                arms, dflt = [], None
                while True:
                    k2, v2 = self.peek()
                    if k2 == 'id' and v2 == '_':
                        self.eat()
                        self.eat('->')
                        dflt = self.expr()
                        break
                    if k2 == 'eof' or v2 in (';', ')'):
                        break
                    lab = self.primary()
                    self.eat('->')
                    val = self.expr()
                    arms.append((lab, val))
                return ('case', idx, arms, dflt)
            if v == 'PrimArrayDynSelect':
                self.eat()
                a = self.primary()
                b = self.primary()
                return ('sel', a, b)
            self.eat()
            return self.mk(('id', v), [])
        raise ValueError('unexpected %r at %d' % ((k, v), self.i))


def parse_expr(s, insts):
    p = Parser(tokenize(s), insts)
    e = p.expr()
    if p.peek()[0] != 'eof':
        raise ValueError('trailing tokens %r in %r' % (p.t[p.i:p.i + 5], s[:80]))
    return e


def split_top(s, sep=';'):
    """Split at top-level separators (outside brackets and strings)."""
    out, depth, cur, q, esc = [], 0, [], False, False
    for ch in s:
        if q:
            cur.append(ch)
            if esc:
                esc = False
            elif ch == '\\':
                esc = True
            elif ch == '"':
                q = False
            continue
        if ch == '"':
            q = True
        elif ch in '([{':
            depth += 1
        elif ch in ')]}':
            depth -= 1
        if ch == sep and depth == 0:
            out.append(''.join(cur))
            cur = []
        else:
            cur.append(ch)
    if ''.join(cur).strip():
        out.append(''.join(cur))
    return [x.strip() for x in out if x.strip()]


def match_brace(s, i):
    """Index just past the brace matching s[i] == '{' (strings skipped)."""
    depth, q, esc = 0, False, False
    for j in range(i, len(s)):
        ch = s[j]
        if q:
            if esc:
                esc = False
            elif ch == '\\':
                esc = True
            elif ch == '"':
                q = False
            continue
        if ch == '"':
            q = True
        elif ch == '{':
            depth += 1
        elif ch == '}':
            depth -= 1
            if depth == 0:
                return j + 1
    raise ValueError('unmatched brace')


def parse_action(s, insts):
    """('call', inst, meth, cond, args) or ('task', name, cond, args) or None."""
    cond = None
    m = re.match(r'^if\s', s)
    if m:
        # 'if C then ACTION': find the top-level 'then'
        toks = tokenize(s)
        depth = 0
        for k, (kind, v) in enumerate(toks):
            if v in ('(', '[', '{'):
                depth += 1
            elif v in (')', ']', '}'):
                depth -= 1
            elif kind == 'id' and v == 'then' and depth == 0:
                break
        p = Parser(toks[1:k], insts)
        cond = p.expr()
        p2 = Parser(toks[k + 1:], insts)
        act = p2.app()
    else:
        p2 = Parser(tokenize(s), insts)
        act = p2.app()
    if act[0] == 'call':
        return ('call', act[1], act[2], cond, act[3])
    if act[0] == 'app' and act[1][0] == 'id':
        n = act[1][1]
        if '.' in n:
            inst, meth = n.split('.', 1)
            if inst in insts:
                return ('call', inst, meth, cond, act[2])
        return ('task', n, cond, act[2])
    if act[0] == 'id':
        n = act[1]
        if '.' in n and n.split('.', 1)[0] in insts:
            inst, meth = n.split('.', 1)
            return ('call', inst, meth, cond, [])
        return ('task', n, cond, [])
    return ('other', repr(act)[:60], cond, [])


class APkg:
    def __init__(self, text):
        self.text = text
        lines = text.split('\n')
        self.insts = set()
        for ln in lines:
            m = re.match(r'^  ([A-Za-z_][A-Za-z0-9_$]*) :: ABSTRACT:', ln)
            if m:
                self.insts.add(m.group(1))
        # instance names can also be found as the targets of calls; the
        # ABSTRACT lines are complete for state elements
        self.defs, self.types = {}, {}
        self.rules, self.ifc_vals, self.meth_args = {}, {}, {}
        self.errors = []
        self._defs(lines)
        self._rules()
        self._ifc(lines)
        self.uses_map, self.ralloc = self._sched(text)
        self.guarded = self._guarded(text)

    def _guarded(self, t):
        """Instance -> value of its 'guarded' parameter, where it has one."""
        out = {}
        for m in re.finditer(r'\n  (\S+) :: ABSTRACT:  \S+ = \S+\n', t):
            j = t.find('meth types', m.end())
            if j < 0:
                continue
            blk = t[m.end():j]
            if 'param guarded;' not in blk:
                continue
            k = blk.rfind(')\n')
            lits = re.findall(r"1'[db][01]", blk[k:])
            if lits:
                out[m.group(1)] = lits[-1].endswith('1')
        return out

    def _defs(self, lines):
        try:
            a = lines.index('  -- AP local definitions')
        except ValueError:
            return
        b = next(i for i in range(a + 1, len(lines)) if lines[i].startswith('  -- AP rules'))
        self._def_block(lines[a + 1:b], self.defs)

    def _def_block(self, ls, into):
        i = 0
        while i < len(ls):
            m = re.match(r'^  (\S+) :: (.*);$', ls[i])
            if m and i + 1 < len(ls) and ls[i + 1].startswith('  ' + m.group(1) + '  = '):
                name, ty = m.group(1), m.group(2)
                body = [ls[i + 1][len('  ' + name + '  = '):]]
                j = i + 2
                while j < len(ls) and not ls[j].startswith('  --') and not re.match(r'^  \S', ls[j]):
                    body.append(ls[j])
                    j += 1
                src = ' '.join(x.strip() for x in body).rstrip()
                if src.endswith(';'):
                    src = src[:-1]
                self.types[name] = ty
                try:
                    into[name] = parse_expr(src, self.insts)
                except Exception as ex:
                    self.errors.append('def %s: %s' % (name, ex))
                    into[name] = ('lit', '?')
                i = j
            else:
                i += 1

    def _rules(self):
        t = self.text
        for m in re.finditer(r'rule (\S+) "(?:[^"\\]|\\.)*":\s*\n\s*when ', t):
            name = m.group(1)
            k = t.index('==>', m.end())
            pred = t[m.end():k].strip()
            b0 = t.index('{', k)
            b1 = match_brace(t, b0)
            body = t[b0 + 1:b1 - 1]
            try:
                p = parse_expr(pred, self.insts)
            except Exception as ex:
                self.errors.append('pred %s: %s' % (name, ex))
                p = ('lit', '?')
            acts = []
            for a in split_top(body, ';'):
                try:
                    acts.append(parse_action(a, self.insts))
                except Exception as ex:
                    self.errors.append('action in %s: %s: %s' % (name, ex, a[:80]))
            self.rules[name] = (p, acts)

    def _ifc(self, lines):
        cur, kind = None, None
        for i, ln in enumerate(lines):
            m = re.match(r'^  --(AIDef|AIAction|AIActionValue) (\S+)$', ln)
            if m:
                kind, cur = m.group(1), m.group(2)
                self.meth_args.setdefault(cur, [])
                continue
            if cur is None:
                continue
            if ln.startswith('  -- AP') or ln.startswith('  AScheduleInfo'):
                cur = None
                continue
            m = re.match(r'^  (\S+) :: (.*);$', ln)
            if m:
                nm = m.group(1)
                if i + 1 < len(lines) and lines[i + 1].startswith('  ' + nm + '  = '):
                    if kind in ('AIDef', 'AIActionValue') and nm == cur:
                        d = {}
                        self._def_block(lines[i:i + 40], d)
                        if nm in d:
                            self.ifc_vals[cur] = d[nm]
                elif kind in ('AIAction', 'AIActionValue'):
                    self.meth_args[cur].append(nm)

    def _sched(self, t):
        uses, ralloc = {}, {}
        k = t.find('-- method uses map')
        if k >= 0:
            e = t.find('-- rule uses map', k)
            blk = t[k:e]
            for m in re.finditer(r'\n    (\S+) -> \[', blk):
                key = m.group(1)
                j = blk.index('[', m.end() - 1)
                depth = 0
                for q in range(j, len(blk)):
                    if blk[q] == '[':
                        depth += 1
                    elif blk[q] == ']':
                        depth -= 1
                        if depth == 0:
                            break
                items = []
                for it in split_top(blk[j + 1:q], ','):
                    it = it.strip()
                    if it.startswith('(') and it.endswith(')'):
                        parts = split_top(it[1:-1], ',')
                        if len(parts) >= 2:
                            e_src = ' '.join(parts[0].split())
                            rs = split_top(parts[1].strip()[1:-1], ',')
                            rr = []
                            for r in rs:
                                r = r.strip()
                                if r.startswith('[') and r.endswith(']'):
                                    rr.append([x.strip() for x in r[1:-1].split(',') if x.strip()])
                            items.append((e_src, rr))
                uses[key] = items
        k = t.find('-- resource allocation table')
        if k >= 0:
            j = t.index('[', k)
            depth = 0
            for q in range(j, len(t)):
                if t[q] == '[':
                    depth += 1
                elif t[q] == ']':
                    depth -= 1
                    if depth == 0:
                        break
            for it in split_top(t[j + 1:q], ','):
                it = it.strip()
                if not (it.startswith('(') and it.endswith(')')):
                    continue
                parts = split_top(it[1:-1], ',')
                if len(parts) < 2:
                    continue
                key = parts[0].strip()
                lst = parts[1].strip()
                if not (lst.startswith('[') and lst.endswith(']')):
                    continue
                al = []
                for x in split_top(lst[1:-1], ','):
                    x = x.strip()
                    if x.startswith('(') and x.endswith(')'):
                        pp = split_top(x[1:-1], ',')
                        if len(pp) == 2:
                            al.append((' '.join(pp[0].split()), int(pp[1])))
                ralloc[key] = al
        return uses, ralloc

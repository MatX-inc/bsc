"""Read a corpus design for the end-to-end emitter: dump.txt (structure) and the
printed APackage (fan-in of guards, call arguments and task statements)."""
import os, re, sys
HERE = os.path.dirname(os.path.abspath(__file__))
SITES = os.path.join(HERE, '..', '..', 'sites', 'tools')
sys.path.insert(0, SITES)
import bapkg  # noqa: E402


def split_top(s, sep=','):
    out, depth, cur = [], 0, ''
    for ch in s:
        if ch in '[(':
            depth += 1
        elif ch in '])':
            depth -= 1
        if ch == sep and depth == 0:
            out.append(cur.strip()); cur = ''
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


def parse_sched(s):
    """bsc's schedule statements as [(ps, rel, qs)]."""
    out = []
    for item in split_top(s):
        m = re.match(r'^(\[[^\]]*\]|\S+)\s+(CF|SB|SBR|C)\s+(\[[^\]]*\]|\S+)$', item)
        if m:
            out.append((names(m.group(1)), m.group(2), names(m.group(3))))
    return out


def pairs(line):
    return re.findall(r'\(([^,()]+), ([^,()]+)\)', line)


class Design:
    def __init__(self, dump, ba):
        self.kids = {}        # name -> (type, sched string, sched list)
        self.kidorder = []
        self.ports = {}       # name -> {method: dict(kind=value/action, hasarg, sees=[...])}
        self.steps = []
        self.info = {}
        self._dump(open(dump).read().split('\n'))
        self.A = bapkg.APkg(open(ba).read()) if ba else None
        self._memo = {}

    def _dump(self, L):
        sec = None; cur = None; kid = None; meth = None
        for ln in L:
            if ln.startswith('children:'):
                sec = 'kids'; continue
            if ln.startswith('execution order:'):
                sec = 'steps'; continue
            if ln and not ln.startswith(' '):
                sec = None
                for key, nm in [('disjoint guards (bsc):', 'disj'), ('urgency blocking', 'urg'),
                                ('mutually_exclusive (user):', 'me')]:
                    if ln.startswith(key):
                        self.info[nm] = pairs(ln.split(":", 1)[1])
            if sec == 'kids':
                m = re.match(r'^  (\S+) : (\S+)\s+\[(.*)\]\s*$', ln)
                if m:
                    kid = m.group(1)
                    self.kids[kid] = (m.group(2), m.group(3), parse_sched(m.group(3)))
                    self.kidorder.append(kid)
                    self.ports[kid] = {}
                    continue
                m = re.match(r'^    (\S+) \((value|action|actionvalue)\)(?:, sees same-cycle (.*))?$', ln)
                if m and kid:
                    meth = m.group(1)
                    sees = [x.strip() for x in (m.group(3) or '').split(',') if x.strip()]
                    self.ports[kid][meth] = dict(kind=m.group(2), hasarg=False, sees=sees)
                    continue
                if kid and meth and re.match(r'^      copy', ln) and ' arg ' in ln:
                    self.ports[kid][meth]['hasarg'] = True
                continue
            if sec == 'steps':
                m = re.match(r'^  (\d+)\. (\S+)\s+\[(\w+)', ln)
                if m:
                    cur = dict(name=m.group(2), kind=m.group(3), reads=[], calls=[], checks=[], ooo=[])
                    self.steps.append(cur); continue
                m = re.match(r'^       reads (\S+?)\.(\S+) when (.*)$', ln)
                if m:
                    cur['reads'].append((m.group(1), m.group(2), m.group(3))); continue
                m = re.match(r'^       calls (\S+?)\.(\S+) when (.*?) with ?(.*)$', ln)
                if m:
                    cur['calls'].append((m.group(1), m.group(2), m.group(3), m.group(4).strip())); continue
                m = re.match(r'^       check (.*)$', ln)
                if m:
                    cur['checks'].append(m.group(1)); continue
                if 'out of order' in ln and cur is not None:
                    cur['ooo'].append(ln.strip())

    # ---- fan-in through the APackage ----
    def reads_of(self, e):
        k = e[0]
        if k == 'id':
            n = e[1]
            if self.A and n in self.A.defs:
                if n not in self._memo:
                    self._memo[n] = frozenset()
                    self._memo[n] = self.reads_of(self.A.defs[n])
                return self._memo[n]
            return frozenset()
        if k == 'lit':
            return frozenset()
        if k == 'call':
            s = {(e[1], e[2])}
            for a in e[3]:
                s |= self.reads_of(a)
            return frozenset(s)
        out = set()

        def walk(x):
            if isinstance(x, tuple) and x and isinstance(x[0], str) and x[0] in (
                    'id', 'lit', 'call', 'if', 'case', 'sel', 'app', 'bin', 'un', 'tup'):
                out.update(self.reads_of(x))
            elif isinstance(x, (list, tuple)):
                for y in x:
                    walk(y)
        for x in e[1:]:
            walk(x)
        return frozenset(out)

    def fanin_name(self, name):
        if self.A and name in self.A.defs:
            return self.reads_of(('id', name))
        return frozenset()

    def rule_acts(self, rname):
        if not self.A or rname not in self.A.rules:
            return None, []
        return self.A.rules[rname]

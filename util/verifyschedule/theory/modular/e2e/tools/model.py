"""The emitter's model of one module: kinds, instances, steps as site lists,
users, exclusions, the scan's tables and the ranks for Acyc.

Everything here is computed from a corpus design (design.Design); mke2e.py
turns it into Lean.  Data and guards are opaque operators over their real
fan-in (from the APackage): ops.f n / ops.g n applied to a tuple of reads.
"""
import re
import sys
import os

sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), '..', '..', 'scan', 'tools'))
import scanlib  # noqa: E402

# ---- child kinds, mirroring the Lean interfaces exactly ----


class KindDef:
    def __init__(self, name, lean_iface, mtype, ports, lports, prec, sbr, conf, share, has_en, always_rdy,
                 paths, builder, usesc, lib):
        self.__dict__.update(locals())
        del self.__dict__['self']

    def code(self, p):
        return self.ports.index(p)

    def fx(self, p):              # argument-less value port: every call carries z
        return self.share(p)


def sched_rel(sched, p, q):
    for ps, r, qs in sched:
        if p in ps and q in qs:
            return r
    return None


def of_sched(sched):
    prec = lambda p, q: p != q and sched_rel(sched, p, q) in ('SB', 'SBR')
    sbr = lambda p, q: sched_rel(sched, p, q) == 'SBR'

    def conf(p, q):
        a, b = sched_rel(sched, p, q), sched_rel(sched, q, p)
        return a == 'C' or b == 'C' or (a is None and b is None)
    return prec, sbr, conf


FL_PORTS = ['enq', 'deq', 'first', 'notFull', 'i_notFull', 'notEmpty', 'i_notEmpty', 'clear']


def make_kinds(fifo_sched, cfg_sched):
    K = {}
    # Reg: cregAnn 1
    isW = lambda p: p == 'write'
    K['reg'] = KindDef('reg', 'Reg.X', 'CRegM 1', ['read', 'write'], ['Reg.read', 'Reg.write'],
                       lambda p, q: (not isW(p) and isW(q)), lambda p, q: isW(p) and isW(q),
                       lambda p, q: False, lambda m: not isW(m), lambda m: isW(m), lambda m: True,
                       [], 'regW', 'regU', True)
    pc, sc, cc = of_sched(cfg_sched)
    K['cfg'] = KindDef('cfg', 'Cfg.X', 'CfgM', ['read', 'write'], ['CfgM.read', 'CfgM.write'], pc, sc, cc,
                       lambda m: m == 'read', lambda m: m == 'write', lambda m: True, [], 'cfgW', 'cfgU', False)
    pf, sf, cf = of_sched(fifo_sched)
    K['fifo'] = KindDef('fifo', 'FL.X', 'FL', FL_PORTS, ['FL.' + p for p in FL_PORTS], pf, sf, cf,
                        lambda m: m not in ('enq', 'deq', 'clear'), lambda m: m in ('enq', 'deq', 'clear'),
                        lambda m: m not in ('enq', 'deq', 'first'),
                        [('deq', 'enq'), ('deq', 'notFull'), ('deq', 'i_notFull'), ('deq', 'clear'), ('enq', 'clear')],
                        'fifoW', 'fifoU', False)
    W = ['wset', 'wget', 'whas']

    def rw(name, sbrRead, multi):
        prec = lambda p, q: p == 'wset' and q != 'wset'
        sbr = lambda p, q: (sbrRead and ((p == 'wset' and q != 'wset') or (q == 'wset' and p != 'wset'))) or \
            (multi and p == 'wset' and q == 'wset')
        conf = lambda p, q: (not multi) and p == 'wset' and q == 'wset'
        return KindDef(name, 'RWire.X %s %s' % (str(sbrRead).lower(), str(multi).lower()), 'WireM', W,
                       ['WireM.' + p for p in W], prec, sbr, conf, lambda m: m != 'wset', lambda m: m == 'wset',
                       lambda m: True, [('wset', 'wget'), ('wset', 'whas')], 'wireW', 'wireU', True)
    K['rwS'] = rw('rwS', True, True)
    K['rwC'] = rw('rwC', True, False)
    W0 = ['wset', 'whas']

    def rw0(name, sbrRead, multi):
        prec = lambda p, q: p == 'wset' and q == 'whas'
        sbr = lambda p, q: (sbrRead and p != q) or (multi and p == 'wset' and q == 'wset')
        conf = lambda p, q: (not multi) and p == 'wset' and q == 'wset'
        return KindDef(name, 'RWire0.X %s %s' % (str(sbrRead).lower(), str(multi).lower()), 'Wire0M', W0,
                       ['Wire0M.' + p for p in W0], prec, sbr, conf, lambda m: m == 'whas', lambda m: m == 'wset',
                       lambda m: True, [('wset', 'whas')], 'wire0W', 'wire0U', True)
    K['rw0S'] = rw0('rw0S', True, True)
    K['rw0C'] = rw0('rw0C', True, False)
    return K


KIND_ORDER = ['reg', 'cfg', 'fifo', 'rwS', 'rwC', 'rw0S', 'rw0C', 'tasks']


def kind_of(typ, sched_str):
    if typ in ('RegN', 'RegUN', 'RegA'):
        return 'reg'
    if typ == 'ConfigRegN':
        return 'cfg'
    if typ == 'FIFOL1':
        return 'fifo'
    if typ == 'RWire':
        return 'rwS' if 'wset SBR [wget, whas, wset]' in sched_str else 'rwC'
    if typ == 'RWire0':
        return 'rw0S' if 'wset SBR [whas, wset]' in sched_str else 'rw0C'
    raise ValueError('unsupported child type %s' % typ)


LIT = re.compile(r"^\d+'[dbh][0-9a-fA-F]+$")


TASKS = os.environ.get('E2E_TASKS', '1') == '1'


class Model:
    def __init__(self, D, nsteps=None):
        self.D = D
        if nsteps is None:
            steps = D.steps
        else:
            # a test subset: the first nsteps rules, and the run-time checks among them
            real = [s for s in D.steps if not s['checks']][:nsteps]
            keep = set(s['name'] for s in real)
            steps = []
            for s in D.steps:
                if s['name'] in keep:
                    steps.append(s)
                elif s['checks']:
                    nm = re.findall(r'WILL_FIRE_(\S+?)(?=[\s)&|]|$)', s['checks'][0])
                    if nm and nm[0] in keep and any(x in keep for x in nm[1:]):
                        steps.append(s)
        self.steps = steps
        self.nR = len(steps)
        self.ri = {s['name']: i for i, s in enumerate(steps)}
        # kinds
        fifo_sched = cfg_sched = None
        for k in D.kidorder:
            typ, ss, sl = D.kids[k]
            if typ == 'FIFOL1' and fifo_sched is None:
                fifo_sched = sl
            if typ == 'ConfigRegN' and cfg_sched is None:
                cfg_sched = sl
        self.fifo_sched = fifo_sched or []
        self.cfg_sched = cfg_sched or []
        self.K = make_kinds(self.fifo_sched, self.cfg_sched)
        used = set()
        for s in steps:
            for (k, m, _) in s['reads']:
                used.add(k)
            for (k, m, _, _) in s['calls']:
                used.add(k)
        self.inst = {}             # child name -> (kind, index)
        self.byk = {k: [] for k in KIND_ORDER}
        for k in D.kidorder:
            if k not in used:
                continue
            typ, ss, sl = D.kids[k]
            kd = kind_of(typ, ss)
            if kd == 'fifo':
                assert sl == self.fifo_sched, 'FIFOL1 schedules differ'
            if kd == 'cfg':
                assert sl == self.cfg_sched, 'ConfigRegN schedules differ'
            self.inst[k] = (kd, len(self.byk[kd]))
            self.byk[kd].append(k)
        self._sites()
        self.tasks_on = TASKS
        self._tasks()
        if self.nT:
            self.byk['tasks'] = ['__tasks']
            self.inst['__tasks'] = ('tasks', 0)
            tp = list(range(self.nT))
            self.K['tasks'] = KindDef('tasks', 'Tasks.X nT', 'Fin nT', tp, None, lambda p, q: p < q,
                                      lambda p, q: False, lambda p, q: p == q, lambda m: False, lambda m: True,
                                      lambda m: True, [], None, None, True)
        self.kinds = [k for k in KIND_ORDER if self.byk[k]]
        self.kn = {k: i for i, k in enumerate(self.kinds)}
        # slots
        self.off = {}
        o = 0
        for k in self.kinds:
            self.off[k] = o
            o += 1 if k == 'tasks' else len(self.byk[k]) * len(self.K[k].ports)
        self.nslots = o
        self.d = max(1, (o - 1).bit_length())
        self._excl()

    # ---- ports ----
    def port(self, child, meth):
        kd, i = self.inst[child]
        return (kd, i, meth)

    def lport(self, p):
        kd, i, m = p
        if kd == 'tasks':
            return '⟨iTasks 0, tP %d⟩' % m
        return '⟨%s %d, %s⟩' % (self.ihelper(kd), i, self.K[kd].lports[self.K[kd].code(m)])

    def ihelper(self, kd):
        return 'i' + kd[0].upper() + kd[1:]

    def ent(self, p):
        kd, i, m = p
        if kd == 'tasks':                      # a chain kind: one slot, code 0
            return (self.off[kd], self.kn[kd], 0)
        K = self.K[kd]
        return (self.off[kd] + i * len(K.ports), self.kn[kd], K.code(m))

    def is_rdy_guarded(self, p):
        return not self.K[p[0]].always_rdy(p[2])

    # ---- steps as site lists ----
    def _sites(self):
        D = self.D
        self.lits = {}
        self.ops_f = {}
        self.ops_g = {}
        self.rules = []
        for k, s in enumerate(self.steps):
            sites = []
            readpos = {}
            for (c, m, _) in s['reads']:
                p = self.port(c, m)
                readpos[(c, m)] = len(sites)
                sites.append(dict(kind='read', port=p))
            for (c, m, cond, a) in s['calls']:
                p = self.port(c, m)
                assert cond == "1'd1", 'conditional call %s' % cond
                sites.append(dict(kind='call', port=p, arg=self.argexpr(a, readpos)))
            g = None
            checks = s['checks']
            if not checks:
                fan = sorted(D.fanin_name('CAN_FIRE_' + s['name']))
                rds = []
                for (c, m) in fan:
                    rds.append(self.port(c, m))
                g = ('g', self.gid('CAN_FIRE_' + s['name']), rds)
            self.rules.append(dict(name=s['name'], sites=sites, guard=g, checks=checks, readpos=readpos))
        # check rules: head and list (indices into the selected steps)
        self.checks = []
        for k, r in enumerate(self.rules):
            if not r['checks']:
                continue
            assert len(r['checks']) == 1
            names = re.findall(r'WILL_FIRE_(\S+?)(?=[\s)&|]|$)', r['checks'][0])
            hd, rest = names[0], names[1:]
            if hd not in self.ri:
                r['guard'] = ('false',)
                continue
            rest = [x for x in rest if x in self.ri]
            r['guard'] = ('check', len(self.checks))
            self.checks.append(dict(rule=k, head=self.ri[hd], lst=[self.ri[x] for x in rest]))
        # blocking (urgency): rule -> blocker
        self.blk = {}
        for a, b in D.info.get('urg', []):
            if a in self.ri and b in self.ri:
                assert self.ri[a] not in self.blk
                self.blk[self.ri[a]] = self.ri[b]

    def _tasks(self):
        """Task statements, numbered in rank order and, within a rule, in action order."""
        D = self.D
        self.nT = 0
        self.tstart = []
        for k, r in enumerate(self.rules):
            self.tstart.append(self.nT)
            r['tasks'] = []
            if not self.tasks_on:
                continue
            pred, acts = D.A.rules.get(r['name'], (None, []))
            for a in acts:
                if not a or a[0] != 'task':
                    continue
                t = self.nT
                self.nT += 1
                if r['checks']:
                    cond = ('true',)          # the rule's own guard is the statement's condition
                    hw = ('chk',)
                elif a[2] is None:
                    cond = hw = ('true',)
                else:
                    fan = sorted(D.reads_of(a[2]))
                    idx = [r['readpos'][x] for x in fan]
                    cond = hw = ('g', self.gid('tcond_%d' % t), idx)
                args = [self.targexpr(x, r['readpos'], t, q) for q, x in enumerate(a[3])]
                r['tasks'].append(t)
                r['sites'].append(dict(kind='task', port=('tasks', 0, t), cond=cond, hw=hw, args=args, arg=('tup',)))
        self.tstart.append(self.nT)

    def targexpr(self, x, readpos, t, q):
        if x[0] == 'lit':
            return ('k', self.lit(x[1]))
        fan = sorted(self.D.reads_of(x))
        if not fan:
            return ('k', self.lit(repr(x)))
        return ('f', self.fid('targ_%d_%d' % (t, q)), [readpos[y] for y in fan])

    def lit(self, s):
        if s not in self.lits:
            self.lits[s] = len(self.lits)
        return self.lits[s]

    def fid(self, name):
        if name not in self.ops_f:
            self.ops_f[name] = len(self.ops_f)
        return self.ops_f[name]

    def gid(self, name):
        if name not in self.ops_g:
            self.ops_g[name] = len(self.ops_g)
        return self.ops_g[name]

    def argexpr(self, a, readpos):
        """('z',) | ('k', n) | ('f', n, [read site indices])."""
        if a == '':
            return ('z',)
        if LIT.match(a):
            return ('k', self.lit(a))
        fan = sorted(self.D.fanin_name(a))
        if not fan:
            return ('k', self.lit(a))
        idx = []
        for (c, m) in fan:
            assert (c, m) in readpos, 'argument reads %s.%s, not a read site' % (c, m)
            idx.append(readpos[(c, m)])
        return ('f', self.fid(a), idx)

    # ---- users per port ----
    def users(self):
        U = {}
        for k, r in enumerate(self.rules):
            for i, st in enumerate(r['sites']):
                U.setdefault(st['port'], []).append((k, i))
        return U

    # ---- exclusions ----
    def _excl(self):
        D = self.D
        self.disj = []
        seen = set()
        for a, b in D.info.get('disj', []):
            if a in self.ri and b in self.ri:
                x, y = self.ri[a], self.ri[b]
                if (x, y) not in seen:
                    seen.add((x, y))
                    self.disj.append((x, y))
        self.dfind = {}
        for d, (x, y) in enumerate(self.disj):
            self.dfind.setdefault((x, y), ('d', d, False))
            self.dfind.setdefault((y, x), ('d', d, True))
        self.mefind = {}
        for c, ch in enumerate(self.checks):
            for p, y in enumerate(ch['lst']):
                self.mefind.setdefault((ch['head'], y), ('me', c, p, False))
                self.mefind.setdefault((y, ch['head']), ('me', c, p, True))

    def excl_src(self, x, y):
        """A proof source that rules x and y never both fire."""
        if self.blk.get(x) == y:
            return ('blk', False)
        if self.blk.get(y) == x:
            return ('blk', True)
        if (x, y) in self.dfind:
            return self.dfind[(x, y)]
        if (x, y) in self.mefind:
            return self.mefind[(x, y)]
        return None

    # ---- the scan ----
    def kind_tables(self):
        kt = []
        for kd in self.kinds:
            if kd == 'tasks':
                kt.append([([], [0], [], [0])])
                continue
            K = self.K[kd]
            sk = scanlib.Kind(kd, K.ports, K.prec, K.sbr, K.conf, K.share, K.fx)
            kt.append([sk.kc(c) for c in range(len(K.ports))])
        return kt

    def design(self, exR):
        fpR = [[self.ent(st['port']) for st in r['sites']] for r in self.rules]
        z = [0]
        return scanlib.Design(0, self.nR, self.d, [], fpR, self.kind_tables(), z, z, z, z, z, [0],
                              exR, [0] * self.nR, [0] * self.nR)

    def scan_masks(self):
        """Needed exception bits: run the scan with every known exclusion, keep the bits it uses."""
        full = [0] * self.nR
        for x in range(self.nR):
            for y in range(self.nR):
                if x != y and self.excl_src(x, y) is not None:
                    full[y] |= 1 << x
        Dz = self.design(full)
        # mirror of run_scan, recording the bits each rule needs
        st = {}
        get = lambda s: st.get(s, (0, 0, 0, 0))
        need = [0] * self.nR
        for i in range(self.nR):
            g = i
            for (b, kd, c) in Dz.fpR[i]:
                cin, cout, gin, gout = Dz.kt[kd][c]
                if any(get(b + c2)[0] == g + 1 for c2 in gin):
                    raise ValueError('grp fails at rule %d' % i)
                o = get(b + c)
                st[b + c] = (g + 1, o[1], o[2] | (1 << i), o[3])
                bad = 0
                for c2 in cin:
                    bad |= get(b + c2)[2]
                bad &= ~(1 << i)
                if bad & ~full[i]:
                    raise ValueError('h4 fails at rule %d: %x' % (i, bad & ~full[i]))
                need[i] |= bad
        Dn = self.design(need)
        ok, states, info = scanlib.run_scan(Dn)
        assert ok, info
        return need, Dn

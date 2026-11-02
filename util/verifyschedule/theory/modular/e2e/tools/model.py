"""The emitter's model of one module: kinds, instances, steps as site lists,
users, exclusions, the scan's tables, escapes and the ranks for Acyc.

Everything here is computed from a corpus design (design.Design); mke2e.py
turns it into Lean.  Data and guards are opaque operators over their real
fan-in (from the APackage): ops.f n / ops.g n applied to a tuple of reads.

Steps: rules (table index k = 0 .. nR-1, schedule order) and exported methods
(table index nR + m).  Site conditions are schedlean's (sites/tools/sitecond.py,
via condsites.py): action calls exact; value reads unconditional unless needed,
(a) the conservative reach of an out-of-order read, (r) a readiness read under
its ready literal, with the use-level readiness fact over R_seq (Ravi's rule,
sequenced by expression structure).  A use of a conditional read is split on
its reach: `bif R then f(fan-in) else f'(fan-in without the read)`.
"""
import re
import sys
import os

sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), '..', '..', 'scan', 'tools'))
import scanlib  # noqa: E402

# ---- child kinds, mirroring the Lean interfaces exactly ----


class KindDef:
    def __init__(self, name, lean_iface, mtype, ports, lports, prec, sbr, conf, share, has_en, always_rdy,
                 paths, builder, usesc, lib, hasarg=None):
        self.__dict__.update(locals())
        del self.__dict__['self']
        if hasarg is None:
            self.hasarg = lambda p: has_en(p) and p not in ('deq', 'clear')

    def code(self, p):
        return self.ports.index(p)

    def fx(self, p):              # argument-less value port: every call carries z
        return self.share(p) and not self.hasarg(p)


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
F2_PORTS = ['enq', 'deq', 'first', 'notEmpty', 'notFull', 'i_notEmpty', 'i_notFull', 'clear']
RF_COPIES = 5


def make_kinds(fifo_sched, cfg_sched):
    K = {}
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
    # FIFO2 (guarded vMk2FIFOF): the library's fifoAnn, proved (fifo2_realizes); no paths, aliases
    f2prec = lambda p, q: (p, q) in {('first', 'deq'), ('first', 'clear'), ('notEmpty', 'deq'), ('notEmpty', 'enq'),
                                     ('notEmpty', 'clear'), ('notFull', 'deq'), ('notFull', 'enq'),
                                     ('notFull', 'clear'), ('deq', 'clear'), ('enq', 'clear')}
    f2sbr = lambda p, q: (p, q) in {('deq', 'clear'), ('enq', 'clear'), ('clear', 'clear')}
    f2conf = lambda p, q: (p, q) in {('enq', 'enq'), ('deq', 'deq')}
    f2share = lambda m: m not in ('enq', 'deq', 'clear')
    f2en = lambda m: m in ('enq', 'deq', 'clear')
    f2rdy = lambda m: m not in ('enq', 'deq', 'first')
    K['f2'] = KindDef('f2', 'FIFO2.X', 'FifoM', F2_PORTS, ['FifoM.' + p for p in F2_PORTS], f2prec, f2sbr, f2conf,
                      f2share, f2en, f2rdy, [], 'f2W', 'f2U', True)
    # SizedFIFO (assumed leaf with FIFO2's interface: the dump prints the same schedule)
    K['sf'] = KindDef('sf', 'FIFO2.X', 'FifoM', F2_PORTS, ['FifoM.' + p for p in F2_PORTS], f2prec, f2sbr, f2conf,
                      f2share, f2en, f2rdy, [], 'f2W', 'f2U', False)
    # RegFile (assumed leaf): upd and RF_COPIES copies of sub; sub's address reaches its result
    RFP = ['upd'] + ['s%d' % c for c in range(RF_COPIES)]
    isS = lambda p: p != 'upd'
    K['rf'] = KindDef('rf', 'RF.X', 'RFM', RFP, ['RFM.' + p for p in RFP],
                      lambda p, q: isS(p) and q == 'upd', lambda p, q: False, lambda p, q: p == 'upd' and q == 'upd',
                      isS, lambda m: m == 'upd', lambda m: True, [('s%d' % c, 's%d' % c) for c in range(RF_COPIES)],
                      'rfW', 'rfU', False, hasarg=lambda p: True)
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
                       lambda m: True, [('wset', 'whas')], 'wire0W', 'wire0U', True, hasarg=lambda p: False)
    K['rw0S'] = rw0('rw0S', True, True)
    K['rw0C'] = rw0('rw0C', True, False)
    return K


KIND_ORDER = ['reg', 'cfg', 'fifo', 'f2', 'sf', 'rf', 'rwS', 'rwC', 'rw0S', 'rw0C', 'tasks']


def kind_of(typ, sched_str):
    if typ in ('RegN', 'RegUN', 'RegA'):
        return 'reg'
    if typ == 'ConfigRegN':
        return 'cfg'
    if typ == 'FIFOL1':
        return 'fifo'
    if typ == 'FIFO2':
        return 'f2'
    if typ == 'SizedFIFO':
        return 'sf'
    if typ == 'RegFile':
        return 'rf'
    if typ == 'RWire':
        return 'rwS' if 'wset SBR [wget, whas, wset]' in sched_str else 'rwC'
    if typ == 'RWire0':
        return 'rw0S' if 'wset SBR [whas, wset]' in sched_str else 'rw0C'
    raise ValueError('unsupported child type %s' % typ)


LIT = re.compile(r"^\d+'[dbh][0-9a-fA-F]+$")
TASKS = os.environ.get('E2E_TASKS', '1') == '1'


def port_meth(m):
    """'sub[2]' -> ('sub', 2); 'read' -> ('read', None)."""
    mm = re.match(r'^(\w+)\[(\d+)\]$', m)
    return (mm.group(1), int(mm.group(2))) if mm else (m, None)


class Model:
    def __init__(self, D, nsteps=None, csites=None):
        self.D = D
        self.cs = csites or {}
        rules = [s for s in D.steps if s['kind'] != 'method']
        meths = [s for s in D.steps if s['kind'] == 'method']
        if nsteps is not None:
            real = [s for s in rules if not s['checks']][:nsteps]
            keep = set(s['name'] for s in real)
            sel = []
            for s in rules:
                if s['name'] in keep:
                    sel.append(s)
                elif s['checks']:
                    nm = re.findall(r'WILL_FIRE_(\S+?)(?=[\s)&|]|$)', s['checks'][0])
                    if nm and nm[0] in keep and any(x in keep for x in nm[1:]):
                        sel.append(s)
            rules = sel
            meths = []
        self.steps = rules
        self.msteps = meths
        self.nR = len(rules)
        self.nM = len(meths)
        self.ri = {s['name']: i for i, s in enumerate(rules)}
        self.mi = {s['name']: i for i, s in enumerate(meths)}
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
        for s in rules + meths:
            for (k, m, _) in s['reads']:
                used.add(k)
            for (k, m, _, _) in s['calls']:
                used.add(k)
        self.inst = {}
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
            if kd in ('f2', 'sf'):
                assert D.A is None or D.A.guarded.get(k, True), 'unguarded FIFO %s' % k
            self.inst[k] = (kd, len(self.byk[kd]))
            self.byk[kd].append(k)
        self.lits, self.ops_f, self.ops_g = {}, {}, {}
        self.facts = []            # readiness facts: kind 'rdy' (R -> ready) or 'inv' (R at site values = at ports)
        self.later_drops = 0       # R literals over a read not before a site that splits on R
        self.fan_outside = 0       # R literal reads that are not read sites of the step
        self.rules = []            # every step: rules then methods (table index = position)
        for s in rules:
            self.rules.append(self._step(s, None))
        for s in meths:
            self.rules.append(self._step(s, s['name']))
        self.nS = len(self.rules)
        self._checks()
        self.tasks_on = TASKS
        self._tasks()
        if self.nT:
            self.byk['tasks'] = ['__tasks']
            self.inst['__tasks'] = ('tasks', 0)
            tp = list(range(self.nT))
            self.K['tasks'] = KindDef('tasks', 'Tasks.X nT', 'Fin nT', tp, None, lambda p, q: p < q,
                                      lambda p, q: False, lambda p, q: p == q, lambda m: False, lambda m: True,
                                      lambda m: True, [], None, None, True, hasarg=lambda p: True)
        self.kinds = [k for k in KIND_ORDER if self.byk[k]]
        self.kn = {k: i for i, k in enumerate(self.kinds)}
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
        m, c = port_meth(meth)
        if kd == 'rf' and m == 'sub':
            m = 's%d' % (c or 0)
        return (kd, i, m)

    def lport(self, p):
        kd, i, m = p
        if kd == 'tasks':
            return '⟨iTasks 0, tP %d⟩' % m
        return '⟨%s %d, %s⟩' % (self.ihelper(kd), i, self.K[kd].lports[self.K[kd].code(m)])

    def ihelper(self, kd):
        return 'i' + kd[0].upper() + kd[1:]

    def ent(self, p):
        kd, i, m = p
        if kd == 'tasks':
            return (self.off[kd], self.kn[kd], 0)
        K = self.K[kd]
        return (self.off[kd] + i * len(K.ports), self.kn[kd], K.code(m))

    def is_rdy_guarded(self, p):
        return not self.K[p[0]].always_rdy(p[2])

    def is_meth(self, k):
        return k >= self.nR

    # ---- expressions over a step's read sites ----
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

    def _fan(self, st, ports, what):
        idx = []
        for p in sorted(ports):
            if p in st['readpos']:
                idx.append(st['readpos'][p])
                continue
            cp = [i for (c, m), i in st['readpos'].items() if c == p[0] and port_meth(m)[0] == p[1]]
            assert cp, '%s reads %s.%s, not a read site of %s' % (what, p[0], p[1], st['name'])
            idx += cp
        return sorted(set(idx))

    def _wrap(self, st, e, boolean, name):
        """Split an expression on the reach of the conditional reads in its fan-in."""
        if e[0] not in ('f', 'g'):
            return e
        cr = [i for i in e[2] if st['sites'][i].get('R') is not None]
        if not cr:
            return e
        rest = [i for i in e[2] if i not in cr]
        alt = (('g' if boolean else 'f'), (self.gid if boolean else self.fid)(name + '#nx'), rest) if rest else \
            (('gk', self.gid(name + '#nx')) if boolean else ('k', self.lit(name + '#nx')))
        return ('split', sorted(set(cr)), e, alt)

    def expr_name(self, st, name, what, boolean=False):
        """An APackage definition (by name) as an opaque operator over its read fan-in."""
        fan = self.D.fanin_name(name)
        if not fan:
            return ('gk', self.gid(name)) if boolean else ('k', self.lit(name))
        e = (('g', self.gid(name), self._fan(st, fan, what)) if boolean else
             ('f', self.fid(name), self._fan(st, fan, what)))
        return self._wrap(st, e, boolean, name)

    def expr_ast(self, st, x, name, what, boolean=False):
        if x[0] == 'lit':
            return ('gk', self.gid(name)) if boolean else ('k', self.lit(x[1]))
        fan = self.D.reads_of(x)
        if not fan:
            return ('gk', self.gid(name)) if boolean else ('k', self.lit(repr(x)))
        e = (('g', self.gid(name), self._fan(st, fan, what)) if boolean else
             ('f', self.fid(name), self._fan(st, fan, what)))
        return self._wrap(st, e, boolean, name)

    # ---- one step as a site list ----
    def _step(self, s, meth):
        D = self.D
        name = s['name']
        cs = {e['label']: e for e in self.cs.get(name, [])}
        # reads: ready literals first, then reads whose arguments or conditions read nothing, ...
        reads = []
        for (c, m, _) in s['reads']:
            reads.append((c, m))
        rp = {}
        for (c, m) in reads:
            rp[(c, m)] = self.port(c, m)
        lits_first = set()
        for lab, e in cs.items():
            if e['kind'] == 'rdy':
                for (k_, m_) in e['rdy']:
                    if (k_, m_) in rp:
                        lits_first.add((k_, m_))
        subA = {}
        if D.A is not None:
            ex_ = []
            r_ = D.A.rules.get(name)
            if r_ is not None:
                ex_.append(r_[0])
                for a_ in r_[1]:
                    if a_ and a_[0] == 'call':
                        ex_ += ([a_[3]] if a_[3] is not None else []) + list(a_[4])
                    elif a_ and a_[0] == 'task':
                        ex_ += ([a_[2]] if a_[2] is not None else []) + list(a_[3])
            for (inst_, a_) in self._sub_calls(ex_):
                subA.setdefault(inst_, set()).update(D.reads_of(a_))
        dep = {}
        for (c, m) in reads:
            d = set()
            if port_meth(m)[0] == 'sub' and c in subA:
                for (c2, m2) in subA[c]:
                    d |= set(x for x in rp if x[0] == c2 and port_meth(x[1])[0] == m2)
            e = cs.get('%s.%s' % (c, m))
            if e is not None and e['kind'] == 'reach' and not e['true']:
                d |= set(e['fan'])
            if e is not None and e['kind'] == 'rdy':
                d |= set(x for x in e['rdy'] if x in rp)
            dep[(c, m)] = d & set(rp)
        order, placed = [], set()
        pend = sorted(reads, key=lambda x: (0 if x in lits_first else 1, reads.index(x)))
        while pend:
            for x in pend:
                if dep[x] <= placed:
                    order.append(x)
                    placed.add(x)
                    pend.remove(x)
                    break
            else:
                raise ValueError('cyclic read conditions in %s' % name)
        st = dict(name=name, meth=meth, sites=[], readpos={}, checks=s['checks'], guard=None, tasks=[])
        for (c, m) in order:
            p = rp[(c, m)]
            st['readpos'][(c, m)] = len(st['sites'])
            st['sites'].append(dict(kind='read', port=p, label='%s.%s' % (c, m), arg=('z',), cond=('true',), port_name=(c, m)))
        # conditions of conditional reads
        for i, site in enumerate(st['sites']):
            e = cs.get(site['label'])
            if e is None:
                continue
            if e['kind'] == 'reach':
                if not e['true']:
                    site['cond'] = ('g', self.gid('reach_%s_%s' % (name, site['label'])), self._fan(st, e['fan'], 'reach'))
                    site['R'] = site['cond']
                    site['ckind'] = 'reach'
            else:
                lp = [st['readpos'][x] for x in e['rdy'] if x in st['readpos']]
                assert lp, 'no ready literal read for %s' % site['label']
                site['cond'] = ('lit', lp[0])
                cr = set((ss['port'][0], ss['port'][1]) for ss in st['sites'] if ss.get('R') is not None)
                fan = [x for x in e['R_fan'] if x in st['readpos']]
                site['ckind'] = 'rdy'
                site['R'] = ('g', self.gid('rseq_%s_%s' % (name, site['label'])), sorted(st['readpos'][x] for x in fan))
                site['rdy_e'] = e
                self.fan_outside += len([x for x in e['R_fan'] if x not in st['readpos']])
        # an (a) condition reads only unconditional reads (it does in both e2e modules; the
        # site-value form below would apply to it too, with an invariance fact)
        for i, site in enumerate(st['sites']):
            if site.get('ckind') == 'reach':
                bad = [j for j in site['R'][2] if st['sites'][j].get('R') is not None]
                assert not bad, 'an (a) condition over a conditional read in %s' % name
        # RegFile sub copies carry an address: pair the step's sub calls with its copies, in order
        subargs = {}
        if D.A is not None:
            exprs = []
            r = D.A.rules.get(name)
            if r is not None:
                exprs.append(r[0])
                for a in r[1]:
                    if a and a[0] == 'call':
                        exprs += ([a[3]] if a[3] is not None else []) + list(a[4])
                    elif a and a[0] == 'task':
                        exprs += ([a[2]] if a[2] is not None else []) + list(a[3])
            if meth is not None and meth in D.A.ifc_vals:
                exprs.append(D.A.ifc_vals[meth])
            for x in self._sub_calls(exprs):
                subargs.setdefault(x[0], [])
                if x[1] not in subargs[x[0]]:
                    subargs[x[0]].append(x[1])
        for inst in list(subargs):
            subargs[inst].sort(key=repr)
        for i, site in enumerate(st['sites']):
            kd, ii, m = site['port']
            if kd == 'rf':
                inst = self.byk['rf'][ii]
                copies = sorted(int(ss['port'][2][1:]) for ss in st['sites'] if ss['port'][0] == 'rf' and ss['port'][1] == ii)
                args = subargs.get(inst, [])
                c = int(m[1:])
                a = args[copies.index(c)] if copies.index(c) < len(args) else (args[-1] if args else None)
                if a is None:
                    site['arg'] = ('k', self.lit('rfaddr_%s_%s' % (name, site['label'])))
                else:
                    an = a[1] if a[0] == 'id' else 'rfaddr_' + repr(a)
                    site['arg'] = self.expr_ast(st, a, an, 'address')
        # calls
        for (c, m, cond, a) in s['calls']:
            p = self.port(c, m)
            if cond == "1'd1":
                cc = ('true',)
            else:
                cc = self.expr_name(st, cond, 'condition', boolean=True) if cond in D.A.defs else \
                    self.expr_ast(st, self._parse(cond), 'cc_%s_%d' % (name, len(st['sites'])), 'condition', True)
                if cc[0] == 'gk':
                    cc = ('true',) if cond in ("1'b1", "1'd1") else cc
            arg = self.argexpr(st, a, meth)
            st['sites'].append(dict(kind='call', port=p, label='%s.%s' % (c, m), arg=arg, cond=cc))
        self._rdy_terms(st)
        # guard: CAN_FIRE's fan-in (a run-time check rule's guard is set by _checks)
        if not s['checks']:
            st['guard'] = self.expr_name(st, 'CAN_FIRE_' + name, 'guard', boolean=True)
            if st['guard'][0] == 'gk':
                st['guard'] = ('g', st['guard'][1], [])
        # a method's result
        st['res'] = ('z',)
        if meth is not None and D.A is not None and meth in D.A.ifc_vals and not meth.startswith('RDY_'):
            st['res'] = self.expr_ast(st, D.A.ifc_vals[meth], 'res_' + meth, 'result')
        return st

    def _rdy_terms(self, st):
        """Readiness R terms in the site-value form (round 7 review s1 M1): R reads the site values of
        the step's conditional reads; a literal over a read not before some site that splits on R is
        dropped (counted), and the verdicts are those of the formula as emitted."""
        def splits(e, acc):
            if not isinstance(e, tuple) or not e:
                return
            if e[0] == 'split':
                acc.update(e[1])
                splits(e[2], acc)
                splits(e[3], acc)
        first_use = {}
        for s_idx, site in enumerate(st['sites']):
            acc = set()
            for ex_ in [site.get('cond'), site.get('arg')] + list(site.get('args', [])):
                splits(ex_, acc)
            for i in acc:
                first_use[i] = min(first_use.get(i, s_idx), s_idx)
        for i, site in enumerate(st['sites']):
            if site.get('ckind') != 'rdy':
                continue
            e = site['rdy_e']
            fan = site['R'][2]
            lim = first_use.get(i, len(st['sites']))
            late = [j for j in fan if j >= lim]
            if late:
                self.later_drops += len(late)
                drop = [(st['sites'][j]['port_name']) for j in late]
                sv = e['check'](drop)
                fan = [j for j in fan if j < lim]
                site['R'] = ('g', site['R'][1], fan)
            else:
                sv = e['sv']
            Y = [j for j in fan if st['sites'][j].get('R') is not None]
            ys = set(st['sites'][j]['port_name'] for j in Y)
            assert ys == set(sv['Y']), 'site-value reads differ in %s / %s: %s vs %s' % (st['name'], site['label'], ys, sv['Y'])
            site['Ysv'] = Y
            site['fact'] = len(self.facts)
            site['confirmed'] = sv['rdy']
            self.facts.append(dict(kind='rdy', site=i, lit=site['cond'][1], ok=sv['rdy'], step=st['name'],
                                   label=site['label'], Y=Y, late=len(late)))
            if [j for j in Y if j != i]:
                site['inv'] = len(self.facts)
                self.facts.append(dict(kind='inv', site=i, ok=bool(sv['inv']), step=st['name'],
                                       label=site['label'], Y=Y, late=len(late)))
            elif Y:
                site['inv'] = 'self'
            else:
                site['inv'] = None

    def _parse(self, txt):
        import bapkg
        return bapkg.parse_expr(txt, self.D.A.insts)

    def _sub_calls(self, exprs):
        out, seen = [], set()
        stack = list(exprs)
        while stack:
            x = stack.pop(0)
            if not isinstance(x, tuple) or not x:
                continue
            t = x[0]
            if t == 'id':
                n = x[1]
                if n in self.D.A.defs and n not in seen:
                    seen.add(n)
                    stack.append(self.D.A.defs[n])
                continue
            if t == 'call':
                if x[2] == 'sub' and x[1] in self.inst and self.inst[x[1]][0] == 'rf':
                    out.append((x[1], x[3][0] if x[3] else ('lit', '0')))
                stack.extend(x[3])
                continue
            for y in x[1:]:
                if isinstance(y, tuple):
                    stack.append(y)
                elif isinstance(y, list):
                    for z in y:
                        if isinstance(z, tuple):
                            stack.append(z)
                            for w in z:
                                if isinstance(w, tuple):
                                    stack.append(w)
        return out

    def argexpr(self, st, a, meth):
        """('z',) | ('k', n) | ('x',) | ('f', n, [read site indices]) | ('split', ...)."""
        if a == '':
            return ('z',)
        if meth is not None and a in self.D.A.meth_args.get(meth, []):
            return ('x',)
        if LIT.match(a):
            return ('k', self.lit(a))
        if a in self.D.A.defs:
            return self.expr_name(st, a, 'argument')
        return ('k', self.lit(a))

    def _checks(self):
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
        # urgency: rule -> its blockers
        self.blk = {}
        for a, b in self.D.info.get('urg', []):
            if a in self.ri and b in self.ri:
                self.blk.setdefault(self.ri[a], []).append(self.ri[b])

    def _tasks(self):
        """Task statements, numbered in rank order and, within a rule, in action order."""
        D = self.D
        self.nT = 0
        self.tstart = []
        for k, r in enumerate(self.rules):
            self.tstart.append(self.nT)
            if not self.tasks_on or r['meth'] is not None or D.A is None:
                continue
            pred, acts = D.A.rules.get(r['name'], (None, []))
            for a in acts:
                if not a or a[0] != 'task':
                    continue
                t = self.nT
                self.nT += 1
                if r['checks']:
                    cond, hw = ('true',), ('chk',)
                elif a[2] is None:
                    cond = hw = ('true',)
                else:
                    cond = self.expr_ast(r, a[2], 'tcond_%d' % t, 'task condition', True)
                    if cond[0] == 'gk':
                        cond = ('g', cond[1], [])
                    hw = cond
                args = [self.expr_ast(r, x, 'targ_%d_%d' % (t, q), 'task argument') for q, x in enumerate(a[3])]
                r['tasks'].append(t)
                r['sites'].append(dict(kind='task', port=('tasks', 0, t), cond=cond, hw=hw, args=args, arg=('tup',)))
        self.tstart.append(self.nT)

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
        if y in self.blk.get(x, []):
            return ('blk', False, self.blk[x].index(y))
        if x in self.blk.get(y, []):
            return ('blk', True, self.blk[y].index(x))
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

    def meth_tables(self, mann):
        """The parent's own interface tables (nfI, nfO, ngI, ngO, h3ok) from its annotation."""
        nM = self.nM
        bit = lambda i: 1 << i
        fol = lambda a, b: (a == b or not mann.prec(b, a)) and not mann.conf(a, b) and not mann.conf(b, a)
        grp = lambda a, b: fol(a, b) and not mann.sbr(a, b) and not mann.sbr(b, a)
        nfI = [sum(bit(a) for a in range(nM) if not fol(a, m2)) for m2 in range(nM)]
        nfO = [sum(bit(b) for b in range(nM) if not fol(m1, b)) for m1 in range(nM)]
        ngI = [sum(bit(a) for a in range(nM) if not grp(a, m2)) for m2 in range(nM)]
        ngO = [sum(bit(b) for b in range(nM) if not grp(m1, b)) for m1 in range(nM)]
        h3ok = [sum(bit(m) for m in range(nM) if m != mp and ((mann.prec(m, mp) and mann.sbr(m, mp)) or
                                                              mann.conf(m, mp) or mann.conf(mp, m))) for mp in range(nM)]
        return nfI, nfO, ngI, ngO, h3ok

    def design(self, exR, escR=None, mann=None):
        fpR = [[self.ent(st['port']) for st in r['sites']] for r in self.rules[:self.nR]]
        fpM = [[self.ent(st['port']) for st in r['sites']] for r in self.rules[self.nR:]]
        z = [0]
        if self.nM:
            nfI, nfO, ngI, ngO, h3ok = self.meth_tables(mann)
            zm = [0] * self.nM
            return scanlib.Design(self.nM, self.nR, self.d, fpM, fpR, self.kind_tables(), nfI, nfO, ngI, ngO, h3ok,
                                  zm, exR, [0] * self.nR, escR or [0] * self.nR)
        return scanlib.Design(0, self.nR, self.d, [], fpR, self.kind_tables(), z, z, z, z, z, [0],
                              exR, [0] * self.nR, escR or [0] * self.nR)

    def scan_masks(self, mann=None):
        """Needed exception bits, and the escaped pairs: run the scan with every known exclusion,
        keep the bits it uses; a static conflict no exclusion covers becomes an escape."""
        nR, nM = self.nR, self.nM
        full = [0] * nR
        for x in range(nR):
            for y in range(nR):
                if x != y and self.excl_src(x, y) is not None:
                    full[y] |= 1 << x
        Dz = self.design(full, None, mann)
        st = {}
        get = lambda s: st.get(s, (0, 0, 0, 0))
        need = [0] * nR
        esc = [0] * nR
        for k in range(nM):
            for (b, kd, c) in Dz.fpM[k]:
                o = get(b + c)
                st[b + c] = (k + 1, o[1] | (1 << k), o[2], o[3])
        for i in range(nR):
            g = nM + i
            for (b, kd, c) in Dz.fpR[i]:
                cin, cout, gin, gout = Dz.kt[kd][c]
                if any(get(b + c2)[0] == g + 1 for c2 in gin):
                    raise ValueError('grp fails at rule %d' % i)
                o = get(b + c)
                st[b + c] = (g + 1, o[1], o[2], o[3])
                bad = 0
                for c2 in cin:
                    bad |= get(b + c2)[2]
                bad &= ~(1 << i)
                need[i] |= bad & full[i]
                esc[i] |= bad & ~full[i]
            for (b, kd, c) in Dz.fpR[i]:
                o = get(b + c)
                st[b + c] = (o[0], o[1], o[2] | (1 << i), o[3])
        Dn = self.design(need, esc, mann)
        ok, states, info = scanlib.run_scan(Dn)
        assert ok, info
        return need, esc, Dn

    def esc_pairs(self, esc):
        """Per escaped rule pair (r1 earlier, r2): the statically failing site pairs."""
        out = []
        for r2 in range(self.nR):
            for r1 in range(self.nR):
                if not (esc[r2] >> r1) & 1:
                    continue
                bad = []
                for i1, s1 in enumerate(self.rules[r1]['sites']):
                    for i2, s2 in enumerate(self.rules[r2]['sites']):
                        if s1['port'][:2] != s2['port'][:2]:
                            continue
                        K = self.K[s1['port'][0]]
                        if s1['port'][0] == 'tasks':
                            continue
                        p, q = s1['port'][2], s2['port'][2]
                        folP = (p == q or not K.prec(q, p)) and not K.conf(p, q) and not K.conf(q, p)
                        argP = p != q or not K.share(p) or K.fx(p)
                        if not (folP and argP):
                            bad.append((i1, i2))
                out.append((r1, r2, bad))
        return out

# Acyc read-list sizes under conventions.md C4: Σ|D| over every child input
# (the reviewer's fanin.py measure) against Σ|D| over path-bearing inputs only
# (action ports some value port of the same child "sees same-cycle", from the
# dump, on kinds with a combinational path: wires, CRegs, FIFOL1, submodules), which are the only ones that need precise lists.
# usage: python3 -I fanin_path.py <design dir with out.lean and dump.txt>...
import re, sys
sys.setrecursionlimit(100000)
NAME = r'(?:«[^»]+»|[A-Za-z_][A-Za-z0-9_.]*(?:\.«[^»]+»)?)'
# kinds whose Verilog has no combinational input-to-output path (registered state)
PATHFREE = {'FIFO2', 'FIFO20', 'SizedFIFO', 'SizedFIFO0', 'RegN', 'RegUN', 'RegA', 'ConfigRegN', 'RevertReg',
            'RegFile', 'BRAM1Load', 'BRAM1BELoad', 'BRAM2', 'BRAM2Load', 'Probe'}
tok = re.compile(r'(?:[iv])\.(«[^»]+»|[A-Za-z_][A-Za-z0-9_]*)|(' + NAME + r')')
for d in sys.argv[1:]:
    src = open(d + '/out.lean').read()
    dump = open(d + '/dump.txt').read().split('\n')
    # path-bearing input fields from the dump
    pb = set(); child = None; meths = {}; seen = set()
    def flush():
        for mth in seen:
            pb.update(meths.get(mth, []))
    for ln in dump:
        if ln.startswith('steps') or ln.startswith('schedule') or (ln and not ln.startswith(' ') and child is not None and not ln.startswith('children')):
            pass
        m = re.match(r'^  (\S+) : (\S+)', ln)
        if m:
            flush(); child = m.group(1); meths = {}; seen = set(); cur = None
            if m.group(2) in PATHFREE: child = None
            continue
        m = re.match(r'^    (\S+) \((value|action)\)(?:, sees same-cycle (.*))?$', ln)
        if m and child is not None:
            cur = m.group(1); meths.setdefault(cur, [])
            if m.group(3):
                for a in m.group(3).split(','):
                    seen.add(a.strip())
            continue
        m = re.match(r'^      copy \S+: (.*)$', ln)
        if m and child is not None and cur is not None:
            for f in re.findall(r'(?:enable|arg) (\S+)', m.group(1)):
                meths[cur].append(f)
        if ln.startswith('steps:') or re.match(r'^  1\. ', ln):
            flush(); child = None
    flush()
    defs = {}
    for m in re.finditer(r'^noncomputable (?:def|abbrev) (' + NAME + r')([^\n]*?):=\s*(.*?)(?=^noncomputable |^theorem |^structure |^/--|^-- =|^section|^end |\Z)', src, re.S | re.M):
        name, body = m.group(1), m.group(3)
        if name.startswith('Seq.') or name.startswith('Agree') or name.startswith('View.at_'): continue
        defs[name] = body
    sched = defs.get('stepScheduled', '')
    fields = re.findall(r'(«[^»]+»|[A-Za-z_][A-Za-z0-9_]*) := ([^,\n]*(?:\n(?!\s*(?:«|[A-Za-z_][A-Za-z0-9_]* :=))[^\n]*)*)', sched)
    memo = {}
    def refs(body):
        ins, ds = set(), set()
        for m in tok.finditer(body):
            if m.group(1): ins.add(m.group(1))
            elif m.group(2) in defs: ds.add(m.group(2))
        return ins, ds
    def clos(dn):
        if dn in memo: return memo[dn]
        memo[dn] = frozenset()
        ins, ds = refs(defs[dn]); a = set(ins)
        for e in ds: a |= clos(e)
        memo[dn] = frozenset(a); return memo[dn]
    cone = {}
    def defcone(dn, acc):
        if dn in acc: return
        acc.add(dn)
        for e in refs(defs[dn])[1]: defcone(e, acc)
    tot = n = totp = np_ = mxp = 0
    nodes = set(); direct = 0
    for (f, expr) in fields:
        if '$' not in f: continue
        ins, ds = refs(expr); a = set(ins)
        for e in ds: a |= clos(e)
        co = [x for x in a if '$' in x or '@' in x]
        n += 1; tot += len(co)
        if f.strip('«»') in pb:
            np_ += 1; totp += len(co); mxp = max(mxp, len(co))
            direct += len([x for x in ins if '$' in x or '@' in x]) + len(ds)
            for e in ds: defcone(e, nodes)
    for dn in nodes:
        i2, d2 = refs(defs[dn])
        direct += len([x for x in i2 if '$' in x or '@' in x]) + len(d2)
    print("%-24s child inputs %5d  sum|D| %7d | path-bearing %5d  sum|D| %6d (max %d) | node lemmas %5d, direct facts %6d" % (
        d.rstrip('/').split('/')[-1], n, tot, np_, totp, mxp, len(nodes), direct))

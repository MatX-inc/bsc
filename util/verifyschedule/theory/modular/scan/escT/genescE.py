"""Escaped pairs through the real emitter (r8/e2e/tools/mke2e.py), from cost/big/genbig.py.
  facts W:  R_0 writes s_0..s_{W-1} under conditions c_k (fan-in g.read); R_1 reads every s_k: one escaped
            pair (R_0, R_1) with W dynamic facts, each an `Unconfirmed` field (E2E_SITES=0) and a fact proof.
  tasks T:  R_0 writes s_0 under c_0 and has T task statements; R_1 reads s_0 and has T task statements:
            one escaped pair with 1 fact, nT = 2T, R_0's T task sites each checked against every Tasks port.
  pairs P W: P disjoint escaped pairs (R_2p writes, R_2p+1 reads), W facts each.
  tri A B C: R_pre with A task statements (no conflict), then the tasks pair with B statements in R_0 and C in
            R_1: R_0's statements are numbered A..A+B-1 of nT = A+B+C (sysConflictFreeOKLarge RL_r2 -> RL_r3:
            513 512 1025).
usage: E2E_SITES=0 MARK=1 python3 -I genescE.py EMITTER_DIR OUT.lean NAME SHAPE N [W]"""
import os
import re
import sys

edir, out, name, shape, N = sys.argv[1], sys.argv[2], sys.argv[3], sys.argv[4], int(sys.argv[5])
W2 = int(sys.argv[6]) if len(sys.argv) > 6 else 0
W3 = int(sys.argv[7]) if len(sys.argv) > 7 else 0
sys.path.insert(0, edir)
import design  # noqa: E402

REG = ('RegN', '[read CF read, read SB write, write SBR write]')


class StubA:
    def __init__(self, defs, rules):
        self.defs = defs
        self.meth_args, self.rules, self.ifc_vals, self.guarded, self.insts = {}, rules, {}, {}, {}


def pair(i, regs, steps, fan, rules, tasks=0, tasks2=None):
    """R_{2i} writes regs under conditions, R_{2i+1} reads them (later in execution order)."""
    g, h = 'g%d' % i, 'h%d' % i
    steps.append(dict(name='R_%d' % (2 * i), kind='rule', reads=[(g, 'read', "1'd1")],
                      calls=[(k, 'write', 'c_%s' % k, 'a_%s' % k) for k in regs], checks=[], ooo=[]))
    for k in regs:
        fan['c_%s' % k] = frozenset([(g, 'read')])
        fan['a_%s' % k] = frozenset([(g, 'read')])
    fan['CAN_FIRE_R_%d' % (2 * i)] = frozenset([(g, 'read')])
    steps.append(dict(name='R_%d' % (2 * i + 1), kind='rule', reads=[(k, 'read', "1'd1") for k in regs],
                      calls=[(h, 'write', "1'd1", 'b_%d' % i)], checks=[], ooo=[]))
    fan['b_%d' % i] = frozenset([(regs[0], 'read')])
    fan['CAN_FIRE_R_%d' % (2 * i + 1)] = frozenset([(regs[0], 'read')])
    if tasks:
        rules['R_%d' % (2 * i)] = (None, [('task', '$display', None, []) for _ in range(tasks)])
        rules['R_%d' % (2 * i + 1)] = (None, [('task', '$display', None, []) for _ in range(tasks if tasks2 is None else tasks2)])
    return [g, h] + regs


class FakeDesign:
    def __init__(self, dump, ba):
        kids, steps, fan, rules = [], [], {}, {}
        if shape == 'facts':
            kids += pair(0, ['s%d' % k for k in range(N)], steps, fan, rules)
        elif shape == 'tasks':
            kids += pair(0, ['s0'], steps, fan, rules, tasks=N)
        elif shape == 'tri':
            steps.append(dict(name='R_pre', kind='rule', reads=[('gp', 'read', "1'd1")],
                              calls=[('hp', 'write', "1'd1", 'ap')], checks=[], ooo=[]))
            fan['ap'] = frozenset([('gp', 'read')]); fan['CAN_FIRE_R_pre'] = frozenset([('gp', 'read')])
            rules['R_pre'] = (None, [('task', '$display', None, []) for _ in range(N)])
            kids += ['gp', 'hp'] + pair(0, ['s0'], steps, fan, rules, tasks=W2, tasks2=W3)
        elif shape == 'pairs':
            for p in range(N):
                kids += pair(p, ['s%d_%d' % (p, k) for k in range(W2)], steps, fan, rules)
        self.kidorder = kids
        self.kids = {k: (REG[0], REG[1][1:-1], design.parse_sched(REG[1][1:-1])) for k in kids}
        self.ports = {k: {} for k in kids}
        self.steps, self.fan = steps, fan
        self.info = {'urg': [], 'disj': []}
        self.A = StubA({n: ('id', n) for n in fan}, rules)
        self.synth, self.bvi, self._memo = set(), set(), {}   # today's design.Design fields (r8b)

    def fanin_name(self, n):
        return self.fan.get(n, frozenset())

    def reads_of(self, e):
        return frozenset()


design.Design = FakeDesign
src_ = os.path.join(edir, 'mke2e.py')
sys.argv = [src_, 'dump', '', out, name]
code_ = open(src_).read()
# PROBE_GATE="NAME=VALUE,...": a probe beyond a bound (the file states the bound it was emitted under)
for kv_ in filter(None, os.environ.get('PROBE_GATE', '').split(',')):
    k_, v_ = kv_.split('=')
    code_, n_ = re.subn(r'^%s = \d+' % k_, '%s = %s' % (k_, v_), code_, count=1, flags=re.M)
    assert n_ == 1, k_
exec(compile(code_, src_, 'exec'), {'__name__': '__main__', '__file__': src_})

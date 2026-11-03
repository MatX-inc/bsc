"""Large-step probe through the e2e emitter (cost review r7-2 M1): rules with given site counts, each
rule reading and then writing its own registers (no two rules share a register: no muxes, no
exception groups), plus task statements without arguments.
usage: E2E_SITES=0 python3 -I genbig.py EMITTER_DIR OUT.lean NAME EMITTER SPEC
SPEC: comma-separated steps, each SITES or SITES:TASKS (SITES includes TASKS; SITES - TASKS even)."""
import os
import re
import runpy
import sys

edir, out, name, emit, spec = sys.argv[1:6]
sys.path.insert(0, edir)
import design  # noqa: E402

REG = ('RegN', '[read CF read, read SB write, write SBR write]')
STEPS = []
for x in spec.split(','):
    n, t = (x.split(':') + ['0'])[:2]
    n, t = int(n), int(t)
    assert (n - t) % 2 == 0 and n > t
    STEPS.append((n, t))


class StubA:
    def __init__(self, defs, rules):
        self.defs = defs
        self.meth_args, self.rules, self.ifc_vals, self.guarded, self.insts = {}, rules, {}, {}, {}


class FakeDesign:
    def __init__(self, dump, ba):
        kids, steps, fan, rules = [], [], {}, {}
        for i, (n, t) in enumerate(STEPS):
            nr = (n - t) // 2
            rk = ['r%d_%d' % (i, j) for j in range(nr)]
            kids += rk
            steps.append(dict(name='R_%d' % i, kind='rule', reads=[(k, 'read', "1'd1") for k in rk],
                              calls=[(k, 'write', "1'd1", 'a_%d_%d' % (i, j)) for j, k in enumerate(rk)], checks=[], ooo=[]))
            for j, k in enumerate(rk):
                fan['a_%d_%d' % (i, j)] = frozenset([(k, 'read')])
            fan['CAN_FIRE_R_%d' % i] = frozenset([(rk[0], 'read')])
            rules['R_%d' % i] = (None, [('task', '$display', None, []) for _ in range(t)])
        self.kidorder = kids
        self.kids = {k: (REG[0], REG[1][1:-1], design.parse_sched(REG[1][1:-1])) for k in kids}
        self.ports = {k: {} for k in kids}
        self.steps, self.fan = steps, fan
        self.info = {'urg': [], 'disj': []}
        self.synth, self.bvi = set(), set()   # today's design.Design fields (cost review r8b-1 minor 3)
        self.A = StubA({n: ('id', n) for n in fan}, rules)

    def fanin_name(self, n):
        return self.fan.get(n, frozenset())

    def reads_of(self, e):
        return frozenset()


design.Design = FakeDesign
sys.argv = [os.path.join(edir, emit), 'dump', '', out, name]
# PROBE_GATE="NAME=VALUE,..." replaces the emitter's gate constants for a probe beyond a bound (the file states them)
src_ = os.path.join(edir, emit)
code_ = open(src_).read()
for kv_ in filter(None, os.environ.get('PROBE_GATE', '').split(',')):
    k_, v_ = kv_.split('=')
    code_, n_ = re.subn(r'^%s = \d+' % k_, '%s = %s' % (k_, v_), code_, count=1, flags=re.M)
    assert n_ == 1, k_
exec(compile(code_, src_, 'exec'), {'__name__': '__main__', '__file__': src_})

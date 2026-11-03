"""genport.py (r7cost) adapted to the round-7 emitter: one register m written by U rules, bsc's
suffix run-time checks C_k; a stub APackage so arguments and guards keep their read fan-in.
usage: E2E_SITES=0 E2E_TASKS=0 python3 -I genport8.py EMITTER_DIR U OUT.lean NAME [EMITTER [COND]]
EMITTER (default mke2e.py) names the emitter script; COND=1 gives every call a condition (a SELMUX mux)."""
import os
import re
import runpy
import sys

edir, U, out, name = sys.argv[1], int(sys.argv[2]), sys.argv[3], sys.argv[4]
EMIT = sys.argv[5] if len(sys.argv) > 5 else 'mke2e.py'
COND = len(sys.argv) > 6 and sys.argv[6] == '1'
sys.path.insert(0, edir)
import design  # noqa: E402

REG = ('RegN', '[read CF read, read SB write, write SBR write]')


class StubA:
    def __init__(self, defs):
        self.defs = defs
        self.meth_args, self.rules, self.ifc_vals, self.guarded, self.insts = {}, {}, {}, {}, {}


class FakeDesign:
    def __init__(self, dump, ba):
        kids = ['sel', 'm'] + ['s%d' % k for k in range(U)]
        self.kidorder = kids
        self.kids = {k: (REG[0], REG[1][1:-1], design.parse_sched(REG[1][1:-1])) for k in kids}
        self.ports = {k: {} for k in kids}
        self.steps = []
        self.fan = {}
        for k in range(U):
            self.steps.append(dict(name='R_%d' % k, kind='rule', reads=[('sel', 'read', "1'd1"), ('s%d' % k, 'read', "1'd1")],
                                   calls=[('m', 'write', ('c_%d' % k) if COND else "1'd1", 'a_%d' % k)], checks=[], ooo=[]))
            self.fan['a_%d' % k] = frozenset([('s%d' % k, 'read')])
            self.fan['c_%d' % k] = frozenset([('s%d' % k, 'read')])
            self.fan['CAN_FIRE_R_%d' % k] = frozenset([('sel', 'read')])
        for k in range(U - 1):
            ys = ['WILL_FIRE_R_%d' % y for y in range(k + 1, U)]
            chk = 'WILL_FIRE_R_%d && %s' % (k, ys[0] if len(ys) == 1 else '(%s)' % ' || '.join(ys))   # as bsc renders it
            self.steps.append(dict(name='C_%d' % k, kind='rule', reads=[], calls=[], checks=[chk], ooo=[]))
        self.info = {'urg': [], 'disj': [], 'me': [('R_%d' % k, 'R_%d' % y) for k in range(U - 1) for y in range(k + 1, U)]}
        self.synth, self.bvi = set(), set()   # today's design.Design fields (cost review r8b-1 minor 3)
        self.A = StubA({n: ('id', n) for n in self.fan})

    def fanin_name(self, n):
        return self.fan.get(n, frozenset())

    def reads_of(self, e):
        return frozenset()


design.Design = FakeDesign
sys.argv = [os.path.join(edir, EMIT), 'dump', '', out, name]
# PROBE_GATE="NAME=VALUE,..." replaces the emitter's gate constants for a probe beyond a bound (the file states them)
src_ = os.path.join(edir, EMIT)
code_ = open(src_).read()
for kv_ in filter(None, os.environ.get('PROBE_GATE', '').split(',')):
    k_, v_ = kv_.split('=')
    code_, n_ = re.subn(r'^%s = \d+' % k_, '%s = %s' % (k_, v_), code_, count=1, flags=re.M)
    assert n_ == 1, k_
exec(compile(code_, src_, 'exec'), {'__name__': '__main__', '__file__': src_})

"""genport.py (r7cost) adapted to the round-7 emitter: one register m written by U rules, bsc's
suffix run-time checks C_k; a stub APackage so arguments and guards keep their read fan-in.
usage: E2E_SITES=0 E2E_TASKS=0 python3 -I genport7.py EMITTER_DIR U OUT.lean NAME"""
import os
import runpy
import sys

edir, U, out, name = sys.argv[1], int(sys.argv[2]), sys.argv[3], sys.argv[4]
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
                                   calls=[('m', 'write', "1'd1", 'a_%d' % k)], checks=[], ooo=[]))
            self.fan['a_%d' % k] = frozenset([('s%d' % k, 'read')])
            self.fan['CAN_FIRE_R_%d' % k] = frozenset([('sel', 'read')])
        for k in range(U - 1):
            chk = 'WILL_FIRE_R_%d && (%s)' % (k, ' || '.join('WILL_FIRE_R_%d' % y for y in range(k + 1, U)))
            self.steps.append(dict(name='C_%d' % k, kind='rule', reads=[], calls=[], checks=[chk], ooo=[]))
        self.info = {'urg': [], 'disj': []}
        self.A = StubA({n: ('id', n) for n in self.fan})

    def fanin_name(self, n):
        return self.fan.get(n, frozenset())

    def reads_of(self, e):
        return frozenset()


design.Design = FakeDesign
sys.argv = [os.path.join(edir, 'mke2e.py'), 'dump', '', out, name]
runpy.run_path(os.path.join(edir, 'mke2e.py'), run_name='__main__')

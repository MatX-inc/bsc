"""A synthetic module with one register written by U rules (a priority mux of U users, as
sysBRAM0Test's largest port), run through an e2e emitter (mke2e.py) in place of a corpus dump.

usage: E2E_TASKS=0 python3 genport.py EMITTER_DIR U OUT.lean NAME

Children: sel, m, s_0 .. s_{U-1} (registers). Rule R_k reads sel (its guard) and s_k, and writes
m with an argument over s_k. bsc's run-time checks C_k cover R_k with R_{k+1} .. R_{U-1} (suffix
lists, as its mutually_exclusive checks), so the mux's chain comes from the checks."""
import os
import runpy
import sys

edir, U, out, name = sys.argv[1], int(sys.argv[2]), sys.argv[3], sys.argv[4]
sys.path.insert(0, edir)
import design  # noqa: E402

REG = ('RegN', '[read CF read, read SB write, write SBR write]')


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
        self.A = None

    def fanin_name(self, n):
        return self.fan.get(n, frozenset())

    def reads_of(self, e):
        return frozenset()


design.Design = FakeDesign
sys.argv = [os.path.join(edir, 'mke2e.py'), 'dump', '', out, name]
runpy.run_path(os.path.join(edir, 'mke2e.py'), run_name='__main__')

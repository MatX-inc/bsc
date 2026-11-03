"""Generate a synthetic scan instance and its checkpointed kernel check.

genbench.py USES STEPS SEGSTEPS OUT.lean [SEED]

Children: registers (most uses), FIFO2s (one or two enqueuers, the two in an
exception group), CReg3s, RWires.  Methods: value methods reading registers,
action methods writing registers rules read (so hits and h3 are exercised).
A few rule pairs fail statically and are escaped.  The Python mirror runs the
pass, asserts it succeeds, and writes a checkpoint every SEGSTEPS steps.
"""
import random
import sys
import os

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import scanlib as S


def gen(U, NSTEPS, seed=1):
    rnd = random.Random(seed)
    nM = 10
    nR = NSTEPS - nM
    kinds = [S.creg(1), S.fifo2(), S.creg(3), S.rwire()]
    kt = [[k.kc(c) for c in range(len(k.ports))] for k in kinds]
    REG, FIFO, CREG, WIRE = 0, 1, 2, 3
    base = 0
    children = []

    def child(kd):
        nonlocal base
        children.append((base, kd))
        b = base
        base += len(kinds[kd].ports)
        return b

    fpR = [[] for _ in range(nR)]
    fpM = [[] for _ in range(nM)]
    exR = [0] * nR
    escR = [0] * nR
    per_step = max(2, U // NSTEPS)
    # FIFOs, CRegs, wires: about a tenth of the uses
    nF = max(1, U // 60)
    for _ in range(nF):
        b = child(FIFO)
        k = kinds[FIFO]
        e1, dq = rnd.sample(range(nR), 2)
        fpR[e1] += [(b, FIFO, k.code('i_notFull')), (b, FIFO, k.code('enq'))]
        fpR[dq] += [(b, FIFO, k.code('i_notEmpty')), (b, FIFO, k.code('first')), (b, FIFO, k.code('deq'))]
        if rnd.random() < 0.3:
            e2 = rnd.choice([r for r in range(nR) if r not in (e1, dq)])
            fpR[e2] += [(b, FIFO, k.code('i_notFull')), (b, FIFO, k.code('enq'))]
            exR[e1] |= 1 << e2
            exR[e2] |= 1 << e1
    nC = max(1, U // 200)
    for _ in range(nC):
        b = child(CREG)
        rs = sorted(rnd.sample(range(nR), 3))
        for port, r in enumerate(rs):
            fpR[r] += [(b, CREG, 2 * port), (b, CREG, 2 * port + 1)]
    nW = max(1, U // 200)
    for _ in range(nW):
        b = child(WIRE)
        a = rnd.randrange(nR - 3)
        fpR[a] += [(b, WIRE, 0)]
        for r in rnd.sample(range(a + 1, nR), 2):
            fpR[r] += [(b, WIRE, 1), (b, WIRE, 2)]
    # registers: readers ranked before writers
    used = sum(len(f) for f in fpR)
    nReg = max(4, (U - used) // 6)
    regs = []
    for _ in range(nReg):
        b = child(REG)
        regs.append((b, rnd.randrange(nR + 1)))
    for r in range(nR):
        want = max(0, per_step - len(fpR[r]))
        seen = set(e[0] for e in fpR[r])
        for (b, s) in rnd.sample(regs, min(len(regs), want * 2)):
            if want <= 0:
                break
            if b in seen:
                continue
            if s > r:
                fpR[r].append((b, REG, 0))
            elif s < r:
                fpR[r].append((b, REG, 1))
            else:
                fpR[r] += [(b, REG, 0), (b, REG, 1)]
            seen.add(b)
            want -= 1
    # methods: value methods read registers, action methods write registers rules read
    for m in range(nM):
        for (b, s) in rnd.sample(regs, min(len(regs), per_step)):
            if m < 6:
                fpM[m].append((b, REG, 0))
            elif s == nR + 0 or rnd.random() < 0.02:
                fpM[m].append((b, REG, 1))
            else:
                fpM[m].append((b, REG, 0))
        fpM[m].sort()
    # escapes: a few later readers of an earlier writer's register
    for _ in range(3):
        b = child(REG)
        r1, r2 = sorted(rnd.sample(range(nR), 2))
        fpR[r1].append((b, REG, 1))
        fpR[r2].append((b, REG, 0))
        escR[r2] |= 1 << r1
    allM = (1 << nM) - 1
    d = max(1, (base - 1).bit_length())
    D = S.Design(nM, nR, d, fpM, fpR, kt, [allM] * nM, [allM] * nM, [allM] * nM, [allM] * nM,
                 [allM] * nM, [0] * nM, exR, [0] * nR, escR)
    return D, len(children)


def main():
    U, NSTEPS, SEG, out = int(sys.argv[1]), int(sys.argv[2]), int(sys.argv[3]), sys.argv[4]
    seed = int(sys.argv[5]) if len(sys.argv) > 5 else 1
    D, nK = gen(U, NSTEPS, seed)
    if os.environ.get('SCAN_CODES', '1') == '1':
        S.compress(D)
    ok, states, info = S.run_scan(D)
    assert ok, info
    uses = sum(len(f) for f in D.fpM) + sum(len(f) for f in D.fpR)
    hit_rules = sum(1 for v in info.values() if v)
    n = D.nM + D.nR
    cuts = list(range(SEG, n, SEG))
    lines = ['import Scan', 'set_option maxRecDepth 100000', 'set_option maxHeartbeats 0',
             'namespace Bench', 'open Modular Scan Design', '',
             '-- uses %d, steps %d, children %d, slots %d, rules with hits %d' %
             (uses, n, nK, 2 ** D.d, hit_rules), '', S.emit_design(D), '']
    for j, c in enumerate(cuts):
        lines.append(S.emit_tree(states[c - 1], D.d, "cp%d" % (j + 1)))
    lines.append('')
    lines.append('set_option profiler true')
    prev, prevn = 0, 'ST.init'
    names = []
    for j, c in enumerate(cuts):
        nm = 'seg%d' % j
        lines.append('theorem %s : D.seg %d %d %s cp%d = true := by decide +kernel' % (nm, prev, c - prev, prevn, j + 1))
        names.append((c - prev, 'cp%d' % (j + 1), nm))
        prev, prevn = c, 'cp%d' % (j + 1)
    lines.append('theorem segZ : D.segLast %d %s = true := by decide +kernel' % (prev, prevn))
    lines.append('set_option profiler false')
    cs = '[' + ', '.join('(%d, %s)' % (m, cp) for (m, cp, _) in names) + ']'
    pf = '⟨' + ', '.join(nm for (_, _, nm) in names) + (', ' if names else '') + 'segZ⟩' if names else 'segZ'
    lines.append('theorem ok : D.Ok := D.ok_of_chain %s %s' % (cs, pf))
    lines.append('end Bench')
    lines.append('#print axioms Bench.ok')
    open(out, 'w').write('\n'.join(lines) + '\n')
    print('uses', uses, 'steps', n, 'children', nK, 'd', D.d, 'segments', len(cuts) + 1,
          'hit rules', hit_rules, 'bytes', os.path.getsize(out))


if __name__ == '__main__':
    main()

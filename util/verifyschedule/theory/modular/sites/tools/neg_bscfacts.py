"""Negative control for bsc's confirmed-fact keys (round 7 review s2 minor 3): the round-7 key
(later step, p, q) drops the first step; BscFacts keeps both steps and follows schedlean's chains.
usage: python3 -I neg_bscfacts.py"""
import os
import sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from sitecond import BscFacts, parse_fact_line  # noqa: E402

FACTS = [
    '  confirmed A / C: x.write then x.read',
    '  confirmed B / C: y.write (any of the 2 uses before) then y.read',
    '  confirmed B / C: z.sub (any of the 2 uses before) then z.upd',
    '  confirmed A / D: w.write then w.read',
    '  UNCONFIRMED A / D: w.write then w.read',
]


def dump():
    fs = [parse_fact_line(ln) for ln in FACTS]
    steps = [{'name': n, 'reads': [], 'calls': [], 'guard': None} for n in 'ABCDE']
    return {'steps': steps, 'facts': fs}


# (step1, use1, step2, use2) -> should a confirmed fact cover it?
CASES = [
    (('A', 'x.write', 'C', 'x.read'), True),
    (('C', 'x.read', 'A', 'x.write'), True),       # either orientation
    (('B', 'x.write', 'C', 'x.read'), False),      # the round-7 key confirms this one
    (('A', 'y.write', 'C', 'y.read'), True),       # a chain covers every earlier use
    (('B', 'y.write', 'C', 'y.read'), True),
    (('E', 'y.write', 'C', 'y.read'), False),      # a step after C is not in the chain
    (('A', 'z.sub[0]', 'C', 'z.upd'), True),       # inst.meth: any copy
    (('A', 'z.sub[1]', 'C', 'z.upd'), True),
    (('A', 'w.write', 'D', 'w.read'), False),      # also listed UNCONFIRMED
]

d = dump()
BF = BscFacts(d)
old = set((f['b'], f['p'], f['q']) for f in d['facts'] if f['ok'])
bad = 0
for (a, pa, b, pb), want in CASES:
    new = BF.covers(a, pa, b, pb) is not None
    e1, e2 = ((a, pa), (b, pb)) if 'ABCDE'.index(a) < 'ABCDE'.index(b) else ((b, pb), (a, pa))
    r7 = (e2[0], e1[1], e2[1]) in old
    ok = new == want
    bad += not ok
    print('%-30s want %-5s new %-5s round-7 key %-5s %s' % ('%s.%s / %s.%s' % (a, pa, b, pb), want, new, r7,
                                                             '' if ok else 'WRONG'))
print('new keys wrong:', bad)

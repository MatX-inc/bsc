"""Per readiness site: the ready literal, whether the guard or R reads the literal's alias port, and the
site-value verdict with all r7-review-s2 fixes and with each one switched off (attribution).
usage: python3 -I probe_rdy.py MODE DESIGN..."""
import os
import sys
HERE = os.path.dirname(os.path.abspath(__file__))
SITES = os.path.dirname(HERE)
mode = sys.argv[1]
os.environ['SITES_DATA'] = os.path.join(SITES, 'modes', mode, 'data')
sys.path.insert(0, os.path.join(SITES, 'tools'))
import sitecond as sc  # noqa: E402
import satc  # noqa: E402

work = os.path.join(HERE, 'tmp_probe_' + mode)
os.makedirs(work, exist_ok=True)
EQ = satc.EncS.eqatom


def verdict(ck, st, s):
    r = ck(st, s)
    return (r['rdy'], r['inv'])


for d in sys.argv[2:]:
    res = sc.analyse(os.path.join(SITES, 'modes', mode, d), work, work)
    ctx = res['_ctx']
    D, DX, RCb, fsb, linfo = ctx['D'], ctx['DX'], ctx['RCb'], ctx['fsb'], ctx['linfo']
    ck = ctx['rdy_site_check']
    for st, s in ctx['need']:
        if 'r' not in s['need']:
            continue
        base = ck(st, s)
        lit = base['lit']
        alias_ports = set(p for p in s['rdy'] if p != lit)
        g = sc.parse_expr if False else None
        greads = D.expr_reads(ctx['ap'].rules[st['name']][0]) if st['name'] in ctx['ap'].rules else set()
        rreads = set()
        for key, pol in fsb.lits(s['reach_b']):
            rreads |= linfo(RCb, key)[0]
        out = {'all': verdict(ck, st, s)}
        for k in ('exact', 'one_lit', 'hw_drop'):
            sc.RDY_FIX[k] = False
            try:
                out['no_' + k] = verdict(ck, st, s)
            finally:
                sc.RDY_FIX[k] = True
        del satc.EncS.eqatom
        try:
            out['no_eq_sv'] = verdict(ck, st, s)
        finally:
            satc.EncS.eqatom = EQ
        same = len(set(out.values())) == 1
        print('%s %s | %s / %s | lit %s | alias ports %s read by guard %s, by R %s | hw lits %d | %s%s' % (
            mode, d, st['name'], s['label'], lit, sorted(alias_ports), bool(greads & alias_ports),
            bool(rreads & alias_ports), base['hw'], out, '' if same else '  <-- A FIX CHANGES THIS VERDICT'))
    # the exact view keeps aliases apart
    for k, kd in res['_ctx']['D'].dump['kids'].items() if hasattr(res['_ctx']['D'], 'dump') else []:
        pass
    ex = [(k, m) for (k, m) in D.alias][:1]
    for (k, m) in ex:
        print('  alias check %s.%s: D %s, DX %s' % (k, m, D.canon(('call', k, m, [])), DX.canon(('call', k, m, []))))

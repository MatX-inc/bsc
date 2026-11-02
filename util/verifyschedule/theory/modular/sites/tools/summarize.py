"""Tabulate sitecond.py's per-design files.

usage: python3 -I summarize.py OUTDIR...   (prints the results table)
"""
import ast
import os
import sys


def load(path):
    res, extra = {}, []
    for ln in open(path).read().split('\n'):
        if ln.startswith('  ') and ': ' in ln and not ln.startswith('   '):
            k, v = ln.strip().split(': ', 1)
            try:
                res[k] = ast.literal_eval(v)
            except Exception:
                res[k] = v
        elif ln.startswith(('FACT AT RISK', 'READY ', 'RERANK', 'BSC FACT', 'CYCLE', 'all_CYCLE', 'DROP', 'HARD')):
            extra.append(ln)
    return res, extra


def main():
    rows = []
    for d in sys.argv[1:]:
        for f in sorted(os.listdir(d)):
            if f.endswith('.txt') and not f.startswith(('list', 'run')):
                r, extra = load(os.path.join(d, f))
                if 'design' in r:
                    rows.append((r, extra))
    hdr = ('design', 'sites', 'reads', 'cond', 'a', 'b', 'c', 'r', 'exHW', 'cyc', 'drop', 'b>R', 'b=R', 'b<R',
           'facts ok/conf/risk', 'rdy ok/all', 'rerank', 'all: true/reads cyc drop')
    print('%-26s %6s %6s %5s %4s %2s %2s %3s %4s %4s %4s %4s %4s %4s %18s %10s %7s %s' % hdr)
    tot = {}
    for r, _ in rows:
        nb = r['need_by']
        cm = r['cmp_d']
        fc = r['fact_class']
        al = r.get('all', {})
        rr = r['rdy_ok']
        vals = dict(sites=r['sites'], reads=r['reads'], cond=r['need'], a=nb['a'], b=nb['b'], c=nb['c'], r=nb['r'],
                    exhw=r['exact_hw'], cyc=r['cyc_steps'], drop=r['dropped'] + r['dropped_hw'],
                    bs=cm['bsc_stronger'], be=cm['eq'], bw=cm['bsc_weaker'] + cm['incomparable'],
                    fok=fc['abstract_unsat'], fcf=fc['same_as_confirmed'], frk=fc['at_risk'],
                    rok=rr['used'], rall=nb['r'], rer=r['rerank_reads'],
                    at=al.get('reach_true', 0), ar=al.get('reads', 0), ac=al.get('cyc_steps', 0),
                    ad=al.get('dropped', 0) + al.get('dropped_hw', 0))
        for k, v in vals.items():
            tot[k] = tot.get(k, 0) + v
        print('%-26s %6d %6d %5d %4d %2d %2d %3d %4d %4d %4d %4d %4d %4d %18s %10s %7d %s' % (
            r['design'][:26], vals['sites'], vals['reads'], vals['cond'], vals['a'], vals['b'], vals['c'], vals['r'],
            vals['exhw'], vals['cyc'], vals['drop'], vals['bs'], vals['be'], vals['bw'],
            '%d/%d/%d' % (vals['fok'], vals['fcf'], vals['frk']), '%d/%d' % (vals['rok'], vals['rall']),
            vals['rer'], '%d/%d %d %d' % (vals['at'], vals['ar'], vals['ac'], vals['ad'])))
    t = tot
    print('%-26s %6d %6d %5d %4d %2d %2d %3d %4d %4d %4d %4d %4d %4d %18s %10s %7d %s' % (
        'TOTAL', t['sites'], t['reads'], t['cond'], t['a'], t['b'], t['c'], t['r'], t['exhw'], t['cyc'], t['drop'],
        t['bs'], t['be'], t['bw'], '%d/%d/%d' % (t['fok'], t['fcf'], t['frk']), '%d/%d' % (t['rok'], t['rall']),
        t['rer'], '%d/%d %d %d' % (t['at'], t['ar'], t['ac'], t['ad'])))
    print()
    print('bsc use-level facts against the new model (confirmed facts only):')
    agg = {}
    for r, _ in rows:
        for k, v in r['fclass'].items():
            agg[k] = agg.get(k, 0) + v
        if any(r['fclass'][k] for k in ('same', 'reask_holds', 'reask_at_risk')):
            print('  %-24s %s' % (r['design'], r['fclass']))
    print('  %-24s %s' % ('TOTAL', agg))
    print()
    print('Other per-design numbers: twoWay rule pairs, action-action pairs failing the scan, shared copies')
    for r, _ in rows:
        if r['two_way'] or r['act_pairs'] or r['shared_copies'] or r['grp_fail'] or r['hard_cyc']:
            print('  %-24s twoWay %d actPairs %d shared %d (clash %d) grpFail %d hardCycles %d' % (
                r['design'], r['two_way'], r['act_pairs'], r['shared_copies'], r['shared_clash'], r['grp_fail'],
                r['hard_cyc']))
    print()
    print('Sites at risk, readiness verdicts, residual sites after re-ranking, stress-mode cycles:')
    for r, extra in rows:
        nrr = 0
        for x in extra:
            if x.startswith('RERANK'):
                nrr += 1
                if nrr > 6:
                    continue
            if x.startswith(('FACT AT RISK', 'READY ', 'RERANK', 'all_CYCLE', 'CYCLE', 'HARD')) and \
                    not x.startswith('READY not implied'):
                print('  %-24s %s' % (r['design'], x[:220]))
        if nrr > 6:
            print('  %-24s ... and %d more RERANK lines' % (r['design'], nrr - 6))


if __name__ == '__main__':
    main()

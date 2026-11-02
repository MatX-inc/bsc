"""Conditional sites from the site tool (sites/tools/sitecond.py), for the end-to-end emitter.

For each step: its conditional reads, each with its condition's kind and fan-in (the reads the
condition formula's literals read), and for readiness sites the fan-in of the short-circuit
reach R_seq (Ravi's rule, sequenced by expression structure) that the readiness fact is about."""
import os
import sys
HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, '..', '..', 'sites', 'tools'))
import sitecond  # noqa: E402


def lit_reads(ctx, RCx, fsx, f):
    D = ctx['D']
    out = set()
    for (key, pol) in fsx.lits(f):
        L = RCx.lit[key]
        out |= set(D.expr_reads(L[1]))
        if L[0] == 'eq':
            pass
    return out


def conditional_sites(ddir, workdir):
    os.makedirs(workdir, exist_ok=True)
    res = sitecond.analyse(ddir, workdir, workdir)
    ctx = res['_ctx']
    out = {}
    for st, s in ctx['need']:
        e = dict(label=s['label'], need=''.join(sorted(s['need'])), inst=s['inst'], meth=s['meth'], copy=s['copy'])
        if 'r' in s['need']:
            e['kind'] = 'rdy'
            e['rdy'] = s['rdy']
            # the verdict for the formula the file states: R over the site values of the step's
            # conditional reads (sites/tools/sitecond.py rdy_site_check); refined by the emitter
            # when it drops later-site literals
            e['sv'] = s['rdy_sv']
            e['ok'] = s['rdy_sv']['ok']
            e['check'] = (lambda drop, st=st, s=s: ctx['rdy_site_check'](st, s, frozenset(drop)))
            e['R_fan'] = sorted(lit_reads(ctx, ctx['RCb'], ctx['fsb'], s['reach_b']))
            e['R_true'] = s['reach_b'] == ctx['fsb'].T
        else:
            f = s.get('cond_used', s['reach'])
            e['kind'] = 'reach'
            e['true'] = f == ctx['fs'].T
            e['enc'] = (lambda E, f=f: ctx['enc_f'](E, f, {}))   # the condition's literal in an encoder
            e['fan'] = sorted(lit_reads(ctx, ctx['RC'], ctx['fs'], f))
            e['partners'] = [(st2['name'], s2['label'], s2['kind']) for st2, s2 in s['partners']]
        out.setdefault(st['name'], []).append(e)
    return out, res


if __name__ == '__main__':
    cs, res = conditional_sites(sys.argv[1], sys.argv[2])
    for k, v in cs.items():
        for e in v:
            print(k, e)

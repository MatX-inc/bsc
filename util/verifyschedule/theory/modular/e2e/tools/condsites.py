"""Conditional sites from the site tool (sites/tools/sitecond.py), for the end-to-end emitter.

For each step: its conditional reads, each with its condition's kind and fan-in (the reads the
condition formula's literals read), and for readiness sites the fan-in of the short-circuit
reach R_seq (Ravi's rule, sequenced by expression structure) that the readiness fact is about.

Round 7 review s2: a readiness site hands the emitter exactly what the tool checked -- its one
ready literal (`rdy`, a one-element list, so model.py's `lp[0]` is the tool's literal) and R_seq
with hardware-wire and input literals dropped (`R_fan`).  No literal of a condition or an R the
file states may read a hardware wire or an input: a file operator over the step's reads cannot."""
import os
import sys
HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, '..', '..', 'sites', 'tools'))
import sitecond  # noqa: E402


def lit_reads(ctx, RCx, fsx, f):
    """The reads of formula f's literals (both operands of an 'eq' literal)."""
    out = set()
    for (key, pol) in fsx.lits(f):
        out |= set(ctx['linfo'](RCx, key)[0])
    return out


def lit_hw(ctx, RCx, fsx, f):
    """The hardware wires and inputs formula f's literals read."""
    out = set()
    for (key, pol) in fsx.lits(f):
        out |= set(ctx['linfo'](RCx, key)[1])
    return out


def conditional_sites(ddir, workdir):
    os.makedirs(workdir, exist_ok=True)
    res = sitecond.analyse(ddir, workdir, workdir)
    ctx = res['_ctx']
    out = {}
    for st, s in ctx['need']:
        e = dict(label=s['label'], need=''.join(sorted(s['need'])), inst=s['inst'], meth=s['meth'], copy=s['copy'])
        if 'r' in s['need']:
            sv = s['rdy_sv']
            assert sv['lit'] is not None, 'readiness site %s / %s: its step reads no ready port' % (st['name'], s['label'])
            e['kind'] = 'rdy'
            # the one ready literal the tool's verdict is about (cond_y and the conclusion)
            e['rdy'] = [sv['lit']]
            # the verdict for the formula the file states: R over the site values of the step's
            # conditional reads (sites/tools/sitecond.py rdy_site_check); refined by the emitter
            # when it drops later-site literals
            e['sv'] = sv
            e['ok'] = sv['ok']
            e['check'] = (lambda drop, st=st, s=s: ctx['rdy_site_check'](st, s, frozenset(drop)))
            assert not lit_hw(ctx, ctx['RCb'], ctx['fsb'], sv['f']), 'an R literal reads a hardware wire'
            e['R_fan'] = sorted(lit_reads(ctx, ctx['RCb'], ctx['fsb'], sv['f']))
            e['R_true'] = sv['f'] == ctx['fsb'].T
        else:
            f = s.get('cond_used', s['reach'])
            e['kind'] = 'reach'
            e['true'] = f == ctx['fs'].T
            assert not lit_hw(ctx, ctx['RC'], ctx['fs'], f), \
                'the condition of %s / %s reads a hardware wire or an input' % (st['name'], s['label'])
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

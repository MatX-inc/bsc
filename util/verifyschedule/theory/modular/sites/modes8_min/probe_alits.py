"""Per design and mode: literals of the (a) conditions the file states (cond_used) and of every
readiness R that read a hardware wire or an input (expr_hw), which a file operator over the step's
reads cannot express.  usage: python3 -I probe_alits.py MODE DESIGN..."""
import os, sys
HERE = os.path.dirname(os.path.abspath(__file__)); SITES = os.path.dirname(HERE)
mode = sys.argv[1]
os.environ['SITES_DATA'] = os.path.join(SITES, 'modes', mode, 'data') if mode != 'corpus' else os.path.join(SITES, 'data')
sys.path.insert(0, os.path.join(SITES, 'tools'))
import sitecond as sc  # noqa
work = os.path.join(HERE, 'tmp_alits'); os.makedirs(work, exist_ok=True)
for d in sys.argv[2:]:
    dd = d if mode == 'corpus' else os.path.join(SITES, 'modes', mode, d)
    res = sc.analyse(dd, work, work)
    ctx = res['_ctx']; fs, RC, fsb, RCb, linfo = ctx['fs'], ctx['RC'], ctx['fsb'], ctx['RCb'], ctx['linfo']
    na = nah = nr = nrh = 0
    ex = []
    for st, s in ctx['need']:
        if 'r' in s['need']:
            nr += 1
            h = [k for k, p in fsb.lits(s['reach_b']) if linfo(RCb, k)[1]]
            nrh += len(h)
        else:
            f = s.get('cond_used', s['reach'])
            if f == fs.T:
                continue
            na += 1
            h = [k for k, p in fs.lits(f) if linfo(RC, k)[1]]
            nah += len(h)
            if h and len(ex) < 2:
                ex.append((st['name'], s['label'], [linfo(RC, k)[1] for k in h][:2]))
    print('%s %s: (a) conditions not true %d, their input/wire literals %d; readiness R %d, input/wire literals %d %s'
          % (mode, os.path.basename(d.rstrip('/')), na, nah, nr, nrh, ex))

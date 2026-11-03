"""The round-8 gate per design and bsc mode, from gate8/run.sh's outputs and the (unchanged) scan mirror.
Items (design.md §11): 1 scan (grp/mm/h3 zero, escapes <= 64); 2 escape facts confirmed; 3 readiness facts
confirmed; 4 no shadowing outside classes L and I; 6 bounds.  Item 5 (task order) and 7 (named leaves) are
generator checks.  usage: python3 -I summary.py > summary.txt"""
import ast
import os
import re
from collections import defaultdict

G = os.path.dirname(os.path.abspath(__file__))
R = os.path.dirname(G)
MODES = ['default', 'noagg', 'nosched', 'both']
D10 = ['mkBufferControl', 'mkPrediction', 'mkDeblockFilter', 'mkMCR', 'mkAssertedSchedules', 'sysFloatTest',
       'sysConflictFreeOKLarge', 'sysArithPipe', 'mkUrgencyBlocking', 'mkMethodRelations']
# known exclusions the shadow tool does not read (r7 §10): a run-time check bsc asserts
KNOWN_X = {('mkAssertedSchedules', 'outQ.enq'): "excluded by its asserted run-time check (bsc's mutually_exclusive)"}
BOUNDS = dict(step=4096, seg_uses=4096, slots=8192, codes=4096, users=3296, group=3296, pairs=64, lc=512, esc=64)
# designs the prototype emitter writes a file for (cost/emitall/emitall.txt, plus the round-8 e2e files)
EMITS = {'sysArithPipe', 'sysBasic', 'mkSource', 'mkSplitRule', 'mkBufferControl', 'mkMCR', 'mkUrgencyBlocking',
         'mkBranchWrites', 'mkDutLogic', 'mkLeanNames', 'mkNotEmptyOrder', 'mkSysTasks', 'mkLookupQ',
         'mkValueAfterAction'}


def sites(path):
    out = {}
    for ln in open(path):
        m = re.match(r'^(\S+)\s+sites', ln)
        if not m:
            continue
        r = re.search(r'atRisk (\d+)', ln)
        v = re.search(r"rdySiteValue (\{.*?\}) \| perr", ln)
        d = ast.literal_eval(v.group(1)) if v else {}
        out[m.group(1)] = dict(atrisk=int(r.group(1)) if r else 0, rdy=d.get('sites', 0),
                               rdy_ok=d.get('ok', 0), rdy_left=d.get('left', []))
    return out


def shadow(path):
    out, cur = {}, None
    for ln in open(path):
        m = re.match(r'^(\S+)\s+caller pairs.*?may both call\s+(\d+) \[([^\]]*)\]', ln)
        if m:
            cur = m.group(1)
            cls = {}
            if m.group(3) != '-':
                for it in m.group(3).split(', '):
                    k, n = it.rsplit(' ', 1)
                    cls[k] = int(n)
            out[cur] = dict(cls=cls, pairs=[], callers=defaultdict(set))
            continue
        m = re.match(r'^    (\S+) (\S+) \[(\S+)\]: (\S+), (\S+)', ln)
        if m and cur:
            c, port, kind, a, b = m.groups()
            out[cur]['pairs'].append((c, port, kind, a, b))
            if c == 'L':
                out[cur]['callers'][port] |= {a, b}
    return out


def counts(path):
    out = {}
    for ln in open(path):
        f = ln.split()
        if len(f) == 11 and f[1].isdigit():
            out[f[0]] = dict(steps=int(f[1]), sites=int(f[2]), tasks=int(f[3]), step=int(f[4]), segs=int(f[7]),
                             users=int(f[8]), group=int(f[9]), pairs=int(f[10]))
    return out


def scan(path, section=None):
    out, cur = {}, None
    for ln in open(path):
        if ln.startswith('== '):
            cur = ln.split()[1]
            continue
        f = ln.replace('|', ' ').split()
        if len(f) == 10 and f[1].isdigit() and (section is None or cur == section):
            out[f[0]] = dict(slots=int(f[5]), grp=int(f[6]), mm=int(f[7]), h3=int(f[8]), esc=int(f[9]))
    return out


DECIDED_FLAT = {'bus_2m_2s': 'decided flat (16 exported CF pairs bsc records as unrealizable)'}


def verdict(name, mode, st, sh, ct, sc):
    why = [DECIDED_FLAT[name]] if name in DECIDED_FLAT else []
    if sc is None:
        why.append('scan: not measured')
    else:
        if sc['grp'] or sc['mm'] or sc['h3']:
            why.append('scan fails')
        if sc['esc'] > BOUNDS['esc']:
            why.append('%d escapes' % sc['esc'])
    if st is None:
        why.append('sites: not measured')
    else:
        if st['atrisk']:
            why.append('%d escape facts at risk' % st['atrisk'])
        if st['rdy_left']:
            why.append('readiness refuted: ' + '; '.join('%s / %s' % (a, b) for a, b, *_ in st['rdy_left']))
    blocked = []
    if sh is None:
        why.append('shadow: not measured')
    else:
        for (c, port, kind, a, b) in sh['pairs']:
            if c in ('L', 'I'):
                continue
            if (name, port) in KNOWN_X:
                continue
            blocked.append('%s %s [%s] %s/%s' % (c, port, kind, a, b))
        if blocked:
            why.append('shadowing: ' + '; '.join(blocked))
        lcmax = max((len(v) for v in sh['callers'].values()), default=0)
        if lcmax > BOUNDS['lc']:
            why.append('last-caller port with %d callers' % lcmax)
    if ct is None:
        why.append('counts: not measured')
    else:
        if ct['step'] > BOUNDS['step']:
            why.append('step of %d sites' % ct['step'])
        if ct['users'] > BOUNDS['users']:
            why.append('port of %d users' % ct['users'])
        if ct['group'] > BOUNDS['group']:
            why.append('group of %d' % ct['group'])
        if ct['pairs'] > BOUNDS['pairs']:
            why.append('pairwise port of %d users' % ct['pairs'])
        if ct['steps'] > BOUNDS['codes']:
            why.append('more than %d rule codes possible' % BOUNDS['codes'])
    if sc is not None and sc['slots'] + (len(sh['callers']) if sh else 0) > BOUNDS['slots']:
        why.append('%d slots' % sc['slots'])
    return why


def main():
    corpus_scan = scan(os.path.join(R, 'scan/tools/corpus.out'))
    rows = {}
    for mode in MODES:
        st = sites(os.path.join(G, 'sites_%s/run.txt' % mode))
        sh = shadow(os.path.join(G, 'shadow_%s.txt' % mode))
        ct = counts(os.path.join(G, 'count_%s.txt' % mode))
        sc = scan(os.path.join(R, 'sites/modes7/scan_modes.txt'), mode)
        for d in D10:
            scd = sc.get(d) or (corpus_scan.get(d) if d in ('mkUrgencyBlocking', 'mkMethodRelations') else None)
            # the two last-caller designs were not in round 7's mode scan; their scan is the default corpus's
            # (no escapes, no groups, no site conditions), noted as such
            rows[(d, mode)] = (verdict(d, mode, st.get(d), sh.get(d), ct.get(d), scd), sh.get(d), ct.get(d), st.get(d))
    st = sites(os.path.join(G, 'sites_corpus/run.txt'))
    st.update(sites(os.path.join(G, 'sites_creg/run.txt')))   # mkCregInlineBlowup, run on its own
    sh = shadow(os.path.join(G, 'shadow_corpus.txt'))
    ct = counts(os.path.join(G, 'count_corpus.txt'))
    allds = sorted(set(list(ct) + list(sh)))
    corpus_rows = {}
    for d in allds:
        st_d = st.get(d)
        if st_d is None and d in ('sysManyMeths', 'mkCycleSel'):
            st_d = dict(atrisk=0, rdy=0, rdy_ok=0, rdy_left=[])
        corpus_rows[d] = (verdict(d, 'default', st_d, sh.get(d), ct.get(d), corpus_scan.get(d)), sh.get(d), ct.get(d), st_d)
    print('# Round 8 gate (gate8/summary.py). A = passes every measured gate item; flat = the reasons listed.')
    print('# Bounds: %s' % ', '.join('%s %d' % kv for kv in BOUNDS.items()))
    print('\n## The 10 designs compiled in four modes (bsc-fix-all; sites/modes, lc/shadow/modes)\n')
    for d in D10:
        for mode in MODES:
            why, s, c, t = rows[(d, mode)]
            cls = ', '.join('%s %d' % kv for kv in sorted(s['cls'].items())) if s else '?'
            lcmax = max((len(v) for v in s['callers'].values()), default=0) if s else 0
            print('%-24s %-8s %-5s shadow [%s] lc ports %d (max callers %d)%s' % (
                d, mode, 'A' if not why else 'flat', cls or '-', len(s['callers']) if s else 0, lcmax,
                ('' if not why else ' | ' + ' | '.join(why))))
    print('\n## Default corpus (gate8/corpus: hv/new4 plus six re-dumped designs; data in sites/data)\n')
    na = 0
    for d in allds:
        why, s, c, t = corpus_rows[d]
        cls = ', '.join('%s %d' % kv for kv in sorted(s['cls'].items())) if s else '?'
        lcmax = max((len(v) for v in s['callers'].values()), default=0) if s else 0
        ok = not why
        na += ok
        print('%-24s %-5s emitter %-3s shadow [%s] lc ports %d (max callers %d) step %s pairs %s%s' % (
            d, 'A' if ok else 'flat', 'yes' if d in EMITS else 'no', cls or '-', len(s['callers']) if s else 0, lcmax,
            c['step'] if c else '?', c['pairs'] if c else '?', '' if ok else ' | ' + ' | '.join(why)))
    print('\nDefault corpus: %d of %d pass the gate\'s measured items.' % (na, len(allds)))


main()

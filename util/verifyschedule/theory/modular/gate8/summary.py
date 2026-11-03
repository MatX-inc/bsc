"""The round-8 gate per design and bsc mode, from gate8/run9.sh's outputs (reviews r8-1, r8-2 and r8b-1 fixes).
Items (design.md §11): 1 the emitter's own scan (grp/mm/mmg/h3 pass, escapes <= 64; gatescan_*.txt, mke2e.py
under E2E_GATE=1); 2 every escaped pair's dynamic facts confirmed, computed by the emitter (escape_facts: the
solver per failing site pair, a keyed bsc fact, or a conflict_free assertion; gatescan; a fact on a footprint-only step
is not checked as a file would and fails it); 3 readiness facts
confirmed (site tool); 4 no shadowing port outside class L (shadow tool, per port, with the same assertions);
6 bounds: slots, rule codes, facts per escaped pair, facts per module (caller-exclusivity facts as an upper
bound), uses of the largest step in a scan segment and the pairwise bound as the emitter counts them (gatescan), callers of a last-caller port = every user of the port (gatescan and the shadow tool), the rest
from gatecount.  Items 5 and 7 are generator checks.  User assertions (bsc's mutually_exclusive and
conflict_free run-time checks) are hypotheses of the module's theorem; the designs and pairs relying on one
are listed.  usage: python3 -I summary.py > summary.txt"""
import ast
import os
import re
from collections import defaultdict

G = os.path.dirname(os.path.abspath(__file__))
R = os.path.dirname(G)
MODES = ['default', 'noagg', 'nosched', 'both']
D10 = ['mkBufferControl', 'mkPrediction', 'mkDeblockFilter', 'mkMCR', 'mkAssertedSchedules', 'sysFloatTest',
       'sysConflictFreeOKLarge', 'sysArithPipe', 'mkUrgencyBlocking', 'mkMethodRelations']
BOUNDS = dict(step=4096, seg_uses=4096, slots=8192, codes=4096, users=3296, group=3296, pairs=64, lc=512, esc=64,
              esc_facts=4096, mod_facts=16384)
# designs the merged emitter writes a file for (gate8/emitall/emitall.txt, rerun after review r8-1)
EMITS = set(ln.split(' | ')[0] for ln in open(os.path.join(G, 'emitall/emitall.txt')) if ' | wrote ' in ln)


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
        m = re.match(r'^(\S+)\s+ports with two callers.*?shadowing ports\s+(\d+) \[([^\]]*)\]', ln)
        if m:
            cur = m.group(1)
            cls = {}
            if m.group(3) != '-':
                for it in m.group(3).split(', '):
                    k, n = it.rsplit(' ', 1)
                    cls[k] = int(n)
            r = re.search(r'relying on mutually_exclusive (\d+), conflict_free (\d+)', ln)
            out[cur] = dict(cls=cls, ports=[], callers={}, rely=(int(r.group(1)), int(r.group(2))) if r else None)
            continue
        m = re.match(r'^    (\S+) (\S+) \[(\S+)\](?: callers (\d+))?[^:]*: (.*)$', ln)
        if m and cur:
            c, port, kind, n, rest = m.groups()
            out[cur]['ports'].append((c, port, kind, rest))
            if c == 'L':
                out[cur]['callers'][port] = int(n)
    return out


def gscan(path):
    import json
    out = {}
    for ln in open(path):
        if ln.startswith('GATE '):
            g = json.loads(ln[5:])
            out[g['name']] = g
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


def verdict(name, mode, st, sh, ct, gs):
    why = [DECIDED_FLAT[name]] if name in DECIDED_FLAT else []
    if gs is None:
        why.append('emitter scan: not run')
    elif 'error' in gs:
        why.append('emitter scan not computed (%s)' % gs['error'])
    else:
        if gs['scan'] != 'ok':
            why.append('emitter scan fails: %s' % gs['scan'])
        elif gs['escapes'] > BOUNDS['esc']:
            why.append('%d escaped pairs' % gs['escapes'])
        elif gs.get('esc_facts_max_per_pair', 0) > BOUNDS['esc_facts']:
            why.append('%d dynamic facts in one escaped pair' % gs['esc_facts_max_per_pair'])
        elif gs['escapes'] and (gs.get('esc_facts') is None or gs.get('esc_facts_err')):
            why.append('escape facts not computed (%s)' % gs.get('esc_facts_err'))
        else:
            nu = (gs.get('esc_facts_how') or {}).get('null', 0)
            if nu:
                why.append('%d escape facts unconfirmed (%s)' % (nu, '; '.join(gs['esc_facts_unconfirmed'][:3])))
            if gs.get('esc_facts_fallback'):
                # a step the site builder could not build: its facts are not the ones a file would state
                why.append('%d escape facts on a footprint-only step (not checked as a file would)'
                           % gs['esc_facts_fallback'])
        if gs.get('seg_uses') is not None and gs['seg_uses'] > BOUNDS['seg_uses']:
            why.append('a step of %d uses in one scan segment' % gs['seg_uses'])
        if gs.get('mod_facts') is not None and gs['mod_facts'] > BOUNDS['mod_facts']:
            why.append('%d facts in the module' % gs['mod_facts'])
        if gs.get('pairwise', 0) > BOUNDS['pairs']:
            why.append('pairwise port of %d users' % gs['pairwise'])
        if gs['slots'] > BOUNDS['slots']:
            why.append('%d slots' % gs['slots'])
        if gs['codes'] is not None and gs['codes'] > BOUNDS['codes']:
            why.append('%d rule codes' % gs['codes'])
        lcg = max((n for _, n in gs['lc']), default=0)
        if lcg > BOUNDS['lc']:
            why.append('last-caller port with %d callers' % lcg)
    if st is None:
        why.append('sites: not measured')
    else:
        if st['rdy_left']:
            why.append('readiness refuted: ' + '; '.join('%s / %s' % (a, b) for a, b, *_ in st['rdy_left']))
    if sh is None:
        why.append('shadow: not measured')
    else:
        blocked = ['%s %s [%s] %s' % (c, port, kind, rest) for (c, port, kind, rest) in sh['ports'] if c != 'L']
        if blocked:
            why.append('shadowing: ' + '; '.join(blocked))
        lcmax = max(sh['callers'].values(), default=0)
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
    return why


def rely(gs, sh):
    """User assertions relied on: (scan exclusions from mutually_exclusive, escape facts from conflict_free,
    shadow caller pairs from mutually_exclusive, from conflict_free)."""
    a = gs.get('me_needed_pairs', 0) if gs and 'error' not in gs else 0
    b = (gs.get('esc_facts_how') or {}).get('assert', 0) if gs and 'error' not in gs else 0
    c, d = (sh.get('rely') or (0, 0)) if sh else (0, 0)
    return (a, b, c, d)


def row(d, why, s, c, g, emits=None):
    cls = ', '.join('%s %d' % kv for kv in sorted(s['cls'].items())) if s else '?'
    lcmax = max(s['callers'].values(), default=0) if s else 0
    gx = '' if g is None or 'error' in g else ' slots %d codes %s esc %s facts %s (%s) pairwise %s' % (
        g['slots'], g['codes'], g['escapes'], g.get('esc_facts'),
        ', '.join('%s %d' % (k, v) for k, v in (g.get('esc_facts_how') or {}).items() if v) or '-', g.get('pairwise'))
    if g is not None and 'error' not in g:
        gx += ' seg_uses %s mod_facts %s' % (g.get('seg_uses'), g.get('mod_facts'))
    ra = rely(g, s)
    if any(ra):
        gx += ' assertions relied on: scan %d me, escape facts %d cf, shadow %d me %d cf' % ra
    if g and g.get('generic_class'):
        gx += ' leaves %s' % ', '.join('%s:%s' % kv for kv in sorted(g['generic_class'].items()))
    em = '' if emits is None else ' emitter %-3s' % ('yes' if emits else 'no')
    return '%-24s %-5s%s shadow ports [%s] lc callers max %d%s%s' % (
        d, 'A' if not why else 'flat', em, cls or '-', lcmax, gx, '' if not why else ' | ' + ' | '.join(why))


def main():
    rows = {}
    for mode in MODES:
        st = sites(os.path.join(G, 'sites_%s/run.txt' % mode))
        sh = shadow(os.path.join(G, 'shadow_%s.txt' % mode))
        ct = counts(os.path.join(G, 'count_%s.txt' % mode))
        gs = gscan(os.path.join(G, 'gatescan_%s.txt' % mode))
        for d in D10:
            rows[(d, mode)] = (verdict(d, mode, st.get(d), sh.get(d), ct.get(d), gs.get(d)), sh.get(d), ct.get(d), gs.get(d))
    st = sites(os.path.join(G, 'sites_corpus/run.txt'))
    st.update(sites(os.path.join(G, 'sites_creg/run.txt')))   # mkCregInlineBlowup, run on its own
    sh = shadow(os.path.join(G, 'shadow_corpus.txt'))
    ct = counts(os.path.join(G, 'count_corpus.txt'))
    gs = gscan(os.path.join(G, 'gatescan_corpus.txt'))
    allds = sorted(set(list(ct) + list(sh)))
    corpus_rows = {}
    for d in allds:
        st_d = st.get(d)
        if st_d is None and d in ('sysManyMeths', 'mkCycleSel'):
            st_d = dict(atrisk=0, rdy=0, rdy_ok=0, rdy_left=[])
        corpus_rows[d] = (verdict(d, 'default', st_d, sh.get(d), ct.get(d), gs.get(d)), sh.get(d), ct.get(d), gs.get(d))
    print('# Round 8 gate after review r8b-1 (gate8/summary.py). A = passes every gate item; flat = the reasons.')
    print('# Bounds: %s' % ', '.join('%s %d' % kv for kv in BOUNDS.items()))
    print('# Scan, slots, rule codes, escapes, escape facts, pairwise: the merged emitter\'s own model (gatescan_*.txt);'
          ' shadow: per port; the site tool\'s facts at risk are a cross-check only (xcheck).')
    print('\n## The 10 designs compiled in four modes (bsc-fix-all; sites/modes, lc/shadow/modes)\n')
    for d in D10:
        for mode in MODES:
            why, s, c, g = rows[(d, mode)]
            print(row('%-24s %-8s' % (d, mode), why, s, c, g))
    print('\n## Default corpus (gate8/corpus: hv/new4 plus six re-dumped designs; data in sites/data)\n')
    na = 0
    for d in allds:
        why, s, c, g = corpus_rows[d]
        na += not why
        print(row(d, why, s, c, g, d in EMITS) + (' step %s' % c['step'] if c else ''))
    print('\nDefault corpus: %d of %d pass the gate.' % (na, len(allds)))
    for mode in MODES:
        print('%s (10 designs in four modes): %d of 10' % (mode, sum(1 for d in D10 if not rows[(d, mode)][0])))
    # user assertions relied on, per design (any mode), and the site tool's cross-check
    rel = {}
    for (d, mode), (why, s, c, g) in list(rows.items()) + [((d, 'corpus'), v) for d, v in corpus_rows.items()]:
        ra = rely(g, s)
        if any(ra):
            rel.setdefault(d, []).append('%s: scan %d me, escape facts %d cf, shadow %d me %d cf%s' % (
                (mode,) + ra + ('' if not why else ' (flat)',)))
    print('\nUser assertions relied on (bsc\'s mutually_exclusive / conflict_free checks, hypotheses of the theorem): '
          '%d designs' % len(rel))
    for d in sorted(rel):
        print('  %-24s %s' % (d, '; '.join(rel[d])))
    print('\nCross-check (not a gate item): the site tool\'s escape facts at risk against the emitter\'s unconfirmed facts')
    for mode in MODES + ['corpus']:
        stp = os.path.join(G, 'sites_%s/run.txt' % mode)
        st = sites(stp)
        gsm = gscan(os.path.join(G, 'gatescan_%s.txt' % mode))
        for d in sorted(st):
            g = gsm.get(d)
            nu = (g.get('esc_facts_how') or {}).get('null', 0) if g and 'error' not in g else None
            if st[d]['atrisk'] or nu:
                print('  %-8s %-24s site tool at risk %d, emitter unconfirmed %s' % (mode, d, st[d]['atrisk'], nu))


main()

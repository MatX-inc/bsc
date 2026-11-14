# usage: python3 -I naming.py <label> <libsrc> <sidedir> <outjson> <root>...
# Naming metric.  The measurement build of bsc (scripts/instrument.py) writes, for
# every heap def iExpand creates, the def name (<binder>__h<N>) and the position of
# the binder that named the heap cell.  For every generated netlist under the roots
# (Verilog .v; Bluesim .cxx/.h, whose names carry a DEF_ prefix) this counts the
# distinct signal names that contain a heap-cell name, and splits them by where the
# binder was written: a library source (position in a bsc library, the "stdlib" flag)
# or the design.  A name is "direct" when it is exactly <binder>__h<N> (Bluesim:
# DEF_<binder>__h<N>), "derived" when it contains one or more such names (e.g.
# x__h12_EQ_y__h13___d5).  Library binders are attributed to the library line that
# binds them (file:line and the enclosing top-level definition or instance).
import os, re, sys, json, glob
from collections import defaultdict, Counter
label, LIB, SIDE, OUT = sys.argv[1:5]; ROOTS = sys.argv[5:]
# --- side records: (cwd, module) -> {N: [(base, origfile, origline, origcol, stdlib, nambig)]}
side = defaultdict(lambda: defaultdict(list))
# env CWDMAP="from=to;from=to": rewrite the recorded cwd (the measurement run ran in
# another tree); env MODONLY=1: key by module name alone (closure replays, where the
# compile cwd is not the netlist directory)
CWDMAP = [tuple(x.split('=', 1)) for x in os.environ.get('CWDMAP', '').split(';') if '=' in x]
MODONLY = os.environ.get('MODONLY') == '1'
nrec = 0
for f in glob.glob(os.path.join(SIDE, '*.tsv')):
    for l in open(f, errors='replace'):
        p = l.rstrip('\n').split('\t')
        if len(p) != 12: continue
        cwd, mod, name, pf, pl, pc, std, of, ol, oc, ostd, namb = p
        mod = re.sub(r'[^A-Za-z0-9_$]', '', mod)
        for a, b in CWDMAP: cwd = cwd.replace(a, b)
        if MODONLY: cwd = '*'
        m = re.match(r'^(.*)__h(\d+)$', name)
        if not m: continue
        # the binder's original position (before ISimplify / FixupDefs moved it to
        # the use site); namb > 1 when the move table had several originals
        rec = (m.group(1), of, int(ol), int(oc), ostd == 'True', int(namb))
        lst = side[(cwd, mod)][int(m.group(2))]
        if rec not in lst: lst.append(rec); nrec += 1
# --- library sources, for labels
libfiles = {}
for dp, dn, fn in os.walk(LIB):
    for f in fn:
        if f.endswith('.bs') or f.endswith('.bsv'): libfiles.setdefault(f, os.path.join(dp, f))
srccache = {}
def src(fname):
    b = os.path.basename(fname)
    if b not in srccache:
        p = libfiles.get(b)
        srccache[b] = open(p, errors='replace').read().split('\n') if p else None
    return srccache[b]
TOPDEF = re.compile(r'^(instance|class|interface|module|function|typeclass|[A-Za-z_(][^ ]*\s*(::|[^=]*=))')
def deflabel(fname, line):
    ls = src(fname)
    b = os.path.basename(fname)
    if not ls or line < 1 or line > len(ls): return '%s:%d' % (b, line), '?'
    text = ls[line - 1].strip()
    # enclosing top-level item: nearest line at or above with no indentation that is
    # not a comment, a blank line or a type signature continuation
    top = '?'
    for i in range(line - 1, -1, -1):
        t = ls[i]
        if t and not t[0].isspace() and not t.startswith('--') and not t.startswith('//') and not t.startswith('{-') and not t.startswith('*') and not t.startswith('/*'):
            top = t.strip()[:90]; break
    return '%s:%d: %s' % (b, line, text[:110]), top
bkcache = {}
def bkind(fname, line, col, base):
    # what the library binder is: 'param' (a parameter of a top-level, instance,
    # local or lambda definition), 'local' (a let / <- / where binding), or
    # 'unwritten' (the name is not written at that position: instance-path
    # prefixes, compiler-generated binders)
    key = (fname, line, col, base)
    if key in bkcache: return bkcache[key]
    ls = src(fname); r = 'unwritten'
    if ls and 1 <= line <= len(ls):
        L = ls[line - 1]
        occ = [m.start() for m in re.finditer(r"(?<![A-Za-z0-9_'])" + re.escape(base) + r"(?![A-Za-z0-9_'])", L)]
        if occ:
            i = min(occ, key=lambda o: abs(o - (col - 1)))
            if abs(i - (col - 1)) <= 3:
                before, after = L[:i], L[i + len(base):]
                if fname.endswith('.bs'):
                    b2 = re.sub(r'^\s*let\s+', '', before)
                    if re.search(r"\\\s*([A-Za-z_][\w']*\s+|\([^)]*\)\s+)*$", before): r = 'param'
                    elif ('=' in after and '=' not in before and '<-' not in before and '::' not in before
                          and b2.strip() != '' and not re.search(r'\b(where|case|of|if|then|else)\b', before)): r = 'param'
                    else: r = 'local'
                else:
                    if re.search(r'\b(function|method|module)\b', before) and before.count('(') > before.count(')'): r = 'param'
                    elif re.search(r'\\\s*$', before): r = 'param'
                    else: r = 'local'
    bkcache[key] = r
    return r
IDENT = re.compile(r'[A-Za-z_][A-Za-z0-9_$]*__h\d+[A-Za-z0-9_$]*')
HN = re.compile(r'__h(\d+)(?![0-9])')
def is_generated(path, head):
    return 'Bluespec Compiler' in head or 'Generated by' in head
tot = Counter(); bybinder = Counter(); bydef = Counter(); deftop = {}; bybinder_der = Counter()
userbinder = Counter(); perroot = defaultdict(Counter); unmatched_ex = []
files = []
fuseidx = {}
perfile = Counter()
seen = set()
for root in ROOTS:
    for dp, dn, fn in os.walk(root):
        for f in fn:
            if f.endswith('.expected') or '.expected' in f: continue
            if f.endswith('.v'): kind = 'verilog'
            elif (f.endswith('.cxx') or f.endswith('.h')) and not f.startswith('model_'): kind = 'bluesim'
            else: continue
            files.append((root, dp, f, kind))
for root, dp, f, kind in files:
    path = os.path.join(dp, f)
    try: txt = open(path, errors='replace').read()
    except Exception: continue
    if not is_generated(path, txt[:2000]): continue
    if kind == 'verilog':
        m = re.search(r'^\s*module\s+([A-Za-z_][A-Za-z0-9_$]*)', txt, re.M)
        if not m: continue
        mod = m.group(1)
    else:
        mod = os.path.splitext(f)[0]
    tot[kind + ' files'] += 1
    recs = side.get(('*' if MODONLY else dp, mod))
    names = set(IDENT.findall(txt))
    # derived names (CASE_..__q, IF_..___d, ...) embed a heap name with its __h
    # dropped: CASE_x3368_0_data_..__q13 holds x__h3368.  Their _-separated tokens
    # of the form <base><N> are matched against the module's heap defs below.
    fused = set()
    if recs:
        # bsc keeps at most the last 4 digits of the heap number in a fused name
        if id(recs) not in fuseidx:
            ix = defaultdict(list)
            for N, rl in recs.items():
                for r in rl: ix[(r[0], str(N)[-4:])].append(r)
            fuseidx[id(recs)] = ix
        ix = fuseidx[id(recs)]
        for n in set(re.findall(r'[A-Za-z_][A-Za-z0-9_$]*(?:__q\d+|___d\d+)', txt)):
            if n in names: continue
            # a Bluesim module's .h and .cxx share names: count each name once per module
            if (kind, dp, mod, n) in seen: continue
            seen.add((kind, dp, mod, n))
            hits = []
            core = re.sub(r'(__q\d+|___d\d+)$', '', n)
            for tok in core.split('_'):
                m = re.match(r'^([A-Za-z][A-Za-z0-9]*?)(\d{1,4})$', tok)
                if not m: continue
                cand = ix.get((m.group(1), m.group(2)), [])
                if cand: hits.append(cand[0])
            if hits:
                fused.add(n)
                lib = [c for c in hits if c[4]]
                who = ('library-' + bkind(lib[0][1], lib[0][2], lib[0][3], lib[0][0])) if lib else 'design'
                tot['%s %s derived-fused' % (kind, who)] += 1
                if who == 'library-param': perfile[(kind, os.path.relpath(os.path.join(dp, mod), root), os.path.basename(root))] += 1
                perroot[root]['%s %s derived-fused' % (kind, who)] += 1
                if lib:
                    lab, top = deflabel(lib[0][1], lib[0][2])
                    bk = bkind(lib[0][1], lib[0][2], lib[0][3], lib[0][0]); lab = bk + ' ' + lab
                    bybinder_der[(kind, bk + ' ' + lib[0][0])] += 1
                    bydef[(kind + '-derived', lab)] += 1; deftop[lab] = top
    if not names: continue
    tot[kind + ' files with heap names'] += 1
    for n in names:
        if (kind, dp, mod, n) in seen: continue
        seen.add((kind, dp, mod, n))
        core = n[4:] if kind == 'bluesim' and n.startswith('DEF_') else n
        comps = []
        ok = True
        for hm in HN.finditer(core):
            N = int(hm.group(1)); end = hm.end()
            cand = recs.get(N, []) if recs else []
            pick = None
            for r in cand:
                s = r[0] + '__h' + str(N)
                if core[:end].endswith(s): 
                    if pick is None or len(r[0]) > len(pick[0]): pick = r
            if pick is None: ok = False
            else: comps.append(pick)
        if not comps:
            tot[kind + ' unmatched'] += 1
            if len(unmatched_ex) < 20: unmatched_ex.append(path + ' ' + n)
            continue
        direct = (len(comps) == 1 and core == comps[0][0] + '__h' + HN.search(core).group(1))
        lib = [c for c in comps if c[4]]
        kindd = 'direct' if direct else 'derived'
        who = 'library' if lib else 'design'
        if lib:
            who = 'library-' + bkind(lib[0][1], lib[0][2], lib[0][3], lib[0][0])
        tot['%s %s %s' % (kind, who, kindd)] += 1
        perroot[root]['%s %s %s' % (kind, who, kindd)] += 1
        if who == 'library-param': perfile[(kind, os.path.relpath(os.path.join(dp, mod), root), os.path.basename(root))] += 1
        if lib:
            c = lib[0]
            if c[5] > 1: tot[kind + ' library ambiguous-origin'] += 1
            lab, top = deflabel(c[1], c[2])
            bk = bkind(c[1], c[2], c[3], c[0])
            lab = bk + ' ' + lab
            if direct:
                bybinder[(kind, bk + ' ' + c[0])] += 1; bydef[(kind, lab)] += 1; deftop[lab] = top
            else:
                bybinder_der[(kind, bk + ' ' + c[0])] += 1
                bydef[(kind + '-derived', lab)] += 1; deftop[lab] = top
        elif direct:
            userbinder[(kind, comps[0][0])] += 1
res = {'label': label, 'side_records': nrec, 'totals': dict(tot),
       'by_binder_direct': sorted([[k[0], k[1], v] for k, v in bybinder.items()], key=lambda x: -x[2]),
       'by_binder_derived': sorted([[k[0], k[1], v] for k, v in bybinder_der.items()], key=lambda x: -x[2]),
       'by_def': sorted([[k[0], k[1], deftop.get(k[1], '?'), v] for k, v in bydef.items()], key=lambda x: -x[3]),
       'design_binder_direct_top': sorted([[k[0], k[1], v] for k, v in userbinder.items()], key=lambda x: -x[2])[:40],
       'per_root': {r: dict(c) for r, c in perroot.items()},
       'per_file_library_param': sorted([[k[0], k[2], k[1], v] for k, v in perfile.items()], key=lambda x: -x[3]), 'unmatched_examples': unmatched_ex}
json.dump(res, open(OUT, 'w'), indent=1)
print(label, json.dumps(res['totals'], sort_keys=True))

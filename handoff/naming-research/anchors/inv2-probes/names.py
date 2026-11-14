# usage: python3 -I names.py <outdir> <module> <base-variant> <variant>...
# Lists the signal names of <module> in each variant (Verilog declarations,
# Bluesim DEF_/INST_/PORT_ members) and, for each variant, the names that
# differ from the base variant.  A name that differs only in its numbers
# (__hN, ___dN, a trailing _N) is reported as renumbered.
import re, sys, os
out, mod, basev = sys.argv[1:4]; vs = sys.argv[4:]
DECL = re.compile(r'^\s*(?:input|output|wire|reg)\s*(?:\[[^\]]*\])?\s*([^;]*);', re.M)
def vnames(v):
    f = os.path.join(out, v, 'v', mod + '.v')
    if not os.path.exists(f): return None
    txt = open(f).read()
    txt = re.sub(r'//[^\n]*', '', txt)
    body = txt.split(');', 1)[1] if ');' in txt else txt
    names = set()
    for m in DECL.finditer(body):
        for n in m.group(1).replace('\n', ' ').split(','):
            n = n.strip()
            if n: names.add(n)
    return names
def cnames(v):
    f = os.path.join(out, v, 'c', mod + '.h')
    if not os.path.exists(f): return None
    txt = open(f).read()
    return set(re.findall(r'\b((?:DEF|INST|PORT)_[A-Za-z0-9_$]+)\s*;', txt))
def shape(n):
    n = re.sub(r'__h\d+', '__h#', n)
    n = re.sub(r'___d\d+', '___d#', n)
    n = re.sub(r'\d+', '#', n)
    return n
def diff(a, b):
    ra, rb = sorted(a - b), sorted(b - a)
    sa = {}
    for n in ra: sa.setdefault(shape(n), []).append(n)
    ren, gone, new = [], [], []
    used = set()
    for n in rb:
        s = shape(n)
        if s in sa and sa[s]:
            o = sa[s].pop(0); used.add(o); ren.append((o, n))
        else:
            new.append(n)
    gone = [n for n in ra if n not in used]
    return ren, gone, new
for kind, fn in (('verilog', vnames), ('bluesim', cnames)):
    base = fn(basev)
    print('== %s %s: %d names' % (kind, basev, len(base)))
    for n in sorted(base): print('   ', n)
    for v in vs:
        cur = fn(v)
        if cur is None: print('== %s %s: missing' % (kind, v)); continue
        ren, gone, new = diff(base, cur)
        print('== %s %s vs %s: %d renumbered, %d gone, %d new' % (kind, v, basev, len(ren), len(gone), len(new)))
        for o, n in ren: print('    renum %s -> %s' % (o, n))
        for n in gone: print('    gone  %s' % n)
        for n in new: print('    new   %s' % n)

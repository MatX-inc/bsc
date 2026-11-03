# cutonly.py: atomize3.py with no atom (the same cut after schedule_sound), the atom tests' control
# atomize2.py (round 5) with inputs read through parts (i.partN.NAME), cost review r8-2
# Abstract-value test of a generated schedlean file (review integration 2, B1).
#
# Every comparison of a value with a constant becomes a free Bool, as with an
# abstract value type V whose constants are compared through one operator
# `eqc` (Modular.EqC): `i.F == C` (an input) and `v.F == C` (a field of a
# view).  Then, as the modular plan emits them:
#   * EqLaw chains: for each operand compared with two or more distinct
#     constants, "at most one of its comparisons holds", as a chain
#     ((a1 || .. || a(k-1)) && ak = false), which EqC.law proves;
#   * lifted congruences: each hypothesis whose conclusion equates two values
#     (A = B) also equates their comparisons with each constant either is
#     compared with (in the modular plan a read is the child's output wire on
#     both sides, so these congruences are not hypotheses at all; here they
#     stand in for that sharing).
# The facts go into a new hypothesis `EqAtoms i`, which the consistency check
# (fail_if_success) also sees.
#
# usage: python3 -I atomize2.py out.lean atomized.lean
import re, sys
from collections import OrderedDict, defaultdict

src, dst = sys.argv[1], sys.argv[2]
s = open(src).read()

NAME = r"(«[^»]+»|[A-Za-z_][A-Za-z0-9_']*)"
CONST = r"«(NOMATCH)(x)(y)»"  # control: no atom, only the cut
LB = r"(?<![A-Za-z0-9_'.»)])"
pat_i = re.compile(LB + r"i\.((?:(?:part[0-9]+|«_part[0-9]+»)\.)?" + NAME + ") == " + CONST)
pat_v = re.compile(LB + r"v\." + NAME + " == " + CONST)

def cval(w, b, d):
    d = d.replace("_", "")
    return (int(w), int(d, {"d": 10, "h": 16, "b": 2}[b]))

atoms_i = OrderedDict()   # (G, w, v) -> eqA_k
atoms_x = OrderedDict()   # (expr, w, v) -> eqX_k
atoms_v = OrderedDict()   # (F, w, v) -> eqV_k

def ai(G, c):
    k = (G, c[0], c[1])
    if k not in atoms_i:
        atoms_i[k] = "eqA_%d" % len(atoms_i)
    return atoms_i[k]

def av(F, c):
    k = (F, c[0], c[1])
    if k not in atoms_v:
        atoms_v[k] = "eqV_%d" % len(atoms_v)
    return atoms_v[k]

def ri(m):
    return "i." + ai(m.group(1), cval(m.group(3), m.group(4), m.group(5)))

def rv(m):
    return "v." + av(m.group(1), cval(m.group(2), m.group(3), m.group(4)))

s = pat_i.sub(ri, s)
s = pat_v.sub(rv, s)

# View.hw's fields
hwfield = {}
m = re.search(r"noncomputable def View\.hw \(i : Inputs\) : View :=\n  \{(.*?)\}\n", s, re.S)
if m:
    for line in m.group(1).split("\n"):
        mm = re.match(r"\s*" + NAME + r" := (.*?),?\s*$", line)
        if mm:
            hwfield[mm.group(1)] = mm.group(2)
views = re.findall(r"noncomputable def (View\.at_[A-Za-z0-9_'«»@$.]+) \(i : Inputs\) : View :=", s)

# hypothesis facts whose conclusion equates two values
NAMEP = NAME
side = re.compile(r"^\s*(i\.((?:(?:part[0-9]+|«_part[0-9]+»)\.)?" + NAMEP + r")|\(View\.at_[A-Za-z0-9_'«»@$.]+ i\)\." + NAMEP +
                  r"|\(View\.hw i\)\." + NAMEP + r")\s*$")
def parse_side(t):
    mm = side.match(t)
    if not mm:
        return None
    if mm.group(2):
        return ("i", mm.group(2), None)
    if mm.group(4):
        inst = t.strip()[: t.strip().index(")") + 1]
        return ("view", mm.group(4), inst)
    return ("hw", mm.group(5), None)

ops_i = defaultdict(set)
for (G, w, v) in atoms_i: ops_i[G].add((w, v))
ops_v = defaultdict(set)
for (F, w, v) in atoms_v: ops_v[F].add((w, v))

def consts_of(sd):
    kind, f, inst = sd
    if kind == "i":
        return set(ops_i.get(f, ()))
    return set(ops_v.get(f, ()))

lifted = []
for sm in re.finditer(r"^structure ([A-Za-z0-9]+) \(i : Inputs\) : Prop where\n(.*?)(?=^\S)", s, re.S | re.M):
    if sm.group(1) in ("Hyps",):
        continue
    for fm in re.finditer(r"^  (\S+) :\n    (.*)$", sm.group(2), re.M):
        fact = fm.group(2)
        parts = fact.split(" → ")
        concl = parts[-1]
        if " = " not in concl or concl.count(" = ") != 1:
            continue
        A, B = concl.split(" = ")
        sa, sb = parse_side(A), parse_side(B)
        if not sa or not sb:
            continue
        cs = consts_of(sa) | consts_of(sb)
        if not cs:
            continue
        pre = " → ".join(parts[:-1])
        lifted.append((pre, sa, sb, sorted(cs)))

def atom_expr(sd, c):
    kind, f, inst = sd
    if kind == "i":
        return "i." + ai(f, c)
    if kind == "view":
        return inst + "." + av(f, c)
    return "(View.hw i)." + av(f, c)

lift_facts = []
for pre, sa, sb, cs in lifted:
    for c in cs:
        a, b = atom_expr(sa, c), atom_expr(sb, c)
        lift_facts.append(((pre + " → ") if pre else "") + "%s = %s" % (a, b))

# View.hw supplies each view atom from its field's expression
hwdefs = []   # (eqV_k, rhs)
for (F, w, v), nm in atoms_v.items():
    e = hwfield.get(F)
    mm = re.match(r"^i\.((?:(?:part[0-9]+|«_part[0-9]+»)\.)?" + NAME + r")$", e) if e else None
    if mm:
        rhs = "i." + ai(mm.group(1), (w, v))
    else:
        key = (e or ("?" + F), w, v)
        if key not in atoms_x:
            atoms_x[key] = "eqX_%d" % len(atoms_x)
        rhs = "i." + atoms_x[key]
    hwdefs.append((nm, rhs))

# EqLaw chains
def chain(terms):
    out = []
    for k in range(1, len(terms)):
        out.append("(%s) && %s = false" % (" || ".join(terms[:k]), terms[k]))
    return out

law = []
grp_i = defaultdict(list)
for (G, w, v), nm in atoms_i.items(): grp_i[G].append(((w, v), "i." + nm))
grp_x = defaultdict(list)
for (e, w, v), nm in atoms_x.items(): grp_x[e].append(((w, v), "i." + nm))
for g in list(grp_i.values()) + list(grp_x.values()):
    if len(g) >= 2:
        law += chain([t for _, t in sorted(g)])
grp_v = defaultdict(list)
for (F, w, v), nm in atoms_v.items(): grp_v[F].append(((w, v), nm))
for inst in ["(%s i)" % vw for vw in views]:
    for g in grp_v.values():
        if len(g) >= 2:
            law += chain([inst + "." + nm for _, nm in sorted(g)])

# fields
fi = "".join("  %s : Bool  -- %s == %d'd%d\n" % (n, G, w, v) for (G, w, v), n in atoms_i.items())
fi += "".join("  %s : Bool  -- (%s) == %d'd%d\n" % (n, e, w, v) for (e, w, v), n in atoms_x.items())
s = s.replace("structure Inputs where\n", "structure Inputs where\n" + fi, 1)
if atoms_v:
    fv = "".join("  %s : Bool  -- %s == %d'd%d\n" % (n, F, w, v) for (F, w, v), n in atoms_v.items())
    s = s.replace("structure View where\n", "structure View where\n" + fv, 1)
    s = re.sub(r"(noncomputable def View\.hw \(i : Inputs\) : View :=\n  \{)",
               lambda m: m.group(1) + " " + ", ".join("%s := %s" % d for d in hwdefs) + ",\n   ", s, count=1)

facts = [("eqlaw_%d" % k, f) for k, f in enumerate(law)] + [("lift_%d" % k, f) for k, f in enumerate(lift_facts)]
st = "/-- Comparisons with constants as atoms: EqLaw chains and lifted congruences. -/\n"
st += "structure EqAtoms (i : Inputs) : Prop where\n"
st += "  dummy : True\n" if not facts else ""
st += "".join("  %s :\n    %s\n" % f for f in facts)
s = s.replace("/-- Everything the theorems assume, as one hypothesis. -/\nstructure Hyps",
              st + "\n/-- Everything the theorems assume, as one hypothesis. -/\nstructure Hyps", 1)
s = s.replace("theorem schedule_sound (i : Inputs) (h : Hyps i)", "theorem schedule_sound (i : Inputs) (h : Hyps i) (eqat : EqAtoms i)", 1)
# keep the file up to schedule_sound's proof: the theorems after it (exported
# orders) are not part of this test
t0 = s.index("theorem schedule_sound")
mnext = re.compile(r"\n(?=(/--|/-!|theorem |def |noncomputable |abbrev |structure |section |namespace |open |end |#print))").search(s, t0)
mend = list(re.finditer(r"\nend [A-Za-z0-9_]+\n", s))[-1]
tail = "".join(l + "\n" for l in s[mend.end():].split("\n") if l.startswith("#print") and "schedule_sound" in l)
s = s[:mnext.start()] + "\n" + s[mend.start():mend.end()] + tail
names = [n for n, _ in facts] or ["_"]
rfls = "".join("  have : (View.hw i).%s = %s := rfl\n" % d for d in hwdefs)
pos = s.index("\n", s.index(":= h\n", s.index("theorem schedule_sound"))) + 1
s = s[:pos] + "  obtain ⟨%s⟩ := eqat\n" % ", ".join(names) + rfls + s[pos:]
open(dst, "w").write(s)
print("%d input atoms, %d view atoms, %d other atoms, %d chain facts, %d lifted facts, %d views"
      % (len(atoms_i), len(atoms_v), len(atoms_x), len(law), len(lift_facts), len(views)))

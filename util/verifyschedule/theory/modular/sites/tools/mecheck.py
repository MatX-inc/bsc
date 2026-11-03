"""bsc's mutually_exclusive run-time check lines, read by their structure (soundness review r8b-1 M1).

bsc's mkMEAssump asserts `aAnd (aOrs WF as) (aOrs WF bs)` (ASchedule.hs): the line is `A && B`, each side one
WILL_FIRE signal or a parenthesised `||` of several (a side has several rules when rJoinMutuallyExclusive joins
rule sets).  It asserts that no rule of `as` fires with a rule of `bs`: the exclusions are as x bs, nothing
within one side.  `me_parse` returns (as, bs) only when re-rendering them gives the line back exactly (the
generator check for trust point 3); a line of WILL_FIRE signals in any other form raises.  The emitter, the
gate's scan and the shadow tool all read check lines through this module, and each asserts that the pairs equal
the dumper's `mutually_exclusive (user)` list."""
import re

WF = r'WILL_FIRE_([A-Za-z0-9_$]+)'
_ONLY_WF = re.compile(r'^(?:\s|&&|\|\||\(|\)|WILL_FIRE_[A-Za-z0-9_$]+)*$')


def render(as_, bs):
    side = lambda xs: ('WILL_FIRE_' + xs[0]) if len(xs) == 1 else '(' + ' || '.join('WILL_FIRE_' + x for x in xs) + ')'
    return side(as_) + ' && ' + side(bs)


def wf_only(line):
    """The line reads only WILL_FIRE signals (a mutually_exclusive check, or a malformed one)."""
    return bool(_ONLY_WF.match(line)) and len(re.findall(WF, line)) >= 2


def me_parse(line):
    """(as, bs) for a mutually_exclusive check line, None for any other line (a conflict_free check)."""
    if not wf_only(line):
        return None
    t = line.strip()
    parts = t.split(' && ')
    if len(parts) == 2:
        sides = []
        for p in parts:
            q = p[1:-1] if p.startswith('(') and p.endswith(')') else p
            xs = q.split(' || ')
            if all(re.fullmatch(WF, x) for x in xs) and (len(xs) > 1) == (p != q):
                sides.append([re.fullmatch(WF, x).group(1) for x in xs])
        if len(sides) == 2 and render(*sides) == t:
            return sides[0], sides[1]
    raise ValueError('a run-time check line of WILL_FIRE signals in an unknown form: %s' % t[:120])


def me_pairs(line):
    """The exclusions a mutually_exclusive line asserts: as x bs."""
    as_, bs = me_parse(line)
    return [(a, b) for a in as_ for b in bs]


def same_pairs(lines, dumper_pairs):
    """Generator check: the check lines' exclusions are the dumper's mutually_exclusive (user) pairs."""
    mine = set(frozenset(p) for ln in lines for p in me_pairs(ln))
    theirs = set(frozenset(p) for p in dumper_pairs)
    assert mine == theirs, 'mutually_exclusive check lines (%d pairs) differ from the dumper\'s list (%d pairs)' % (
        len(mine), len(theirs))


def suffix_groups(lines):
    """bsc's mutually_exclusive groups: one-head lines (h, lst) chained so that each list is the suffix of one
    sequence [x0, .., xn] after its head (the emitter's GROUPS, cost review r7-1 M1). A group's rules pairwise
    never fire together, exactly what its lines assert. Returns (groups, the pairs of the other lines)."""
    stars = []
    pairs = []
    for ln in lines:
        as_, bs = me_parse(ln)
        if len(as_) == 1:
            stars.append((as_[0], tuple(bs)))
        else:
            pairs += [(a, b) for a in as_ for b in bs]
    bylst = {}
    for c, (_, lst) in enumerate(stars):
        bylst.setdefault(lst, []).append(c)
    used, groups = set(), []
    for c0, (h0, l0) in enumerate(stars):
        if len(l0) != 1 or c0 in used:
            continue
        seq, chain = [h0] + list(l0), [c0]
        while True:
            nx = [c for c in bylst.get(tuple(seq), []) if c not in used and c not in chain]
            if not nx:
                break
            chain.append(nx[0])
            seq = [stars[nx[0]][0]] + seq
        if len(seq) > 2:
            groups.append(seq)
            used |= set(chain)
    for c, (h, lst) in enumerate(stars):
        if c not in used:
            pairs += [(h, b) for b in lst]
    return groups, pairs

#!/usr/bin/env python3
"""Mutation test for a flat Lean file schedcheck wrote (--flat): break the hardware and
check that the proof then fails.

    mutate.py [-n N] FILE.lean

Makes up to N mutants of each kind (default 2) by editing `stepScheduled`,
the hardware's submodule inputs:

  enable  negate one submodule method's enable
  arg     replace one argument by a value no term has (the largest token)

and one of each kind of the hardware's system-task calls (bsc's run-time
checks left out, which the hypotheses say never run):

  task-order  swap the last two calls of different rules adjacent in the
              always block: the later one comes first, one more than the
              count before the pair, the other one more again if it is made
  task-cond   drop the last call's own condition (of those with one),
              keeping its rule's WILL_FIRE, one more than the count before it
  task-arg    replace one argument by the code no term has (0, unset)

each of which must fail with a counterexample (a cycle that shows it) in
`schedule_sound` alone,

and, for a module whose file checks other orders (a pair of exported
conflict-free methods that commute only by a fact, `theorem Comm.<a>.<b>`):

  order   let the two methods run in the same cycle and give the hardware's
          mux of a port they both drive to the later of them in the
          schedule's order: the schedule's order still agrees with that
          hardware (checked: the file up to `schedule_sound` still checks),
          the order that runs the later method first does not

Each mutant must fail to check: a mutant that still checks means the theorem
does not pin down that input (or, for an order mutant, that order).  Prints
the tally and exits non-zero if any mutant survives.  LEAN names the lean
binary (default: lean).
"""
import os
import re
import subprocess
import sys
import tempfile

LEAN = os.environ.get("LEAN", "lean")


def run(text, workdir):
    """Check a file: whether it checks, and Lean's output."""
    fd, path = tempfile.mkstemp(suffix=".lean", dir=workdir)
    with os.fdopen(fd, "w") as f:
        f.write(text)
    try:
        r = subprocess.run([LEAN, path], capture_output=True, text=True)
        out = r.stdout + r.stderr
        return r.returncode == 0 and "error" not in out, out
    finally:
        os.unlink(path)


def checks(text, workdir):
    return run(text, workdir)[0]


def error_decls(text, out):
    """The top-level declarations of `text` that Lean's errors in `out` are in."""
    lines = text.split("\n")
    names = set()
    for n in {int(x) for x in re.findall(r"^[^:\n]*:(\d+):\d+: error", out, re.M)}:
        k = min(n, len(lines)) - 1
        while k > 0 and not re.match(r"[^\s/-]", lines[k]):
            k -= 1
        m = re.search(r"\b(?:theorem|def|abbrev|structure|instance) (\S+)", lines[k])
        names.add(m.group(1) if m else lines[k].split(" ")[0])
    return names


def is_task_field(name):
    return name.strip("«»").startswith("$task")


def mutants(text, per_kind):
    lines = text.split("\n")
    start = next(i for i, l in enumerate(lines)
                 if l.startswith("noncomputable def stepScheduled"))
    end = start + 1
    while end < len(lines) and lines[end].startswith("  "):
        end += 1
    tokens = {name: int(w) for name, w in re.findall(r"abbrev (Tok\w+) := BitVec (\d+)", text)}
    enables, args = [], []
    for i in range(start + 1, end):
        m = re.match(r"(\s+[{ ]\s*)(\S+) := (.*?)(,| })$", lines[i])
        if (not m or m.group(2) in ("illegal_access", "guard_violated") or "$" not in m.group(2)
                or is_task_field(m.group(2))):
            continue
        (enables if "$EN_" in m.group(2) else args).append((i, m))
    for i, m in enables[:per_kind]:
        new = list(lines)
        new[i] = f"{m.group(1)}{m.group(2)} := !({m.group(3)}){m.group(4)}"
        yield "enable " + m.group(2), "\n".join(new)
    for i, m in args[:per_kind]:
        decl = re.search(r"^\s+" + re.escape(m.group(2)) + r" : (\w+)", text, re.M)
        if not decl:
            continue
        sort = decl.group(1)
        if sort == "Bool":
            value = f"!({m.group(3)})"
        elif sort in tokens:
            value = f"{(1 << tokens[sort]) - 1}#{tokens[sort]}"
        else:
            continue
        new = list(lines)
        new[i] = f"{m.group(1)}{m.group(2)} := {value}{m.group(4)}"
        yield "arg " + m.group(2), "\n".join(new)


def top_conjuncts(t):
    """The operands of a top-level `&&` chain (one if there is none)."""
    parts, depth, start, k = [], 0, 0, 0
    while k < len(t):
        c = t[k]
        if c in "(⟨[":
            depth += 1
        elif c in ")⟩]":
            depth -= 1
        elif depth == 0 and t.startswith(" && ", k):
            parts.append(t[start:k])
            start = k + 4
            k += 4
            continue
        k += 1
    parts.append(t[start:])
    return parts


def task_mutants(text, per_kind):
    """Mutants of the hardware's system-task calls in `stepScheduled`: call k's
    position «$taskk» (`bif <made> then Hw.«$tasks@n» i else 0`), arguments «$taskk_j»."""
    lines = text.split("\n")
    start = next(i for i, l in enumerate(lines)
                 if l.startswith("noncomputable def stepScheduled"))
    end = start + 1
    while end < len(lines) and lines[end].startswith("  "):
        end += 1
    width = re.search(r"^abbrev TaskPos := BitVec (\d+)", text, re.M)
    if not width:
        return
    w = int(width.group(1))
    tokens = {name: int(x) for name, x in re.findall(r"abbrev (Tok\w+) := BitVec (\d+)", text)}
    # each call's rule (its comment in `State`) and whether it is a run-time check
    notes = dict(re.findall(r"^\s+«\$task(\d+)» : TaskPos\s+-- (.*)$", text, re.M))
    field = {}
    for i in range(start + 1, end):
        m = re.match(r"(\s+[{ ]\s*)«\$task([^»]*)» := (.*?)(,| })$", lines[i])
        if m:
            field[m.group(2)] = (i, m)
    calls = []
    for k, note in notes.items():
        rule = re.search(r" in (\S+)", note)
        if k not in field or ", a run-time check" in note or not rule:
            continue
        pos = field[k][1].group(3)
        m = (re.fullmatch(r"bif (.*) then Hw\.«\$tasks@(\d+)» i else 0#\d+", pos)
             or re.fullmatch(r"()Hw\.«\$tasks@(\d+)» i", pos))
        if m:
            calls.append((int(m.group(2)), k, rule.group(1), m.group(1) or "true"))
    calls.sort()

    def before(n):
        """The count of calls made before the call at the n-th count."""
        return f"0#{w}" if n == 1 else f"Hw.«$tasks@{n - 1}» i"

    def edit(changes):
        new = list(lines)
        for name, value in changes:
            i, m = field[name]
            new[i] = f"{m.group(1)}«$task{name}» := {value}{m.group(4)}"
        return "\n".join(new)

    # task-order: adjacent calls of different rules, the later first (the
    # last such pairs)
    made = 0
    for (na, a, ra, ea), (nb, b, rb, eb) in reversed(list(zip(calls, calls[1:]))):
        if made == per_kind:
            break
        if nb == na + 1 and ra != rb:
            made += 1
            p = before(na)
            yield ("task-order $task" + a + "/$task" + b,
                   edit([(b, f"bif {eb} then {p} + 1#{w} else 0#{w}"),
                         (a, f"bif {ea} then {p} + (bif {eb} then 1#{w} else 0#{w}) + 1#{w} else 0#{w}")]))
    # task-cond: the last calls with a condition of their own
    made = 0
    for n, k, _, en in reversed(calls):
        if made == per_kind:
            break
        ops = top_conjuncts(en)
        keep = [o for o in ops if o.startswith("WILL_FIRE_")]
        if len(ops) > 1 and len(keep) < len(ops):
            made += 1
            yield ("task-cond $task" + k,
                   edit([(k, f"bif {' && '.join(keep) if keep else 'true'} then {before(n)} + 1#{w} else 0#{w}")]))
    # task-arg: an argument of a token sort
    made = 0
    for _, k, _, _ in calls:
        for j in range(1, 100):
            name = f"{k}_{j}"
            if name not in field or made == per_kind:
                break
            decl = re.search(r"^\s+«\$task" + re.escape(name) + r"» : (\w+)", text, re.M)
            if decl and decl.group(1) in tokens:
                made += 1
                yield "task-arg $task" + name, edit([(name, f"0#{tokens[decl.group(1)]}")])
                break


def def_body(lines, k):
    """The body of the definition whose header is line k, on one line."""
    body = [lines[k].split(" :=", 1)[1].strip()]
    j = k + 1
    while j < len(lines) and lines[j].startswith("  "):
        body.append(lines[j].strip())
        j += 1
    return " ".join(b for b in body if b)


def restate(text, name, body):
    """The proof states each definition's equation by `rfl`, its body written
    out: restate a mutated definition's from its new body."""
    pat = re.compile(r"have : " + re.escape(name) + r" i =.*? := rfl", re.S)
    return pat.sub(lambda m: "have : " + name + " i = (" + body + ") := rfl", text)


def order_mutants(text, per_kind):
    """For each pair of steps that commute only by a fact (a `Comm.<a>.<b>`
    lemma, a moved after b), widen b's guard by a's (CAN_FIRE), so that both
    may run in one cycle; state as true the facts that said they never do;
    give the hardware's muxes that select between them to b, which runs
    later in the schedule's order; and drop their one-hot condition."""
    ns = re.search(r"^namespace (\S+)", text, re.M)
    sched = text.find("\ntheorem schedule_sound")
    if not ns or sched < 0:
        return
    rest = text.find("\n/-! ### Other orders", sched)
    if rest < 0:
        return
    pairs = re.findall(r"^theorem Comm\.(\S+?)\.(\S+) \(i : Inputs\)", text, re.M)
    for a, b in pairs[:per_kind]:
        a, b = a.strip("«»"), b.strip("«»")
        new = text
        # b is ready whenever a is
        new, n = re.subn(r"^(noncomputable def CAN_FIRE_" + re.escape(b) + r" \(i : Inputs\) : Bool :=)(.*)$",
                         lambda m: m.group(1) + " (" + m.group(2).strip() + ") || CAN_FIRE_" + a + " i",
                         new, count=1, flags=re.M)
        if n == 0:
            continue
        # the facts that the two never run together no longer hold
        lines = new.split("\n")
        for k, line in enumerate(lines):
            if (("CAN_FIRE_" + a + " i") in line and ("CAN_FIRE_" + b + " i") in line
                    and line.strip().endswith("= true") and line.startswith("    ")):
                lines[k] = "    true = true"
        # the muxes give b, the later step, priority
        swapped = set()
        k = 0
        while k < len(lines):
            m1 = re.match(r"^  (else )?bif WILL_FIRE_" + re.escape(a) + r" i then (.*)$", lines[k])
            m2 = k + 1 < len(lines) and re.match(r"^  else bif WILL_FIRE_" + re.escape(b) + r" i then (.*)$", lines[k + 1])
            if m1 and m2:
                lead = "  else " if m1.group(1) else "  "
                lines[k] = lead + "bif WILL_FIRE_" + b + " i then " + m2.group(1)
                lines[k + 1] = "  else bif WILL_FIRE_" + a + " i then " + m1.group(2)
                h = k
                while not lines[h].startswith("noncomputable def "):
                    h -= 1
                swapped.add(h)
                k += 2
            else:
                k += 1
        # (a pair with no port both drive has nothing to reorder)
        if not swapped:
            continue
        # (and the equations of the definitions changed say so)
        heads = sorted(swapped) + [h for h, l in enumerate(lines)
                                   if l.startswith("noncomputable def CAN_FIRE_" + b + " (i : Inputs)")]
        new = "\n".join(lines)
        for h in heads:
            new = restate(new, lines[h].split()[2], def_body(lines, h))
        # and their one-hot condition is gone
        new = new.replace("!(WILL_FIRE_" + a + " i && WILL_FIRE_" + b + " i)", "true")
        cut = new.find("\n/-! ### Other orders", new.find("\ntheorem schedule_sound"))
        schedule_only = new[:cut] + "\n\nend " + ns.group(1) + "\n"
        yield "order " + a + "/" + b, new, schedule_only


def main():
    args = sys.argv[1:]
    per_kind = 2
    if len(args) == 3 and args[0] == "-n":
        per_kind, args = int(args[1]), args[2:]
    if len(args) != 1:
        sys.exit(__doc__)
    path = args[0]
    text = open(path).read()
    workdir = os.path.dirname(os.path.abspath(path))
    total, survivors = 0, []
    for name, mutant in mutants(text, per_kind):
        total += 1
        if checks(mutant, workdir):
            survivors.append(name)
    # (one of each: a second choice may change nothing, as a call made in every
    # cycle, or two calls never made together, would)
    for name, mutant in task_mutants(text, min(per_kind, 1)):
        total += 1
        ok, out = run(mutant, workdir)
        if ok:
            survivors.append(name)
        elif "counterexample" not in out or error_decls(mutant, out) != {"schedule_sound"}:
            survivors.append(name + " (fails other than by a counterexample in schedule_sound)")
    for name, mutant, schedule_only in order_mutants(text, per_kind):
        total += 1
        if checks(mutant, workdir):
            survivors.append(name)
        elif not checks(schedule_only, workdir):
            # (the schedule's order must still agree, or the mutant is not
            # one that only an order catches)
            survivors.append(name + " (the schedule's order disagrees too)")
    print(f"{total - len(survivors)}/{total} mutants fail to check"
          + (f"; survivors: {', '.join(survivors)}" if survivors else ""))
    sys.exit(1 if survivors else 0)


if __name__ == "__main__":
    main()

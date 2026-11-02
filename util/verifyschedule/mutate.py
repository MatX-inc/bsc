#!/usr/bin/env python3
"""Mutation test for a Lean file verifyschedule wrote: break the hardware and
check that the proof then fails.

    mutate.py [-n N] FILE.lean

Makes up to N mutants of each kind (default 2) by editing `stepScheduled`,
the hardware's submodule inputs:

  enable  negate one submodule method's enable
  arg     replace one argument by a value no term has (the largest token)

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


def checks(text, workdir):
    fd, path = tempfile.mkstemp(suffix=".lean", dir=workdir)
    with os.fdopen(fd, "w") as f:
        f.write(text)
    try:
        r = subprocess.run([LEAN, path], capture_output=True, text=True)
        return r.returncode == 0 and "error" not in r.stdout + r.stderr
    finally:
        os.unlink(path)


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
        if not m or m.group(2) in ("illegal_access", "guard_violated") or "$" not in m.group(2):
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

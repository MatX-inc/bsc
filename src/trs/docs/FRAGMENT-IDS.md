# Fragment identifiers

Per-module export means producing a module's BIR from that module's
`.ba` and its children's interface summaries, with no view of the design
it will be linked into.  A fragment produced that way is cacheable: the
same module source yields the same bytes whatever design instantiates
it, and a design that changes one module relinks rather than re-exports.

Every identifier a fragment writes has to mean something without a view
of the whole design.  This note describes the identifier model that
makes that true — the scopes, what crosses between them, and the
invariants the hashes depend on.

## Three scopes

**Module scope — `ModuleStrId`.**  Names meaningful against one module
type: its defs, rules, methods, ports, clock domains, resets, and the
names it gives its own instances.  A fragment carries the table these
index, so they can be renumbered freely within it and mean the same
thing after the fragment moves.  This is the default: `StrId` is an
alias for `ModuleStrId`, so an identifier is module-scoped unless it
says otherwise.

**Link scope — `GlobalStrId`.**  The vocabulary fragments share, and the
only names one fragment can write for another to read: module type
names, instance paths, and the primitive method names every design
agrees on.  A fragment cannot mint a link-scoped name for something
design-specific, because it does not know where it will be instantiated
or under what path; the link assembles this table from what the
fragments declare.

**Runtime scope — `DynStrId`.**  Strings that exist only while a design
runs, in an arena the interpreter owns: the results of string-producing
expressions, `$sformat` output, values read back from BDPI.  These have
no place in a fragment, because they are not properties of the module.
A `StrRef` is the tagged union of the two spaces a running program
reaches for — a static id into the design's table, or a dynamic id into
the arena.  It packs into one word (`DYN_TAG` distinguishing the two)
so a string-valued `Value` stays a scalar.

Distinct types rather than a bare `u32` alias, because the failure they
prevent is a silent wrong answer, not a crash: name lookup returns the
first match, so a rule name repeated across modules resolves to
whichever module interned it first.  A misuse is now a compile error.

## What crosses a fragment boundary

A reference that leaves a fragment names a **position** in the target,
not a name to be resolved against it.  A position is meaningful the
moment the callee's shape is known, needs no shared string space, and
cannot silently bind to the wrong entry.

**`MethRef`** is the shape this takes for method calls, and it is
split because the two cases genuinely differ.  `MethRef::User(u32)` is
an index into the callee's method list — a user module's methods are its
own, so a call carries no identifier out of the callee's scope at all.
`MethRef::Prim(GlobalStrId)` names a primitive's method, which is
vocabulary every design shares and so belongs in the link table.

**`Instance::kind = Module(GlobalStrId)`** is the one identifier class
that stays a name.  Type names are how fragments refer to each other, so
they are the link-scoped vocabulary by definition, and they are the edge
along which the interface hash propagates.

**`Instance::method_order` and `port_counts`** describe the *child's*
methods from inside the parent.  The parent cannot express these in its
own scope, and with positions it does not need to: they are the child's
method list read positionally.

**`Composition`, `QualifiedTick`, `SchedAlt::guard`, `cross_inhibits`.**
These are the design superstructure holding module-scoped content — an
instance path (link-scoped) beside a segment, domain, or rule index
(module-scoped), and in `guard`'s case a whole `Expr` written in the
scope of the module at `guard_inst`.  They are read by resolving the
instance first and then reading the module-scoped part against the
module that instance points to; the pairing is what makes them
readable, so neither half may be separated from the other.

## Generated names carry ids, not spellings

A name the compiler generates — an enable signal, a ready signal, a
method's argument port — is recorded once, at the point of generation,
as an id.  Everything downstream takes the id.  No consumer reconstructs
the name from a convention, and no consumer strips a prefix back off.

`Method::en` holds the enable port's id, `Method::rdy` a `MethRef` to
the ready method, and `Port::base` the argument's own name with the
`<method>_` qualifier already removed by the exporter.

Two reasons this matters more than it looks.  A reconstructed name is a
lookup by string in a space that is about to become per-module, so every
such site is a link-time scan that has to be deleted before the tables
can split.  And the convention is duplicated at every site that spells
it, so it is only as stable as the least-examined copy.

## The fragment's table is hashed content

`Module::content_hash` is the field a fragment cache keys on.  It is
reserved in the format and written as zeros; what it will cover is the
fragment's own bytes, and those include its string table, so the table
is part of what is hashed.  Two rules follow, and they constrain the
exporter now rather than when the hash is filled.

**Interning is canonical.**  The exporter interns in a deterministic
order derived from the module, not in encounter order, or the same
module hashes differently depending on which fields the encoder happened
to walk first.

**The hash covers names, not what they name.**  A `$readmemh` filename
is content: change only which file a module reads and the hash must
differ, or a cached fragment loads the wrong data at runtime.  The
*contents* of that file are not part of the fragment.

A second hash over a module together with its children is what makes a
change anywhere below a module invalidate it while an unrelated
sibling's change does not.  It composes from `content_hash`, so it
waits on the same thing.

## Where this stands

The scopes are types and the boundary-crossing references carry
positions.  One table still backs both `ModuleStrId` and `GlobalStrId`,
so the distinction is enforced by the compiler but not yet reflected in
the format: splitting `Design::strings` into a link-level table plus one
table per fragment is the next break, and it is what `content_hash`
waits on.  Hashing a table that embeds design-global positions would
give a hash that differs per design, so the cache would serve relink and
not reuse across designs — which is most of the reason to have one.

Instance paths are still dotted strings.  Replacing them with structured
references — an instance index against a parent — is the remaining
link-scoped name that a fragment cannot mint and therefore the remaining
obstacle to a fragment that names its children without knowing the
design.

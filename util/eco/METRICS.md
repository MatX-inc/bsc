# OLD-relative reporting

The optional `measure.hs` command reports two Git Myers added-plus-deleted line
counts. It reports existing outputs without rerunning or changing their search
objective, and it does not prove equivalence. See the
[search objective options](README.md#synthesis-translated-search-objective) for
whole-file and translated cost policies.

```sh
bluehs -i/path/to/sdk/lib /path/to/sdk/scripts/measure.hs \
  /path/to/old/mkDemo.eco-ast /path/to/new/mkDemo.eco-ast fresh-report
```

For a selected or direct AST, provide its authenticated capture context and its
actual emitted RTL:

```sh
bluehs -i/path/to/sdk/lib /path/to/sdk/scripts/measure.hs --candidate \
  /path/to/old/mkDemo.eco-ast /path/to/new/mkDemo.eco-ast \
  /path/to/selected.ast /path/to/selected.v fresh-selected-report
```

Repeat with the **same OLD input** for ordinary NEW, automatic and direct
outputs. Automatic-to-direct distance is informational only. Equal-cost correct
alternatives need not use the same placement.

`report.json` records `raw` and `normalized` additions, deletions and total
changed lines, plus exact input/output identities. Raw counts use actual RTL
bytes. The normalized files are rendered from native ASTs with soft wrapping,
indentation and blank lines removed. Ordinary informational comment fields,
including positional parameter descriptions, are excluded. Complete fields
containing known synthesis/tool/directive markers are retained, as are native
macros, translate-off markers and case flags. This known-marker convention
does not interpret every possible third-party comment language.

Normalization preserves declaration partition and placement, statement/member
order, names, operators, literal values and string expressions. It does not
reorder statements or simplify expressions. The normalized unit is a separate
textual line metric; it is not raw cost minus a subset and may exceed raw cost.
Neither count establishes physical ECO cost, timing or a global minimum.

The command requires checked native snapshot context and exact AST-to-actual-RTL
rendering. It rejects missing context, altered capture artifacts, different
renderer policies, changed candidate ports/parameters, and candidate RTL that
does not exactly match its AST under the authenticated renderer. Filters that
change the ordinary AST rendering cannot silently qualify. A manually
rewrapped direct file therefore needs an explicit independently audited witness
outside this automatic command; it is not accepted by ignoring a byte mismatch.

Candidate AST files are hashed but **are not proved correct by this command**.
Use the independent proof and saved-plan replay tools first; keep their receipts
with the metric report. Reporting never treats a count or a saved proof receipt
as authorization to emit a candidate.

The command needs the matched bluehs SDK and Git. It introduces no Python, RTL
parser or SAT dependency. Run its native-AST and capture-integrity controls with
the existing `eco_metrics.check` DejaGNU target.

# Optional artifact reports

These two bluehs programs project typed native artifacts into JSON on explicit
request. Native compilation, adaptation, matching and proof do not invoke them.
They preserve the useful graph, port, provenance and byte-control information
previously written automatically as `.eco-backend.json` and
`adapter-receipt.json`.

Use a source SDK packaged from this revision with its matching compiler and
bluehs library. Each command requires a new output directory whose parent
already exists; existing files, directories and aliases are rejected.

```sh
bluehs -i/path/to/sdk/lib /path/to/sdk/scripts/backend-report.hs \
  capture/mkExample.eco-ast capture/mkExample.eco-preopt \
  capture/mkExample.eco-graph capture/mkExample.eco-ports backend-report

bluehs -i/path/to/sdk/lib /path/to/sdk/scripts/adapter-receipt-json.hs \
  adapted/adapter-receipt adapter-report
```

The first command writes `backend-report/backend-report.json`. It reads checked
Verilog information, validates the two graphs and their module/checkpoint names,
checks ports against the native `VProgram`, and validates any bound keep plan's
capture and preoptimization identities. The report includes exact identities,
both graphs, requested keeps and definition-use/survival descriptors. Graph
sidecars are supplied diagnostic inputs: structural validation and reporting a
hash do not establish their compiler origin or authorize a transformation.

The second command writes `adapter-report/adapter-receipt.json`. It checks the
typed receipt's source/adapted capture and producer links, all referenced capture
files, and the recorded original/current output digests. It also validates
optional observation companions when present. It does not rerun either producer;
the receipt's scope remains recorded provenance. The report deliberately marks
`producerControlsRerun` false.

Both programs mark their output `diagnosticOnly: true` and `proofAuthority: false`.
Neither report is a proof receipt, a new compiler contract, or permission to
accept an ECO. The existing typed adapter receipt, native replay controls and
independent candidate proofs remain authoritative for their respective checks.
Input identities are checked again after report creation. Source artifacts are
never rewritten. The backend report uses the native reader's library-version
checks. The adapter report compares the source and adapted native versions
recorded in the manifests; it does not decode native payloads or establish
compatibility with the running compiler. Neither command performs migration.

Run the optional report tests with the same `ECO_BLUEHS`, `ECO_SDK` and compiler
selection used by the main ECO suite, selecting `eco_diagnostics.exp`. They cover
explicit generation, absent automatic output, a real retained helper and its
bound plan, malformed or changed inputs, existing destinations, symlinks and
hard links. No SAT solver is needed.

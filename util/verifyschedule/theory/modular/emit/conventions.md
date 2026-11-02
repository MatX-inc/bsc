# Emission conventions for linear per-module cost

This pass covers decision 2 (integration review 3, M2) and the site-list
part of M3. It says what a generated module file must look like for its
check to grow linearly in children, ports, sites and netlist edges. Every
convention below is used by a probe that proves the whole module:

- `Obl`, from a full `Cert` through `obl_of_cert`;
- `Acyc`;
- `Built` and `Built.top`.

The probe uses only the three standard axioms, and its cost is measured from
35 to 2,051 children and from 33 to 2,049 sites in one step.

## Files (all in this directory)

| file | what |
|---|---|
| `EmitLemmas.lean` | library additions, proved once (listed at the end) |
| `gen/mkemit.py` | the probe emitter: `python3 -I gen/mkemit.py m out/Em<m>.lean` |
| `out/Em<m>.lean`, `out/series.txt` | emitted probes, and their measurements |
| `gen/runseries.sh` | the measurement series (one Lean process at a time) |
| `gen/runprof.sh`, `gen/sections.py`, `prof3/` | per-section profile, with wall time and heartbeats |
| `gen/mktabbench.py`, `scratch/TA*.out`, `scratch/TB*.out` | table micro-benchmark (C2); `TB*` are the bare-binder runs (`mktabbench.py n out bare`) |
| `gen/fanin_path.py`, `prof/fanin_path.txt` | corpus read-list sizes (C5) |

    lean -o build/Modular.olean -i build/Modular.ilean build/Modular.lean     # r5/Modular.lean, unchanged
    LEAN_PATH=build lean -o build/EmitLemmas.olean -i build/EmitLemmas.ilean EmitLemmas.lean
    LEAN_PATH=build lean out/Em<m>.lean

Every run used Lean 4.35.0 under `prlimit --as=25769803776` and `timeout`,
with `-j1 -DElab.async=false`.

## The probe

`Em<m>` is a module with `2m + 3` children of two kinds:

- registers: `sel`, `r_0 … r_{m-1}`, `t0`, `t1`;
- RWires `w_0 … w_{m-1}`, whose `wset` is a path-bearing input
  (`wset → wget, whas`).

It has two rules and one method:

- **`go`**: reads `sel`, then for each `k` reads `r_k` and calls
  `w_k.wset(g r_k)` when `sel == k`. That is one step of `2m + 1` sites.
  - Every fourth read of `r_k` is conditional on `sel == k`: an
    array-selection arm, decision 1's reach rule.
  - The call's condition is its exact enable.
- **`back`**: for each `k`, reads `w_k.wget` and calls
  `r_k.write(h wget)` when `p wget` (`2m` sites).
- **method `peek`**: returns `t0 + t1`. It is the parent output whose read
  list is `Do`.

The sizes measured are m = 16 … 1024. That gives 35 … 2,051 children,
33 … 2,049 sites in `go` alone, and 4m + 3 sites in all. For comparison,
sysConflictFreeOKLarge's widest step has 2,050 sites over 514 registers and
3,075 RWires.

## The conventions

### C1. Children are indexed per kind

```lean
inductive Kind where | reg | wire          -- one constructor per child module type
abbrev nk : Kind → Nat | .reg => 1027 | .wire => 1024
abbrev KI : Kind → Iface | .reg => Reg.X | .wire => RWire.X false false
abbrev I := Σ κ : Kind, Fin (nk κ)
abbrev CI : I → Iface := fun j => KI j.1
def rI (i : Nat) : I := ⟨.reg, ⟨i % nk .reg, Nat.mod_lt _ (by decide)⟩⟩
```

- `Kind` has one constructor per child module type: each primitive used,
  each generated child type, and each BVI key (`<vName>_<hash>`). It is never
  one constructor per instance.
- `kids := allKids kinds nk`, with `allKids_nodup` and `mem_allKids` from
  the library. No file enumerates its instances in a proof.
- Instance terms are built by a helper such as `rI`.
  - Never write a numeral of type `Fin (nk κ)`: its `OfNat` instance needs
    `NeZero (nk κ)`. That fails for a plain `def nk`, and costs an instance
    search per occurrence for an `abbrev`.
  - Inside per-instance lemmas the index is `⟨κ, ⟨i, h⟩⟩`, which is
    definitionally `rI i`.
- Few rules or methods: an inductive, matched once (the probe). Many:
  `Fin n` with the step tables of C6, dispatched through `Tab.AllR` as for
  children (C4). Not measured here.

### C2. Every table is a closed term

A `Tab` (round 4's balanced search tree, now universe-polymorphic) holds:

- arms (C3);
- site keys and site functions (C6);
- ports' uses (C7).

A table of things that depend on the value type stores closed functions:

```lean
def armsR : Tab ((V : Type) → Ops V → EqC V → OC V → Wires Unit V → Wires (CRegM 1) V) :=
  (.node 513 (.node 256 … (.leaf (armR_5)) …) …)
def sfGo : Tab ((V : Type) → Ops V → EqC V → SF V) :=
  … (.leaf (fun V (ops : Ops V) (E : EqC V) => ((fun ρ => E.eqc (ρ 0) 3, fun ρ => ops.g (ρ 7)) : SF V))) …
```

Two rules, each measured with `gen/mktabbench.py`:

- **Never parameterize a table** (`def t (ops : Ops V) : Tab …`).
  - Unfolding `t ops` substitutes `ops` into the whole body, so every lookup
    costs the table's size.
  - The benchmark below makes 256 lookups in a table of n entries. In
    heartbeats (allocations, independent of machine load), parameterized
    lookups grow with n; closed ones grow with log n.

  | entries n | 256 lookups, parameterized | 256 lookups, closed |
  |---|---|---|
  | 512 | 22.3 Mhb, 1.8 s | 13.2 Mhb, 0.9 s |
  | 1,024 | 32.4 Mhb, 4.4 s | 14.1 Mhb, 1.0 s |
  | 2,048 | 51.6 Mhb, 10.5 s | 15.1 Mhb, 1.2 s |
  | 4,096 | 89.2 Mhb, 22.5 s | 16.0 Mhb, 1.3 s |

- **Annotate every lambda binder in a table leaf, or use a constant.** With
  bare binders (`fun V ops x => …`), defining the table is quadratic. This
  was measured, not investigated; postponed elaboration of the bare lambdas
  is the likely cause. Cost of defining one table:

  | binders | 512 entries | 1,024 | 2,048 | 4,096 |
  |---|---|---|---|---|
  | bare (`scratch/TB*.out`) | 84 Mhb | 317 Mhb, 26.5 s | ≈1,226 Mhb, 100 s | — |
  | annotated (`scratch/TA*.out`) | 9.5 Mhb | 19.6 Mhb | 41.4 Mhb | 91.3 Mhb, 8.2 s |

- Table leaves never contain `match`: each would add its own auxiliary
  matcher definition. Use one constructor function per kind (`regW`,
  `wireW`, `regU`, `wireU`).

### C3. `comb` goes through per-kind arm tables, built from per-port definitions

```lean
def enR_6 (ops : Ops V) (E : EqC V) (oc : OC V) (w : Wires Unit V) : Bool :=
  WFback ops E oc w && ops.p ((oc (wI 5)).ret WireM.wget)            -- a per-port definition
def argR_6 … : V := ops.h ((oc (wI 5)).ret WireM.wget)
def armR_6 (V : Type) (ops : Ops V) (E : EqC V) (oc : OC V) (w : Wires Unit V) : Wires (CRegM 1) V :=
  regW (enR_6 ops E oc w) (argR_6 ops E oc w) ops.z                    -- per-kind builder
def comb (ops : Ops V) (E : EqC V) (oc : OC V) (w : Wires Unit V) :=
  (fun j => (arms j.1).get j.2.val V ops E oc w, ⟨fun _ => true, fun _ => retPeek ops E oc w⟩)
```

- **Definitions.**
  - Each child input's enable and argument is its own definition.
  - So is each shared comb node: `WILL_FIRE`, `CAN_FIRE`, mux selects
    (`WFgo`, `WFback`).
  - Each parent output is its own definition (`retPeek`).
- **Projections.** One builder per kind assembles an instance's `Wires`.
  Its projections are definitional, so they are the "rfl projection lemmas".
  They are stated once per kind if a proof needs to rewrite with them, never
  once per instance.
- **No proof mentions `comb`.**
  - Per-instance lemmas are about the arm constant (`armR_6`) and its
    per-port definitions.
  - The only step from `comb` to an arm is definitional: `Tab.get` on a
    closed table, O(log n), inside the dispatch's `exact` (C4).
  - Nothing does `simp [comb]` or `cases j` over the children.

### C4. Per-instance facts, assembled by a balanced proof tree

For each fact family:

- one predicate, over kind, index and arm:
  - `EAQ` for `Cert.en` together with `Cert.arg`;
  - `DQ` for `Acyc.dep` on path-bearing inputs;
- one lemma per instance;
- one balanced proof term per kind, mirroring the arm table;
- one dispatch lemma.

```lean
theorem eaR_6 … : EAQ ops E u .reg 6 armR_6 := by …
theorem eaAllR … : Tab.AllR (EAQ ops E u .reg) armsR 0 1027 := ⟨⟨Tab.one (eaR_0 ops E u), …⟩, …⟩
theorem eaAll … (κ : Kind) (i : Nat) (h : i < nk κ) : EAQ ops E u κ i ((arms κ).get i) := by
  cases κ                                                             -- one case per kind
  · exact Tab.get_allR _ 0 _ (eaAllR ops E u) i (Nat.zero_le _) h
  · exact Tab.get_allR _ 0 _ (eaAllW ops E u) i (Nat.zero_le _) h
-- in the Cert:
  en oc w j cm hE hcm := by obtain ⟨κ, i, hi⟩ := j; exact (eaAll ops E u κ i hi hi oc w hE).1 cm hcm
```

Inside a per-instance lemma:

- ports are split by a per-kind lemma (`regAll`, `wireAll`);
- each port takes three or four library applications with explicit
  arguments:
  - `en_one` or `en_none`;
  - `calls_one`, whose three static side conditions are each `rfl`;
  - `and_iff_of_eq (a := …) (f := …)`;
  - `arg_one`, with a value built by `congrArg`/`congr2` from the value
    lemmas (C6).
- No per-port proof relies on higher-order unification. The generator passes
  the per-port definition `e`, the node `a` and the function `f`, because it
  knows them.
- Dispatch predicates contain no `let`: `intro` would introduce it.

### C5. `Acyc` read lists are filters of one complete list

| input | `D`, `Dw` | `dep` proof |
|---|---|---|
| no declared path (registers, FIFOs, every read port) | `allOuts`, `allIns` | `dep_of_all`: agreement on everything is equality; no per-port work |
| path-bearing (wire `wset`, CReg write below a read, FIFOL1 `deq`, child methods with paths) | `allOuts.filter (rank < bound_p [&& reach ⊆ R_p])` | per instance: one decided fact `rk y < r` (`hR y rfl`) per direct read, plus one agreement lemma per shared comb node |
| parent output `q` | `Do q := allOuts.filter (reach ⊆ declared paths of q)`, `Dwo q` := its declared path inputs | per output, the same way |

- `rk_ok` and `path_o` follow from `List.mem_filter`, with no check.
- `reach y` is `[]` for every output that is not downstream of a parent
  input through a path-bearing child. That is every output in the probe, so
  the filter's `reach ⊆ R_p` part is specified here but not exercised.
- Read lists are never written out as lists. A written list costs one
  membership proof per entry, and the entries are the transitive fan-in
  through `comb`'s internal nodes, which the review measured at inputs ×
  outputs × 0.25–0.45. A filter costs one decided fact per **direct** read.
  - **Node agreement lemmas** carry the transitive part, once per node:

    ```lean
    theorem agWFgo … (hR : ∀ y, Nat.blt (rk y) 1 = true → AgreeO (oc y.1) (oc' y.1) y.2) :
        WFgo ops E oc w = WFgo ops E oc' w' := congrArg ops.gd (hR ⟨rI 0, Reg.read⟩ rfl).2
    ```

Corpus (`gen/fanin_path.py` on `hv/new4`):

- Path-bearing inputs are action ports that a value port of the same child
  "sees same-cycle", on kinds with a combinational path: wires, CRegs,
  FIFOL1, Counter and generated children.
- "Direct facts" counts the direct child-output and node reads in the
  path-bearing inputs' cones. Under the filter form, the proof work is the
  node lemmas plus the direct facts. A written list needs, in addition, one
  membership proof per entry (Σ|D|).

| design | child inputs | Σ\|D\| all inputs (review's measure) | path-bearing inputs | their Σ\|D\| as written lists | node lemmas | direct facts |
|---|---|---|---|---|---|---|
| mkBufferControl | 133 | 2,871 | 0 | 0 | 0 | 0 |
| mkDeblockFilter | 164 | 1,808 | 0 | 0 | 0 | 0 |
| mkAes | 180 | 4,115 | 10 | 57 | 19 | 53 |
| sysArithPipe | 197 | 8,685 | 145 | 6,826 | 167 | 534 |
| sysFloatTest | 168 | 11,385 | 72 | 4,381 | 47 | 217 |
| sysTrafficBRAM | 1,696 | 184,219 | 1,002 | 144,087 | 649 | 2,591 |
| sysBRAM0Test | 1,119 | 445,699 | 459 | 180,409 | 447 | 1,736 |
| sysConflictFreeOKLarge | 7,178 | 512 | 6,150 | 512 | 2,055 | 8,199 |

So restricting written lists to path-bearing inputs (M2's fix as stated)
removes 20–100 % of Σ|D|: 21 % for sysArithPipe, 62 % for sysFloatTest, all
of it for mkBufferControl. It does not save the BRAM designs, which keep
144k and 180k entries. Filters with node lemmas need 649 + 2,591 and
447 + 1,736 proof steps there.

### C6. Steps are numbered site tables

```lean
def keyGo : Tab (Σ j : I, (CI j).M) := …                              -- static: child and port per site
def sfGo  : Tab ((V : Type) → Ops V → EqC V → SF V) := …              -- condition and argument per site
def siteGo (ops : Ops V) (E : EqC V) := mkSite keyGo.get (fun i => sfGo.get i V ops E)
  rbody := fun r => match r with | .go => runSites u (fun _ => some ops.z) (sitesF (siteGo ops E) kGo) 0 u | …
theorem rbGo … : Tab.AllR (fun i F => SFReads i (F V ops E)) sfGo 0 kGo := ⟨…, Tab.one (sfr_ca (fun x => E.eqc x 3) ops.g 0 7 (by decide) (by decide)), …⟩
theorem hOKGo … : ∀ i, i < kGo → ReadsBefore i (siteGo ops E i) := fun i hi => Tab.get_allR _ 0 kGo (rbGo ops E) i (Nat.zero_le _) hi
theorem trGoF … : trR (P ops E u) oc .go = (callsOf (envHw …) (sitesF (siteGo ops E) kGo), some ops.z) := trace_fn u oc _ (siteGo ops E) kGo (hOKGo ops E)
theorem evGo_7 … : envHw u oc (sitesF (siteGo ops E) kGo) 0 u 7 =
    if E.eqc (envHw u oc (sitesF (siteGo ops E) kGo) 0 u 0) 3 = true then (oc (rI 4)).ret Reg.read else u 7 :=
  (envHw_fn u oc (siteGo ops E) kGo (hOKGo ops E) 7 (by decide)).trans rfl
```

- **Conditions** follow decision 1.
  - An action call's condition is its exact enable.
  - A read is unconditional unless the static scan needs a condition. Then
    its condition is the hardware's exact one when there is one (a shared
    copy's mux select). Otherwise it is schedlean's conservative reach
    condition:
    - `&&` and `||` count both operands unconditionally;
    - only an arm of an if, case, mux or array selection is conditional on
      its select.
  - The generator fixes the site order: data dependence, then the child's
    SB order. It drops any select literal that reads a site not earlier in
    that order, so `SFReads` holds by construction.
- **Proofs per site:**
  - one `SFReads` leaf, by a combinator (`sfr_const`, `sfr_c`, `sfr_ca`;
    more shapes are added the same way);
  - one value lemma per read site whose value is used.
- **Never `List.filter`, `List.get` or `ss[i]` on a literal site list.** The
  site-list probe of round 5 did, which is its k^1.4–1.8.
- **Conditional reads:** the contextual rewrite is `if_pos` under the site's
  condition. In the probe, `w_k.wset`'s argument for a conditional read of
  `r_k` is `congrArg ops.g ((evGo_i …).trans (if_pos hc))`.

### C7. Ports' uses: one table, one decided check per step

Each instance's leaf lists every port's uses `(step, site index)`, built by
a per-kind constructor (`regU rd wr`, `wireU set get`). The library derives
`users` (`usersOf`) and each step's per-port index list (`idxOf`):

```lean
def usesAt (p : Σ j : I, (CI j).M) := ((usesK p.1.1).get p.1.2.val).uses p.2
def users (j : I) (cm : (CI j).M) := usersOf (usesAt ⟨j, cm⟩)
def idx (s : St) (p : Σ j : I, (CI j).M) := idxOf (usesAt p) s
theorem chkGo : Tab.allB (fun i p => (idx (.inr .go) p).contains i) keyGo 0 kGo = true := by decide +kernel
```

- `chk_spec` turns the check into "every site is in its port's index list".
- `mem_usersOf` gives `Cert.users_ok`.
- `calls_one`, `calls_none` and `arg_one` give the per-port facts. Their
  static premises are `rfl`:
  - `idx s ⟨j, cm⟩ = [i0]`;
  - `Nat.blt i0 k = true`;
  - `key i0 = ⟨j, cm⟩`.

### C8. Kernel checks

- Decided facts are closed `Bool` computations over a table (`Tab.allB`),
  proved by `decide +kernel`. Their recursion depth is the table's depth.
- Never make the kernel iterate with `Nat.rec` or list recursion over
  thousands of elements. A 5,000-step `Nat.rec` loop hits "deep recursion".
- `decide` refuses goals with free variables, so a check is a top-level
  theorem about closed data: keys and uses, never `ops`.
- Never `decide` a `List.Nodup` (quadratic).

### C9. Probe-local stand-ins (not conventions)

These keep the probe's `Cert` complete. They belong to other pieces of this
pass.

- **`grpR`/`grpM`:** each step calls a child at most once. `nodup_of_mono`
  then uses a child numbering that increases along the step, checked by the
  kernel. The general one-caller fact is the scan's.
- **`h1`, `h4`:** port classes (`okT_of_cls`). A static argument fit for two
  or three rules. The general pair fields are `cert_of_scan`.
- **`peek C peek`:** exported to make `mm`/`mmg` vacuous.
- Every child port has at most one user, so ports with many users (an
  argument mux over u users, Σ users² in the review) are not measured here.

## Measurements

`out/series.txt`; `gen/runseries.sh 16 32 64 128 256 512 1024`.

- Each row is a whole module file: `Obl`, `Acyc`, `Built.top`, all with the
  standard three axioms (`std_axioms=2` means both `#print axioms` lines show
  exactly them).
- The machine was shared: load average 20–24 on 32 cores (bazel, ghc, bsc,
  other Lean runs). So wall time is noisy, and user time and heartbeats are
  the better guides.

| m | children | sites in `go` (one step) | sites in all | lines | `.lean` | `.olean` | wall | user | peak RSS | user ×, per doubling |
|---|---|---|---|---|---|---|---|---|---|---|
| 16 | 35 | 33 | 67 | 1,601 | 0.10 MB | 3.6 MB | 4.9 s | 4.6 s | 0.62 GB | — |
| 32 | 67 | 65 | 131 | 2,657 | 0.18 MB | 5.7 MB | 9.8 s | 8.7 s | 0.70 GB | ×1.90 |
| 64 | 131 | 129 | 259 | 4,769 | 0.33 MB | 9.7 MB | 21.7 s | 18.3 s | 0.86 GB | ×2.11 |
| 128 | 259 | 257 | 515 | 8,993 | 0.63 MB | 17.9 MB | 31.8 s | 31.6 s | 1.15 GB | ×1.72 |
| 256 | 515 | 513 | 1,027 | 17,441 | 1.24 MB | 34.2 MB | 67.8 s | 66.3 s | 1.76 GB | ×2.10 |
| 512 | 1,027 | 1,025 | 2,051 | 34,337 | 2.46 MB | 66.9 MB | 169.0 s | 143.6 s | 3.08 GB | ×2.17 |
| 1024 | 2,051 | 2,049 | 4,099 | 68,129 | 4.92 MB | 132.3 MB | 352.5 s | 304.8 s | 7.11 GB | ×2.12 |

- **Growth.** User time from 32 to 1,024 (×32 in children and in sites)
  grows by ×35, about n^1.03. Per doubling it is ×1.7–2.2, and wall time
  behaves the same.
- **File size.** The `.lean` file grows linearly, at about 1.2 KB per site,
  where round 5's site form needed 1.7 KB. The `.olean` file is also linear
  but large: about 32 KB per site, 132 MB at m = 1024.
- **Memory.** It grows linearly, plus ~0.5 GB of imports. At m = 1024 the
  peak is 7.1 GB.
- **Kernel replay**, `leanchecker Em<m>` (the module's declarations,
  without `--fresh`), is linear and small next to elaboration:

  | m | `leanchecker Em<m>` |
  |---|---|
  | 64 | 1.96 s, 0.50 GB |
  | 256 | 6.58 s, 0.57 GB |
  | 1024 | 25.9 s, 0.92 GB |

- **One wide step.** The 2,049-site `go` step at m = 1024 needs no special
  treatment.
  - No term in the file scans a step's site list.
  - The per-site sections below (value lemmas, `SFReads` trees, per-instance
    lemmas) double with m, whatever the step's width.

Per section, in heartbeats (allocations ×10⁶, load-independent) and wall
time (`prof3/`, `MARK=1`):

| section | m=16 | m=64 | m=128 | m=256 | ×, 128→256 (Mhb) |
|---|---|---|---|---|---|
| ports' uses tables | 1.8 Mhb, 0.1 s | 5.8 Mhb, 0.6 s | 11.3 Mhb, 1.0 s | 22.3 Mhb, 2.3 s | 1.97 |
| site key tables | 1.4 Mhb, 0.1 s | 5.0 Mhb, 1.2 s | 9.8 Mhb, 1.1 s | 19.4 Mhb, 2.1 s | 1.99 |
| decided checks (kernel), `hokC` | 4.0 Mhb, 0.6 s | 6.1 Mhb, 1.0 s | 9.0 Mhb, 2.3 s | 15.2 Mhb, 3.4 s | 1.69 |
| per-port definitions, arms | 3.9 Mhb, 0.6 s | 15.2 Mhb, 2.1 s | 30.2 Mhb, 4.5 s | 60.1 Mhb, 7.9 s | 1.99 |
| arm and site tables, `comb` | 2.5 Mhb, 0.2 s | 8.1 Mhb, 1.1 s | 15.6 Mhb, 2.0 s | 30.9 Mhb, 2.8 s | 1.97 |
| `SFReads` trees | 1.5 Mhb, 0.2 s | 5.4 Mhb, 0.8 s | 10.7 Mhb, 1.1 s | 21.2 Mhb, 2.8 s | 1.99 |
| `P`, trace lemmas | 1.5 Mhb, 0.1 s | 1.6 Mhb, 0.2 s | 1.6 Mhb, 0.2 s | 1.6 Mhb, 0.2 s | 1.01 |
| value lemmas (one per read site) | 2.8 Mhb, 0.5 s | 11.5 Mhb, 1.5 s | 23.9 Mhb, 3.3 s | 49.6 Mhb, 5.7 s | 2.08 |
| per-instance `en`/`arg` lemmas | 32.3 Mhb, 2.9 s | 139.5 Mhb, 16.4 s | 292.8 Mhb, 30.9 s | 615.3 Mhb, 53.0 s | 2.10 |
| `Tab.AllR` trees, `eaAll` | 0.7 Mhb, 0.1 s | 2.2 Mhb, 0.3 s | 4.3 Mhb, 0.6 s | 8.4 Mhb, 0.6 s | 1.96 |
| the `Cert`, `obl` | 3.5 Mhb, 0.5 s | 3.5 Mhb, 0.6 s | 3.5 Mhb, 0.4 s | 3.5 Mhb, 0.3 s | 1.00 |
| `Acyc` lists, node and `dep` lemmas | 2.5 Mhb, 0.4 s | 7.4 Mhb, 0.9 s | 13.8 Mhb, 1.6 s | 26.6 Mhb, 2.5 s | 1.93 |
| `acyc`, `built`, `top` | 2.2 Mhb, 0.2 s | 3.3 Mhb, 0.3 s | 4.6 Mhb, 0.4 s | 7.3 Mhb, 0.6 s | 1.59 |
| **sum** | 61 Mhb | 215 Mhb | 431 Mhb | 881 Mhb | 2.04 |

- **Every section is linear.** The two that grow ×2.08–2.10 per doubling
  carry a log factor, from table depth. Sections that do not depend on the
  size stay flat: the `Cert` itself, `P`.
- **Per-instance lemmas** are 70 % of the work: about 0.8 Mhb and 70 ms per
  instance. They are the place to optimize next, by shorter elaborated
  statements per port. Linearity does not depend on that.

**Rough estimate for the parts measured here.** These are the per-port
facts, site lists, arms and `Acyc`.

- The probe's slope at large m is 0.31 s of user time per m-unit. An m-unit
  is two children, four sites and four used ports, so about 0.08 s per site
  together with its port's facts.
- sysConflictFreeOKLarge:
  - The review's site count is 6,667 sites (`sitecost.py`, `reads`/`calls`
    lines), on 4,103 ports and 3,589 children.
  - At 0.08 s per site that is about 9 minutes, with no quadratic term.
  - The review estimated about 6 h for the round-5 site form, and today's
    flat file takes 382 s.
- What the estimate leaves out:
  - the pair fields (`cert_of_scan`);
  - `skel`;
  - ports with many users (sysFloatTest: 115 users on one port, Σ users²
    258k);
  - FIFO-like kinds with more ports.

## For comparison (round 5 and its review)

| probe | 32 | 64 | 128 | 256 | growth per doubling |
|---|---|---|---|---|---|
| review `Comb<n>_toy` (`cases j <;> simp [comb]`, `dep` only) | 7.7 s | 44.8 s | 332.9 s | — | ×5.8, ×7.4 |
| review `Comb<n>_arm` (per-arm, `dep` only) | 0.85 s | 1.56 s | 3.44 s | 8.81 s | ×1.8, ×2.2, ×2.6 |
| round 5 `Sites<k>` (one step of 2k+1 sites, full `Obl`) | 12.6 s (65 sites) | 33.8 s (129) | 118.8 s (257) | — | ×2.7, ×3.5 |

## Library additions (`EmitLemmas.lean`, imports `Modular`, unchanged)

- **Tables:** `Tab`, `Tab.get`, `Tab.get_node`, `Tab.AllR`, `Tab.get_allR`,
  `Tab.one`, `Tab.allB`, `Tab.allB_spec`, `blt_true`, `blt_false`.
- **Numbered sites:** `mkSite`, `sitesF`, `sitesF_length`, `sitesF_get`,
  `sitesF_map`, `sitesOK_range'`, `sitesOK_fn`, `SF`, `SFReads`,
  `sfr_const`, `sfr_c`, `sfr_ca`, `envHw_fn`, `trace_fn`,
  `callsOf_fn_ports`, `mem_sitesF`, `site_mem_sitesF`, `mem_keys`,
  `calls_all`.
- **Per-port facts:** `onPort_iff`, `mem_proj_fn`, `calls_fn`, `calls_one`,
  `calls_none`, `arg_one`, `en_one`, `en_none`, `and_iff_of_eq`, `congr2`.
- **Uses:** `idxOf`, `usersOf`, `mem_usersOf`, `chk_spec`.
- **Children:** `allKids`, `mem_allKids`, `allKids_nodup`, `allPorts`,
  `mem_allPorts`.
- **`Acyc`:** `forall_filter`, `Wires.ext'`, `dep_of_all`, `AllL`, `allL_of`.
- **Probe-local** (C9): `pairwise_range'`, `nodup_of_mono`, `mono_spec`,
  `okT_of_cls`.

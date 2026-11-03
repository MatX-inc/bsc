# Emission conventions for linear per-module cost

schedlean's emitter (`src/comp/Lean/Sched/Emit*`) writes files in these conventions. Where the notes
name `mke2e.py` or `scanlib.py`, they mean the Python prototype it was ported from (retired in round 9).

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

## Round 7: wide ports, large tables, escapes (C10–C15)

These replace C7 and extend C2 and C4. They are used by `e2e/tools/mke2e.py`
and measured by the probes named below (round 7 cost review, majors 1–2 and
minors 4–6). Library lemmas are in `EmitLemmas.lean` and `Scan.lean`.

### C10. No declaration grows with a table

`stab(name, ty, items)` in the emitter writes a balanced table whose
subtrees of at most `E2E_SPLITW` source characters (default 20,000) are their
own definitions `name_lo_hi`; the top definition only links them. A proof
over such a table (`stabproof`) has one theorem per subtree definition, in
the same shape, so no theorem grows either (`tqAll`, `sq_k`, `rqT`, `sqT`,
the mux chains). Every per-rule, per-statement and per-instance table is
emitted this way: footprints (`fpRT`), site keys and functions (`key_k`,
`sf_k`), results (`resT`), guards (`gT`), checks (`chkLst`, `chkHead`),
uses (`uses_<kind>`, `tusesT`), task enables and arguments (`tenT`,
`targT`), groups (`memT`, `gidT`), masks and codes (`exRT`, `rcT`,
`rinvT`). The scan probes do the same (`scan/tools/scanlib.py`:
`emit_table`, `emit_design`, checkpoint subtrees in `emit_tree`).

The whole module is a `noncomputable section`: nothing in it is run by the
compiler, so no table is compiled.

Measured on the task-chain probe (`scan/tasks/`, checkpoint every 64 rules),
whole file:

| statements | wall | kernel | peak RSS | round 6 (review) wall, RSS |
|---|---|---|---|---|
| 10,240 | 14.8 s | 10.0 s | 1.05 GB | 19.5 s, 1.27 GB |
| 20,480 | 30.4 s | 20.9 s | 1.53 GB | 44.4 s, 1.97 GB |
| 40,960 | 65.6 s | 46.6 s | 2.58 GB | 129.1 s, 3.51 GB |
| 81,920 | 139.6 s | 99.0 s | 4.69 GB | 434.5 s, 6.64 GB |

×2.05–2.15 per doubling in wall time (was ×2.3–3.4); the part outside the
kernel is 4.5 / 9.2 / 18.4 / 39.5 s, linear.

### C11. A port's uses are a table keyed by rule

```lean
structure Uses (M : Type) where
  uses : M → Tab (Nat × List Nat)   -- leaves (rule, sites), midpoints rule numbers
  us : M → List Nat                 -- the users, as a list (cheap for the elaborator to unfold)
def users (j : I) (cm : (CI j).M) : List St := (usL ⟨j, cm⟩).map (fun k => .inr (rR k))
def idx (s : St) (p : Σ j : I, (CI j).M) : List Nat := match s with | .inl e => e.elim | .inr r => idxT (usesAt p) r.val
theorem usOK_fifo : Tab.allB (fun _ U => [FL.enq, …].all (fun m => usersT (U.uses m) == U.us m)) uses_fifo 0 (nk .fifo) = true := by
  decide +kernel
```

- `idxT t k` is one lookup (O(log users)): the per-user `hx` facts and the
  per-site check `chkAll` no longer walk the port's use list.
- The list `us` is tied to the table by one decided check per kind
  (`usOK_<kind>`, linear); `usersT_eq` and `mem_usersT` give
  `Cert.users_ok`. Without the list, every per-port proof unfolded `usersT`
  in the elaborator, which doubled the per-instance section (sysArithPipe
  without tasks: 654 against 323 Mhb).
- **Ports with more than 8 users** (`E2E_BIGPORT`): the per-user facts are a
  balanced proof over the uses table, `users_all` (from `Tab.All (fun e =>
  e.2.isEmpty = false → Q (.inr (rR e.1))) (usesAt ⟨j, cm⟩)` to `∀ s ∈ users
  j cm, Q s`), and the enable's iff is `en_lit`. A nested `⟨p₁, …, pₙ,
  trivial⟩` against `AllL` elaborates in time quadratic in n (15 s at 512
  entries in the chain probe); the balanced tree is linear. Smaller ports
  keep the `AllL` forms, which are cheaper at a few users.

### C12. A mux is a table; an entry is a lookup

```lean
def mT_q : Tab ((V : Type) → Ops V → EqC V → OC V → Bool × V) := …    -- (WILL_FIRE, argument) per member
def mlist_q (V) (ops) (E) (oc) : List (Bool × V) := (List.range' 0 u).map (fun t => (mT_q.get t) V ops E oc)
-- user at position p:  (arg_muxT ops.z (chain_q …) (p := p) rfl hav).trans (aRk …)
```

`arg_muxT` gets `l[p]?` from `List.getElem?_map`/`getElem?_range'` and a
table lookup, not a walk of p list cells.

### C13. A mux's chain is one decided pass over cover sets

```lean
def msT_q : Tab (Nat × Nat) := … (k, cvS (chkLst.get c) [uncovered later members]) …
theorem chain_q … : amoChain false ((mlist_q V ops E oc).map Prod.fst) :=
  amoChain_of_cover (fun k => WF ops E oc k) R hR msT_q [members] (by decide +kernel) (by decide +kernel)
    ⟨⟨cover_of (chkC ops E oc hA c rfl) ⟨pairwise proofs of the uncovered⟩, …⟩, …⟩
```

- `covB` walks the members from the last, keeping the bit set of the later
  ones, and checks it is inside each member's cover set: O(members + the
  check lists' entries) in the kernel, where round 6 decided
  `T.all (cov.contains ∨ unc.contains)` per member (cubic).
- `cvS` (a member's check bits OR its uncovered members' bits) is
  `@[irreducible]`: only the kernel computes it. Left reducible, the
  elaborator evaluated each member's check list while matching the proof
  (15.5 s against 1.5 s at 512 members).
- `bits` and `covB` are defined by the recursor (`noncomputable`), which
  the kernel reduces directly (4× faster than structural recursion here).
- Only the pairs no check covers keep a proof each.

Chain probe (`scan/chain7/genchain7.py`, the review's shape: member k's
check lists all later members, so the checks alone are u²/2 entries):

| members | chain section (wall) | chain theorem: kernel, elaboration | whole file wall, peak RSS | round 6 (review): wall, kernel, RSS |
|---|---|---|---|---|
| 64 | 0.3 s | —, 0.12 s | 0.84 s, 0.52 GB | 2.9 s, 1.75 s, 0.57 GB |
| 128 | 0.6 s | —, 0.26 s | 1.60 s, 0.58 GB | 21.1 s, 16.5 s, 0.79 GB |
| 256 | 1.3 s | 0.13 s, 0.53 s | 4.57 s, 0.76 GB | 172.0 s, 152 s, 1.78 GB |
| 512 | 3.0 s | 0.81 s, 1.17 s | 14.2 s, 1.42 GB | — |
| 1,024 | 7.5 s | 2.74 s, 2.58 s | 51.2 s, 3.94 GB | — |

(`scan/chain7/logs/`; "—": below the profiler's 100 ms threshold.) The
chain section grows ×2.0–2.5 per doubling; its kernel part is the `covB`
pass reading the u²/2 check entries. The rest of each file is the check
lists themselves, the module's assertions (10.7 s at 512, 42.5 s at 1,024).

### C14. Rule codes in the scan

`Design.rc` (default `fun i => i`) numbers the rules for the scan's rule
bit sets, `exR` and `escR`. The emitter gives every member of an exception
group its own code and all other rules code 0, which no mask contains;
`chains_inj` (two decided checks over an inverse table `rinvT`) gives the
injectivity `exR_of_chains` needs. A rule's own bit is no longer recorded
before its check (pass 2 records it), so code 0 can be shared. The slot
records in checkpoints no longer grow with the number of rules.

### C15. Escapes over numbered site tables

`okT_except_fn` proves an escaped pair's `OkT` from the two steps' key
tables: for each site of the first step and each port of its child, either
the port order is fine (`okP`) or every site of the second step on that port
(its index list, `idxT`) pairs with it in the short `bad` list, whose pairs
the conditions exclude (`hbad`, one proof per bad pair). The check is one
`Tab.allB` over the first step's keys.

Probe `scan/esc7/genescfn.py` (two W-site steps, one bad pair):

| sites per step | escape (check + theorem) | of which the escape check (kernel) | the second step's index check (kernel) |
|---|---|---|---|
| 512 | 0.8 s | 0.42 s | 0.32 s |
| 2,500 | 6.0 s | 3.27 s | 2.41 s |
| 5,000 | 10.1 s | 5.37 s | 4.20 s |

So an escape costs about 1.1–1.3 ms per site of its first step, linear; the
index check is the one every step already has (`chkAll`).

## For comparison (round 5 and its review)

| probe | 32 | 64 | 128 | 256 | growth per doubling |
|---|---|---|---|---|---|
| review `Comb<n>_toy` (`cases j <;> simp [comb]`, `dep` only) | 7.7 s | 44.8 s | 332.9 s | — | ×5.8, ×7.4 |
| review `Comb<n>_arm` (per-arm, `dep` only) | 0.85 s | 1.56 s | 3.44 s | 8.81 s | ×1.8, ×2.2, ×2.6 |
| round 5 `Sites<k>` (one step of 2k+1 sites, full `Obl`) | 12.6 s (65 sites) | 33.8 s (129) | 118.8 s (257) | — | ×2.7, ×3.5 |

## Library additions (`EmitLemmas.lean`, imports `Modular`)

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
- **Round 7** (C11–C12): `Tab.toList`, `Tab.get_mem`, `Tab.All`, `Tab.all_mem`,
  `idxT`, `usersT`, `mem_usersT`, `usersT_all`. In `Scan.lean`: `covB`,
  `cvS`, `cover_of`, `amoChain_of_cover`, `arg_muxT`, `en_lit`,
  `okT_except_fn`, `rc_inj`, `chains_inj`.

## C16. Round 7: the family statement in emitted files

- A file defines `F : ParentU V` whose step programs are `runSites v … v` of
  the step's own skipped values (`body m x v`, `rbody r v`); `P u := F.at u`.
- `certU : CertU F` (rules, firing and hits fixed before `u`; `cert u`; `rfl`
  equations) and `obl : OblU F := oblU_of_cert _ certU`; `acyc : Acyc F.base`;
  `top : RealizesU (F.toModuleU …)`.
- `trU` from a per-site tree `HQ` (each site's condition and argument agree at
  two skipped values when the step may run), dispatched by `callsOf_fn_congr`.
  Where a guarded site is conditional, a second tree `RY` gives each taken
  site's readiness (CAN_FIRE, the guard's lifted `!c ∨ RDY`, or the child's
  alias for a readiness read).
- Value lemmas take the step's argument `x` (rules pass `z`), so each lemma's
  environment is syntactically the step's; with `envR` against an `envS … x`
  goal the 60-rule file's `HQ` section took 52 s, with matching lemmas 3.1 s.
- Rules and methods share one table index (rules first, method `m` at
  `nR + m`); uses tables are keyed by that index.
- Mux selectors carry the users' call conditions where a user is conditional
  or no recorded exclusion covers a pair; their one-hot chains are pairwise
  over selectors, from static exclusions or solver-confirmed caller facts.
- Escapes: `okT_exceptZ` (an fx-aware `okT_except_fn`) with a closed decided
  table check per escaped pair and one dynamic fact per bad site pair.

## Round 7 revision (C17–C19; reviews r7-1)

### C17. bsc's `mutually_exclusive` groups are stated once (cost review r7-1 M1)

bsc's run-time checks are pairwise: `ASchedule.mkMEPairs` builds every pair
of a group and `mkMEAssump` keys each pair by its first rule, so the check of
member j lists every later member and a u-member group has u²/2 check
entries. The emitter now finds these groups (a chain of checks whose lists
are suffixes of one list) and asserts, as a generator check, that every
check of the group is exactly `(x_j, [x_j+1 … x_n])`. It then writes:

```lean
def grpT : Tab (List Nat) := …                    -- each group's members, bsc's order: O(u) data
def asmP ops E oc : Prop :=
  (∀ g, Nat.blt g nG = true → SufAsm (WF ops E oc) (grpT.get g)) ∧     -- the group's checks
  ∀ c, Nat.blt c nC = true → (WF … (chkHead.get c) && orL …) = false  -- other checks, one by one
def G_k … := WFd_head … && orL (((grpT.get g).drop (j+1)).map (fun y => (wfdT.get y) V ops E oc))
theorem chain_q … := amoChain_of_grp (fun k => WF ops E oc k) [members] (grpT.get g) (by decide +kernel) (hA.1 g rfl)
```

- `SufAsm f (x :: y :: l) = (f x && orL ((y :: l).map f)) = false ∧ SufAsm f (y :: l)`
  (`Scan.lean`): exactly the group's checks, so the module's assertions are
  still bsc's list (trust point 3).
- `sufAsm_pairwise` (induction, proved once) gives every pair; a mux's
  members of the group, in group order, are a sublist decided in one pass
  (`List.isSublist`), with no per-member data or decision
  (`amoChain_of_grp`).
- A mux with members outside the group (sysArithPipe's two largest)
  places them first, with cover sets as in C13, and `covB` starts from the
  group members' bits (`amoChain_of_cover_grp`); the pairs with a group
  member are proved one by one, as before.
- A pair inside a group, needed elsewhere, comes from `meG` (positions in
  `grpT.get g`, decided).
- Checks that are not group suffixes keep C13's form. sysArithPipe's 94
  checks are two groups (51 and 45 members); mkBufferControl has none.

### C18. The scan in segments (cost review r7-1 M2)

`mke2e.py` cuts the scan before a step that would take a segment past
`E2E_SEG_USES` footprint entries (default 4,096, the gate's bound) and writes
each checkpoint as the full state there (`scanlib.emit_tree`, subtree
definitions), one decided `seg_j` per segment, `segZ` for the last, and
`scanOK := D.ok_of_chain [(m_j, cp_j)…] ⟨seg_0, …, segZ⟩`. A step with more
uses than a segment fails a generator assertion (the gate's uses-per-step
bound). `SysArithPipe60.lean` is emitted with `E2E_SEG_USES=128` (several
segments) as the regression test; the full sysArithPipe with 600-use
segments (4 segments) is measured in design.md §13.

### C19. Readiness facts over site values, and steps by `rfl` (soundness review r7-1)

- A readiness `R` reads the site values of the step's conditional reads
  (the read itself included). Facts: `rdy_n : ∀ oc t…, WF k → ops.g n (…
  (if RDY_y then port_y else t_y) …) = true → RDY_x` and, where `R` reads
  another conditional read, `inv_n : ∀ oc t…, WF k → ops.g n (site values)
  = ops.g n (ports)`. `evs_k_j` rewrites a site value to that `if`; a split's
  selector proof is `congrArg … evs … ▸ inv_n` (or `inv_self` from `rdy_n`
  when `R` reads only its own site). Each fact's label is the site tool's
  verdict for that formula; an unconfirmed one goes to `Unconfirmed`.
- An `R` literal over a read not before some site that splits on `R` is
  dropped and counted, and the tool is re-asked for the formula emitted
  (0 in mkBufferControl).
- Escape facts are labelled per fact: the solver over the two steps'
  `WILL_FIRE` and the two sites' conditions as the file states them (all 50
  of mkBufferControl's), or bsc's confirmed fact.
- `F_sites : F ops E = withSitesX (F ops E) …` (or `withRuleSites`) by `rfl`
  (`EmitLemmas.lean`): each step's program takes its skipped values only
  through `runSites`; its sites and result receive the method's argument,
  never `v`.

### Large steps (Em series, cost review r7-1 M2 (c))

`out/Em512.lean` and `out/Em1024.lean` are regenerated with `trU` (byte-identical
to the cost reviewer's: 132.5 / 293.2 s user, 3.60 / 8.13 GB for a 1,025- and a
2,049-site step). `out/Em1536.lean` (a 3,073-site step): 517.1 s wall, 476.3 s
user, 12.9 GB at load 21 (`out/Em1536.log`). `out/Em2048.lean` (a 4,097-site step)
did not complete: exit 1 after 1,251 s wall, 812 s user, 294 s system, 16.9 GB peak
RSS under the 24 GB address-space cap, at load 45–60 (`out/Em2048.log`). The gate
bounds a step at 3,072 sites (design.md §10), and `mke2e.py` asserts it
(`E2E_STEP_SITES`).

## Round 8 (C20–C24; cost review r7-2)

The emitter with these conventions is schedlean's (`src/comp/Lean/Sched/Emit.hs` and `Emit/*.hs`), ported
in round 9 from the Python prototype `e2e/tools/mke2e.py` (the round-8 merge), which the notes below still
name; the probes and their logs are in `cost/`. C25 and C26 below are the merge's other pieces.

### C20. Exception groups decided against one mask per group (M2)

`cmT : Tab Nat` holds each group's code mask, `bits` of its members' codes, checked once
(`cmAll`, `Scan.cm_of_check`). Membership of a rule in its groups is a mask lookup
(`ghg`: `testBit (cmT.get g) (D.rc x)`, `Scan.gid_of_check`), and the exception masks are subsets
of an OR of group masks (`ghT` over `cmT.get`). `Scan.exR_of_cmask` gives `GlueOK.exR` from these
and the members' injective codes, so no decision walks a group's member list once per member
(the old `ghg` was O(u²) list steps in one decision).

Every decision over a whole table (`stMono`, `chainAll`, `chkAll`, `fpAll`, `fpAllM`, `cmAll`,
`ghg`, `ghT`) is one decision per subtree definition of the table (`allB_thm`, combined by
`EmitLemmas.Tab.allB_node`); the task-chain probe does the same for `inT` and `monoT`
(`scanlib.emit_allB`).

### C21. At most 128 entries per table definition

`split_struct` also cuts a table at 128 entries (`E2E_SPLITN`), so a proof over a table
(`stabproof`) has at most 128 leaves per declaration. Before, a table of short entries (`gT`, the
guards) stayed one definition, and `rqT` over it was one declaration with a leaf per step
(1,647 leaves in the 824-user port probe). Per-kind arm tables are split the same way, and
`eaAll_κ` / `dqAll_κ` follow them (`stabproof`).

### C22. Two kernel and unifier blow-ups in the emitted proofs

- `certU.rules` is stated as `(List.range nR).map rR`, not `(G ops E z).rules`. Checking
  `rules_eq` by `rfl` made the kernel compare `P ops E u` with `P ops E z`; with one rule every
  one-leaf table lookup reduces for a symbolic index, and the comparison unfolded the whole step
  program (an 8-site single-rule module: 17.2 s of kernel time in `certU`; 128 sites: over 20 GB).
- `RD` (a step's ready signals, ANDed into its `CAN_FIRE`) is `@[irreducible]`; the three proofs
  that read it `unfold RD`. Unifying a port's enable with its user's `stepOn` used to evaluate `RD`
  over the whole step, once per child instance: a 1,024-site step spent 645 s in the per-instance
  lemmas and then ran out of memory.

### C23. A module in parts (whole-file bound)

A file above 4 MB of source (`GATE_PART`) is cut into parts of about 2 MB (`E2E_PART_BYTES`), each
importing the one before (`NAME_p0.lean`, …, then `NAME.lean`, which holds `obl`, `top` and the
`#print axioms`). Cuts fall between top-level declarations; a cut inside a `section` closes it
and reopens it (`section` / `variable {V : Type}`, asserted). The statement and the trust story
are unchanged: the last part's `#print axioms` covers its imports, and `leanchecker --fresh` of
the last part replays all of them. `cost/tools/runparts.sh` builds the parts in order. Peak memory
is the largest part's, not the whole module's.

### C24. The gate's bounds in the file

`mke2e.py` asserts every numeric bound of design.md §11 item 6 before it writes the scan, writes
each bound and the module's value into the file (`/-! Gate bounds … -/`), and appends the parts
line (`/-! Gate: k part(s) … -/`). An environment variable (`E2E_SEG_USES`, `E2E_STEP_SITES`,
`E2E_PART_BYTES`) can lower a bound, never raise it. Probes beyond a bound replace the constant
in a copy of the emitter's source (`PROBE_GATE` in `cost/port/genport8.py`, `cost/big/genbig.py`),
and the file then states the replaced bound. Since review r8-1 (cost minor 7) the list also holds
the escape bound (64 escaped rule pairs, gate item 1) and the table settings `E2E_SPLITN` (128
entries per table definition), `E2E_SPLITW` (20,000 characters) and `E2E_BIGPORT` (8 users listed
entry by entry), each asserted to be at most its default and written into the file (as "setting
used", not a module value, since review r8-2). Since review r8-2 it also holds the facts bound (4,096
dynamic facts in one escaped pair, C27).

### Round 8 measurements (`cost/logs/summary.txt`; GiB; load quoted, 32 cores, shared)

Wide port (`cost/port/genport8.py`: one register written by U rules, bsc's suffix checks, one group):

| users | sites | r7 emitter (cost review r7-2) | C20 only, one file | C20–C22, one file | C20–C23, in parts |
|---|---|---|---|---|---|
| 412 | 1,236 | 112.7–145.9 s user, 3.46–3.78 GB | 106.8 s, 2.70 GB (load 16) | 76.4 s, 3.41 GB (load 12) | 5 parts of 0.4 MB: 115.3 s, 1.25 GB (C20 only) |
| 824 | 2,472 | 312.6–400.1 s, 8.25–9.95 GB | 251.8 s, 5.88 GB (load 14) | 183.0 s, 7.50 GB (load 12) | 3 parts: 227.9 s, 3.60 GB (load 17–24) |
| 1,648 | 4,944 | — | out of memory at 15.0 GB | — | 5 parts: 540.7 s, 3.88 GB (load 24–75) |
| 3,296 | 9,888 | — | — | — | 10 parts: 1,161 s, 4.14 GB (load 20–35) |

- The `glue` transient is gone: at 824 users the peak is the scan's (5.88 GB, C20 only), and `glue`
  falls to 3.40 GB; `cmAll`/`ghg`/`ghT` take 1.51 s at 412 users and 3.65 s at 824 (12 → 27 Mhb).
- One file still grows ×2.2 per doubling in memory (C21–C22 together add about a quarter, and take
  a quarter to a third off the time); parts hold the peak
  at 3.6–4.1 GB from 824 to 3,296 users. User time in parts ×2.37, ×2.15 per doubling, at unequal
  loads; per-section heartbeats in one file grow ×2.0–2.1.

Large steps on this emitter (`cost/big/genbig.py`: one step reading and writing its own registers,
a quarter of its sites task statements, plus a 2-site step), the scan in one segment:

| step | one file | heartbeats, per-instance lemmas |
|---|---|---|
| 514 | 32.9 s wall, 32.7 s user, 1.00 GB (load 13) | 413 Mhb |
| 1,026 | 69.4 / 69.2 s, 1.50 GB | 880 Mhb |
| 2,050 | 148.3 / 147.1 s, 2.69 GB | 1,873 Mhb |
| 4,098 | 3 parts: 503 s wall, 433 s user, 3.32 GB (load 20–31) | — |

Before C22, a 1,024-site single-rule step ran 1,248 s and failed at 19.5 GB; 128- and 256-site ones
also passed 19 GB. Every section's heartbeats double with the step except the scan's (42 → 176 Mhb
from 2,050 to 4,098 uses, one segment), which the segment bound holds.

sysConflictFreeOKLarge's shape (`cost/big/parts/Bcfq*.lean`: steps of 3,074 / 2,562 / 2,050 /
1,027 / 3 sites with 1,024 / 512 / 0 / 513 / 1 statements, 3,334 registers, 8,716 sites, 3 scan
segments): 6 parts, 1,736 s wall, 1,146 s user, 3.36 GB (load 33–62), `obl` and `top` with the
standard three axioms.

Task chain (`scan/tools/gentasksC.py`, `inT` and `monoT` per subtree): 20,480 / 40,960 / 81,920
statements: 36.0 / 65.0 / 143.7 s wall, 1.25 / 1.75 / 2.74 GB (r7: 30.4 / 65.6 / 139.6 s,
1.45 / 2.46 / 4.47 GB); `inT` 2.10 / 3.90 / 8.30 s of kernel time in 8 / 16 / 32 decisions of at
most 0.29 s (r7: one decision, 7.0 and 14.8 s at the last two).

Conditional-user mux (`genport8.py … 1`, every user's call conditional, all users in one group):

| users | chain pair by pair (`PS`, r7 form) | chain from the group (`PT`, `Scan.amoChain_le_range`) | per-instance lemmas (`PT`) |
|---|---|---|---|
| 16 | 1.47 s, 28 Mhb | 0.54 s, 4 Mhb; file proved, 7.3 s user | 13 Mhb |
| 64 | 65.9 s, 1,421 Mhb (2,016 pairs) | 2.52 s, 10 Mhb; file proved, 32.9 s user | 164 Mhb |
| 256 | — | 6.48 s, 37 Mhb | 9,180 Mhb, 366 s, and the enable proofs fail |

The chain is now linear when the users lie in one group. The enable proof of a port with a
conditional user (and of every port in a module with methods) still lists the users' pairs
(`en_pairs`), which grows faster than u² and fails at 256 users. So the gate bounds such a port,
and a mux whose pairs are proved one by one, at 64 users (corpus: 20, mkMCR, a module with a
method; 11 with conditional users, mkMCR `t_state.write`). A table form of that enable proof, as
`en_lit` does for unconditional users, would remove the bound.

## Round 8, the merge: last caller and method task calls (C25–C26)

### C25. Last-caller ports (`LastCaller.lean`; design.md §8)

- **Which ports.** A child action port is a last-caller port when its kind and port are in the
  emitter's key (`LC_KEY`: RegN/RegUN/RegA `write`, ConfigRegN/UN/A `write`), at least two steps call it
  with different argument expressions, and the solver finds two callers that may both call in one
  cycle (both `WILL_FIRE`s and both call conditions, every port its own atom). Pairs a recorded
  exclusion covers are skipped (`excl_lc`): urgency, disjoint guards and run-time checks between rules;
  since review r8-1, bsc's disjoint guards and urgency between a rule and a method, and exported C
  between two methods. Every other port keeps today's argument check (`CertL.argN`, the old `Cert.arg`).
- **The mux.** `lcT_q` lists the callers latest first (greatest rank first), each entry the pair
  (selector, argument) with selector `WILL_FIRE ∧ call condition`; `arg_… := pmux (lclist_q …) ops.z`,
  bsc's `PrimPriMux` before AOpt (trust point 1). `argL_of_pmuxN` gives `CertL.argL` with one selection
  fact per entry; the order is one `incB` decision and one list equality, so the check is linear in
  callers. bsc's folded mux (the last arm as the default) has `argL_of_pmuxF`/`NF`, not used by the
  synthetic netlist.
- **The order facts.** Each last-caller port gets one virtual scan slot (`vbK`, a kind `κv` with
  `cout 0 = [0]`) appended to every caller's footprint (`virtT`, `fpAll … ++ virtT.get k`), so the
  unchanged scan keeps its callers in rank order (`lcMR_of_scan`, `lcRR_of_scan`, `lcMM_of_scan`, from
  `vchk`, `hkv`, `hvR`, `hvM`). The model is monkeypatched (`M.design`, `M.kind_tables`); `model.py` is
  unchanged.
- **Certificate.** `cert` is `LC.PairFacts.toCertL`, `certU : CertUL`, `obl := oblU_of_certL`. A file
  with a last-caller port or a ConfigRegN child imports `LastCaller` instead of `Scan`.
- **ConfigRegN** is LastCaller's model (`LC.CfgReg`, `Built.leaf _ LC.cfg_realizes LC.cfg_causal`);
  `cfgAnn_lib : cfgAnn = LC.cfgAnn := rfl` checks the instance's schedule against the library's.
  `E2E_CFGLIB=0` keeps the assumed leaf; `E2E_LC=0` turns last caller off.
- **Not emitted:** method callers on a last-caller port (asserted), CReg ports, RWireSBR, PulseWireOR,
  FIFO `clear`, Counter `setF` (their laws are proved, the corpus does not need them).
- With no last-caller port and no ConfigRegN child the output is the cost piece's, byte for byte
  (checked on SysArithPipe60 with `E2E_CFGLIB=0`).

### C26. Task calls inside interface methods (Ravi's decision, design.md §12)

Since review r8-1 a left-out call must be a system task that returns nothing (`model.NOVALUE_TASKS`);
any other task or BDPI call in a method stops the emitter.

System-task and BDPI calls made inside interface methods (bsc warning G0020) are not statements of
the module's `Tasks` child: they are neither in the task trace nor in the scan's task positions
(chain kinds), and the method's step has no task site. Task calls in rules stay statements, numbered
in rank order. `model.py` records each method it skips (`M.mtasks`); the emitter prints
`note: NAME: method m: k task call(s) left out of task agreement (bsc G0020)` and writes the same list
into the file's header comment and `top`'s docstring. Example: `e2e/mtask/` (design.md §12).

## Review r8-2 (C27–C28)

### C27. An escaped pair's table check, linear in its facts (cost review r8-2 M1)

Each escaped pair `(r1, r2)` emits `escB_n : Tab (Nat × List Nat)`, its bad site pairs as a search tree
keyed by first-step site (each node's `mid` the key of its right half's first entry; leaves `(i, i2s)`
with `i2s` ascending; at most 128 entries per definition, `stab_keyed`), and decides

    Tab.allB (fun i p => (portsJ p.1).all (fun b => okP p.1 p.2 b ||
      subS (idxT (usesAt ⟨p.1, b⟩) r2) (idxT escB_n i))) key_r1 0 k1

once per subtree of `key_r1` (`allB_thm`, as C20). `Emit.subS l1 l2` walks `l2` once, dropping `l1`'s
leading entries equal to each element (`subS_mem`: every entry of `l1` is in `l2`), so a site with F
partners costs O(F) and the pair O(its facts). `okT_exceptZ` (in the file) and `Scan.okT_except_fnS`
take `Tab.All` over `escB_n` (one `AllL` of facts per leaf, a balanced proof shaped as the table,
`taball`). Round 8's form, `bad.contains (i, i2)` over a literal list of all bad pairs, was quadratic.
Probe `scan/escS/genescS.py` (spread: W sites with one partner; fan: one site with W partners; neg:
a partner missing, must fail at the decision); `scan/escS/run.txt`. The emitter also asserts one site per
port in each step, which keeps the step index check `hchkT` linear (the fan probe's index check is
quadratic: 32 s at 1,024 sites of one step on one port, 474 s at 4,096).

### C28. User assertions in the file

A file's first comment lists the user assertions it assumes (`mutually_exclusive` groups by size and
single checks; they are `asmP`). A `conflict_free` check line stops the emitter (not stated in a file
yet); a check step with several `mutually_exclusive` lines is the gate's run only.


## Review r8b-1 (C29–C31)

### C29. A first-step task statement in an escaped pair is one comparison (cost review r8b-1 M1)

Task statements are numbered in rank order (generator check 5), so every statement of the earlier rule `r1`
is below `lo`, the later rule `r2`'s first statement. Each pair's check is

    Tab.allB (fun i p => (isT p.1.1 && Nat.blt (tnum p.1.1 p.2) lo) || (portsJ p.1).all (…as C27…)) key_r1 0 k1
    Tab.allB (fun _ p => !isT p.1.1 || Nat.ble lo (tnum p.1.1 p.2)) key_r2 0 k2          -- escT2S_n

each decided once per subtree. `isT` and `tnum` are per-kind definitions in the file (`tnum .tasks t = t.val`,
0 elsewhere); `okP_task` proves `okP` for two task ports in number order from `taskAnn`, and `okT_exceptZ`
takes `lo` and the second decision. A task site of `r1` is then O(1), not a loop over the `nT` ports of the
`Tasks` child (round 8's form: O(t1 × nT) per pair). Non-task sites keep C27's loop over their kind's ports
(at most a library kind's port count). Negative: `scan/escT/EscT_neg_8.lean` (`lo` one too high) fails at
`escT2S_0`.

### C30. Facts in groups of at most 128 fields (cost review r8b-1 M2)

`Hyps` and `Unconfirmed` hold their facts in sub-structures `HypsG<k>`, `UnconfirmedG<k>` of at most
`GATE_STRUCT = 128` fields each; the top structure has the fixed fields (`disj`, `tob`) and one field `g<k>` per
group, and every use is `hy.g<k>.esc_n` (the emitter rewrites the projections when it writes the file). One
structure declaration grows about ×5 per doubling of its fields, so no structure is wider than 128. A module
has at most `GATE_MOD_FACTS = 16,384` facts (128 groups), asserted and written into the header; the gate's run
counts them (escape facts, readiness and invariance facts, and every unexcluded user pair of a port counted for
the pairwise bound, an upper bound on the caller-exclusivity facts). The emitter also checks the pairwise bound
before it enumerates a mux's pairs, so a wide pairwise port stops it before any solver call.

### C31. `mutually_exclusive` lines read by structure (soundness review r8b-1 M1)

`sites/tools/mecheck.py` reads a check line as bsc's `aAnd (aOrs WF as) (aOrs WF bs)`: `A && B`, each side one
`WILL_FIRE` or a parenthesised `||`, accepted only when re-rendering gives the line back; any other line of
`WILL_FIRE` signals raises. The exclusions are `as × bs`. The emitter, the gate's scan and the shadow tool all
read lines through it, and the emitter's model and the shadow tool assert that the pairs equal the dumper's
`mutually_exclusive (user)` list. A file states one check per line with one head (`|as| = 1`; a longer first
side stops the emitter), and asserts that each stated check, re-rendered, is bsc's line; the gate's scan takes
the `|as|` checks `(a, bs)`, which together are exactly bsc's line. Negative control `e2e/mejoin/`.

# Site conditions (decision 1, review M1)

This answers the question behind decision 1: what happens if every site
condition is schedlean's own conservative approximation, with both operands
of `&&` and `||` unconditional?

- **Fail safe, not a soundness lemma.** Every condition a file states is
  checked by that file's obligation (`trU` and the value proofs): a condition
  that is too strong fails it. Weakening a condition keeps every step's result
  and adds only calls, so it costs completeness (`SiteCond.oblU_weakens`, §3).
  The step from the model to the real design is an argument outside Lean
  (design.md §7).
- **Not cyclic in practice.** No conditional site in the corpus has a
  cyclic condition, so no literal is dropped. Cycles appear only in a stress
  test that makes every read conditional, and dropping removes them (§5).
- **Not complete on the corpus.** Three designs that today's flat file
  proves would fail. Each loses protection that bsc expressed as `&&` or
  `||` (§6):
  - sysFloatTest: 10 dynamic legality facts;
  - mkPrediction: 6 readiness obligations (bsc's use conditions would not
    save 2 of them either);
  - mkBufferControl: 5 readiness obligations.

  The readiness obligations are a fourth kind of needed condition, which
  decision 1's list does not name (§1, item r).

Files, all in this directory:

| file | what |
|---|---|
| `SiteCond.lean` | the three Lean results (§3); imports `build/Modular.olean`, compiled from `r5/Modular.lean` |
| `tools/bapkg.py` | parser for `dumpba` text: definitions, rules, interface, uses map, resource allocation, FIFO `guarded` |
| `tools/satc.py` | Boolean skeletons, Tseitin CNF, Lean's bundled `cadical` |
| `tools/sitecond.py` | the rule, measured per design (one detail file per design) |
| `tools/summarize.py` | the tables in `results.txt` |
| `data/*.ba.txt` | `dumpba` of each corpus `.ba` (bsc-fix-all, build 5e4395b1) |
| `out/`, `out_qf/`, `out_sc/` | per-design detail files: hv/new4 (36 designs), quadfix/r3v examples (23 modules), and the short-circuit option of §8 (`SITES_SC=1`, 7 designs) |
| `probe/` | `CycleSel.bs`, a 20-line Classic design with decision 1's cyclic pattern |
| `results.txt` | all numbers |

Rerun: `SITES_ALL=1 python3 -I tools/sitecond.py out <hv/new4 design dirs>`
(86 s for the corpus), then `python3 -I tools/summarize.py out`.

## 1. The rule as measured

- **Action calls:** their exact call conditions, from the `.ba` actions
  (`if C then inst.m args`). These are never weakened.
- **Value reads:** unconditional, unless one of the following holds.
  - **(a) The read fails the static footprint scan.** A pair of steps in
    rank order, not in an exception group (disjoint guards, urgency
    blocking, user `mutually_exclusive`), has uses on one child that may
    not follow each other. The read member of the pair gets a condition.
    Action/action pairs need dynamic facts on exact conditions, not site
    conditions. The scan is revision 2's `portscan.py` `h4` and `grp`
    checks, kept per use. Its `mm`, `mmg` and `h3` checks fail nowhere in
    the corpus (`r6/tools/portscan.out`).
  - **(b) The read is of a shared value copy** that two steps that may fire
    together read with different arguments.
  - **(c) The read is the earlier read of a value/value pair** that may not
    follow (read flags).
  - **(r) Readiness, not in decision 1.** The port has a ready signal, and
    the step's guard does not imply it.
    - The ports are a guarded FIFO's `first` (RDY is `notEmpty`, alias
      `i_notEmpty`) and a generated child's `m` with `RDY_m`.
    - `Obl` demands readiness for every taken call, reads included. FIFO2's
      `first` has `alwaysRdy = false` in `Modular.lean`.
    - bsc lifts implicit conditions into selection arms and through `&&` and
      `||`, so an unconditional read can be taken when the FIFO is empty.
- **The condition of a conditional read:**
  - an exact hardware condition where the hardware has one that the step
    can compute (§4);
  - otherwise the reach condition. It is the OR over paths of the AND of
    path literals. An arm of an `if`, a `case` or an array selection is
    conditional on its select. A call's or a task's arguments are
    conditional on its condition. Every other operator, `&&` and `||`
    included, passes its operands on unconditionally. This is schedlean's
    `reachEdges` rule ("`x && false` still counts `x`"), with `orAll`'s
    absorption. It runs over the APackage that `dumpba` prints.
  - bsc's use conditions play no part.
- **The site order:**
  - Data dependence and the child's in-step SB order are fixed (§2).
  - A literal is dropped (replaced by true) when it reads a site that is not
    earlier, or a hardware-only signal: another step's `WILL_FIRE`, or a
    module input that is not the step's own argument or enable.

## 2. The order check and the argument

- **Sites.** Per step: the reads and calls the dump lists. Sites are
  numbered from 1.
- **Arguments.** `ρ 0` holds the method's argument, a tuple `ops.tup` for
  several, and `ops.z` for a rule. `body m x := runSites u res sites 1 (setN ρ₀ 0 x)`,
  so every site may read the argument (`SiteCond.envHw_arg`).
- **Order.** Hard edges:
  - a read's argument reads (value methods with arguments, such as
    `RegFile.sub`);
  - a call's condition and argument reads;
  - for two sites on one instance with `p SB q` or `p SBR q`, `p` first.

  The order is Kahn's algorithm on the hard edges. Ties go first to a site
  whose condition literals read only placed sites, then to the dump's
  order. A cycle of hard edges means "refuse to emit"; there are none in
  the corpus.
- **Conditions.** Each is emitted as a `Cond`: a list of paths, each a list
  of `Lit`, each with its read positions `rd` and a locality proof. An
  action site's condition reads only earlier sites by construction, so
  dropping leaves it unchanged (`at_eq_exact`).
- **SitesOK.** The generator proves one `ArgsBefore` fact per site: its
  argument reads only earlier positions, by one rewrite per argument read.
  `sitesOK_dropLater` then gives `SitesOK` for the dropped list, and
  `weakens_dropLater` shows that the dropped list only weakens. The check
  is linear in literal occurrences. design.md's current plan, one
  `ReadsBefore` per site, is the same check without the `Cond` structure.

## 3. Lean (`SiteCond.lean`)

`lean -j1 -DElab.async=false` with `LEAN_PATH=build`, under
`prlimit --as=25769803776` and `timeout`: 1.0 s, 0.50 GB. There is no
`sorry`, `admit`, `axiom` or `native_decide`. Every `#print axioms` lists at
most `propext` and `Quot.sound`.

1. **An unconditional read is the special case.** `Site.always j m arg` is a
   `Site` whose condition is the constant `true`, so `trace_runSites`,
   `envHw_at` and `calls_sites` apply as they are.
   - `readsBefore_always`: it reads before `n` exactly when its argument
     does.
   - `envHw_at_always`: it returns the child's output for every `u`.
   - `mem_callsOf`: a taken site's call is in the trace.
   - `uncond_at`: the syntactic form with `cond := [[]]` is `Site.always` at
     every position, with no literal to drop.
   - `envHw_arg`: the method's argument is at `ρ 0`.
2. **Weakening keeps results and adds calls** (a completeness cost, not a
   soundness lemma; round 7's `oblU_weakens`). `Weakens ss ss'` holds when `ss'` has the same
   ports and arguments as `ss`, and each condition of `ss'` holds wherever
   the matching one of `ss` does.
   - `weakens_env_exact`, which needs `SitesOK` only for `ss'`: in the
     weakened step's hardware environment, every site whose exact condition
     holds returns the child's output. The weakened environment is
     therefore a consistent assignment even for exact conditions that are
     cyclic.
   - `callsOf_weakens`: at one environment, the exact calls are a sublist of
     the weakened calls.
   - `legal_of_sublists`, `ready_of_sublists`: legality and readiness
     survive dropping calls, given that no always-enabled call is dropped.
     Weakened sites are value reads, so none is.
   - `envHw_unique`, `envHw_weakens`, `trace_weakens`, with `SitesOK` for
     both lists: every run of the weakened step is a run of the exact step
     at some skipped values, with the same result and a superset of its
     calls.
3. **Dropping later-site literals gives `SitesOK`, for any fixed order
   whose arguments read only earlier sites.**
   - `Cond.eval_dropFrom`: dropping weakens.
   - `Cond.dropFrom_local`: the dropped condition reads only earlier
     positions.
   - `sitesOK_dropLater`, `weakens_dropLater`, `at_eq_exact`.
   - `dropLater_spec` combines these with item 2 for exact conditions that
     may be cyclic.
   - The two `example`s are decision 1's cyclic pair in the order
     `C D A B`: `A`'s literal `B` is dropped, and `B` keeps `A`.

Not proved:

- `Drives.arg` for a shared value port after its reads are dropped. All
  taken calls of a `share` port carry one argument, so the last argument is
  unchanged while one call remains. This is not formalised.
- That the reach condition over-approximates "the value matters". Soundness
  does not need it, because the per-design proof checks insensitivity for
  every `u`.

## 4. Exact hardware conditions: none apply

- **bsc's argument mux** for a value-method copy that several uses share is
  built in `AState.mkEmux`. Each arm's select is the `WILL_FIRE` of the
  rules that use that argument (`RDY` for the module's own value methods).
  For value methods there is no extra condition: `cnd = const aTrue`. The
  mux is a `PrimMux` when all users are exclusive and a `PrimPriMux` in
  earliness order otherwise.
- **Within the user's own step, that select is `true`.** A `PrimPriMux` arm's
  exact condition is `¬WILL_FIRE` of the higher-priority users. A site
  condition cannot read that, because the parent's programs see only `ρ`
  (`Site.cond : (Nat → V) → Bool`, and `rbody` does not depend on `oc`).
- **bsc never builds the case that would need one.** `RSchedule.uuGraph`
  joins two uses with different arguments by "simultaneous" rules; the
  coloring puts only non-adjacent uses on one port; and a conflict that
  cannot be colored becomes arbitration (G0003, urgency).
- **Corpus.**
  - hv/new4 has one copy with two argument expressions: mkBufferControl
    `refPicList_rf.sub[3]`.
  - Across all 2,055 sweep7 modules (grepping `MUX_…$<m>_<copy>_<arg>__VAL_2`
    in the Verilog) the only other is mkInterpolator `storeFile_rf.sub[0]`.
  - Both pairs of users are recorded disjoint guards.
  - The Verilog is `ADDR_k = WILL_FIRE_x ? VAL_1 : VAL_2`.
- **So category (b) is empty by construction.** These sites stay
  unconditional, and the `share` obligation follows from the exception
  group. Exact conditions: 0. Conservative conditions: all 1,094.

## 5. Cycles and dropped literals

| mode | conditional reads | steps with a cycle | literals dropped |
|---|---|---|---|
| decision 1, hv/new4 | 1,094 of 13,591 (1,070 from the static scan, 24 for readiness) | 0 | 0 |
| decision 1, quadfix examples | 1 of 128 | 0 | 0 |
| stress: every read under its reach condition, hv/new4 | 2,780 not `true` (10,811 of 13,591 are `true`) | 10 steps: mkPrediction 6, mkDeblockFilter 2, mkBufferControl 1, sysArithPipe 1 | 149, none for a hardware-only signal |
| integration review 3, M1: every read under bsc's use condition | 5,671 of 6,786 in its 11 designs | 756 of 2,145 steps | — |

- **The reach rule removes most conditions.** bsc rewrites Boolean `if`s
  into `&&`/`||`, so most reads become unconditional. In sysFloatTest all
  2,050 reads reach unconditionally; under bsc's use conditions, 2,037 are
  conditional.
- **The probe shows the mechanism on real bsc output** (`probe/CycleSel.bs`,
  stress mode).
  - bsc's own use conditions are `a: (d && b==5) || c` and
    `b: d || (c && a==3)`. The reach conditions are the same.
  - The tool finds the cycle, orders `c d a b o.write`, drops `a`'s literal
    `b == 5`, and keeps `b`'s `a == 3`.

## 6. Completeness cost against bsc's use conditions

### Per design (decision 1 plus readiness)

| design | conditional sites (a + r) | bsc's condition strictly stronger | facts at risk | readiness fails under reach |
|---|---|---|---|---|
| sysConflictFreeOKLarge | 1,024 + 0 | 0 (all 1,024 equivalent) | 0 | — |
| mkBufferControl | 30 + 5 | 14 | 0 | 5 of 5 |
| mkPrediction | 5 + 9 | 7 (+2 weaker or incomparable) | 0 | 6 of 9 |
| sysFloatTest | 10 + 0 | 10 | 10 (5 with re-ranking) | — |
| mkDeblockFilter | 0 + 9 | 0 (8 where bsc's is weaker) | — | 0 of 9 |
| mkMCR | 0 + 1 | 0 | — | 0 of 1 |
| mkAssertedSchedules | 1 + 0 | 0 | 0 | — |
| other 29 designs | 0 | — | — | — |

### How each verdict was decided

- **The conditions.** Each conditional site's reach condition, after
  dropping, is compared with bsc's use condition from the dump:
  - `use_cond_k` comes from the dump's recomputed section;
  - `bsc_uses_k` is the `.ba`'s `__dusesk`.

  The comparison is two cadical queries over the Boolean skeleton:
  - comparisons with data are free atoms;
  - `x == k` for distinct constants exclude each other (EqC's law);
  - FIFO `notEmpty`/`i_notEmpty` and `notFull`/`i_notFull` are one atom
    (`Ann.same`).

  UNSAT is exact for real values.
- **Dynamic facts.** Each conditional read needs `¬(WF₁ ∧ c₁ ∧ WF₂ ∧ c₂)`
  against each partner use. The read's own condition is the new one, a
  call's is exact, and `WF` is expanded from the APackage. A fact counts as
  holding in either of two cases:
  - cadical finds it UNSAT: 52 facts;
  - the new condition is equivalent to bsc's and bsc's solver confirmed the
    fact: 1,536 facts, all in sysConflictFreeOKLarge, whose facts need
    arithmetic (`i == k` against `i+1 == k`).

  Otherwise it is "at risk", and the tool prints a satisfying assignment
  for judging by hand.
- **Readiness.** `guard ∧ c ⇒ RDY` is checked the same way.
- **Today's results.** Today's flat file proves every design with a
  conditional site:
  - sweep7 `results.txt` records PASS for mkBufferControl, mkPrediction,
    mkDeblockFilter, mkMCR and sysConflictFreeOKLarge;
  - `quadfix/r3v/results.txt` records PASS for mkAssertedSchedules;
  - sweep7's sysFloatTest entry is LEANFAIL with an empty log, meaning the
    run was killed, not a Lean error; design.md records it at 170 s.

  So each failure below is a new failure, caused by the rule.

### Why each design fails

- **sysFloatTest** fails.
  - The sites are `fCheck_*.notEmpty`, read in StmtFSM's `fsm_start` and
    `action_l…` rules after `check_*`'s `deq`.
  - The read is in the guard as `!notEmpty && state == k`. bsc's use
    condition keeps `state == k`. The reach condition is `true`.
  - The witness for `RL_div_fsm_fsm_start` against `RL_check_div` sets one
    data atom (`state == 0`) and only Bool port values: FSM idle, `abort`
    and `start_reg` set, `fCheck_div` non-empty, the divider's response
    ready. Both rules fire, and the read is taken after the `deq`. That is
    a real state (the theorem covers every state), so `Legal` fails.
  - Re-ranking the rules (§8) removes the five `fsm_start` sites. The five
    `action_l…` sites are two-way pairs: the other order fails on the
    divider's `response_get SBR request_put`.
- **mkPrediction** and **mkBufferControl** fail on readiness while FIFO2's
  `first` stays not always ready.
  - Example: mkBufferControl `RL_adjustingFreeSlots`. The read of
    `longTermPicList_returnList.first` reaches the condition of
    `freeSlots_slots.write` through `adjustFreeSlots == 1 || st.first[5] || (!st.first[5] && lt.first[5] && …)`.
  - The guard requires `lt.i_notEmpty` only when `adjustFreeSlots != 1` and
    `!st.first[5]`, which is exactly bsc's use condition. The reach
    condition is `true`.
  - Two of mkPrediction's six (`infifo_ITB.first`, `predictedfifo.first` in
    `RL_outputing`) fail under bsc's use condition as well.
- **mkDeblockFilter** goes the other way. bsc's `AUses` does not follow
  `case` arms: its conditions are weaker at 8 sites (readiness 1 of 9). The
  reach rule's `pixelNum == k` arms prove all 9.

### Sites where bsc's use condition is strictly stronger

31 sites:

- mkBufferControl: 14. Their facts hold (abstract UNSAT), but the 5
  readiness sites fail.
- mkPrediction: 7.
- sysFloatTest: 10.

In the cases traced by hand (sysFloatTest's guards, mkBufferControl's call
conditions, mkPrediction's `RL_outputing` guard), the extra literals come
from `&&`/`||` context: bsc's short-circuit, or a Boolean `if` that bsc
rewrote. The reverse also occurs
(10 sites, mkDeblockFilter and mkPrediction): `case` and array arms that
bsc's analysis misses.

## 7. bsc's use-level solver facts

Of the 3,241 confirmed facts in the dumps:

| class | count | what happens |
|---|---|---|
| action/action | 1,439 | unchanged: exact call conditions on both sides |
| the read stays unconditional | 750 | not needed: the pair is in an exception group, or the read is in order (for example mkMCR's 28 `t_state.write then t_state.read`, all disjoint guards) |
| new condition equivalent to bsc's | 1,036 | the same fact (sysConflictFreeOKLarge 1,024, mkBufferControl 9, mkPrediction 2, mkAssertedSchedules 1) |
| re-ask over the new condition | 16 | 6 hold, by cadical UNSAT (mkBufferControl 3, mkPrediction 3); **10 no longer hold** (sysFloatTest, §6) |

- The 22 UNCONFIRMED facts are unchanged: today's file does not use them
  either.
- Readiness adds new per-site `skel` conjuncts (`guard ∧ c ⇒ RDY`). They are
  not solver facts.
- Re-asking means schedlean builds the fact over the dropped reach
  condition. The condition is an AExpr DAG over the package's own
  definitions: `if` selects, `case`-label equalities and array-index
  equalities. schedlean passes it to bsc's solver through the interface it
  already uses for unrecorded disjointness.

## 8. Options that would recover the cost (for Ravi; none adopted)

Each keeps every condition checked and none trusts bsc.

1. **Both-way short-circuit, then the same drop rule.**
   - Each operand of `&&` (`||`) is conditional on the other being true
     (false), as in bsc's use analysis. The fixed order and the drop rule
     still make every condition acyclic.
   - Measured with `SITES_SC=1` on the seven designs with conditional
     sites (`out_sc/`):
     - sysFloatTest: 5 of 10 facts hold;
     - mkBufferControl: 1 of 5 readiness sites;
     - mkPrediction: readiness unchanged (3 of 9), and 2 facts that held now
       fail because their literals were dropped.
   - Conditions become cyclic again: 20 steps in decision-1 mode, 171
     literals dropped.
   - So the greedy order loses what the short-circuit gains. It is not a
     drop-in fix; an order chosen per protecting literal might be.
2. **Choose the rule rank.**
   - `place_ok` only needs some rank; bsc's order is not required.
   - Ranking by the static must-precede graph, with bsc's order breaking
     ties, removes 6 of the 1,070 (a) sites.
   - It cuts sysFloatTest's at-risk facts from 10 to 5. It does not
     remove the two-way pairs (23 in the corpus).
3. **Model FIFO `first` as always ready.**
   - FIFO2's Verilog has no ready port for `first`; `D_OUT` is a register.
   - The implicit condition is a separate read of `i_notEmpty` in the guard.
   - With `alwaysRdy first = true` in `Modular.lean`, category (r)
     disappears: 24 sites, 11 failing. The FIFO2 contract would have to say
     what `first` returns when the FIFO is empty.
   - That is a library decision.

## 9. Replacement for design.md §7, "use-condition bugs … are therefore caught"

> One rewrite per use site. With the order and every child's call sequence
> fixed before the skipped values (`OblU`), a use that depends on a skipped
> value cannot be proved for every `u`, so a condition that is too strong makes
> the proof fail. Weakening keeps every step's result and adds only calls
> (`SiteCond.oblU_weakens`): it costs completeness (legality and readiness of
> the extra calls), and is not a soundness lemma. Every site condition is
> schedlean's own: a call's exact condition, `true` for a read, the reach
> condition for the few reads that need one, or the ready literal for a
> readiness read. bsc's use conditions are not used, so an error in bsc's use
> analysis cannot change what is proved. A schedule that bsc justified only by
> a wrong use condition still fails, at that pair's legality fact.

## 10. Revision 2: the readiness rule and the compilation modes (design.md §7)

- **Readiness (r)** now uses the decided rule: short-circuit reach, the
  operand that reads the site's own ready signal first, otherwise left to
  right; up to three flipped operators where that fails. The site's condition
  is the ready literal itself (exact under the guard when `guard ∧ R_sc ∧
  ¬RDY` is UNSAT), read first, so no literal is dropped. 22 of the 24 corpus
  readiness sites hold (13 under the conservative rule of §1); the 2 left
  (mkPrediction `RL_outputing / outstatefifo.first`, mkBufferControl
  `RL_inputing / shortTermPicList_returnList.first`) fail safe. `tools/sitecond.py` prints the counts as `rdyRule`;
  `SITES_RDY_TRACE=1` prints each site's condition and witnesses
  (`out_trace/`).
- **Four bsc modes**: `modes/compile.sh` (default, `-no-aggressive-conditions`,
  `-no-sched-conditions`, both), `modes/run_sites.sh`, `modes/results.txt`.
  The tool reads the newer dump format (`reads X  (hint: used when C)`) too.

## 11. Round 7: the readiness rule sequenced by expression structure

Ravi's decision (2026-11-02): `&&` and `||` are sequenced by the structure of
the expression as bsc builds it (an n-ary `&&` or `||` is its argument list in
order); a later operand's condition includes the earlier operands'
short-circuit outcome (true for `&&`, false for `||`). This replaces the
readiness-first bias and the orientation search (`tools/sitecond_r6.py` keeps
the round-6 tool). The read's condition stays its ready literal; the check is
`guard ∧ R_seq ∧ ¬RDY` UNSAT. Results (`modes7/`, `modes7/run_sites.sh`):

| mode | readiness sites | recovered | failing (fail safe) |
|---|---|---|---|
| default | 24 | 22 | mkBufferControl `RL_inputing / shortTermPicList_returnList.first`, mkPrediction `RL_outputing / outstatefifo.first` |
| `-no-sched-conditions` | 24 | 22 | the same two |
| `-no-aggressive-conditions`, both off | 0 | — | — |

**The site-value form (round 7 revision, soundness review r7-1 M1).** The
first round-7 verdict above (`rdyRule`) encoded a literal over a conditional
read on the hardware's port. The model reads the skipped value there whenever
the read is skipped, so that verdict did not show the uses independent of the
skipped values. The verdict now used is `rdySiteValue`
(`tools/sitecond.py`, `rdy_site_check`; encoder `satc.EncS`):

- a literal over a conditional read `y` of the same step (the site itself
  included) reads `y`'s site value, `ite(cond_y, port_y, fresh)`, where
  `cond_y` is `y`'s ready literal (an (a) read's: its reach condition);
- **readiness**: `guard ∧ R(site values) ∧ ¬RDY` UNSAT;
- **invariance**, where `R` reads another conditional read:
  `guard ∧ R(site values) ≠ R(ports)` UNSAT. Where `R` reads only the site
  itself, Lean derives invariance from readiness (`inv_self`).

The file states exactly these facts (`rdy_n`, `inv_n` over `ops.g n` of the
site values) and each is labelled with this verdict; its `R` reads the site
values, so `trU` checks the use. A negative control (`modes7/neg_rdy.py`, output `modes7/neg_rdy.txt`) with
the ready literals replaced by free atoms loses mkBufferControl's
`RL_adjustingFreeSlots / longTermPicList_returnList.first`, as it must.

| mode | readiness sites | all facts confirmed | of which need invariance (all hold) | failing (fail safe) |
|---|---|---|---|---|
| default | 24 | 22 | 7 | mkBufferControl `RL_inputing / shortTermPicList_returnList.first`, mkPrediction `RL_outputing / outstatefifo.first` |
| `-no-sched-conditions` | 24 | 22 | 7 | the same two |
| `-no-aggressive-conditions`, both off | 0 | — | — | — |

Whole-literal dropping (the decided fallback) is used only where an `R` would
read a site that is not before a site splitting on it; the emitter then drops
those literals, counts them, and re-asks the tool for the formula it emits.
mkBufferControl needs none (0 later-site literals). For the three designs not
emitted the count assumes the same; they are flat for action shadowing anyway.
With whole-literal dropping everywhere (the first fix the review offered) the
count would be 13 of 24 (mkBufferControl 1/5, mkPrediction 2/9,
mkDeblockFilter 9/9, mkMCR 1/1). The order pass's later-site drops for the
reach conditions themselves: 5 literals in mkBufferControl, 65 in
mkPrediction; cycles before weakening: mkBufferControl 3 steps / 4 sites,
mkPrediction 2 / 5.

## 12. Round 8: four latent gaps closed (soundness review r7-2 minors 1–4)

None of these changed a verdict or a count in any mode (`modes8_min/compare.txt`; rerun with the
merged tools in `../gate8/sites_*`). The readiness count stays 22 of 24 in the default and
`-no-sched-conditions` modes, with the same two failing sites.

1. **`'eq'` literals** (case arms, array selections) read site values: `satc.EncS.eqatom`.
2. **One ready literal**: the tool's `cond_y` and conclusion are the literal the file states (the
   first ready port the step reads; `rdy_lit`), and the facts are checked with every port its own
   atom (`Design.exact()`: `notEmpty` and `i_notEmpty` apart), because the file's facts carry no
   `AliasOK`. `condsites.py` hands the emitter exactly that literal.
3. **bsc's facts keyed exactly** (`BscFacts`): both steps and both uses; a chain covers every earlier
   use of its port; a text also listed UNCONFIRMED covers nothing; and the stated conditions with `WF`
   must imply bsc's guard and use condition (`mke2e.py` `bsc_fact`).
4. **Input and wire literals in `R`** are dropped (monotone weakening) and the formula re-checked;
   `condsites.py` asserts none remain in a stated `R` or (a) condition (corpus: 0).

Negative controls (`modes8_min/neg_sitecond.txt`, `neg_bscfacts.txt`): round 7's tool confirms four
false readiness facts (`eqcase`, `eqsel`, `alias`, `hwlit`), the new tool none; the bsc key gets 9 of
9 cases right. The two decisions to confirm with Ravi (soundness review r7-2 minor 6) are in
design.md §7.

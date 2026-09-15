# Bluesim VCD code generation (bsc -sim): scope hierarchy, signal selection, id assignment, and dump_VCD runtime behavior

Bluesim VCD emission is split three ways: SimMakeCBlocks.hs builds each module's SimCCBlock (deciding which defs/ports exist and pre-sorting them), SimCCBlock.hs's simCCBlockToClassDefinition (src/comp/SimCCBlock.hs:1869-2088) generates per-module dump_VCD_defs (header/$var emission) plus dump_VCD/vcd_defs/vcd_prims/vcd_submodules (value emission against a shadow "backing" instance), and SimBlocksToC.hs generates the model_*.cxx wrapper (MODEL_x::dump_VCD_defs / dump_VCD, SimBlocksToC.hs:527-545) plus the construct-on-first-use backing instance (mkBacking, SimBlocksToC.hs:604-624). The runtime (src/bluesim/vcd.cxx, kernel.cxx) allocates ids from a single global counter via vcd_reserve_ids, wraps everything in a "main" scope, and calls model->dump_VCD(dt) from a once-per-timeslice vcd_event at AFTER_LOGIC priority; each user module writes its own scope named by inst_name containing (in order) clock defs/aliases, alphabetized members (reset defs + local defs incl. CAN_FIRE/WILL_FIRE), alphabetized method ports, alphabetized primitive instances, then alphabetized submodule scopes (depth-limited by vcd_depth). Verified against a compiled two-level example (mkTop/mkSub) whose generated mkTop.cxx/mkSub.cxx/model_mkTop.cxx and actual .vcd output match this description, including the one-unused-id-per-primitive gap in the id space.

## Top of hierarchy: kernel 'main' scope wrapping the model
The kernel, not generated code, opens the outermost scope: on first VCD activation vcd_event calls vcd_write_header then vcd_write_scope_start(simHdl, "main"), simHdl->model->dump_VCD_defs(), vcd_write_scope_end, and '$enddefinitions $end' (src/bluesim/kernel.cxx:336-345). MODEL_top::dump_VCD_defs() just forwards to <top>_instance->dump_VCD_defs(vcd_depth(sim_hdl)) (SimBlocksToC.hs:531-534, generated in model_<top>.cxx). So the VCD hierarchy is: main -> top -> <instance names...>.

## Top module instance name is literally "top"
create_model allocates the top block with `new MOD_<name>(sim_hdl, "top", NULL)` (newInst, src/comp/SimBlocksToC.hs:412-415); the backing instance uses the same name "top" (mkBacking, SimBlocksToC.hs:610-611). Module::inst_name (src/bluesim/bs_module.h:31) holds it; each generated dump_VCD_defs opens its scope with vcd_write_scope_start(sim_hdl, inst_name) (SimCCBlock.hs:1970, runtime emits '$scope module <name> $end' at vcd.cxx:366-369). Submodule scope names are the BSV instance names (the ctor's inst_name), e.g. 'asub', 'zsub'.

## Per-module dump_VCD_defs body order (SimCCBlock.hs:1972-1988)
Generated as: (1) vcd_write_scope_start(sim_hdl, inst_name); (2) vcd_num = vcd_reserve_ids(sim_hdl, num_ids) and local `num = vcd_num` (num_init, :1932-1937), where num_ids = length members + length ports + length prims (:1928); (3) a for-loop over all kernel clocks calling vcd_add_clock_def(sim_hdl, this, bk_clock_name(clk), bk_clock_vcd_num(clk)) (clk_def_loop, :1938-1946) — the runtime only emits the $var if the clock's dotted name matches this module's position in the hierarchy (match_hierarchy, src/bluesim/vcd.cxx:300-356), so an undotted clock like 'CLK' is emitted only in the root module's scope; (4) clock aliases: for each input clock port (sb_inputClocks) a vcd_write_def(sim_hdl, bk_clock_vcd_num(clk_handle), "CLK", 1) reusing the kernel clock's id (clk_aliases, :1947-1953); (5) member_calls then port_calls ($var defs for defs/ports, :1962-1963 via mkVCDDef :1553-1569); (6) prim_calls: num = INST_<prim>.dump_VCD_defs(num) for each primitive (:1954-1957); (7) if there are submodules, guarded by `if (levels != 1)`: `unsigned int l = (levels == 0) ? 0 : levels - 1;` then num = INST_<sub>.dump_VCD_defs(l) for each (vcd_recurse/sub_calls, :1958-1969, :1980-1985); (8) vcd_write_scope_end(sim_hdl); return num (:1971, :1986-1987). So primitives' $vars land in the parent's scope textually after the module's own members/ports and before submodule scopes.

## Ordering rule: alphabetized, NOT declaration order
Everything is sorted by cmpIdByName = plain case-sensitive `getIdString i `compare` getIdString i'` (src/comp/Id.hs:261-262; ASCII order, so all uppercase CAN_FIRE_*/RST_N/WILL_FIRE_* sort before lowercase names): members via cmp_def (SimCCBlock.hs:1913, :1916-1924), ports (:1925-1927), prim instances (:1903-1907), and submodule instances (:1908-1912). Example: zsub declared before asub in BSV, but VCD scope order is asub then zsub. (The case-insensitive symOrd at SimCCBlock.hs:1731-1747 is for the symbol table only, not VCD.) Note also raw_avis in SimMakeCBlocks.hs:248-250 sorts sb_state by instance name, and defs/ports are pre-sorted by base name at SimMakeCBlocks.hs:263 and :297-299, but the VCD-visible order is the cmpIdByName sorts in SimCCBlock.hs.

## Which signals a USER module contributes (members + ports)
members = sb_resetDefs (e.g. RST_N, 1 bit; from sp_reset_list, SimMakeCBlocks.hs:351-353) ++ all sb_privateDefs and sb_publicDefs except string-typed ones (SimCCBlock.hs:1914-1924). Public/private split: public defs are those reachable from CAN_FIRE_*/WILL_FIRE_* expressions (isFire), the rest private (SimMakeCBlocks.hs:258-269); both are dumped, so the VCD contains every surviving local def including CAN_FIRE_<rule>/WILL_FIRE_<rule>/CAN_FIRE_<meth> (1 bit each) and intermediate defs like cnt___d1 (their AType width). Whether CF/WF defs survive optimization is decided upstream (-keep-fires). ports = method ports from sb_methodPorts: EN_<meth> enables (aTBool, SimMakeCBlocks.hs:290-292), method argument ports <meth>_<arg> (:293), and return-value ports incl. RDY_<meth> and value-method outputs like 'total' (:294-296). Instantiation parameters (sb_parameters) and rules themselves are NOT dumped. $var name is getIdBaseString of the id; width is aSize of the AType via mkVCDCallArgs (SimCCBlock.hs:1717-1726; ATString Nothing gets width 0). Each def additionally gets vcd_set_clock(sim_hdl, num, <clk_handle>) before its $var if its id appears in clk_map = rules-by-clock + methods-by-clock + schedule dom_map (mkVCDDef :1553-1569; clk_map built at :1871-1902 and SimBlocksToC.hs:49-83) — the runtime uses this to backdate combinational changes to the clock's combinational time (time_of_change, src/bluesim/vcd.cxx:462-489).

## VCD id assignment: one global counter, depth-first pre-order, with per-prim gaps
Ids come from a single monotonically increasing counter: vcd_reserve_ids(simHdl, n) returns next_seq_num and bumps it by n (src/bluesim/vcd.cxx:273-278). Clocks reserve their id (clocks[clk].vcd_num) at bk_get_or_define_clock time during create_model (kernel.cxx:923), i.e., BEFORE any module ids; vcd_keep_ids after create_model (kernel.cxx:714) records kept_seq_num so vcd_write_header can reset next_seq_num = kept_seq_num (vcd.cxx:228, :280-283) — module ids are re-reserved from the same base each time a new header/file is started. Each user module reserves one contiguous block of (members+ports+prims) ids into this->vcd_num at dump_VCD_defs time; members then ports use vcd_num+0..; the per-prim slots in that block are NEVER used because every primitive's dump_VCD_defs ignores its `num` argument and self-reserves (e.g. MOD_Reg: vcd_num = vcd_reserve_ids(sim_hdl,1) at src/bluesim/bs_prim_mod_reg.h:156-161), leaving one unused id per primitive (verified in generated output: mkTop reserves 7, uses ids 0-5 for 6 members, id 6 unused, prim reg gets id 7). The `num` threading (prim calls pass/receive num, SimCCBlock.hs:1954-1961; user-module proto `unsigned int dump_VCD_defs(unsigned int levels)` returns num, vcdHdrFnProto :1529-1533) therefore only mirrors the global counter; correctness comes from vcd_reserve_ids. Ids are written base-94 starting at '!' (vcd_write_id, vcd.cxx:285-298). Clock aliases reuse the kernel clock's id (same id, several $var names). If vcd_depth truncates recursion, un-visited submodules never reserve ids at all.

## Backing (shadow) instance = change-detection proxy
model_<top>.cxx defines `MOD_<top>& <top>_backing(tSimStateHdl)` (mkBacking, SimBlocksToC.hs:604-624): a static construct-on-first-use full second instantiation of the top module named "top" with NULL parent, wrapped in vcd_set_backing_instance(simHdl,true/false) so constructors can detect it (flag: vcd.cxx:255-258, query vcd_is_backing_instance :268-271; used to skip reset registration in src/bluesim/reset.cxx:81,104 and to skip/alter setup in bs_prim_mod_regfile.h:182,197, bs_prim_mod_bram.h:283,315,342, bs_prim_mod_synchronizers.h:1393). MODEL_x::dump_VCD(dt) calls <top>_instance->dump_VCD(dt, vcd_depth(sim_hdl), <top>_backing(sim_hdl)) (SimBlocksToC.hs:536-543); the backing reference is passed down structurally (backing.INST_<sub>, backing.INST_<prim>) through vcd_submodules/vcd_prims (SimCCBlock.hs:2036-2056). Every value write is paired with `backing.<field> = <field>` so the backing holds last-dumped values (vcd_write, SimCCBlock.hs:1997-2005).

## Generated value-dump functions: dump_VCD / vcd_defs / vcd_prims / vcd_submodules
Per user module (SimCCBlock.hs:2026-2088): MOD_x::dump_VCD(tVCDDumpType dt, unsigned int levels, MOD_x& backing) calls vcd_defs(dt,backing) if it has members, vcd_prims(dt,backing) if it has prims, and `if (levels != 1) vcd_submodules(dt, levels-1, backing)` (:2057-2076); each is only emitted/declared when nonempty (has_members/has_prims/has_submodules, :1653-1673). vcd_defs starts `unsigned int num = vcd_num;` then three branches on dt (:2027-2035): dt==VCD_DUMP_XS -> vcd_write_x(sim_hdl, num++, width) per member+port (no value; runtime prints 'x'/'bx', vcd.cxx:491-503,620-629); dt==VCD_DUMP_CHANGES -> per signal `if (backing.<f> != <f>) { vcd_write_val(sim_hdl, num, <f>, width); backing.<f> = <f>; } ++num;` (num incremented unconditionally to stay aligned, vcd_write_changed :2011-2019); else (VCD_DUMP_INITIAL/$dumpvars, VCD_DUMP_CHECKPOINT/$dumpall, VCD_DUMP_RESTART/$dumpon) -> unconditional vcd_write_val + backing update for every signal (:2020-2025). vcd_prims calls INST_p.dump_VCD(dt, backing.INST_p) (prim signature has no levels, :2036-2045); vcd_submodules calls INST_s.dump_VCD(dt, levels, backing.INST_s) (:2046-2056). Order inside these functions is the same alphabetized members-then-ports / prims / subs order as the defs function, keeping num in sync with the $var ids.

## When dump_VCD runs and how output is timed
When VCD is active (vcd_is_active, vcd.cxx:168-173), the kernel schedules one vcd_event per timeslice at priority make_priority(PG_AFTER_LOGIC, PS_VCD), i.e., after all clock-edge logic for that sim time (setup_vcd_events, src/bluesim/kernel.cxx:297-299,593-609). vcd_event (kernel.cxx:336-407): writes the header+defs if the file is fresh, calls vcd_advance (flushes buffered changes older than min_pending = min over clocks of bk_clock_combinational_time, vcd.cxx:387-434), picks dt from a state machine (get_VCD_dump_type, vcd.cxx:175-205: first activation -> VCD_DUMP_INITIAL with '$dumpvars' task; steady state -> VCD_DUMP_CHANGES; re-enable -> VCD_DUMP_RESTART '$dumpon'; $dumpoff -> VCD_DUMP_XS; checkpoint -> VCD_DUMP_CHECKPOINT '$dumpall'), writes clock values itself (for CHANGES only clocks that edged at `now`, kernel.cxx:352-403), then calls model->dump_VCD(dt). vcd_write_val/vcd_write_x compute a per-signal timestamp via time_of_change (vcd.cxx:462-489): signals registered with vcd_set_clock are stamped with their clock's most recent combinational time (emulating Verilog eager evaluation of combinational logic; see comment vcd.cxx:15-34), unclocked signals with bk_now; changes later than min_pending are buffered in (simHdl->vcd).changes and flushed time-ordered by flush_changes (vcd.cxx:405-434), which prints '#<time>' plus buffered $dump* task markers via vcd_output_at_time (vcd.cxx:436-460). Values print as VCD scalars ('0'/'1') for 1-bit and 'b<binary> ' (leading zeros elided) otherwise (print_change, vcd.cxx:596-661); all $var defs are type 'reg' (vcd_write_def, vcd.cxx:376-385).

## Primitive dump_VCD_defs variants (for parity when reimplementing)
Simple prims (MOD_Reg, MOD_ConfigReg, MOD_RegTwo) emit exactly one $var named by inst_name with their bit width in the PARENT's scope: `vcd_num = vcd_reserve_ids(sim_hdl,1); vcd_write_def(sim_hdl, vcd_num, inst_name, bits); return vcd_num+1;` (src/bluesim/bs_prim_mod_reg.h:156-161,578-583,748-753). Fancier prims open their own child scope with internal port names and reuse the first id as an alias for the parent-scope $var: e.g. a clocked-reg variant reserves 4 ids and writes inst_name in the parent scope, then a scope containing CLK (kernel clock id), RST, EN, D_IN (with vcd_set_clock on EN/D_IN) and Q_OUT aliased to the parent-scope id (bs_prim_mod_reg.h:347-359); ports-style prims reserve 3*ports ids with Q_OUT_#/EN_#/D_IN_# names and a parent-scope alias sharing Q_OUT's id (bs_prim_mod_reg.h:989-1001). Prim dump_VCD(dt, MOD_X&backing) mirrors the user-module classes: XS -> vcd_write_x, CHANGES -> compare against backing.value, else unconditional write (bs_prim_mod_reg.h:162-168,584-593,754-762).

## Class declaration side and reproduction artifacts
The .h declares `unsigned int dump_VCD_defs(unsigned int levels)`, `void dump_VCD(tVCDDumpType, unsigned int levels, MOD_x&)`, and conditionally vcd_defs/vcd_prims/vcd_submodules (vcdHdrFnProto SimCCBlock.hs:1529-1533, vcdDumpFnProto :1535-1544, vcd_changes selection :1650-1674); Module base supplies inst_name and the per-module vcd_num field (src/bluesim/bs_module.h:31,36). Verified example lives at /tmp/claude-0/-home-user-bsc/e236fbcd-0f62-56f2-9365-97a217968d47/scratchpad/vcdtest (Top.bsv with mkTop{cnt reg, asub/zsub = mkSub{acc,flag regs, bump/total methods}}; compiled with /home/user/bsc/inst/bin/bsc -sim -keep-fires; see mkTop.cxx / mkSub.cxx / model_mkTop.cxx VCD sections and top.vcd). Observed header: main -> top {CLK x2 (loop def + alias, same id '!'), CAN_FIRE_RL_count, CAN_FIRE_RL_stop, RST_N, WILL_FIRE_RL_count, WILL_FIRE_RL_stop, cnt___d1, cnt(prim), asub{CLK alias, CF/WF..., acc___d3, EN_bump, RDY_bump, RDY_total, bump_amt, total, acc, flag}, zsub{...}}, confirming: alphabetized order, ports after members, prims after ports, submodule scopes last and alphabetized, and unused ids at each module's per-prim slots.

# Bluesim VCD dumping (src/bluesim/vcd.cxx, bs_vcd.h, kernel.cxx VCD event machinery, generated-code contract from src/comp/SimCCBlock.hs / SimBlocksToC.hs)

Bluesim writes VCD from a single kernel event (vcd_event, kernel.cxx:336-410) scheduled at priority (PG_AFTER_LOGIC, PS_VCD) whenever any clock-edge schedule runs while VCD is active. The file format itself lives in vcd.cxx: a ctime-based $date/$version("Bluespec VCD dumper 2.1")/$timescale header, "$scope module NAME $end" scopes, "$var reg W id name $end" definitions (everything is type reg), base-94 printable identifiers starting at '!' with id 0 = the first clock, #time markers via "#%llu", 1-bit changes as "0id/1id/xid" and multi-bit as "b<binary-no-leading-zeros> id" / "bx id" (no z ever). Change detection is done by diffing the live model against a lazily-constructed second "backing" instance of the whole design; combinational signals are back-dated to the previous edge of their associated clock via a per-time change buffer (changes map + min_pending/vcd_advance), flushed only once all clocks have passed that time. $dumpvars/$dumpoff/$dumpall/$dumpon appear as "tasks" bracketing a value section that is closed with $end just before the next #time marker. The five dump modes are VCD_DUMP_INITIAL (real values; clocks skipped unless has_initial_value or already ticked), CHANGES (diff vs backing), CHECKPOINT ($dumpall, everything), XS ($dumpoff, everything x), RESTART ($dumpon, everything real).

## Header: $date / $version / $timescale exact text
vcd_write_header (vcd.cxx:207-231) writes, only when state==VCD_OFF (then sets state=VCD_HEADER): (1) "$date\n\t%s$end\n" with %s = ctime(&t) which itself ends in '\n', so the rendered form is `$date` NL TAB `<Www Mmm dd hh:mm:ss yyyy>` NL `$end` NL (vcd.cxx:221-222); (2) "$version\n" + "\tBluespec VCD dumper %d.%d\n" + "$end\n" with major_rev=2, minor_rev=1 (vcd.cxx:11-13, 223-225) => literal line `\tBluespec VCD dumper 2.1`; (3) "$timescale\n\t%s\n$end\n" with %s = simHdl->vcd.vcd_timescale (vcd.cxx:226), default "1 us" (kernel.cxx:694), settable via bk_set_timescale only at sim_time==0 and only to (1|10|100)+space+(s|ms|us|ns|ps|fs) (kernel.cxx:1180-1222). It also resets next_seq_num = kept_seq_num (vcd.cxx:228).

## Hierarchy/defs section syntax
Scopes: vcd_write_scope_start prints "$scope module %s $end\n" and vcd_write_scope_end prints "$upscope $end\n" (vcd.cxx:366-374). Signal defs: vcd_write_def prints "$var reg %d " (width, decimal) + base-94 id + " %s $end\n" (name) — every signal is declared as type `reg`, including clocks (vcd.cxx:376-385); clock defs via vcd_add_clock_def print the identical "$var reg 1 <id> <name> $end\n" but only in the module scope whose hierarchical path matches the clock's dotted name (match_hierarchy, vcd.cxx:300-356). After the header, vcd_event wraps the whole design in a scope named "main": vcd_write_scope_start(simHdl,"main"); model->dump_VCD_defs(); vcd_write_scope_end(); then fputs("$enddefinitions $end\n") (kernel.cxx:339-345). Each generated module then opens its own "$scope module <inst_name> $end" (SimCCBlock.hs:1970-1971), so the top module appears as main/<top-inst>/... Signal names are the raw identifier base strings; widths never print a [msb:lsb] range.

## Identifier encoding (base-94)
vcd_write_id (vcd.cxx:285-298) converts the numeric id to a big-endian base-94 string with digit character '!' + (num % 94): alphabet is the 94 printable ASCII chars '!'(33)..'~'(126); id 0 = "!", 1 = '"', 93 = '~', 94 = "\"!" wait no — 94 renders as digits [1,0] => '"','!' i.e. `"!`. Buffer is char[6] so at most 5 digits (~7.3e9 ids). No id value is skipped or reserved; ids are plain sequence numbers from vcd_reserve_ids (vcd.cxx:273-278: returns next_seq_num, then next_seq_num += num).

## ID allocation order: clocks first (permanent), model ids per-header
Clock ids: bk_define_clock assigns ci.vcd_num = vcd_reserve_ids(simHdl,1) at definition time (kernel.cxx:923), during model->create_model inside bk_init (kernel.cxx:709). bk_init then calls vcd_keep_ids (kernel.cxx:714) which snapshots kept_seq_num = next_seq_num (vcd.cxx:280-283), making clock ids [0..#clocks) permanent; the first defined clock gets id 0 ("!"). vcd_write_header rewinds next_seq_num = kept_seq_num (vcd.cxx:228), so all model/instance ids are (re)allocated afresh each time a header is emitted, starting immediately after the clock ids, in dump_VCD_defs traversal order: each generated module reserves (members+ports+prims) contiguous ids up front (SimCCBlock.hs:1928, 1932-1937), primitives reserve their own inside their dump_VCD_defs (e.g. bs_prim_mod_reg.h:158, 349, 994). Aliases reuse an existing id with a second $var (register Q_OUT alias bs_prim_mod_reg.h:351-359; input-clock port aliases via bk_clock_vcd_num, SimCCBlock.hs:1947-1953). bk_clock_vcd_num returns clocks[clk].vcd_num, or 0 (colliding with the first clock) for an out-of-range handle (kernel.cxx:1154-1159).

## Value-change encodings
1-bit: single char '0'/'1' immediately followed by the id, no space (print_change vcd.cxx:631-645; clock/bool writers map CLK_HIGH/true to 1, vcd.cxx:505-533). Multi-bit (2..64 bits): 'b' + binary with leading zeros elided (print_binary vcd.cxx:596-618; all-zero value prints single '0') + one space + id. Wide (>64 bits): 'b' + WideData::print_binary with field_width=0 which also elides leading zeros and prints '0' for zero (wide_data.cxx:665-717) + space + id (vcd.cxx:647-661; the bits==1 wide branch at vcd.cxx:651-652 omits the 'b' and is practically unreachable). Every change line ends with '\n'. X: print_X (vcd.cxx:620-629) writes "x<id>" for 1-bit and "bx <id>" for multi-bit (single x digit, sign-extends per VCD semantics). Bluesim is 2-state: 'z' is never emitted, and 'x' only via vcd_write_x (dumpoff/XS mode, plus generated code for don't-care cases).

## Time markers and task sections
vcd_output_at_time (vcd.cxx:436-460) writes "#%llu\n" where the value is bk_now = sim_timescale * sim_time (kernel.cxx:1175-1178), deduped via last_time_written (vcd.cxx:438-441). If a task string was registered for that time via vcd_task (vcd.cxx:145-148), it prints the task name (e.g. "$dumpvars") on its own line right after the #time and sets need_end_task; the closing "$end\n" is emitted lazily just before the NEXT #time marker (vcd.cxx:445-446). Resulting shape: `#0` / `$dumpvars` / <changes...> / `$end` / `#5` / <changes...>. Tasks used: "$dumpvars" (kernel.cxx:362), "$dumpoff" (354), "$dumpall" (375), "$dumpon" (397).

## WHEN: the vcd_event and its priority
run_edge_schedule_event (each clock edge, kernel.cxx:222-313) calls setup_vcd_events when vcd_is_active() (kernel.cxx:297-299; active = vcd_enabled || vcd_checkpoint || go_xs, vcd.cxx:168-173). setup_vcd_events (kernel.cxx:593-609) schedules ONE vcd_event per timestep (dedupe via isVCDEvent find, kernel.cxx:598) at ev.at = sim_time, priority make_priority(PG_AFTER_LOGIC, PS_VCD), data.flag=false. Priority encoding: (group<<28)|(slot<<24)|clock, lower value fires first, events ordered by (at, priority) (priority.cxx:3-8; event_queue.cxx:18-26). Groups PG_INITIAL=0 < PG_BEFORE_LOGIC=1 < PG_LOGIC=2 < PG_AFTER_LOGIC=3 < PG_FINAL=4; slots PS_RESET=0 < PS_UI=1 < PS_CYCLE_DUMP=2 < PS_VCD=3 < PS_EXECUTE=4 < PS_RULE_DUMP=5 < PS_STATE_DUMP=6 < PS_COMBINATIONAL=7 (priority.h:18-35). Relative order in one timestep: clock-edge logic (PG_LOGIC,PS_EXECUTE — kernel.cxx:800,808; time-0 initial edge at PG_INITIAL,PS_EXECUTE kernel.cxx:872) -> reset deassert (PG_AFTER_LOGIC,PS_RESET, before VCD since slot 0<3) -> vcd_event (PG_AFTER_LOGIC,PS_VCD) -> after-edge combinational schedules (PG_FINAL,PS_COMBINATIONAL, kernel.cxx:804,812). Default reset: assert at t=0 (PG_INITIAL,PS_RESET), deassert at t=2 (PG_AFTER_LOGIC,PS_RESET) (setup_reset_events kernel.cxx:514-533). bk_VCD_combo_update (SystemC) schedules an extra vcd_event at (PG_BEFORE_LOGIC,PS_VCD) with data.flag=true = 'immediate' (kernel.cxx:1555-1568). When VCD is enabled, add_dummy_schedule_events forces edge events to exist even for clocks with no logic so every edge produces a VCD sample (kernel.cxx:493-501, 1527).

## vcd_event body and dump-type state machine
vcd_event (kernel.cxx:336-410): (1) if vcd_write_header succeeded (first time after file open / state VCD_OFF) emit main scope + defs + "$enddefinitions $end" (kernel.cxx:339-345); (2) vcd_advance(simHdl, ev.data.flag) to flush back-dated changes (kernel.cxx:347); (3) dt = get_VCD_dump_type (vcd.cxx:175-205): vcd_checkpoint pending -> VCD_DUMP_CHECKPOINT (clears flag; sets go_xs = !vcd_enabled so a one-shot $dumpall while disabled x's back out on the next event); else go_xs -> VCD_DUMP_XS (state=VCD_DISABLED); else by state: VCD_HEADER->VCD_DUMP_INITIAL, VCD_ENABLED->VCD_DUMP_CHANGES, VCD_DISABLED->VCD_DUMP_RESTART, then state=VCD_ENABLED; (4) per-case kernel writes clock values then model->dump_VCD(dt) (kernel.cxx:350-405); (5) vcd_check_file_size (kernel.cxx:407).

## VCD_DUMP_INITIAL: what is x vs real; has_initial_value
First dump after a header (state VCD_HEADER): vcd_task("$dumpvars") at now (kernel.cxx:362). Clocks: value written ONLY if clocks[clk].has_initial_value || bk_clock_cycle_count(clk)!=0 (kernel.cxx:363-369, the ~line-365 interaction); a clock declared without an initial value that has not yet ticked is simply omitted from the $dumpvars section — never explicitly x — so viewers show it as x until its first edge; bk_clock_cycle_count = max(posedge_count,negedge_count) (kernel.cxx:1133-1140). All model signals are dumped with REAL two-state values: generated dump_VCD dispatches dt==XS -> x-writes, dt==CHANGES -> diffed, ELSE (INITIAL/CHECKPOINT/RESTART) -> unconditional vcd_write_val of every member/port and primitive signal (SimCCBlock.hs:2027-2034; e.g. bs_prim_mod_reg.h:162-179). Nothing in the model is dumped as x at INITIAL.

## $dumpoff semantics (dump-all-x)
$dumpoff -> dollar_dumpoff -> bk_disable_VCD_dumping (dollar_dumpvars.cxx:28-31; kernel.cxx:1534-1545): removes any queued vcd_event, removes dummy edges, vcd_set_state(false), vcd_dump_xs sets go_xs=true (vcd.cxx:150-153). Because go_xs keeps vcd_is_active true, the next clock edge schedules one final vcd_event; get_VCD_dump_type returns VCD_DUMP_XS and sets state=VCD_DISABLED (vcd.cxx:186-191). That event writes task "$dumpoff", x for every clock (vcd_write_x width 1, kernel.cxx:352-359) and model->dump_VCD(VCD_DUMP_XS) which writes x for every signal (SimCCBlock.hs:2006-2010, 2030-2031) — i.e. Verilog-style checkpoint-to-x. Same XS path fires after a one-shot $dumpall issued while dumping is disabled (go_xs set at vcd.cxx:184).

## $dumpon / VCD_DUMP_RESTART
$dumpon -> bk_enable_VCD_dumping (dollar_dumpvars.cxx:23-26; kernel.cxx:1521-1532): vcd_set_state(true) (opens default file "dump.vcd" if none set, vcd.cxx:155-166) + add_dummy_schedule_events. At the next edge's vcd_event, state==VCD_DISABLED yields VCD_DUMP_RESTART (vcd.cxx:198): writes task "$dumpon", all clock values, and model->dump_VCD(RESTART) which dumps every signal unconditionally (kernel.cxx:395-403), restoring values after the x gap.

## $dumpall / bk_VCD_checkpoint
$dumpall -> bk_VCD_checkpoint (dollar_dumpvars.cxx:33-36; vcd.cxx:82-93): opens "dump.vcd" if no file, sets vcd_checkpoint=true (makes vcd_is_active true even if disabled). Next vcd_event: dt=VCD_DUMP_CHECKPOINT, task "$dumpall", all clocks + all model values unconditionally (kernel.cxx:373-381); afterwards go_xs = !vcd_enabled (vcd.cxx:184) so a checkpoint taken while dumping was off is followed by an XS/$dumpoff section.

## $dumpfile / $dumplimit / $dumpflush
$dumpfile -> bk_set_VCD_file (dollar_dumpvars.cxx:5-14; vcd.cxx:36-69): closes any open file, state=VCD_OFF; default name "dump.vcd"; if the name is in previous_files it reopens with "a" and state=VCD_DISABLED to avoid rewriting the header — but note nothing ever inserts into previous_files in the current code (only find vcd.cxx:52 and clear vcd.cxx:128), so that branch is dead. $dumplimit -> bk_set_VCD_filesize_limit (dollar_dumpvars.cxx:38-42; vcd.cxx:95-98); enforced after every vcd_event by vcd_check_file_size (vcd.cxx:233-247, called kernel.cxx:407): if ftello(file) > limit it writes "$comment\nVCD file size limit exceeded\n$end\n" (via vcd_write_comment vcd.cxx:249-253) and calls vcd_reset, which flushes pending changes, closes the file and turns VCD fully off (vcd.cxx:116-143). $dumpflush -> bk_flush_VCD_output = fflush only (dollar_dumpvars.cxx:44-47; vcd.cxx:100-104). $dumpvars(depth) -> bk_set_VCD_depth (only honored while state==VCD_OFF, vcd.cxx:76-80) + enable; depth 0 = unlimited, recursion into submodules stops when levels==1 (SimCCBlock.hs:1964-1985, 2066-2074; top passes vcd_depth(sim_hdl), SimBlocksToC.hs:530-543).

## Change detection: backing instance diffing
"Changed since last dump" is tracked by instantiating a complete second copy of the top module (the 'backing' instance) via construct-on-first-use <top>_backing(simHdl), bracketed by vcd_set_backing_instance(true/false) so constructors can detect they are building the shadow copy (SimBlocksToC.hs:604-624; vcd.cxx:255-271; bs_vcd.h:35,41). Model::dump_VCD(dt) calls <top>->dump_VCD(dt, vcd_depth, backing) (SimBlocksToC.hs:536-543). For VCD_DUMP_CHANGES each member/port/primitive value is compared to the backing copy and vcd_write_val is called only on inequality; the backing field is then assigned the new value (generated: SimCCBlock.hs:1997-2019 vcd_write/vcd_write_changed; primitives: e.g. bs_prim_mod_reg.h:167-178 `if (value != backing.value) vcd_write_val(...); backing.value = value;`). For INITIAL/CHECKPOINT/RESTART everything is written unconditionally and backing is still updated. Kernel clocks use edge-time instead: in VCD_DUMP_CHANGES a clock is written iff posedge_at==now || negedge_at==now (kernel.cxx:382-394). Ids must be consumed in lockstep with definition order, so skipped (unchanged) signals still increment the local id counter (`++num`, bs_prim_mod_reg.h:172).

## Back-dating combinational changes: clk_map / time_of_change / changes buffer
Bluesim evaluates everything at the posedge but Verilog-matching VCD wants combinational values to appear after the PREVIOUS edge (comment block vcd.cxx:15-34). Signals whose value 'lags' are associated with clock handles via vcd_set_clock(num, handle) into multimap clk_map (vcd.cxx:358-364; kernel.h:48,61; emitted for defs in a clock domain, SimCCBlock.hs:1562-1565, and primitive ports e.g. bs_prim_mod_reg.h:355-357). time_of_change(num) (vcd.cxx:462-489) returns: bk_now if changes_now (immediate mode) or if num has no clk_map entry; otherwise max over associated clocks of bk_clock_combinational_time = clocks[clk].combinational_at (kernel.cxx:1310-1316), which run_edge_schedule_event sets to the previous same-direction edge time before updating posedge_at/negedge_at (kernel.cxx:256-277). Every vcd_write_val/vcd_write_x overload (vcd.cxx:491-593) computes t=time_of_change; if t > min_pending the Change{num,bits,isX,narrow/wide} (kernel.h:22-46) is buffered in std::map<tTime,tChangeList> changes[t]; else it is written immediately after vcd_output_at_time(t).

## vcd_advance / min_pending / flush ordering
At the start of every vcd_event, vcd_advance(immediate) (vcd.cxx:387-403) recomputes min_pending = min(bk_now, min over all clocks of combinational_at), calls flush_changes, then sets changes_now = immediate (normal events pass false, kernel.cxx:605; combo-update events pass true, kernel.cxx:1565). flush_changes (vcd.cxx:405-434) walks the time-ordered changes map and for every time t STRICTLY < min_pending emits "#t" (vcd_output_at_time) followed by each buffered change (x via print_X, <=64-bit via narrow print_change, wide via wide print_change), erasing entries as it goes; it stops at the first t >= min_pending. This guarantees a time's changes are only written after every clock's combinational window has passed it, keeping #time markers monotonic.

## End-of-simulation flush behavior
There is no dedicated final-dump event. (1) Whenever the sim thread pauses (end of each bk_advance / yield / $finish), pause_sim and wait_for_sim_stop call fflush(NULL), flushing the VCD FILE* along with all stdio buffers (kernel.cxx:37-53, 59-65). (2) bk_shutdown (kernel.cxx:738-769) calls vcd_reset at kernel.cxx:767; vcd_reset (vcd.cxx:116-143) sets changes_now=false and min_pending=bk_now, runs flush_changes — which writes buffered changes at times STRICTLY BEFORE now and silently DROPS any buffered changes at exactly t==now (flush_changes early-return at vcd.cxx:412-413) — then fclose()s the file and zeroes all VCD state. A trailing task's closing "$end" is also never written if no later #time is emitted (need_end_task only flushes in vcd_output_at_time, vcd.cxx:445-446). bk_disable_VCD_dumping before shutdown yields a final $dumpoff/x section instead (kernel.cxx:1534-1545).

## Kernel-owned signals (clocks) in the dump
Each kernel clock (bk_define_clock) owns one 1-bit id, clocks[clk].vcd_num (kernel.cxx:923; accessor bk_clock_vcd_num kernel.cxx:1154-1159). Defs: every generated module's dump_VCD_defs loops over bk_num_clocks calling vcd_add_clock_def(this, bk_clock_name(clk), bk_clock_vcd_num(clk)) (SimCCBlock.hs:1938-1946), and vcd_add_clock_def emits "$var reg 1 <id> <name> $end" only when match_hierarchy accepts the module for that clock's dotted name (vcd.cxx:347-356; a top-level clock name without dots matches only the root module, vcd.cxx:317); input-clock ports of a module are declared as aliases sharing the clock's id (SimCCBlock.hs:1947-1953), as is the per-register CLK inside register sub-scopes (bs_prim_mod_reg.h:353). Values are written exclusively by vcd_event's kernel loops using bk_clock_val (current_value, kernel.cxx:1125-1131): INITIAL kernel.cxx:363-369 (skip if no initial value and never ticked), CHANGES kernel.cxx:384-391 (only at an edge time), CHECKPOINT/RESTART all clocks, XS all clocks as x. Clock ids are NOT in clk_map (vcd_set_clock is never called with a clock's own id by the kernel), so clock value changes timestamp at bk_now — the edge time itself. RST is not a kernel signal: reset waveforms appear via primitives dumping !in_reset on their RST ids (e.g. bs_prim_mod_reg.h:354, 380, 408), driven by reset_model_event at t=0 (assert, PG_INITIAL/PS_RESET) and t=2 (deassert, PG_AFTER_LOGIC/PS_RESET, which runs before the same-time vcd_event since PS_RESET<PS_VCD) (kernel.cxx:177-181, 514-533).

## API surface for a reimplementation
Module-facing contract (bs_vcd.h:39-82): vcd_reserve_ids(n)->first id; vcd_write_id(num) prints base-94 id; vcd_write_scope_start/end; vcd_write_def(num,name,width); vcd_add_clock_def(module,name,num); vcd_set_clock(num,clock_handle) registers the lag clock; vcd_write_x(num,width); vcd_write_val overloads for tClockValue/bool/tUInt8/tUInt32/tUInt64/tUWide (1-bit overloads ignore the width arg, vcd.cxx:505-533); vcd_depth() and vcd_is_backing_instance() for construction-time queries. Kernel-facing (bs_vcd.h:24-37): vcd_reset, vcd_dump_xs, vcd_set_state, vcd_is_active, vcd_keep_ids, vcd_write_comment, vcd_write_header, get_VCD_dump_type, vcd_check_file_size, vcd_set_backing_instance, vcd_task, vcd_advance, vcd_output_at_time. Dump-type enum tVCDDumpType {NONE, XS, INITIAL, CHECKPOINT, CHANGES, RESTART} (bs_vcd.h:16-22); VCD status enum {VCD_OFF, VCD_HEADER, VCD_ENABLED, VCD_DISABLED} and tVCDState fields in kernel.h:19-79. Public control API: bk_set_VCD_file/bk_get_VCD_file_name/bk_set_VCD_depth/bk_VCD_checkpoint/bk_set_VCD_filesize_limit/bk_flush_VCD_output (vcd.cxx:36-104), bk_enable/disable/is_VCD_dumping (kernel.cxx:1521-1550), bk_VCD_combo_update (kernel.cxx:1555-1568).

# Bluesim primitive VCD dumping: FIFO, RegFile, BRAM (/home/user/bsc/src/bluesim/)

One template class per header covers all the library variants: MOD_Fifo&lt;T&gt; (bs_prim_mod_fifo.h:32-496, parameterized by width/depth/guarded/fifo_type, so FIFO1/FIFO2/SizedFIFO/FIFOL/FIFOL1/BypassFIFO are all the same VCD code), MOD_RegFile&lt;AT,DT&gt; (bs_prim_mod_regfile.h:171-539, plain + memfile-load constructors), and MOD_BRAM&lt;AT,DT,ET&gt; (bs_prim_mod_bram.h, 4 constructors for single/dual-port x plain/file-loaded, plus a pipelined flag). FIFOs dump a scope named after the instance containing RST/FULL_N/EMPTY_N/ENQ/D_IN/DEQ/CLR plus one arr_i var per slot in queue order (arr_0 is the head and shares its VCD id with D_OUT); there is no "level" VCD var (level exists only as a debug symbol). RegFile dumps absolutely nothing (no scope, no vars — not even ports). BRAM dumps ports only (EN/WE/ADDR/DI/DO per port), never memory contents. FIFOs write X into every slot at index &gt;= elems (hence D_OUT is X whenever empty); BRAM/RegFile never emit X outside the checkpoint VCD_DUMP_XS pass. Each dump_VCD takes a `backing` shadow instance for change detection and syncs it at the end.

## FIFO: one class, all variants
MOD_Fifo<T> (bs_prim_mod_fifo.h:32-496) is the only FIFO primitive; constructor args width/depth/guarded/fifo_type (line 36-38) with tFifoType {FIFO_SIMPLE, FIFO_LOOPY, FIFO_BYPASS} (line 11) distinguish FIFO1/FIFO2/SizedFIFO/FIFOL/BypassFIFO. dump_VCD_defs/dump_VCD are identical for all variants; only `size` (depth) and `bits` (width) change the var list.

## FIFO: scope and var list (dump_VCD_defs, bs_prim_mod_fifo.h:263-293)
One scope named inst_name (vcd_write_scope_start line 268, end line 291). Vars in order: CLK (1 bit, defined with the global clock's id via bk_clock_vcd_num, line 269 — an alias, no id reserved), RST (1, line 270), FULL_N (1, line 271), EMPTY_N (1, line 272), ENQ (1, line 274), D_IN (bits wide, only if bits>0, line 278), DEQ (1, line 281), CLR (1, line 283), D_OUT (bits, only if bits>0, line 285), arr_0..arr_{size-1} (bits each, lines 286-290, name via snprintf "arr_%d"). ENQ, D_IN, DEQ, CLR are bound to the clock domain with vcd_set_clock (lines 273/277/280/282); RST/FULL_N/EMPTY_N/D_OUT/arr_i are not.

## FIFO: no level/full/empty-count var; head is arr_0; D_OUT aliases arr_0
There is NO VCD var for the fill level — "level" is only a symbol-table entry (symbols[2], bs_prim_mod_fifo.h:74-76) for the debug API. Occupancy is visible via FULL_N/EMPTY_N and via which arr_i are X. dump_VCD_defs reserves size + 6 + (bits>0?1:0) ids (line 265); D_OUT is written with id `n` WITHOUT incrementing (line 285, comment "alias of arr_0"), so D_OUT and arr_0 share one VCD id — arr_0 IS the head element, since values are dumped in queue order starting at fst.

## FIFO: full/initial dump values (bs_prim_mod_fifo.h:386-410)
The else branch (VCD_DUMP_INITIAL and full checkpoints) writes: RST = !in_reset (line 388), FULL_N = METH_notFull() (389), EMPTY_N = METH_notEmpty() (390), ENQ/DEQ/CLR = whether enq_at/deq_at/clear_at == bk_now (391-398), D_IN = dummyval (396, only if bits>0), then for each slot i: data[(fst+i)%size] if i<elems else vcd_write_x (399-406). At t=0: elems=0 so ALL arr_i and D_OUT dump as X; ENQ/DEQ/CLR are 0 (timestamps init to ~bk_now, lines 41-42); in_reset=false (line 52) so RST=1; FULL_N=1, EMPTY_N=0; D_IN shows the write_undet pattern (dummyval initialized undet, lines 55-56).

## FIFO: data-dependent X convention and D_IN semantics
Empty slots are X: any arr index >= elems is written with vcd_write_x (bs_prim_mod_fifo.h:405, and in CHANGES mode line 380-381 when a slot transitions occupied->empty). Consequently D_OUT (= arr_0) is X exactly when the FIFO is empty — D_OUT carries real data only when nonempty. Slots are renormalized each dump to queue order data[(fst+i)%size] (lines 375, 401), so arr_i is the i-th oldest element, not the raw ring-buffer cell. D_IN = dummyval, which METH_enq assigns BEFORE any full/guard checks (line 124), so D_IN shows the last attempted enqueue value and holds it forever after.

## FIFO: VCD_DUMP_CHANGES specifics (bs_prim_mod_fifo.h:310-385)
RST/FULL_N/EMPTY_N compared against the backing instance and dumped on change (312-323). ENQ/DEQ/CLR are only re-evaluated when at_posedge of __clk_handle_0 (bk_clock_val==CLK_HIGH && last_edge==now, lines 324-326); off-edge they are skipped (++num). did_enq/did_deq/did_clear = bk_is_same_time(enq_at/deq_at/clear_at) (329, 349, 362). Element loop (373-384): write value if i<elems and (slot newly occupied i>=backing.elems, or value changed vs backing at its queue position); write X if slot became empty (i>=elems && i<backing.elems); else skip. VCD_DUMP_XS (297-309) writes X to everything: 4x 1-bit, D_IN(bits), 2x 1-bit, size x bits. Backing state (fst/elems/data/in_reset/dummyval) synced at 412-417.

## RegFile/RegFileLoad: nothing is dumped at all
MOD_RegFile<AT,DT> covers both RegFile (ctor bs_prim_mod_regfile.h:175) and RegFileLoad (memfile ctor line 189, bin/hex via Bin/HexFormatHandler). dump_VCD_defs (467-473) just `return (num)` — no scope, no vars, no ids reserved: neither memory contents NOR ports/addresses appear in the VCD. dump_VCD (474-479) is an empty no-op. Comment at 469-471: "Memory contents are not dumped / Please update ../lib/tcllib/bluespec/Waves.tcl proc correct_regfile_names when vcd dumping is enabled." Related: backing instances skip storage allocation entirely (lines 182-183, 197-198) since dump_VCD never reads them.

## BRAM: ports only, never memory contents (dump_VCD_defs, bs_prim_mod_bram.h:756-797)
Comment lines 758 and 800: "Memory contents are not dumped, only ports". Reserves dual_port ? 10 : 5 ids (759); scope = inst_name (761, end 795). Dual-port var order: CLKA (alias of clk0's id via bk_clock_vcd_num, 763), ENA (1, 765), WEA (num_wens bits, 767), ADDRA (addr_bits, 769), DIA (data_bits, 771) — each of ENA/WEA/ADDRA/DIA vcd_set_clock'd to __clk_handle_0 (764/766/768/770) — then DOA (data_bits, NOT clock-bound, 772); then CLKB (alias of clk1, 773), ENB/WEB/ADDRB/DIB bound to __clk_handle_1 (774-781), DOB (782). Single-port: CLK/EN/WE/ADDR/DI (clock-bound) and DO (784-793). No per-address vars, no read/write of the sparse storage in VCD code.

## BRAM: dump_VCD value semantics (bs_prim_mod_bram.h:798-948)
Full/initial branch (906-931): ENA=did_ena=bk_is_same_time(upd_a_at) (908-909), WEA=upd_a_wens raw (910), ADDRA=upd_a_addr (911), DIA=upd_a_val (912), DOA=out_reg2_a if pipelined else out_reg_a (913-916); port B symmetric via upd_b_* on 917-928. So ADDR/DI/WE reflect the last put() request (read OR write — a read is a put with write_ens==0, see METH_a_put 610-641), and DO is the registered (or 2-stage pipelined) read output. VCD_DUMP_XS (802-817) writes X across EN(1)/WE(num_wens)/ADDR(addr_bits)/DI(data_bits)/DO(data_bits) per port. Backing sync at 933-947.

## BRAM: CHANGES-mode edge gating and WE-forced-to-zero quirk
In VCD_DUMP_CHANGES (818-905), port A signals update only at posedge of __clk_handle_0 (820-822), else num+=5 (860); port B likewise on __clk_handle_1 (863-865, 903). ENA change-detected vs backing.did_ena (828-834). WEA: dumped when did_write or wens changed, and if did_ena is false it writes literal 0 instead of upd_a_wens (835-843, line 838 `vcd_write_val(..., 0llu, num_wens)` with comment "it's OK that 0 may not be of type ET") — i.e. in CHANGES mode WE displays 0 when EN is low, whereas the full-dump branch dumps raw upd_a_wens unconditionally (910); a minor asymmetry. ADDRA/DIA/DO change-detected at 844-857. Port B mirrors this at 866-901.

## BRAM: initial values (constructor init, bs_prim_mod_bram.h:288-310, 365-366)
upd_a_addr/upd_b_addr get init_val only, NO write_undet (288, 300) — so initial ADDR dumps as 0. upd_a_val/out_reg_a/out_reg2_a/upd_a_wens (and B equivalents) are write_undet (289-310), so initial DI/WE/DO show the undet pattern, not X. Memory array entries are also write_undet at allocation (365-366) but never appear in VCD. Initial EN=0 (upd_a_at initialized to ~now). Outside VCD_DUMP_XS, BRAM never writes X — unlike the FIFO's empty-slot convention, stale DO after reads of unwritten cells just shows the undet bit pattern (out_reg re-undet'd at 450/509/537/596).

## Cross-cutting mechanics relevant to a reimplementation
(1) CLK/CLKA/CLKB vars are defined with bk_clock_vcd_num(sim_hdl, clk_handle) — the module aliases the kernel-owned clock waveform id and never dumps clock values itself (fifo.h:269, bram.h:763/773/784). (2) vcd_set_clock(sim_hdl, id, clk) marks a var as edge-sampled so CHANGES-mode dumps for it happen only at that clock's posedge — matching the at_posedge guards in dump_VCD. (3) Every dump_VCD(dt, backing) receives a shadow "backing" instance of the same primitive used for change detection and mirrors all VCD-relevant state into it before returning (fifo.h:412-417, bram.h:933-947); RegFile needs none. (4) dump_VCD_defs returns the running id counter `num`/`n` so the caller can continue numbering (fifo.h:292, regfile.h:472, bram.h:796), and ids are pre-reserved with vcd_reserve_ids (fifo.h:265, bram.h:759).

# Bluesim C++ runtime VCD behavior for register/wire/probe/counter primitives (src/bluesim/bs_prim_mod_reg.h, bs_prim_mod_wire.h, bs_prim_mod_probe.h, bs_prim_mod_counter.h, with shared helpers in bs_vcd.h / vcd.cxx)

Every Bluesim primitive declares its VCD vars via vcd_write_def, which ALWAYS emits "$var reg WIDTH id NAME $end" (vcd.cxx:376-385) — there is no wire-typed $var anywhere; even clocks are "$var reg 1" (vcd.cxx:352). Dump modes come from tVCDDumpType (bs_vcd.h:16-22): VCD_DUMP_INITIAL is the $dumpvars pass (kernel.cxx:360-372), VCD_DUMP_CHANGES the steady-state delta pass, VCD_DUMP_XS the $dumpoff/end-of-sim pass that forces every signal to x (kernel.cxx:352-359, 1544), and CHECKPOINT/RESTART ($dumpall/$dumpon) fall into each primitive's dump-everything else-branch. Registers are initialized with write_undet (alternating-bit pattern 0xAAAA..., wide_data.cxx:1377-1412), so at $dumpvars they dump that concrete pattern, NOT x; x appears only from VCD_DUMP_XS and from unwritten RWire data. Simple prims (Reg, ConfigReg, RegTwo) declare one flat $var named inst_name in the parent module scope; structured prims (RegAligned, CReg, Counter) declare a parent-scope alias plus a "$scope module inst_name" sub-scope of port signals whose ids are back-dated to the clock edge via vcd_set_clock (vcd.cxx:358-364, time_of_change vcd.cxx:462-487). Aliases reuse the same VCD id without incrementing. RegTwo's two write ports share one signal; CReg names per-port signals Q_OUT_i/EN_i/D_IN_i for its fixed 5 ports.

## vcd_write_def always emits reg-typed vars
vcd.cxx:376-385 vcd_write_def writes literally "$var reg %d <id> <name> $end" for every signal of every primitive; vcd_add_clock_def (vcd.cxx:347-356) writes "$var reg 1" for clocks. Bluesim never emits a wire-typed $var, so a reimplementation should use var type reg for all primitive signals. Scopes are opened with "$scope module <name>" (vcd_write_scope_start, vcd.cxx:366-369) and closed with "$upscope $end" (371-374). 1-bit values print as 0/1<id> with no space; multi-bit as b<binary> <id>; x as 'x'/'bx ' (print_change vcd.cxx:631-644, print_X ~vcd.cxx:620-629, leading zeros stripped by print_binary vcd.cxx:597-616).

## Dump-type protocol shared by all prims
bs_vcd.h:16-22 defines VCD_DUMP_{NONE,XS,INITIAL,CHECKPOINT,CHANGES,RESTART}. kernel.cxx:349-401: XS => $dumpoff + every clock and every prim writes x; INITIAL => $dumpvars + full-value dump; CHECKPOINT => $dumpall; RESTART => $dumpon; CHANGES => steady state. Each primitive's dump_VCD(dt, backing) switches three ways: dt==VCD_DUMP_XS => write x for every id; dt==VCD_DUMP_CHANGES => write only signals that differ from the 'backing' shadow instance; anything else (INITIAL/CHECKPOINT/RESTART) => write all values unconditionally. Every path then refreshes backing.* so the next CHANGES pass diffs correctly. At sim end, kernel.cxx:1544 forces one final XS pass (all signals go x).

## x-at-start convention: registers do NOT start as x
Reg/ConfigReg/RegTwo/CReg/Probe values are initialized with write_undet (bs_prim_mod_reg.h:29-31,67-71,486-510,658-677,830-846,864-881; bs_prim_mod_probe.h:19-20), which stores the alternating-bit pattern 0xAAAAAAAA... masked to width (wide_data.cxx:1377-1412; bool => false). The VCD_DUMP_INITIAL pass dumps these as concrete binary values (e.g. b1010... ), never x. The only x sources are: (1) any VCD_DUMP_XS pass ($dumpoff / end of sim), and (2) MOD_Wire data wires when 'written' is false (bs_prim_mod_wire.h:108-111) — so an RWire whose wset didn't fire shows x during normal dumping, including the initial dump (written starts false, bs_prim_mod_wire.h:18,39).

## Clock-shifting via vcd_set_clock (back-dating changes to the clock edge)
vcd_set_clock(simHdl,num,handle) (vcd.cxx:358-364) registers id->clock in clk_map; time_of_change (vcd.cxx:462-487) then timestamps that id's writes at the clock's most recent combinational/edge time (bk_clock_combinational_time) instead of the current time, buffering them in vcd.changes until flush (vcd_write_x/val, vcd.cxx:491-503; flush_changes vcd.cxx:405+). Prims apply this to method/input signals sampled after the edge: Wire (bs_prim_mod_wire.h:90-91, only when shift_vcd), Probe (bs_prim_mod_probe.h:49), CReg all port signals (bs_prim_mod_reg.h:1008-1016), Counter all EN/DATA signals (bs_prim_mod_counter.h:174-189), RegAligned EN and D_IN (bs_prim_mod_reg.h:355-357).

## MOD_Reg (RegN/RegA/RegUN, CrossingRegN/A/UN, RevertReg)
bs_prim_mod_reg.h:18-227; primMap (src/comp/SimPrimitiveModules.hs:264-274) maps RegN/RegUN/RegA, CrossingRegN/UN/A and RevertReg to this one class. dump_VCD_defs (156-161): no scope; reserves 1 id; single '$var reg <bits> <id> <inst_name>' in the PARENT module's scope; returns vcd_num+1. dump_VCD (162-179): XS => x(bits); CHANGES => write value only if value!=backing.value else skip id; else => always write value; then backing.value=value. Initial $dumpvars therefore shows the write_undet pattern for never-written/never-reset regs. Only the current 'value' is dumped — prev_value/CrossingReg 'crossed' view is not a VCD signal.

## MOD_ConfigReg (ConfigRegN/A/UN) and MOD_RegTwo (RegTwo) — single flat var; RegTwo ports NOT split
MOD_ConfigReg bs_prim_mod_reg.h:476-641: dump_VCD_defs (578-583) = 1 id, '$var reg bits <id> <inst_name>', no scope; dump_VCD (584-593): XS => x, else write value unless (CHANGES && unchanged). MOD_RegTwo bs_prim_mod_reg.h:648-812: despite two write methods setA/setB (690-715, A beats B within a cycle via a_at), VCD is IDENTICAL to ConfigReg — dump_VCD_defs (748-753) declares only the single <inst_name> var of width bits and dump_VCD (754-763) dumps only 'value'; there are no per-port A/B signals. Both dump the post-write value, not the old_value the get/read method returns mid-cycle.

## MOD_CReg (CRegN5/CRegA5/CRegUN5) — 5 fixed ports, Q_OUT_i/EN_i/D_IN_i naming, parent alias
bs_prim_mod_reg.h:817-1131; ports==max_ports==5 always (1080-1081, ctors 824,859). dump_VCD_defs (989-1022): reserves 3*ports ids; first writes '<inst_name>' (width bits) in the parent scope WITHOUT incrementing num (998) so it aliases Q_OUT_0's id; then '$scope module <inst_name>' containing, for i in 0..4: Q_OUT_%u (bits) [i=0 reuses alias id], EN_%u (1), D_IN_%u (bits) — names snprintf'd (1007-1017), each id vcd_set_clock'd to __clk_handle_0. dump_VCD (1023-1076): XS => x for all 3*ports ids; full dump computes a chain: tmp_q_out starts at read_val[0] (=value_rec, the registered value latched in clk() at 943-955) and after emitting port i's Q_OUT_i=tmp_q_out, EN_i=did_write_rec[i], D_IN_i=write_val[i], sets tmp_q_out=write_val[i] if did_write_rec[i] — i.e. Q_OUT_i is the value port i reads (register value updated by writes of lower-numbered ports); CHANGES does the same chain but emits each signal only when != backing. EN_i reflects whether port i wrote during the just-ended cycle (did_write latched into did_write_rec on clk, 951-954); D_IN_i is sticky (last written data, undet pattern until first write).

## MOD_RegAligned — parent var + CLK/RST/EN/D_IN/Q_OUT sub-scope
bs_prim_mod_reg.h:234-469 (RegAligned primitive; included for completeness alongside Reg). dump_VCD_defs (347-362): reserves 4 ids; '<inst_name>' (bits) in parent scope; scope '<inst_name>' contains CLK (1 bit, REUSES the global clock's id via bk_clock_vcd_num — no new id, 353), RST (1), EN (1, clock-shifted to __clk_handle_1), D_IN (bits, clock-shifted to __clk_handle_1), and Q_OUT (bits) as an alias of the first id (359); returns first_id+4. dump_VCD (363-415): XS => x,x,x,x for value/RST/EN/D_IN; RST is dumped as !in_reset (active-low, 380/408); EN is did_write=bk_is_same_time(written_at) and in CHANGES mode is only re-sampled at the input clock's posedge (383-396); D_IN is next_value. CLK itself is dumped by the kernel with the clocks, not here.

## MOD_Wire (RWire, RWire0/PulseWire, BypassWire, BypassWire0, CrossingBypassWire)
bs_prim_mod_wire.h:12-131; primMap (SimPrimitiveModules.hs:270,278-281,325) maps all of RWire/RWire0/BypassWire/BypassWire0 with is_sync_wire=false and CrossingBypassWire/BypassCrossingWire with true; shift_vcd=!is_sync_wire (19,39; 2-arg ctor defaults is_sync_wire=false at 37). dump_VCD_defs (87-97): 1 id, no scope; if shift_vcd the id is vcd_set_clock'd to __clk_handle_0 (changes back-dated to the last clock edge); var name is inst_name, width=bits, EXCEPT bits==0 (RWire0/PulseWire/BypassWire0) which declares width 1 (94-95). dump_VCD (98-122): XS => vcd_write_x(bits) [note: passes raw bits even when 0]; dump condition for non-XS: (dt!=CHANGES) || written!=backing.written || (both written && value!=backing.value). Emitted value: bits>0 => 'written ? value : x' — an un-set wire dumps x every non-XS pass including $dumpvars (written starts false, 18/39); bits==0 => the 1-bit 'written' flag itself (whas), 0/1, never x outside XS (115-117). 'written' is isValid latched then cleared at clk() (73-77), so the VCD shows whether the wire fired during the cycle that ended at the (back-dated) edge.

## MOD_Probe and MOD_ProbeWire
bs_prim_mod_probe.h. MOD_Probe (11-99): dump_VCD_defs (42-53) reserves 1 id, names the var '<inst_name>$PROBE' (asprintf, 45), width bits, no scope, id vcd_set_clock'd to __clk_handle_0 (49) so changes back-date to the clock edge. dump_VCD (54-63): XS => x; else write value unless (CHANGES && value==backing.value). value starts as the write_undet pattern (19-20) so the initial dump is concrete, not x. MOD_ProbeWire (103-133) contributes NOTHING to VCD: dump_VCD_defs returns vcd_num unchanged (123-126) and dump_VCD is empty (127-129).

## MOD_Counter — parent var + 8 method signals + q_state/Q_OUT aliases
bs_prim_mod_counter.h:11-311. dump_VCD_defs (168-194): reserves 9 ids; '<inst_name>' (bits) in parent scope on the first id; scope '<inst_name>' declares, in order and each clock-shifted to __clk_handle_0: ADDA (1), DATA_A (bits), ADDB (1), DATA_B (bits), SETC (1), DATA_C (bits), SETF (1), DATA_F (bits); then 'q_state' AND 'Q_OUT' (bits) both as aliases reusing the first id (190-191); returns vcd_num+9. dump_VCD (195-286): XS => x for all 9 ids; full dump => val, then for each method m in {addA,addB,setC,setF}: EN bit did_*=bk_is_same_time(sim_hdl,*_at) (whether the method fired in the cycle ending now) followed by its sticky data arg (a/b/c/f); CHANGES => val if changed, and the 8 method signals are only re-evaluated when the clock is at a posedge occurring now (213-215), each emitted only if different from backing. Initial values: val gets write_undet (23-24) so dumps the 0xAA pattern; a/b/c/f only get init_val (25-28), which for scalar types is a no-op (wide_data.cxx:1354-1357) / size-set for WideData (1349-1352), so their first dumped values are effectively uninitialized/zero until the methods fire; the did_* bools (307-310) are set during the INITIAL dump before backing comparison.

## Alias mechanics and id accounting a reimplementation must copy
Aliases are made by calling vcd_write_def twice (or more) with the SAME id and different names — CReg parent alias (bs_prim_mod_reg.h:994-998 comment: 'aliases re-use the same number, so we reserve 3*ports'), RegAligned Q_OUT (359), Counter q_state/Q_OUT (190-191), RegAligned CLK reusing the kernel clock id (353). Ids are allocated contiguously per-prim by vcd_reserve_ids(sim_hdl,n) and each prim stores the base in Module::vcd_num; the num argument passed into dump_VCD_defs is ignored by all these prims. During CHANGES dumps, skipped (unchanged) signals still increment the local id counter so positions stay aligned (e.g. bs_prim_mod_reg.h:171-172,377-402; bs_prim_mod_counter.h:211-256).

# Bluesim VCD dumping for clock/synchronizer/reset primitives (src/bluesim/bs_prim_mod_clockgen.h, bs_prim_mod_clockmux.h, bs_prim_mod_gatedclock.h, bs_prim_mod_synchronizers.h, bs_prim_mod_resets.h)

Each primitive implements dump_VCD_defs (writes a $scope named inst_name, declares $var entries, returns the running id count) and dump_VCD(dt, backing) (three modes: VCD_DUMP_XS writes X to every owned id, VCD_DUMP_CHANGES diffs against a shadow 'backing' instance and writes only changed ids while still incrementing the id cursor for unchanged ones, otherwise full-dump all values; backing fields are then updated). Ids are allocated via vcd_reserve_ids into the Module::vcd_num field — most prims ignore the incoming num argument. Output (and some input) clocks are NOT given fresh ids: the prim writes a $var def whose id is bk_clock_vcd_num(sim_hdl, clk_handle), the kernel-owned shared id for that clock, so the identical VCD identifier code appears both at top level and inside the prim scope (an alias); the prim's dump_VCD never writes values for aliased ids because the kernel emits those value changes when the clock toggles. Several prims keep tClock handles purely for this VCD aliasing (ClockDivider's __clk_handle_1, SyncHandshake's __clk_handle_0/1, SyncReset's __clk_handle_0). ClockMux, ClockSelect, SyncReset0, ResetMux, ResetEither, ResetToBool, and DualPortRam dump nothing; InitialReset writes an empty scope yet reserves 3 ids that are never used.

## MOD_ClockGen (mkAbsoluteClock) — pure clock alias, no owned vars
bs_prim_mod_clockgen.h:40-46: dump_VCD_defs opens scope inst_name (line 42), declares single 1-bit var "CLK_OUT" with the SHARED id bk_clock_vcd_num(sim_hdl, __clk_handle_0) (line 43) — no vcd_reserve_ids call, returns num unchanged (line 45). dump_VCD (lines 47-50) is a no-op: the kernel drives the aliased clock id itself. No initial-X, no change detection.

## MOD_MakeClock (mkClock) — 2 owned ids + CLK_OUT alias
bs_prim_mod_clockgen.h:154-164: reserves 2 ids into vcd_num (156); scope inst_name (158); "CLK_OUT" aliased to bk_clock_vcd_num(__clk_handle_0) (159); owned 1-bit vars "CLK_GATE_OUT" (160, dumps PORT_CLK_GATE_OUT) and "CLK_VAL_OUT" (161, dumps current_clk register — not old_out_clk). dump_VCD 165-191: XS writes 2 Xs (170-171); CHANGES compares PORT_CLK_GATE_OUT and current_clk against backing (175-182); full dump 186-187; backing updated unconditionally (189-190). Initial values from ctor 71-74: current_clk=old_out_clk=initValue (CLK_HIGH iff initClock param), PORT_CLK_GATE_OUT=new_gate=initGate.

## MOD_ClockInverter — 4 owned ids + CLK_OUT alias; PREEDGE is write-once
bs_prim_mod_clockgen.h:255-267: reserves 4 ids (257); scope; owned "CLK_IN" (260), "CLK_GATE_IN" (261), "PREEDGE" (262); "CLK_OUT" aliased via bk_clock_vcd_num(__clk_handle_0) BETWEEN owned defs (263); owned "CLK_GATE_OUT" (264). dump_VCD 268-311: XS writes 4 Xs and resets preedge=false (273-277); CHANGES compares clk_in, clk_gate_in (recorded in clk() at 248-249), then sets preedge=true before comparing (289-293) so PREEDGE dumps 1 once after each XS dump and never changes again; PORT_CLK_GATE_OUT compared at 294-297; full dump 301-305 also forces preedge=true. Initial values ctor 215-217: current_clk=CLK_LOW, PORT_CLK_GATE_OUT=1, preedge=false; clk_in/clk_gate_in unset until first clk().

## MOD_ClockDivider — 2 owned ids + BOTH clocks aliased; VCD-only input clock handle
bs_prim_mod_clockgen.h:416-427: reserves 2 ids (418); scope; "CLK_IN" aliased to bk_clock_vcd_num(__clk_handle_1) (421) where __clk_handle_1 is a handle kept ONLY for VCD (set_clk_1 at 359-362; field comment 470-471); "CLK_OUT" aliased to bk_clock_vcd_num(__clk_handle_0) (422); owned "RST" (423) and "PREEDGE" (424). dump_VCD 428-455: RST dumped active-low as !in_reset when in_reset changed (438-441, full dump 450); PREEDGE is the derived value (cntr == transition-1) — CHANGES guard at 442-444 ((cntr!=backing.cntr) && (cntr==transition-1 || backing.cntr!=transition-1)) suppresses the 1->0 write at the dump where cntr leaves transition-1 (the 0 only appears at the NEXT cntr change) and writes redundant 0s on other cntr moves; full dump 451. backing.in_reset/cntr updated at 453-454. Initial ctor 340-343: cntr=upper-offset, transition=1<<(width-1), in_reset=false, PORT_CLK_GATE_OUT=0 — note the gate is NOT dumped.

## MOD_ClockMux / MOD_ClockSelect — no VCD output at all
bs_prim_mod_clockmux.h:78-85 (ClockMux) and 234-241 (ClockSelect): dump_VCD_defs returns num unchanged with no scope, dump_VCD empty. Their output clock __clk_handle_0 still appears in the waveform only via the kernel's top-level clock dump (bk_get_or_define_clock at 44 / 168), never aliased into a prim scope.

## MOD_GatedClock — 1 owned id, NO clock alias
bs_prim_mod_gatedclock.h:92-99: reserves 1 id (94); scope inst_name (95); single 1-bit var "new_gate" (96) dumping PORT_CLK_GATE_OUT; returns vcd_num+1 (98) ignoring incoming num. The gated output clock is not aliased inside the scope (clk_in_hdl obtained via bk_get_clock_by_name at 43 is used only to model the latch). dump_VCD 100-110: XS -> X; otherwise writes on full dump or when backing.PORT_CLK_GATE_OUT differs, updating backing only when written (104-108). Initial ctor 21-22: PORT_CLK_GATE_OUT=0, internal reg write_undet.

## MOD_Sync2 / MOD_Sync15 (SyncBit-style, 2- and 1.5-cycle) — 3 owned ids
bs_prim_mod_synchronizers.h:116-125 (Sync2) and 213-221 (Sync15): reserve 3 ids; scope inst_name; 1-bit vars "dSyncReg1" (vcd_num), "dSyncReg2" (vcd_num+1), "sSyncReg" (vcd_num+2); return vcd_num+3. dump_VCD 126-153 / 223-250: XS -> 3 Xs; else per-var (dt!=VCD_DUMP_CHANGES || backing差) pattern with backing updated per-var only when written; sSyncReg compared and dumped via sSyncReg.read() (136-151) — SyncVar::read (28-34) returns prev_value if written at the current timestamp, so the dumped value has non-blocking-assignment semantics. Initial ctor 70-78 / 168-176: dSyncReg1/dSyncReg2 write_undet, SyncVar contents write_undet (22-24).

## MOD_Sync1 (1- and 0.5-cycle bit sync) — 2 owned ids
bs_prim_mod_synchronizers.h:307-314: reserves 2 ids; scope; "dSyncReg1" (vcd_num) and "sSyncReg" (vcd_num+1), 1 bit each; returns vcd_num+2. dump_VCD 316-337 follows the same per-var full/changes pattern with sSyncReg.read(). Initial: dSyncReg1 write_undet (270-271).

## MOD_SyncPulse — 4 owned ids
bs_prim_mod_synchronizers.h:406-415: reserves 4 ids; scope; 1-bit "dSyncReg1", "dSyncReg2", "dSyncPulse", "sSyncReg" at vcd_num..vcd_num+3; returns vcd_num+4. dump_VCD 417-450: XS -> 4 Xs; else per-var compare/write with per-var backing update; sSyncReg via .read(). Initial ctor 353-363: all three d-regs write_undet.

## MOD_SyncHandshake — 12 ids reserved / 10 used, sCLK+dCLK aliases, vcd_set_clock on sEN
bs_prim_mod_synchronizers.h:586-605: reserves 12 ids (588) but only 10 are consumed; scope inst_name (590); owned 1-bit vars in id order: "dSyncReg1","dSyncReg2","dLastState","sToggleReg","sSyncReg1","sSyncReg2","sRDY" (591-597); vcd_set_clock(sim_hdl, n, __clk_handle_0) at 598 registers the NEXT id (sEN) in the VCD clk_map — vcd.cxx:358-364 inserts (num,handle) so the kernel ties that signal to sCLK's domain; "sEN" (599); "sCLK" aliased to bk_clock_vcd_num(__clk_handle_0) (600) and "dCLK" to bk_clock_vcd_num(__clk_handle_1) (601) — both handles exist only for VCD (set_clk_0/set_clk_1 at 533-541, field comments 707-709); "sRST" (602) and "dPulse" (603). dump_VCD 607-691: XS -> 10 Xs (612-621); CHANGES compares dSyncReg1, dSyncReg2.probe(), dLastState.probe(), sToggleReg.probe() (SyncVar::probe at line 35 = raw current value, unlike .read()), sSyncReg1, sSyncReg2, sRDY, did_send (sEN, backing.did_send updated only when written, 653-657), !in_reset for sRST (active-low, 660-663), pulsing for dPulse (664-667); full dump 671-681; remaining backing fields synced unconditionally 683-690. Initial ctor 480-497: sSyncReg1=sSyncReg2=1, sRDY=0, dSyncReg1 write_undet, en=did_send=pulsing=in_reset=false.

## MOD_SyncReg<T> — 2 owned ids plus NESTED handshake scope
bs_prim_mod_synchronizers.h:784-793: reserves 2 ids; scope inst_name; width-bit vars "dD_OUT" (vcd_num) and "sDataSyncIn" (vcd_num+1) (788-789); then calls sync.dump_VCD_defs(vcd_num+2) at 790 while the scope is still open, producing an inner scope named "sync" (the embedded MOD_SyncHandshake's inst_name, ctors at 722/730) with all its vars/aliases; scope end 791; returns the nested count. dump_VCD 794-816: XS -> 2 Xs; else per-var compare (sDataSyncIn via .read(), 808-813); always delegates sync.dump_VCD(dt, backing.sync) (815). Initial ctor 719-738: dD_OUT write_undet; unresettable variant also write_undet's reset_value.

## MOD_SyncFIFO<T,I> — depth+13 ids, Verilog-style names mapped from C++ fields, nested sClrSync/dClrSync
bs_prim_mod_synchronizers.h:1161-1189: reserves depth+13 ids (1163); scope; 1-bit "FULL_N","EMPTY_N" (1167-1168); (idx_bits+1)-bit "dEnqPtr","dGDeqPtr","dGDeqPtr1","dSyncReg1","sDeqPtr","sGEnqPtr","sGEnqPtr1","sSyncReg1" (1169-1176); idx_bits-wide "sCount","dCount" (1177-1178); width-wide "dDoutReg" (1179) and "arr_0".."arr_{depth-1}" (1180-1184); nested sClrSync/dClrSync MOD_SyncHandshake scopes emitted inside the FIFO scope (1185-1186). Field mapping in dump_VCD (full dump 1290-1313): FULL_N=METH_RDY_enq(), EMPTY_N=METH_RDY_first(), dEnqPtr=dst_hi, dGDeqPtr=dst_lo.probe(), dGDeqPtr1=dst_lo_plus_1, dSyncReg1, sDeqPtr=src_lo, sGEnqPtr=src_hi.probe(), sGEnqPtr1=src_hi_plus_1, sSyncReg1, sCount=sCountReg, dCount=dCountReg. CHANGES mode 1211-1286: FULL_N compares raw not_full (1213) — an asymmetry vs full-dump's METH_RDY_enq() which also includes !s_reset; EMPTY_N compares METH_RDY_first(); SyncVar pointers compare .probe() vs backing .read() (1225,1241); dDoutReg gets value on empty->nonempty or change-while-ready and X on nonempty->empty (1261-1268); each arr_i uses occupied() (1118-1129) — newly occupied writes value, newly vacated writes X, value change writes value, else id skipped (1269-1286). XS mode 1193-1209 Xs every owned id. backing fully copied 1316-1332 then nested handshake dumps 1334-1335. Initial ctor 855-883: pointers 0 (plus_1 vars 1), counts 0, not_empty=false, not_full=true; data entries only init_val'd (857), not write_undet.

## MOD_DualPortRam — memory contents not dumped
bs_prim_mod_synchronizers.h:1431-1441: dump_VCD_defs returns num unchanged, dump_VCD empty (comments "Memory contents are not dumped"). Note ctor guard vcd_is_backing_instance (1393-1394) skips storage allocation for the backing copy.

## MOD_LatchCrossingReg (CrossingReg) — NO scope; '$'-joined names in parent scope
bs_prim_mod_synchronizers.h:1541-1550: reserves 2 ids but does NOT call vcd_write_scope_start/end; instead snprintf's names "<inst_name>$L_OUT" (vcd_num, dumps dLatch) and "<inst_name>$Q_OUT" (vcd_num+1, dumps sFlop), width bits, emitted into the enclosing module's scope; returns vcd_num+2. dump_VCD 1551-1571: XS -> 2 Xs; else per-var compare/write with per-var backing update. Initial ctor 1466-1476: dLatch, sFlop, prev_value all write_undet.

## MOD_SyncReset — CLK alias + active-low IN_RST/OUT_RST with derived OUT_RST
bs_prim_mod_resets.h:107-117: reserves 2 ids (109); scope (111); "CLK" aliased to bk_clock_vcd_num(__clk_handle_0) (112) — the clock handle is kept solely for VCD (set_clk_0 at 74-77, field comment 156-157); owned 1-bit "IN_RST" (113) and "OUT_RST" (114). dump_VCD 118-146: XS -> 2 Xs; CHANGES writes IN_RST as !in_reset when in_reset changed (128-131) and OUT_RST as !(in_reset || count>1), comparing the derived rst_out against the same derivation on backing (132-137); full dump 141-142; backing.in_reset and backing.count updated unconditionally (144-145). Both signals active-low (0 = asserted). Initial ctor 45-48: count=0, in_reset=false.

## MOD_InitialReset — empty scope but LEAKS 3 reserved ids
bs_prim_mod_resets.h:241-247: dump_VCD_defs reserves 3 ids (243) then writes an empty scope (vcd_write_scope_start immediately followed by vcd_write_scope_end, 244-245) with zero $var defs, and returns vcd_num+3 — three ids allocated but never declared or dumped; dump_VCD (248-250) is empty.

## MOD_MakeReset / MOD_MakeReset0 — single 'rst' var; internal rstSync SyncReset NOT dumped
bs_prim_mod_resets.h:340-347 (MakeReset) and 444-451 (MakeReset0): reserve 1 id; scope inst_name; single 1-bit var "rst" (active-low output register); return vcd_num+1. dump_VCD 348-357 / 452-461: XS -> X; else write on full dump or backing.rst != rst, backing updated only when written. MOD_MakeReset embeds a MOD_SyncReset submodule named "rstSync" (member at 360; ctor prim_mod_resets.cxx:4-16) but, unlike SyncReg/SyncFIFO which nest their handshakes, dump_VCD_defs never calls sync.dump_VCD_defs, so the internal synchronizer produces no VCD. Initial: rst=1 (prim_mod_resets.cxx:13 for MakeReset; bs_prim_mod_resets.h:382 for MakeReset0).

## MOD_SyncReset0 / MOD_ResetMux / MOD_ResetEither / MOD_ResetToBool — no VCD
bs_prim_mod_resets.h:191-199 (SyncReset0), 554-563 (ResetMux), 625-634 (ResetEither), 674-683 (ResetToBool): dump_VCD_defs returns num unchanged (no scope, no ids) and dump_VCD is empty in all four.

## Shared convention: id allocation and return value
Prims that dump ignore the incoming 'num' argument, call vcd_reserve_ids(sim_hdl, k) storing the base in the Module::vcd_num field, and return the updated running count (base+k or the nested count); prims with nothing to dump return num unchanged. Aliased clock defs consume no reserved ids. In VCD_DUMP_CHANGES mode the id cursor must be incremented (++num / else branch) even for unchanged signals so subsequent ids stay aligned — e.g. bs_prim_mod_clockgen.h:177-182, bs_prim_mod_synchronizers.h:625-667, bs_prim_mod_resets.h:126-137.

## Shared convention: clock-id aliasing via bk_clock_vcd_num
bk_clock_vcd_num(sim_hdl, handle) returns the kernel-owned VCD id of a clock; a prim aliases a clock into its scope by passing that id to vcd_write_def with a local name (CLK_OUT/CLK_IN/sCLK/dCLK/CLK), e.g. bs_prim_mod_clockgen.h:43,159,263,421-422, bs_prim_mod_synchronizers.h:600-601, bs_prim_mod_resets.h:112. The identical VCD identifier code then appears in two scopes, and the prim's dump_VCD never writes values (or Xs) for it — the kernel emits value changes when the clock toggles. Several tClock members exist solely to support this (ClockDivider __clk_handle_1 at bs_prim_mod_clockgen.h:470-471; SyncHandshake __clk_handle_0/1 at bs_prim_mod_synchronizers.h:707-709; SyncReset __clk_handle_0 at bs_prim_mod_resets.h:156-157). Separately, vcd_set_clock(sim_hdl, id, handle) (bs_vcd.h:46, vcd.cxx:358-364) registers a NON-clock signal id in vcd.clk_map to associate it with a clock domain — used only for SyncHandshake's sEN (bs_prim_mod_synchronizers.h:598).

## Shared convention: dump modes, initial values, change detection
dump_VCD(dt, backing) has three modes: VCD_DUMP_XS writes vcd_write_x for every owned id (used for the initial checkpoint / X-ing out), never for aliased clock ids; VCD_DUMP_CHANGES compares each live value with the shadow 'backing' instance of the same class and writes only differences; any other dt full-dumps every owned value. backing is refreshed either per-variable-when-written (Sync1/2/15, SyncPulse, SyncReg data vars, GatedClock, MakeReset, LatchCrossingReg) or wholesale at the end (MakeClock 189-190, ClockInverter 307-310, ClockDivider 453-454, SyncHandshake 683-690, SyncFIFO 1316-1332, SyncReset 144-145). Pre-dump initial values are set in constructors: clock prims start from ctor params (MakeClock initClock/initCond at 71-74, ClockDivider cntr=upper-offset at 340), synchronizer state is mostly write_undet (undetermined), and reset prims start deasserted (rst=1, in_reset=false); the first XS pass makes everything X in the file regardless.

# Bluesim VCD dumping: bluesim.tcl -V flag, bluetcl 'sim vcd' command, bk_* VCD kernel API, and $dump* system tasks

Bluesim VCD dumping is driven from three layers: the generated-model wrapper script (/home/user/bsc/src/bluetcl/bluesim.tcl) turns '-V [file]' into a 'sim vcd on' or 'sim vcd &lt;file&gt;' Tcl command issued after 'sim load' and before 'sim run'; bluetcl (/home/user/bsc/src/comp/bluetcl.hs:3044-3068) maps those onto the kernel API bk_set_VCD_file/bk_enable_VCD_dumping declared in /home/user/bsc/src/bluesim/bluesim_kernel_api.h:257-268; and the $dumpfile/$dumpvars/$dumpon/$dumpoff/$dumpall/$dumplimit/$dumpflush tasks (/home/user/bsc/src/bluesim/dollar_dumpvars.cxx) call the same API from compiled designs. The VCD file is opened lazily (default name "dump.vcd" everywhere), the header is written at the first VCD event, none of the $dump* tasks print anything to stdout (only perror to stderr on fopen failure), they work fine without -V (they auto-open dump.vcd), -V and $dumpvars are idempotent over the same enable flag, and the default timescale is "1 us" with scale factor 1.

## -V argument parsing (with/without filename)
bluesim.tcl:168-177: '-V' looks at the next argv token; if there is none, or it begins with '-' or '+', it sets vcd_arg="on" (no filename consumed); otherwise it consumes that token as the VCD filename (vcd_arg=<file>). Usage text bluesim.tcl:39 documents: '-V [<file>] = dump waveforms to VCD file (default: dump.vcd)'. The command is issued at bluesim.tcl:232-234 via `eval "sim vcd $vcd_arg"` — so '-V on'/'-V off' would be parsed as the on/off subcommands, and a filename containing spaces would be word-split (eval quirk). Ordering: 'sim load' at :188, plus-args at :227-229, 'sim vcd' at :232-234, then 'sim run'/'sim step N' at :238 — so -V enables dumping before simulation starts, i.e. from time 0.

## 'sim vcd' subcommands and which the -V path uses
bluetcl.hs:2681-2685 (grammar) and :3044-3068 (implementation). Four forms: (1) 'sim vcd' with no args returns the active VCD file name as a Tcl list (empty list if none) (:3048-3050); (2) 'sim vcd on' calls bk_enable_VCD_dumping and records active_vcd_file="dump.vcd" if none was set (:3051-3056); (3) 'sim vcd off' calls bk_disable_VCD_dumping (:3057-3059); (4) 'sim vcd <file>' calls bk_set_VCD_file(file) then bk_enable_VCD_dumping (:3060-3065). The -V path uses form (2) when no filename was given and form (4) when one was. Also relevant: 'sim timescale <ts>' at bluetcl.hs:3011-3023 calls bk_set_timescale.

## bk_* VCD API surface
bluesim_kernel_api.h:257-268 declares: bk_enable_VCD_dumping, bk_disable_VCD_dumping, bk_is_VCD_dumping_enabled, bk_VCD_combo_update (SystemC-only immediate update, kernel.cxx:1552-1568), bk_set_VCD_file, bk_get_VCD_file_name, bk_set_VCD_depth, bk_VCD_checkpoint, bk_set_VCD_filesize_limit, bk_flush_VCD_output. bk_set_timescale is at bluesim_kernel_api.h:166-173. Implementations: vcd.cxx:36-104 (file/depth/checkpoint/limit/flush), kernel.cxx:1519-1550 (enable/disable/query).

## VCD file open timing and default filename
The file is opened eagerly only by bk_set_VCD_file (vcd.cxx:36-69): it fclose()s any current file (:40-41), sets state=VCD_OFF, then fopen(name,"w") (:59) — or "a" without rewriting the header if the name is in previous_files (:52-57), but nothing ever inserts into previous_files (only find at vcd.cxx:52, clear at :128, decl kernel.h:59), so the append path is dead code. On fopen failure: perror(name) to stderr, filename cleared, returns BK_ERROR (:61-66). Lazy open with default name: enabling dumping with no file open calls bk_set_VCD_file(simHdl,"dump.vcd") inside vcd_set_state (vcd.cxx:155-166), and bk_VCD_checkpoint does the same (vcd.cxx:82-88). The header/defs are NOT written at open — they are written at the first VCD event: vcd_event (kernel.cxx:336-345) calls vcd_write_header (vcd.cxx:207-231, only when state==VCD_OFF, sets state=VCD_HEADER), then '$scope module main', model dump_VCD_defs(), '$upscope', '$enddefinitions $end' (kernel.cxx:341-344).

## VCD file close timing
The file is closed in exactly three places: (1) bk_set_VCD_file when switching to a new file (vcd.cxx:40-41); (2) vcd_reset (vcd.cxx:116-143, fclose at :123-124), which is called from bk_destroy at model unload (kernel.cxx:767) — the normal end-of-run close, triggered by 'sim unload' at bluesim.tcl:263 — and (3) vcd_reset invoked from vcd_check_file_size when the $dumplimit size is exceeded (vcd.cxx:233-247). There is no explicit close on $dumpoff; $dumpflush (fflush, vcd.cxx:100-104) is the only way to force bytes out mid-run.

## $dumpfile semantics/output
dollar_dumpvars.cxx:5-8 ($dumpfile with no args) calls bk_set_VCD_file(simHdl,"dump.vcd"); :10-14 ($dumpfile(name)) sets the named file. Declared in bs_system_tasks.h:113-116. Effect: closes any open VCD file and opens the new one for writing, resetting VCD state to VCD_OFF so a fresh header is written on next dump (vcd.cxx:36-69). No stdout output; the BK_ERROR status is ignored by the task, so the only user-visible failure is perror() on stderr (vcd.cxx:64).

## $dumpvars semantics/output
dollar_dumpvars.cxx:16-21: bk_set_VCD_depth(depth) then bk_enable_VCD_dumping. The depth argument defaults to 0 = unlimited (bs_system_tasks.h:117-119 gives default args); Bluesim supports only the numeric depth form, not Verilog's module-path arguments. Depth is latched only while state==VCD_OFF (vcd.cxx:76-80), i.e. before the header is written; module dump code reads it via vcd_depth() (vcd.cxx:263-266). Enabling with no file set silently opens dump.vcd (vcd.cxx:155-166) — NO error and NO message when -V was not given. No stdout output at call time; the string '$dumpvars' is written INTO the VCD stream as a task marker at the first dump time (kernel.cxx:360-372 VCD_DUMP_INITIAL; marker emission in vcd_output_at_time vcd.cxx:436-460: '#<t>' then '$dumpvars' then value changes, with the closing '$end' emitted just before the next '#<t>' line via need_end_task :445-446,:454).

## $dumpon semantics/output
dollar_dumpvars.cxx:23-26 → bk_enable_VCD_dumping (kernel.cxx:1521-1532): returns immediately if already enabled (idempotent, :1523-1524); otherwise vcd_set_state(true) (opens dump.vcd if needed) and add_dummy_schedule_events so clock edges without logic still produce VCD events (:1527). First-ever enable produces the VCD_DUMP_INITIAL '$dumpvars' checkpoint; re-enable after a $dumpoff hits state VCD_DISABLED → VCD_DUMP_RESTART (vcd.cxx:194-201), writing a '$dumpon' marker plus current clock values and a full value checkpoint of all signals (kernel.cxx:395-403). No stdout output; no error without -V (auto dump.vcd).

## $dumpoff semantics/output
dollar_dumpvars.cxx:28-31 → bk_disable_VCD_dumping (kernel.cxx:1534-1545): no-op if not enabled; otherwise removes pending VCD events from the queue, removes dummy edges, vcd_set_state(false), and vcd_dump_xs (sets go_xs, vcd.cxx:150-153). The next VCD event takes the VCD_DUMP_XS branch (get_VCD_dump_type vcd.cxx:186-191, sets state=VCD_DISABLED): writes a '$dumpoff' marker into the VCD file and dumps X for every clock and signal (kernel.cxx:352-359; print_X emits 'x<id>' for 1-bit or 'bx <id>' for wider, vcd.cxx:620-629). No stdout output. Note vcd_is_active (vcd.cxx:168-173) keeps VCD events alive while a checkpoint or X-dump is pending even though vcd_enabled is false.

## $dumpall semantics/output
dollar_dumpvars.cxx:33-36 → bk_VCD_checkpoint (vcd.cxx:82-93): opens dump.vcd if no file is open (so it also works without -V), then sets vcd_checkpoint=true. The next VCD event takes VCD_DUMP_CHECKPOINT (vcd.cxx:180-185; note it sets go_xs = !vcd_enabled afterwards, so if dumping was off the values are X'd out again after the checkpoint): writes a '$dumpall' marker plus current values of all clocks and all signals (kernel.cxx:373-381). Does not itself enable or disable continuous dumping. No stdout output.

## $dumplimit semantics/output
dollar_dumpvars.cxx:38-42 → bk_set_VCD_filesize_limit(bytes) (vcd.cxx:95-98), 0 = unlimited (default, kernel.cxx:676). After every VCD event vcd_check_file_size runs (kernel.cxx:407; vcd.cxx:233-247): if ftello(vcd_file) > limit it writes '$comment\nVCD file size limit exceeded\n$end\n' INTO the VCD file (vcd_write_comment vcd.cxx:249-253 — not stdout), then vcd_reset closes the file and wipes all VCD state (file name, enable flag, depth, limit, pending changes), so dumping stops permanently for the run. No stdout/stderr output.

## $dumpflush semantics/output
dollar_dumpvars.cxx:44-47 → bk_flush_VCD_output (vcd.cxx:100-104): fflush() on the VCD FILE* if one is open, else no-op. No output anywhere, no return value.

## Stdout/stderr behavior of all $dump* tasks
None of the seven tasks print anything to stdout under any circumstance (dollar_dumpvars.cxx:1-48 contains no printf). The only terminal output in the whole VCD path is perror(name) to stderr when fopen fails in bk_set_VCD_file (vcd.cxx:64). Using any $dump* task without -V is not an error: $dumpvars/$dumpon/$dumpall silently create ./dump.vcd (vcd.cxx:86,159); the compiled tasks are dispatched by name-mangling '$dumpX' -> 'dollar_dumpX' with an implicit simHdl argument (ForeignFunctions.hs:365-381, mapFnName :392-397).

## Interaction between -V and $dumpvars
Both converge on the same single enable flag: -V drives bluetcl.hs:3051-3065 which calls bk_enable_VCD_dumping, exactly what $dumpvars/$dumpon call. bk_enable_VCD_dumping is idempotent (kernel.cxx:1523-1524 returns 1 if already enabled), so with -V given, a later $dumpvars in the design is effectively a no-op — except its depth argument, which still takes effect only if the first VCD event has not yet fired (state must be VCD_OFF, vcd.cxx:76-80; the header at the first event flips state to VCD_HEADER, vcd.cxx:217). Because bluesim.tcl issues 'sim vcd' at :232-234 before 'sim run' at :238, -V starts dumping at time 0 regardless of when/whether the design calls $dumpvars. Conversely $dumpfile(name) after '-V file.vcd' closes file.vcd and redirects subsequent output (with a fresh header) to the new name (vcd.cxx:40-59). There is only one dump stream — no separate files for -V vs $dumpvars.

## Timescale default and control
Default is hard-coded at model init: sim_timescale factor = 1 and vcd_timescale string = "1 us" (kernel.cxx:692-694), printed in the header as '$timescale\n\t1 us\n$end' (vcd.cxx:226). bk_set_timescale (kernel.cxx:1206-1222) may only be called before simulation time advances (sim_time != 0 → BK_ERROR) and accepts only '(1|10|100) (s|ms|us|ns|ps|fs)' (valid_unit, kernel.cxx:1183-1203); it sets both the VCD header unit and the multiplier used by bk_now = sim_timescale * sim_time (kernel.cxx:1175-1178). From Tcl: 'sim timescale <timeunit/timeprecision>' (bluetcl.hs:3011-3023) parses Verilog-style strings like '1 ns / 10 ps' via parseTimescale (bluetcl.hs:4233-4257): the VCD unit becomes the precision string and the scale factor becomes unit/precision (must be >= 1). bluesim.tcl has no command-line flag for timescale; -V runs always get '1 us' unless a script calls 'sim timescale'.

## VCD stream format details (for reimplementation)
Header: '$date\n\t<ctime>$end\n', '$version\n\tBluespec VCD dumper 2.1\n$end\n' (revs at vcd.cxx:12-13), '$timescale...' (vcd.cxx:219-226); then kernel wraps everything in '$scope module main $end' ... '$upscope $end' and '$enddefinitions $end' (kernel.cxx:341-344). Var defs: '$var reg <width> <id> <name> $end' (vcd_write_def vcd.cxx:376-385; clock defs vcd_add_clock_def :347-356); ids are base-94 ASCII starting at '!' (vcd_write_id vcd.cxx:285-298). Value changes: '#<time>' lines from vcd_output_at_time (vcd.cxx:436-460); scalars '0'/'1'/'x' + id, vectors 'b<binary> <id>' with leading zeros elided ('bx <id>' for X) (vcd.cxx:596-661). Task markers ($dumpvars/$dumpoff/$dumpall/$dumpon) are recorded per-time via vcd_task (vcd.cxx:145-148) and closed with '$end' before the next timestamp (:445-446). Changes are buffered per-time and flushed once all clocks' combinational times pass, to convert Bluesim's posedge-eager evaluation into Verilog-style timing (comment block vcd.cxx:15-34; vcd_advance/flush_changes :387-434; time_of_change :462-489).

# ground-truth

Empirical Bluesim VCD ground truth from bsc at /home/user/bsc/inst/bin/bsc (run 2026-07-08 in /tmp/claude-0/-home-user-bsc/e236fbcd-0f62-56f2-9365-97a217968d47/scratchpad/vcdgt). Two designs: (1) custom sysVcdGT (VcdGT.bsv) with mkReg UInt#(8), mkRegU Bit#(16), RWire#(Bit#(8)), mkFIFO#(Bit#(8)), submodule mkVcdGT_Sub with its own 4-bit register, $display each cycle, $finish at cycle 10 — run with '-V out.vcd -m 12' produced a 213-line VCD; (2) existing testsuite sysVCDTest1 (testsuite/bsc.bluesim/vcd/VCDTest1.bsv) run identically — because that design calls $dumpfile("test1.vcd")/$dumpvars at counter==2, the run produced TWO files: out.vcd (123 lines, header + activity up to time #10, cut off when $dumpfile switched files) and test1.vcd (284 lines, starting at time #15, containing a mid-stream '$dumpvars ... $end' checkpoint block at time #30 emitted by the $dumpvars call). Key observed behaviors: header is $date/$version ('Bluespec VCD dumper 2.1')/$timescale ('1 us'); hierarchy is '$scope module main' wrapping '$scope module top'; CLK is defined TWICE with the same code '!' at top level; all vars use type 'reg'; ID codes are printable chars from '!' with gaps (codes are allocated per 32-bit word, so a 32-bit var consumes 1 code but the allocator skips ahead: e.g. 8-bit 'cyc__h414'='#' then next var '('; multi-word or method-proxy signals reserve intermediate codes); a register and its combinational alias can share one code (D_OUT and arr_0 both '0'); signal-value lines use 'b<binary> <code>' with NO leading zeros (b0, b1, b10101010) and 1-bit values as '0!'/'1!' with no space; uninitialized/undetermined values dump as 'bx <code>' and mkRegU/pre-reset registers show the 0xAA...AA pattern (b10101010...); time advances 5 units per half-clock (#5, #10, ... posedge at multiples of 10, CLK '1!' printed at posedge, '0!' at #5-offsets); initial dump at time 0 has no #0 marker (values follow $enddefinitions directly); RST_N starts 0 then goes 1 at #5; FIFO internals dumped are CLK RST FULL_N EMPTY_N ENQ D_IN DEQ CLR D_OUT arr_0 arr_1 (FIFO scope named after instance 'fifo', RST not RST_N); RWire dumps as single 8-bit 'rw' signal that is 'bx' when invalid; -m 12 stops after 12 cycle-boundary edges without $finish (custom design hit $finish at cycle 10, last VCD time #105).

## cmd:sysVcdGT
Working dir: /tmp/claude-0/-home-user-bsc/e236fbcd-0f62-56f2-9365-97a217968d47/scratchpad/vcdgt. Env: export BLUESPECDIR=/home/user/bsc/inst/lib; export PATH=/home/user/bsc/inst/bin:$PATH; export LD_LIBRARY_PATH=/home/user/bsc/src/vendor/stp/lib:/home/user/bsc/src/vendor/yices/lib.
Commands:
  bsc -sim -g sysVcdGT -u VcdGT.bsv
  bsc -sim -e sysVcdGT -o sysVcdGT.exe
  ./sysVcdGT.exe -V out.vcd -m 12
Compile output: created mkVcdGT_Sub.ba, sysVcdGT.ba, Bluesim objects, sysVcdGT.exe.so, sysVcdGT.exe (no warnings).
Sim stdout (exit code 0):
Warning: the Bluesim kernel version does not match the BSC version used to
generate the Bluesim model
cycle 0: wide=01fe sub=0 rw_valid=1
deq 0
cycle 1: wide=0000 sub=1 rw_valid=0
cycle 2: wide=0003 sub=2 rw_valid=1
deq 2
cycle 3: wide=0006 sub=3 rw_valid=0
cycle 4: wide=0009 sub=4 rw_valid=1
deq 4
cycle 5: wide=000c sub=5 rw_valid=0
cycle 6: wide=000f sub=6 rw_valid=1
deq 6
cycle 7: wide=0012 sub=7 rw_valid=0
cycle 8: wide=0015 sub=8 rw_valid=1
deq 8
cycle 9: wide=0018 sub=9 rw_valid=0
cycle 10: wide=001b sub=a rw_valid=1
(design $finish(0) at cyc==10; -m 12 limit not reached)
Source file: /tmp/claude-0/-home-user-bsc/e236fbcd-0f62-56f2-9365-97a217968d47/scratchpad/vcdgt/VcdGT.bsv — sysVcdGT contains Reg#(UInt#(8)) cyc <- mkReg(0); Reg#(Bit#(16)) wide <- mkRegU; RWire#(Bit#(8)) rw <- mkRWire; FIFO#(Bit#(8)) fifo <- mkFIFO; SubIfc sub <- mkVcdGT_Sub (submodule with Reg#(Bit#(4)) subreg <- mkReg(0) incrementing every cycle); rules: count (cyc<=cyc+1, wide<=zeroExtend(pack(cyc))*3), putwire/enq on even cycles, deq+$display on odd cycles, show ($display every cycle), done ($finish(0) at cyc==10).

## vcd:sysVcdGT
Complete out.vcd (213 lines; lines 2,5,8 begin with a tab character):
$date
	Wed Jul  8 04:50:06 2026
$end
$version
	Bluespec VCD dumper 2.1
$end
$timescale
	1 us
$end
$scope module main $end
$scope module top $end
$var reg 1 ! CLK $end
$var reg 1 ! CLK $end
$var reg 1 " RST_N $end
$var reg 8 # cyc__h414 $end
$var reg 8 ( cyc $end
$scope module fifo $end
$var reg 1 ! CLK $end
$var reg 1 ) RST $end
$var reg 1 * FULL_N $end
$var reg 1 + EMPTY_N $end
$var reg 1 , ENQ $end
$var reg 8 - D_IN $end
$var reg 1 . DEQ $end
$var reg 1 / CLR $end
$var reg 8 0 D_OUT $end
$var reg 8 0 arr_0 $end
$var reg 8 1 arr_1 $end
$upscope $end
$var reg 8 2 rw $end
$var reg 16 3 wide $end
$scope module sub $end
$var reg 1 ! CLK $end
$var reg 1 4 RST_N $end
$var reg 4 5 subreg___d1 $end
$var reg 4 7 subreg $end
$upscope $end
$upscope $end
$upscope $end
$enddefinitions $end
1!
0"
b10101010 #
b0 (
0)
1*
0+
1,
b10101010 -
0.
1/
bx 0
bx 1
b10111010 2
b111111110 3
04
b1010 5
b0 7
b0 #
b0 -
0/
b10000 2
b0 5
#5
0!
1"
1)
14
#10
b1 #
0,
1.
bx 2
b1 5
1!
b1 (
1+
b0 0
b0 3
b1 7
#15
0!
#20
b10 #
1,
b10 -
0.
b10010 2
b10 5
1!
b10 (
0+
bx 0
b11 3
b10 7
#25
0!
#30
b11 #
0,
1.
bx 2
b11 5
1!
b11 (
1+
b10 0
b110 3
b11 7
#35
0!
#40
b100 #
1,
b100 -
0.
b10100 2
b100 5
1!
b100 (
0+
bx 0
b1001 3
b100 7
#45
0!
#50
b101 #
0,
1.
bx 2
b101 5
1!
b101 (
1+
b100 0
b1100 3
b101 7
#55
0!
#60
b110 #
1,
b110 -
0.
b10110 2
b110 5
1!
b110 (
0+
bx 0
b1111 3
b110 7
#65
0!
#70
b111 #
0,
1.
bx 2
b111 5
1!
b111 (
1+
b110 0
b10010 3
b111 7
#75
0!
#80
b1000 #
1,
b1000 -
0.
b11000 2
b1000 5
1!
b1000 (
0+
bx 0
b10101 3
b1000 7
#85
0!
#90
b1001 #
0,
1.
bx 2
b1001 5
1!
b1001 (
1+
b1000 0
b11000 3
b1001 7
#95
0!
#100
b1010 #
1,
b1010 -
0.
b11010 2
b1010 5
1!
b1010 (
0+
bx 0
b11011 3
b1010 7
#105
0!

## cmd:sysVCDTest1
Working dir: /tmp/claude-0/-home-user-bsc/e236fbcd-0f62-56f2-9365-97a217968d47/scratchpad/vcdgt/vcdtest1. Same env exports as sysVcdGT.
Commands:
  cp /home/user/bsc/testsuite/bsc.bluesim/vcd/VCDTest1.bsv .
  bsc -sim -g sysVCDTest1 -u VCDTest1.bsv
  bsc -sim -e sysVCDTest1 -o sysVCDTest1.exe
  ./sysVCDTest1.exe -V out.vcd -m 12
Compile output: one scheduling warning (G0010, rule "in" more urgent than "rotate" in mkVCDTest1_Sub); created mkVCDTest1_Sub.ba, mkVCDTest1_Mid.ba, sysVCDTest1.ba, sysVCDTest1.exe.so, sysVCDTest1.exe.
Sim stdout (exit code 0) — design has no $display, only the kernel warning:
Warning: the Bluesim kernel version does not match the BSC version used to
generate the Bluesim model
IMPORTANT observed behavior: the run produced TWO VCD files. out.vcd (123 lines, 1902 bytes) holds the -V dump from time 0 through #10; at counter==2 the design's rule startVCD executes $dumpfile("test1.vcd") + $dumpvars, which closes out.vcd and redirects dumping to test1.vcd (284 lines, 5027 bytes), whose value stream starts at #15 and whose $dumpvars call emits a '$dumpvars ... $end' full-checkpoint block at time #30. Run stopped at the -m 12 cycle limit (last time #105); the design's own $finish is at counter>=200, never reached. Design source: /home/user/bsc/testsuite/bsc.bluesim/vcd/VCDTest1.bsv (sysVCDTest1: Reg#(UInt#(8)) counter, two mkVCDTest1_Mid instances each containing two mkVCDTest1_Sub instances each with two Reg#(Bit#(32)) x,y).

## vcd:sysVCDTest1-out.vcd
Complete out.vcd from sysVCDTest1 run (123 lines; the -V file, truncated at #10 when $dumpfile switched; lines 2,5,8 begin with a tab):
$date
	Wed Jul  8 04:50:37 2026
$end
$version
	Bluespec VCD dumper 2.1
$end
$timescale
	1 us
$end
$scope module main $end
$scope module top $end
$var reg 1 ! CLK $end
$var reg 1 ! CLK $end
$var reg 1 " RST_N $end
$var reg 8 # counter__h390 $end
$var reg 8 % counter $end
$scope module mid1 $end
$var reg 1 ! CLK $end
$var reg 1 & RST_N $end
$scope module sub1 $end
$var reg 1 ! CLK $end
$var reg 1 ' RST_N $end
$var reg 1 ( WILL_FIRE_in $end
$var reg 32 ) x___d1 $end
$var reg 32 * y___d5 $end
$var reg 1 + EN_in $end
$var reg 32 . x $end
$var reg 32 / y $end
$upscope $end
$scope module sub2 $end
$var reg 1 ! CLK $end
$var reg 1 0 RST_N $end
$var reg 1 1 WILL_FIRE_in $end
$var reg 32 2 x___d1 $end
$var reg 32 3 y___d5 $end
$var reg 1 4 EN_in $end
$var reg 32 7 x $end
$var reg 32 8 y $end
$upscope $end
$upscope $end
$scope module mid2 $end
$var reg 1 ! CLK $end
$var reg 1 9 RST_N $end
$scope module sub1 $end
$var reg 1 ! CLK $end
$var reg 1 : RST_N $end
$var reg 1 ; WILL_FIRE_in $end
$var reg 32 < x___d1 $end
$var reg 32 = y___d5 $end
$var reg 1 > EN_in $end
$var reg 32 A x $end
$var reg 32 B y $end
$upscope $end
$scope module sub2 $end
$var reg 1 ! CLK $end
$var reg 1 C RST_N $end
$var reg 1 D WILL_FIRE_in $end
$var reg 32 E x___d1 $end
$var reg 32 F y___d5 $end
$var reg 1 G EN_in $end
$var reg 32 J x $end
$var reg 32 K y $end
$upscope $end
$upscope $end
$upscope $end
$upscope $end
$enddefinitions $end
1!
0"
b10101010 #
b0 %
0&
0'
1(
b10101010101010101010101010101010 )
b10101010101010101010101010101010 *
1+
b0 .
b0 /
00
11
b10101010101010101010101010101010 2
b10101010101010101010101010101010 3
14
b0 7
b0 8
09
0:
1;
b10101010101010101010101010101010 <
b10101010101010101010101010101010 =
1>
b0 A
b0 B
0C
1D
b10101010101010101010101010101010 E
b10101010101010101010101010101010 F
1G
b0 J
b0 K
b0 #
b0 )
b0 *
b0 2
b0 3
b0 <
b0 =
b0 E
b0 F
#5
0!
1"
1&
1'
10
19
1:
1C
#10
b1 #
1!
b1 %

## vcd:sysVCDTest1-test1.vcd
Complete test1.vcd from the same sysVCDTest1 run (284 lines; created by $dumpfile("test1.vcd")+$dumpvars at counter==2; identical header/defs to out.vcd, value stream starts at #15; note the '$dumpvars ... $end' checkpoint block at #30; lines 2,5,8 begin with a tab):
$date
	Wed Jul  8 04:50:37 2026
$end
$version
	Bluespec VCD dumper 2.1
$end
$timescale
	1 us
$end
$scope module main $end
$scope module top $end
$var reg 1 ! CLK $end
$var reg 1 ! CLK $end
$var reg 1 " RST_N $end
$var reg 8 # counter__h390 $end
$var reg 8 % counter $end
$scope module mid1 $end
$var reg 1 ! CLK $end
$var reg 1 & RST_N $end
$scope module sub1 $end
$var reg 1 ! CLK $end
$var reg 1 ' RST_N $end
$var reg 1 ( WILL_FIRE_in $end
$var reg 32 ) x___d1 $end
$var reg 32 * y___d5 $end
$var reg 1 + EN_in $end
$var reg 32 . x $end
$var reg 32 / y $end
$upscope $end
$scope module sub2 $end
$var reg 1 ! CLK $end
$var reg 1 0 RST_N $end
$var reg 1 1 WILL_FIRE_in $end
$var reg 32 2 x___d1 $end
$var reg 32 3 y___d5 $end
$var reg 1 4 EN_in $end
$var reg 32 7 x $end
$var reg 32 8 y $end
$upscope $end
$upscope $end
$scope module mid2 $end
$var reg 1 ! CLK $end
$var reg 1 9 RST_N $end
$scope module sub1 $end
$var reg 1 ! CLK $end
$var reg 1 : RST_N $end
$var reg 1 ; WILL_FIRE_in $end
$var reg 32 < x___d1 $end
$var reg 32 = y___d5 $end
$var reg 1 > EN_in $end
$var reg 32 A x $end
$var reg 32 B y $end
$upscope $end
$scope module sub2 $end
$var reg 1 ! CLK $end
$var reg 1 C RST_N $end
$var reg 1 D WILL_FIRE_in $end
$var reg 32 E x___d1 $end
$var reg 32 F y___d5 $end
$var reg 1 G EN_in $end
$var reg 32 J x $end
$var reg 32 K y $end
$upscope $end
$upscope $end
$upscope $end
$upscope $end
$enddefinitions $end
#15
0!
#20
b10 #
1(
b100000000 )
b10000000000000000 *
1+
11
b1000000000000000000000000 2
b1 3
14
1;
b1000000000000000000000000 <
b1 =
1>
1D
b1 E
b1000000000000000000000000 F
1G
1!
b10 %
b100000000 .
b10000000000000000 /
b1000000000000000000000000 7
b1 8
b1000000000000000000000000 A
b1 B
b1 J
b1000000000000000000000000 K
#25
0!
#30
$dumpvars
b11 #
b10000001000000000 )
b100000000100000000 *
b10000000000000000000000001 2
b1000000000000000000000010 3
b10000000000000000100000000 <
b10000000000000010 =
b10000000000000010 E
b10000000000000000100000000 F
1!
1"
b11 %
1&
1'
b10000001000000000 .
b100000000100000000 /
10
b10000000000000000000000001 7
b1000000000000000000000010 8
19
1:
b10000000000000000100000000 A
b10000000000000010 B
1C
b10000000000000010 J
b10000000000000000100000000 K
$end
#35
0!
#40
b100 #
b100000001000000000 )
b100000001000000000 *
b10000000000000000000000010 2
b10000000000000000000000010 3
b11000000010000001100000001 <
b1000000110000000100000011 =
b1000000110000000100000011 E
b11000000010000001100000001 F
1!
b100 %
b100000001000000000 .
b100000001000000000 /
b10000000000000000000000010 7
b10000000000000000000000010 8
b11000000010000001100000001 A
b1000000110000000100000011 B
b1000000110000000100000011 J
b11000000010000001100000001 K
#45
0!
#50
b101 #
b100000011000000000 )
b1100000001000000000 *
b110000000000000000000000010 2
b10000000000000000000000110 3
b101000000110000000100000011 <
b11000000010000001100000101 =
b11000000010000001100000101 E
b101000000110000000100000011 F
1!
b101 %
b100000011000000000 .
b1100000001000000000 /
b110000000000000000000000010 7
b10000000000000000000000110 8
b101000000110000000100000011 A
b11000000010000001100000101 B
b11000000010000001100000101 J
b101000000110000000100000011 K
#55
0!
#60
b110 #
b1100000011100000000 )
b1110000011000000000 *
b111000000000000000000000110 2
b110000000000000000000000111 3
b110000000010000011100000001 <
b1000001110000000100000110 =
b1000001110000000100000110 E
b110000000010000011100000001 F
1!
b110 %
b1100000011100000000 .
b1110000011000000000 /
b111000000000000000000000110 7
b110000000000000000000000111 8
b110000000010000011100000001 A
b1000001110000000100000110 B
b1000001110000000100000110 J
b110000000010000011100000001 K
#65
0!
#70
b111 #
b1110000000000000000 )
b11100000000 *
b111 2
b111000000000000000000000000 3
b111000001110000000000000111 <
b111000000000000011100000111 =
b111000000000000011100000111 E
b111000001110000000000000111 F
1!
b111 %
b1110000000000000000 .
b11100000000 /
b111 7
b111000000000000000000000000 8
b111000001110000000000000111 A
b111000000000000011100000111 B
b111000000000000011100000111 J
b111000001110000000000000111 K
#75
0!
#80
b1000 #
b0 )
b0 *
b0 2
b0 3
b0 <
b0 =
b0 E
b0 F
1!
b1000 %
b0 .
b0 /
b0 7
b0 8
b0 A
b0 B
b0 J
b0 K
#85
0!
#90
b1001 #
b100000000000 )
b10000000000000000000 *
b1000000000000000000000000000 2
b1000 3
b1000000000000000000000000000 <
b1000 =
b1000 E
b1000000000000000000000000000 F
1!
b1001 %
b100000000000 .
b10000000000000000000 /
b1000000000000000000000000000 7
b1000 8
b1000000000000000000000000000 A
b1000 B
b1000 J
b1000000000000000000000000000 K
#95
0!
#100
b1010 #
b10000000100100000000 )
b10010000100000000000 *
b1001000000000000000000001000 2
b1000000000000000000000001001 3
b1001000000000000100000000000 <
b10000000000000001001 =
b10000000000000001001 E
b1001000000000000100000000000 F
1!
b1010 %
b10000000100100000000 .
b10010000100000000000 /
b1001000000000000000000001000 7
b1000000000000000000000001001 8
b1001000000000000100000000000 A
b10000000000000001001 B
b10000000000000001001 J
b1001000000000000100000000000 K
#105
0!


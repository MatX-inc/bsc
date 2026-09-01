#ifndef __BS_RESET_H__
#define __BS_RESET_H__

#include "bluesim_types.h"

/* Routines called from the schedule to determine if reset
 * tick calls are required.
 */
void init_reset_request_counters(tSimStateHdl simHdl);
bool do_reset_ticks(tSimStateHdl simHdl);

/* Routines called from primitives to control reset period */
void start_reset_ticks(tSimStateHdl simHdl);
void stop_reset_ticks(tSimStateHdl simHdl);

/* Routine called from primitives to trigger delayed reset function */
void reset_init(tSimStateHdl simHdl, tResetFn fn, void* parent, tUInt8 rst);
void reset_at_end_of_timeslice(tSimStateHdl simHdl,
			       tResetFn fn, void* parent, tUInt8 rst);

/* Simulation-wide count of asserted reset outputs.
 *
 * Every reset domain in a design is driven by one reset source: the
 * kernel's default reset waveform (which drives the top module's
 * input resets) or a reset-generating primitive (SyncReset,
 * SyncReset0, InitialReset, MakeReset via its embedded SyncReset,
 * MakeReset0, ClockSelect).  Each source records the level of its
 * output reset in a bool of its own and reports level changes through
 * set_reset_output(), which counts the sources whose output is
 * currently asserted.  Pass-through primitives (ResetMux,
 * ResetEither) do not report: their outputs can only be asserted
 * while an already-counted source's output is asserted.
 *
 * any_reset_asserted() therefore answers "is some reset (possibly
 * scheduled but not yet delivered) asserted anywhere in the
 * simulation".  The RegFile and BRAM primitives use it to tolerate
 * out-of-bounds accesses while a reset is asserted instead of failing
 * through the out_of_bounds host operation: generated rule bodies
 * execute their memory reads before their in-reset check, so during
 * reset such reads legitimately see address registers still at their
 * undetermined initial pattern (see bluesim_host_ops.h).  It is a
 * simulation-wide approximation of "the accessing rule's domain is in
 * reset"; the precise domain is not visible at the primitive access
 * site.
 */
void set_reset_output(tSimStateHdl simHdl, bool* level, bool asserted);
bool any_reset_asserted(tSimStateHdl simHdl);

#endif /* __BS_RESET_H__ */


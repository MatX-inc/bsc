/* These routines are used to allow the schedule code to
 * handle resets.  Since reset should be a very small percentage
 * of the simulation, we want to minimize the cost of testing
 * for reset and we are less concerned with the actual cost of
 * processing when reset is active.
 *
 * Primitives that are reset synchronously but have asynchronous
 * input resets need to delay entry into reset until the next clock
 * edge for the primitive.  During normal operation, they may not
 * need to do anything on each clock edge, and we want to avoid them
 * having a permanent tick() method that slows down non-reset cycles.
 * So we partition the tick methods into the permanent ones and the
 * reset ones and only execute the reset ones (all of them) when
 * some primitive (even if it is only one) requires it.
 */

#include "bs_reset.h"
#include "kernel.h"
#include "event_queue.h"

/* A pending reset call is embedded directly in its event (the
 * tResetRequest member of tEventData, see event_queue.h), so
 * scheduling and executing resets allocates nothing.
 */

/* Initialize the reset request counting system */
void init_reset_request_counters(tSimStateHdl simHdl)
{
  simHdl->reset_tick_requests = 0;
  simHdl->resets_asserted = 0;
  simHdl->default_reset_asserted = false;
}

/* Test if a any primitive still needs reset ticks */
bool do_reset_ticks(tSimStateHdl simHdl)
{
  return (simHdl->reset_tick_requests > 0);
}

/* Record a request for reset ticks from a primitive */
void start_reset_ticks(tSimStateHdl simHdl)
{
  ++(simHdl->reset_tick_requests);
}

/* Withdraw a request for reset ticks from a primitive */
void stop_reset_ticks(tSimStateHdl simHdl)
{
  if (simHdl->reset_tick_requests > 0)
    --(simHdl->reset_tick_requests);
}

/* Record a level change of one reset source's output reset (see
 * bs_reset.h).  'level' is the source's own record of its output;
 * only transitions touch the simulation-wide count, so repeated
 * assertions or deassertions of the same source stay balanced.
 */
void set_reset_output(tSimStateHdl simHdl, bool* level, bool asserted)
{
  if (*level == asserted)
    return;
  *level = asserted;
  if (asserted)
    ++(simHdl->resets_asserted);
  else if (simHdl->resets_asserted > 0)
    --(simHdl->resets_asserted);
}

/* Test if any reset source's output reset is currently asserted */
bool any_reset_asserted(tSimStateHdl simHdl)
{
  return (simHdl->resets_asserted > 0);
}


/* Routine called from kernel to execute delayed reset functions */
static tTime reset_event(tSimStateHdl simHdl, tEvent& ev)
{
  const tResetRequest& req = ev.data.reset;

  if (req.fn == NULL)
    return 0llu;

  req.fn(req.parent, req.rst);
  if (req.rst == 0)
    start_reset_ticks(simHdl);
  else
    stop_reset_ticks(simHdl);

  return 0llu;
}

/* Queue a reset function to be called at the beginning of the time-slice */
void reset_init(tSimStateHdl simHdl, tResetFn fn, void* parent, tUInt8 rst)
{
  tEvent ev;

  ev.at       = simHdl->sim_time;
  ev.priority = make_priority(PG_INITIAL, PS_RESET);
  ev.fn       = reset_event;
  ev.data.reset.fn     = fn;
  ev.data.reset.parent = parent;
  ev.data.reset.rst    = rst;

  simHdl->queue->schedule(ev);
}

/* Queue a reset function to be called at the end of the time-slice */
void reset_at_end_of_timeslice(tSimStateHdl simHdl,
			       tResetFn fn, void* parent, tUInt8 rst)
{
  tEvent ev;

  ev.at       = simHdl->sim_time;
  ev.priority = make_priority(PG_AFTER_LOGIC, PS_RESET);
  ev.fn       = reset_event;
  ev.data.reset.fn     = fn;
  ev.data.reset.parent = parent;
  ev.data.reset.rst    = rst;

  simHdl->queue->schedule(ev);
}

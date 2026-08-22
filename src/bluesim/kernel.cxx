#include <list>
#include <algorithm>
#include <cstring>
#include <cstdio>

#include "mem_alloc.h"
#include "kernel.h"
#include "bs_module.h"
#include "plusargs.h"
#include "version.h"
#include "portability.h"


/* forward declarations of some static helper functions */
static void setup_clock_edges(tSimStateHdl simHdl, tClock clk);

/*
 * Functions to abstract the implementation of the event data
 * for clock events.
 */

tClock get_clock_event_clk(unsigned int value)
{
  return ((tClock) (value >> 1));
}

tEdgeDirection get_clock_event_dir(unsigned int value)
{
  return ((tEdgeDirection) (value & 1));
}

unsigned int set_clock_event_data(tClock clk, tEdgeDirection dir)
{
  if (dir == POSEDGE)
    return ( (clk << 1) | 1 );
  else
    return ( (clk << 1)     );
}

/*
 * Event callbacks which are used to trigger execution
 * of kernel features.
 */

static tTime reset_model_event(tSimStateHdl simHdl, tEvent& ev)
{
  simHdl->model->reset_model(ev.data.flag);
  return 0llu; // not a recurring event
}

static void print_cycle_description(tSimStateHdl simHdl,
				    tClock clk,
                                    tEdgeDirection dir,
                                    tTime time,
                                    bool combo = false)
{
  const char* clock_name = simHdl->clocks[clk].name;

  tUInt64 cycle_count;
  if (dir == POSEDGE)
    cycle_count = simHdl->clocks[clk].posedge_count + 1;
  else
    cycle_count = simHdl->clocks[clk].negedge_count + 1;
  const char* combo_str = combo ? "after-" : "";
  char dir_char = (dir == POSEDGE) ? '/' : '\\';
  printf("%s%c%s @ %llu (cycle %llu)\n",
         combo_str, dir_char, clock_name, time, cycle_count);
}

static tTime dump_cycle_event(tSimStateHdl simHdl, tEvent& ev)
{
  unsigned int n = ev.data.value;
  tClock         clk = get_clock_event_clk(n);
  tEdgeDirection dir = get_clock_event_dir(n);

  if (clk == BAD_CLOCK_HANDLE)
    return 0llu;

  print_cycle_description(simHdl, clk, dir, ev.at);

  // don't reschedule initial cycle descriptions
  if (priority_group(ev.priority) != PG_INITIAL)
    return simHdl->clocks[clk].period;
  else
    return 0llu;
}

static tTime run_edge_schedule_event(tSimStateHdl simHdl, tEvent& ev)
{
  unsigned int n = ev.data.value;
  tClock         clk = get_clock_event_clk(n);
  tEdgeDirection dir = get_clock_event_dir(n);
  bool need_to_yield = false;

  // update time
  simHdl->sim_time = ev.at;

  // update the clock edge information
  tScheduleFn schedule;
  if (dir == POSEDGE)
  {
    schedule = simHdl->clocks[clk].on_posedge;

    // if necessary, dump the cycle count for an aperiodic clock here
    if (simHdl->call_dump_cycle_counts && schedule &&
        (simHdl->clocks[clk].low_phase_length == 0) &&
        (simHdl->clocks[clk].high_phase_length == 0))
    {
      print_cycle_description(simHdl, clk, POSEDGE, simHdl->sim_time);
    }

    simHdl->clocks[clk].current_value = CLK_HIGH;
    simHdl->clocks[clk].combinational_at = simHdl->clocks[clk].posedge_at;
    simHdl->clocks[clk].posedge_at = simHdl->sim_time;
    simHdl->clocks[clk].posedge_count += 1llu;
    need_to_yield |=
      (simHdl->clocks[clk].posedge_count == simHdl->clocks[clk].posedge_limit);
  }
  else
  {
    schedule = simHdl->clocks[clk].on_negedge;

    // if necessary, dump the cycle count for an aperiodic clock here
    if (simHdl->call_dump_cycle_counts && schedule &&
        (simHdl->clocks[clk].low_phase_length == 0) &&
        (simHdl->clocks[clk].high_phase_length == 0))
    {
      print_cycle_description(simHdl, clk, NEGEDGE, simHdl->sim_time);
    }

    simHdl->clocks[clk].current_value = CLK_LOW;
    simHdl->clocks[clk].combinational_at = simHdl->clocks[clk].negedge_at;
    simHdl->clocks[clk].negedge_at = simHdl->sim_time;
    simHdl->clocks[clk].negedge_count += 1llu;
    need_to_yield |=
      (simHdl->clocks[clk].negedge_count == simHdl->clocks[clk].negedge_limit);
  }

  // reset the stop/abort flags
  simHdl->stop_called = false;
  simHdl->abort_called = false;

  // run the schedule function
  if (schedule)
    schedule(simHdl, simHdl->model->get_instance());

  // if necessary, setup to yield to the UI at the end of this timeslice
  if (need_to_yield || simHdl->force_halt)
  {
    simHdl->force_halt = false;
    bk_schedule_ui_event(simHdl, simHdl->sim_time);
  }

  // don't repeat initial events
  if (priority_group(ev.priority) != PG_INITIAL)
    return simHdl->clocks[clk].period;
  else
    return 0llu;
}

static tTime run_combo_schedule_event(tSimStateHdl simHdl, tEvent& ev)
{
  unsigned int n = ev.data.value;
  tClock         clk = get_clock_event_clk(n);
  tEdgeDirection dir = get_clock_event_dir(n);

  tScheduleFn schedule;
  if (dir == POSEDGE)
    schedule = simHdl->clocks[clk].after_posedge;
  else
    schedule = simHdl->clocks[clk].after_negedge;

  if (schedule) {
    simHdl->in_combo_schedule = true;
    schedule(simHdl, simHdl->model->get_instance());
    simHdl->in_combo_schedule = false;
  }

  return simHdl->clocks[clk].period;
}

static tTime quit_event(tSimStateHdl simHdl, tEvent& /* unused */)
{
  simHdl->queue->clear();
  return 0llu;
}

static tTime yield_event(tSimStateHdl simHdl, tEvent& ev)
{
  simHdl->sim_running = false;
  simHdl->sim_time = ev.at;

  /* return control to the caller of bk_sync_run();
   * sync_run_events() flushes file buffers before returning
   */
  simHdl->queue->halt();
  return (0llu);
}

/*
 * Utility routines used to manage event callbacks.
 */

static bool isMatchingYieldEvent(tSimStateHdl simHdl, const tEvent& ev)
{
  return ((ev.fn == yield_event) &&
          (ev.at == simHdl->target_yield_time) &&
          (ev.priority == make_priority(PG_FINAL, PS_UI)));
}

static bool isMatchingScheduleEvent(tSimStateHdl simHdl, const tEvent& ev)
{
  return ((ev.fn == run_edge_schedule_event ||
           ev.fn == run_combo_schedule_event) &&
          ev.data.value == simHdl->data_to_match);
}

static bool isRealScheduleEvent(tSimStateHdl simHdl, const tEvent& ev)
{
  if (ev.fn == run_edge_schedule_event)
  {
    unsigned int n = ev.data.value;
    tClock         clk = get_clock_event_clk(n);
    tEdgeDirection dir = get_clock_event_dir(n);

    return (((dir == POSEDGE) && (simHdl->clocks[clk].on_posedge != NULL)) ||
            ((dir == NEGEDGE) && (simHdl->clocks[clk].on_negedge != NULL)));
  }
  else
    return (ev.fn == run_combo_schedule_event);
}

static bool isCycleDumpEvent(tSimStateHdl /* unused */, const tEvent& ev)
{
  return (ev.fn == dump_cycle_event);
}

static void add_dummy_schedule_events(tSimStateHdl simHdl)
{
  ++(simHdl->need_dummy_edges);
  if (simHdl->queue && (simHdl->need_dummy_edges == 1))
  {
    for (tClock clk = 0; clk < simHdl->clocks.size(); ++clk)
      setup_clock_edges(simHdl, clk);
  }
}

static void setup_reset_events(tSimStateHdl simHdl)
{
  if ((simHdl == NULL) || (simHdl->queue == NULL))
    return;

  tEvent assert_event;
  assert_event.at = 0llu;
  assert_event.priority = make_priority(PG_INITIAL, PS_RESET);
  assert_event.fn = reset_model_event;
  assert_event.data.flag = true; // reset asserted

  tEvent deassert_event;
  deassert_event.at = 2llu;
  deassert_event.priority = make_priority(PG_AFTER_LOGIC, PS_RESET);
  deassert_event.fn = reset_model_event;
  deassert_event.data.flag = false; // reset deasserted

  simHdl->queue->schedule(assert_event);
  simHdl->queue->schedule(deassert_event);
}

static void setup_cycle_dump_events(tSimStateHdl simHdl)
{
  if ((simHdl == NULL) || (simHdl->queue == NULL))
    return;

  // make an event for each real schedule event in the queue
  std::list<tEvent> new_events;
  for (const tEvent* pos = simHdl->queue->find(simHdl, isRealScheduleEvent);
       pos != NULL;
       pos = simHdl->queue->find(simHdl, NULL))
  {
    unsigned int n = pos->data.value;
    tClock clk = get_clock_event_clk(n);

    tEvent ev;
    ev.at         = pos->at;
    if (priority_group(pos->priority) == PG_INITIAL)
      ev.priority = make_priority(PG_INITIAL, PS_CYCLE_DUMP, clk);
    else
      ev.priority = make_priority(PG_BEFORE_LOGIC, PS_CYCLE_DUMP, clk);
    ev.fn         = dump_cycle_event;
    ev.data.value = n;

    new_events.push_front(ev);
  }

  // add new events to the queue
  for (std::list<tEvent>::const_iterator p = new_events.begin();
       p != new_events.end();
       ++p)
    simHdl->queue->schedule(*p);
  new_events.clear();
}

/*
 * Kernel API routines
 */

/* Get version information about the Bluesim model */
void bk_version(tSimStateHdl simHdl, tBluesimVersionInfo* version)
{
  if (version == NULL)
    return;
  (simHdl->model)->get_version(&(version->name), &(version->build));
  version->creation_time = (simHdl->model)->get_creation_time();
}

/* helper routine for checking that model and kernel versions match */
bool check_version(tBluesimVersionInfo* version)
{
  // NULL fields indicate that the model was created without version info
  // in which case the check always succeeds
  // XXX What we really want to check is that the Bluesim API version
  // XXX is the same, which should always be included regardless of
  // XXX the -show-version flag
  //
  if ((version->name == NULL) && (version->build == NULL))
    return true;

  return ((version_name != NULL) && (version->name != NULL) &&
          !strcmp(version_name,version->name));
}

/* Initialize the Bluesim kernel */
tSimStateHdl bk_sync_init(tModel model, tBool master)
{
  tSimStateHdl simHdl = new tSimState;

  simHdl->model = (Model*)model;

  simHdl->sim_time = 0llu;
  simHdl->queue = NULL;

  simHdl->flush_on_pause = true;
  simHdl->sim_running = false;

  simHdl->in_combo_schedule = false;

  simHdl->stop_called = false;
  simHdl->finish_called = false;
  simHdl->fatal_called = false;
  simHdl->abort_called = false;
  simHdl->exit_status = 0;
  simHdl->force_halt = false;

  simHdl->call_dump_cycle_counts = false;

  simHdl->need_dummy_edges = 0;

  simHdl->rule_name_indent = 0;

  simHdl->reset_tick_requests = 0;

  simHdl->sim_timescale = 1;

  tBluesimVersionInfo version;
  simHdl->model->get_version(&(version.name), &(version.build));
  version.creation_time = simHdl->model->get_creation_time();
  if (! check_version(&version)) {
    fprintf(stderr,
	    "%s\n%s\n",
	    "Warning: the Bluesim kernel version does not match the BSC version used to",
	    "generate the Bluesim model");
  }
  init_mem_allocator();
  simHdl->sim_time = 0llu;
  simHdl->queue = new EventQueue();
  simHdl->need_dummy_edges = 0;
  simHdl->model->create_model(simHdl, master != 0);
  simHdl->top_symbol.key = "";
  simHdl->top_symbol.info = SYM_MODULE;
  simHdl->top_symbol.value = bk_get_model_instance(simHdl);

  return simHdl;
}

/* Shutdown the Bluesim kernel */
void bk_shutdown(tSimStateHdl simHdl)
{
  if ((simHdl == NULL) || (simHdl->queue == NULL))
    return;

  simHdl->sim_running = false;

  simHdl->model->destroy_model();
  shutdown_mem_allocator();
  for (unsigned int i = 0; i < simHdl->clocks.size(); ++i)
    free(simHdl->clocks[i].name);
  simHdl->clocks.clear();
  delete simHdl->queue;
  simHdl->queue = NULL;
  clear_plusargs(simHdl);
  delete simHdl;
}

/* Add edges into the event queue for a particular clock waveform.
 * The clock and direction are encoded in the event data.
 */
static void setup_clock_edges(tSimStateHdl simHdl, tClock clk)
{
  // if the clock has no predefined waveform, do not re-schedule events
  if ((simHdl->clocks[clk].low_phase_length == 0llu) &&
      (simHdl->clocks[clk].high_phase_length == 0llu))
    return;

  // otherwise, remove any existing schedule events for this clock

  simHdl->data_to_match = set_clock_event_data(clk, NEGEDGE);
  simHdl->queue->remove(simHdl, isMatchingScheduleEvent);
  simHdl->data_to_match = set_clock_event_data(clk, POSEDGE);
  simHdl->queue->remove(simHdl, isMatchingScheduleEvent);

  // and, add new schedule events for the altered clock

  // determine if the initial edge has already occurred
  bool initial_done = false;
  if (simHdl->sim_time > simHdl->clocks[clk].initial_delay)
    initial_done = true;
  else if ((simHdl->sim_time == simHdl->clocks[clk].initial_delay) &&
           (simHdl->clocks[clk].current_value != simHdl->clocks[clk].initial_value))
    initial_done = true;

  // setup the events correctly for the current time
  tEvent pos_ev, neg_ev, after_pos_ev, after_neg_ev;
  pos_ev.priority   = make_priority(PG_LOGIC, PS_EXECUTE, clk);
  pos_ev.fn         = run_edge_schedule_event;
  pos_ev.data.value = set_clock_event_data(clk, POSEDGE);

  after_pos_ev.priority   = make_priority(PG_FINAL, PS_COMBINATIONAL, clk);
  after_pos_ev.fn         = run_combo_schedule_event;
  after_pos_ev.data.value = set_clock_event_data(clk, POSEDGE);

  neg_ev.priority   = make_priority(PG_LOGIC, PS_EXECUTE, clk);
  neg_ev.fn         = run_edge_schedule_event;
  neg_ev.data.value = set_clock_event_data(clk, NEGEDGE);

  after_neg_ev.priority   = make_priority(PG_FINAL, PS_COMBINATIONAL, clk);
  after_neg_ev.fn         = run_combo_schedule_event;
  after_neg_ev.data.value = set_clock_event_data(clk, NEGEDGE);

  if (!initial_done)
  {
    // we need events for the initial edge and the following edge
    if (simHdl->clocks[clk].initial_value == CLK_LOW)
    {
      pos_ev.at = simHdl->clocks[clk].initial_delay;
      neg_ev.at = pos_ev.at + simHdl->clocks[clk].high_phase_length;
    }
    else
    {
      neg_ev.at = simHdl->clocks[clk].initial_delay;
      pos_ev.at = neg_ev.at + simHdl->clocks[clk].low_phase_length;
    }
  }
  else
  {
    // we need to determine the last true edge time and use it to
    // compute the next 2 edge times
    tTime pos = simHdl->clocks[clk].posedge_at;
    tTime neg = simHdl->clocks[clk].negedge_at;
    if (simHdl->clocks[clk].current_value == CLK_LOW)
    {
      // last edge was negative
      if (neg > pos)
        pos_ev.at = neg + simHdl->clocks[clk].low_phase_length;
      else
        pos_ev.at = pos + simHdl->clocks[clk].period;
      neg_ev.at = pos_ev.at + simHdl->clocks[clk].high_phase_length;
    }
    else
    {
      // last edge was positive
      if (pos > neg)
        neg_ev.at = pos + simHdl->clocks[clk].high_phase_length;
      else
        neg_ev.at = neg + simHdl->clocks[clk].period;
      pos_ev.at = neg_ev.at + simHdl->clocks[clk].low_phase_length;
    }
  }
  after_pos_ev.at = pos_ev.at;
  after_neg_ev.at = neg_ev.at;

  // if the initial waveform edge has not happened
  // and the simulation time is 0, then insert the time-0 edge
  // (the real "initial" edge -- if the clock has one)
  if ( (simHdl->clocks[clk].has_initial_value) &&
       (!initial_done) &&
       (simHdl->sim_time == 0) )
  {
    // XXX can this be reached when sim_time is 0 but the initial edge
    // XXX has already be executed -- in which case, we don't want to
    // XXX the initial execute it again?
    tEdgeDirection dir =
      ( simHdl->clocks[clk].initial_value == CLK_LOW ) ? NEGEDGE : POSEDGE ;
    tEvent init_ev;
    init_ev.at         = 0llu;
    init_ev.priority   = make_priority(PG_INITIAL, PS_EXECUTE, clk);
    init_ev.fn         = run_edge_schedule_event;
    init_ev.data.value = set_clock_event_data(clk, dir);
    simHdl->queue->schedule(init_ev);
  }

  // schedule the edge events we want in the queue
  if ((simHdl->need_dummy_edges > 0) || (simHdl->clocks[clk].on_posedge != NULL))
    simHdl->queue->schedule(pos_ev);
  if ((simHdl->need_dummy_edges > 0) || (simHdl->clocks[clk].on_negedge != NULL))
    simHdl->queue->schedule(neg_ev);
  if (simHdl->clocks[clk].after_posedge != NULL)
    simHdl->queue->schedule(after_pos_ev);
  if (simHdl->clocks[clk].after_negedge != NULL)
    simHdl->queue->schedule(after_neg_ev);
}

/* Define a clock waveform */
tClock bk_define_clock(tSimStateHdl simHdl,
		       const char* name,
                       tClockValue initial_value,
                       tBool       has_initial_value,
                       tTime       first_edge,
                       tTime       phase1_duration,
                       tTime       phase0_duration)
{
  if ((simHdl == NULL) || (simHdl->queue == NULL) || (name == NULL))
    return BAD_CLOCK_HANDLE;

  tClock clk = simHdl->clocks.size();

  tClockInfo ci;
  ci.name = strdup(name);
  ci.current_value = initial_value;
  ci.initial_value = initial_value;
  ci.has_initial_value = (has_initial_value != 0);
  ci.initial_delay = first_edge;
  ci.low_phase_length = phase0_duration;
  ci.high_phase_length = phase1_duration;
  ci.period = phase0_duration + phase1_duration;
  ci.negedge_at = 0llu;
  ci.posedge_at = 0llu;
  ci.combinational_at = 0llu;
  ci.on_posedge    = NULL;
  ci.after_posedge = NULL;
  ci.on_negedge    = NULL;
  ci.after_negedge = NULL;
  ci.posedge_count = 0llu;
  ci.negedge_count = 0llu;
  ci.posedge_limit = 0llu;
  ci.negedge_limit = 0llu;

  simHdl->clocks.push_back(ci);

  return clk;
}

/* Allow a clock definition to be altered (overridden from the UI, etc.) */
tStatus bk_alter_clock(tSimStateHdl simHdl,
		       tClock      clk,
                       tClockValue initial_value,
                       tBool       has_initial_value,
                       tTime       first_edge,
                       tTime       high_duration,
                       tTime       low_duration)
{
  if ((simHdl == NULL) || (simHdl->queue == NULL) ||
      (clk >= simHdl->clocks.size()))
    return BK_ERROR;

  simHdl->clocks[clk].current_value     = initial_value;
  simHdl->clocks[clk].initial_value     = initial_value;
  simHdl->clocks[clk].has_initial_value = (has_initial_value != 0);
  simHdl->clocks[clk].initial_delay     = first_edge;
  simHdl->clocks[clk].low_phase_length  = low_duration;
  simHdl->clocks[clk].high_phase_length = high_duration;
  simHdl->clocks[clk].period            = low_duration + high_duration;

  setup_clock_edges(simHdl, clk);

  return BK_SUCCESS;
}

/* Setup a callback for a clock event */
tStatus bk_set_clock_event_fn(tSimStateHdl simHdl,
			      tClock clk,
                              tScheduleFn on_edge_callback,
                              tScheduleFn after_edge_callback,
                              tEdgeDirection dir)
{
  if ((simHdl == NULL) || (simHdl->queue == NULL) ||
      (clk >= simHdl->clocks.size()))
    return BK_ERROR;

  if (dir == POSEDGE)
  {
    simHdl->clocks[clk].on_posedge    = on_edge_callback;
    simHdl->clocks[clk].after_posedge = after_edge_callback;
  }
  else
  {
    simHdl->clocks[clk].on_negedge    = on_edge_callback;
    simHdl->clocks[clk].after_negedge = after_edge_callback;
  }

  setup_clock_edges(simHdl, clk);

  return BK_SUCCESS;
}

/* Trigger a clock edge at a given time, for aperiodic clocks */
tStatus bk_trigger_clock_edge(tSimStateHdl simHdl,
			      tClock clk, tEdgeDirection dir, tTime at)
{
  if ((simHdl == NULL) || (simHdl->queue == NULL) ||
      (clk >= simHdl->clocks.size()) || (at < simHdl->sim_time))
    return BK_ERROR;

  if ( (simHdl->need_dummy_edges > 0) ||
       ((dir == POSEDGE) && (simHdl->clocks[clk].on_posedge != NULL)) ||
       ((dir == NEGEDGE) && (simHdl->clocks[clk].on_negedge != NULL)) )
  {
    tEvent ev, after_ev;
    ev.at         = at;
    ev.priority   = make_priority(PG_LOGIC, PS_EXECUTE, clk);
    ev.fn         = run_edge_schedule_event;
    ev.data.value = set_clock_event_data(clk, dir);
    after_ev.at         = at;
    after_ev.priority   = make_priority(PG_FINAL, PS_COMBINATIONAL, clk);
    after_ev.fn         = run_combo_schedule_event;
    after_ev.data.value = set_clock_event_data(clk, dir);
    simHdl->queue->schedule(ev);
    simHdl->queue->schedule(after_ev);
    return 1; /* events scheduled */
  }

  return 0; /* no events scheduled */
}

/* Enqueue an initial clock edge, for periodic and aperiodic clocks */
tStatus bk_enqueue_initial_clock_edge(tSimStateHdl simHdl,
				      tClock clk, tEdgeDirection dir)
{
  if ((simHdl == NULL) || (simHdl->queue == NULL) ||
      (clk >= simHdl->clocks.size()))
    return BK_ERROR;

  // XXX when a warning/error mechanism becomes available,
  // this would be a good place to warn
  if (simHdl->sim_time != 0llu)
    return BK_ERROR;

  if ( (simHdl->need_dummy_edges > 0) ||
       ((dir == POSEDGE) && (simHdl->clocks[clk].on_posedge != NULL)) ||
       ((dir == NEGEDGE) && (simHdl->clocks[clk].on_negedge != NULL)) )
  {
    tEvent ev;
    ev.at         = 0llu;
    ev.priority   = make_priority(PG_INITIAL, PS_EXECUTE, clk);
    ev.fn         = run_edge_schedule_event;
    ev.data.value = set_clock_event_data(clk, dir);
    simHdl->queue->schedule(ev);
    return 1; /* 1 event scheduled */
  }

  return 0; /* no events scheduled */
}

/* Lookup a clock handle by name */
tClock bk_get_clock_by_name(tSimStateHdl simHdl, const char* name)
{
  if (name)
  {
    tClock clk = simHdl->clocks.size();
    while (clk > 0)
    {
      if (!strcmp(name, simHdl->clocks[--clk].name))
        return clk;
    }
  }
  return BAD_CLOCK_HANDLE;
}

/* If there is already a clock domain with the given name,
 * return the handle for it.  If there is no clock domain with
 * this name yet, then create one and return the handle of the
 * new domain.  The domain characteristics can be set with
 * a subsequent call to bk_alter_clock().
 */
tClock bk_get_or_define_clock(tSimStateHdl simHdl, const char* name)
{
  // look for existing domain
  tClock clk = bk_get_clock_by_name(simHdl, name);
  if (clk != BAD_CLOCK_HANDLE)
    return clk;

  // no existing domain, so create one that must be toggled
  // using bk_trigger_clock_edge() or setup using bk_alter_clock().
  return (bk_define_clock(simHdl, name, CLK_LOW, 0, 0, 0, 0));
}

/* Get the number of clocks defined in the kernel */
tUInt32 bk_num_clocks(tSimStateHdl simHdl)
{
  return simHdl->clocks.size();
}

/* Get the clock handle for the nth clock.
 *
 * Returns the clock handle on success or BAD_CLOCK_HANDLE on error.
 */
tClock bk_get_nth_clock(tSimStateHdl simHdl, tUInt32 n)
{
  if (n >= simHdl->clocks.size())
    return BAD_CLOCK_HANDLE;
  else
    return ((tClock) n);
}

/* Get various information for a clock */

const char* bk_clock_name(tSimStateHdl simHdl, tClock clk)
{
  if (clk >= simHdl->clocks.size())
    return NULL;
  return simHdl->clocks[clk].name;
}

tClockValue bk_clock_initial_value(tSimStateHdl simHdl, tClock clk)
{
  if (clk >= simHdl->clocks.size())
    return CLK_LOW;
  return simHdl->clocks[clk].initial_value;
}

tTime bk_clock_first_edge(tSimStateHdl simHdl, tClock clk)
{
  if (clk >= simHdl->clocks.size())
    return 0;
  return simHdl->clocks[clk].initial_delay;
}

tTime bk_clock_duration(tSimStateHdl simHdl, tClock clk, tClockValue value)
{
  if (clk >= simHdl->clocks.size())
    return 0;
  if (value == CLK_LOW)
    return simHdl->clocks[clk].low_phase_length;
  else
    return simHdl->clocks[clk].high_phase_length;
}

tClockValue bk_clock_val(tSimStateHdl simHdl, tClock clk)
{
  if (clk >= simHdl->clocks.size())
    return CLK_LOW;

  return simHdl->clocks[clk].current_value;
}

tUInt64 bk_clock_cycle_count(tSimStateHdl simHdl, tClock clk)
{
  if (clk >= simHdl->clocks.size())
    return 0llu;

  return std::max(simHdl->clocks[clk].posedge_count,
                  simHdl->clocks[clk].negedge_count);
}

tUInt64 bk_clock_edge_count(tSimStateHdl simHdl,
			    tClock clk, tEdgeDirection dir)
{
  if (clk >= simHdl->clocks.size())
    return 0llu;

  if (dir == POSEDGE)
    return simHdl->clocks[clk].posedge_count;
  else
    return simHdl->clocks[clk].negedge_count;
}

/*
 * Setup a default reset waveform (asserted at time 0, deasserted at time 2).
 * This should be called before the first bk_sync_run() call.
 */
void bk_use_default_reset(tSimStateHdl simHdl)
{
  setup_reset_events(simHdl);
}

/*
 * Simulation control
 */

/* Get the current simulation time */
tTime bk_now(tSimStateHdl simHdl)
{
  return (simHdl->sim_timescale) * (simHdl->sim_time);
}

// A valid time unit is of the form: (1 | 10 | 100)<space>(s | ms | us | ns | ps | fs)
// The Verilog standard allows more whitespace, but that doesn't seem useful here.
// This test is ugly, but hopefully simple and correct.
bool valid_unit(const char* scale_unit) {
  std::string scale_unit_str(scale_unit);
  size_t unit_pos = 0;

  if(scale_unit_str.find("1 ") == 0) {
    unit_pos = 2;
  } else if(scale_unit_str.find("10 ") == 0) {
    unit_pos = 3;
  } else if(scale_unit_str.find("100 ") == 0) {
    unit_pos = 4;
  }

  // We didn't match a valid scale, so fail.
  if(unit_pos == 0)
    return false;

  std::string unit_str = scale_unit_str.substr(unit_pos);

  return (unit_str == "s"  || unit_str == "ms" || unit_str == "us" ||
          unit_str == "ns" || unit_str == "ps" || unit_str == "fs");
}

/* Set the simulation timescale */
tStatus bk_set_timescale(tSimStateHdl simHdl, const char* scale_unit, tTime scale_factor)
{
  if (simHdl->sim_time != 0)
    return BK_ERROR;

  if (!valid_unit(scale_unit))
    return BK_ERROR;

  simHdl->sim_timescale = scale_factor;

  return BK_SUCCESS;
}

/* Test if a given simulation time is still ongoing.
 * WARNING: This is a specialized function for use by
 * Bluesim primitives to facilitate connections to
 * event-driven simulation.  FOR EXPERT USE ONLY!
 *
 * Returns 1 (True) if the given simulation time is ongoing,
 * and 0 (False) otherwise.
 */
tBool bk_is_same_time(tSimStateHdl simHdl, tTime t)
{
  /* This will only be called from primitives during event execution,
   * or from a SystemC wrapper after bk_sync_run has returned.
   */
  if (simHdl->sim_running && (simHdl->sim_time == t))
    return 1;
  else
    return 0;
}

/* Test if we are currently executing within a combinational
 * schedule.
 * WARNING: This is a specialized function for use by Bluesim
 * primitives to facilitate clock-crossing implementations.
 * FOR EXPERT USE ONLY!
 *
 * Returns 1 (True) if currently executing a combinational schedule,
 * and 0 (False) otherwise.
 */
tBool bk_is_combo_sched(tSimStateHdl simHdl)
{
  /* This access is not protected by a mutex, for performance reasons.
   * It should be safe to access, because this will only be called from
   * primitives during event execution.
   */
  return simHdl->in_combo_schedule ? 1 : 0;
}

/* Get information on the clock event queue */

tTime bk_clock_last_edge(tSimStateHdl simHdl, tClock clk)
{
  if (clk >= simHdl->clocks.size())
    return ((tTime) 0llu);

  // Case 1: we are before the initial edge time
  if (simHdl->sim_time < simHdl->clocks[clk].initial_delay)
    return ((tTime) 0llu);

  // Case 2: we are at the time of the initial edge
  if (simHdl->sim_time == simHdl->clocks[clk].initial_delay)
  {
    if (simHdl->clocks[clk].current_value != simHdl->clocks[clk].initial_value)
      return (simHdl->sim_time);     // edge has already happened
    else
      return ((tTime) 0llu); // edge has not happened yet
  }

  // Case 3: we are beyond the first edge

  // Note: we are not guaranteed to have a schedule for both edges,
  // so we have to figure out which of the edge times is accurate.
  // We should trust the most recent edge time and reconstruct the
  // other one.  Also, for aperiodic clocks both edge times are
  // accurate.
  tTime pos = simHdl->clocks[clk].posedge_at;
  tTime neg = simHdl->clocks[clk].negedge_at;
  if (simHdl->clocks[clk].current_value == CLK_LOW)
  {
    // last edge was negedge, so determine the time
    if (neg > pos)
      return neg;
    else
      return (pos + simHdl->clocks[clk].high_phase_length);
  }
  else
  {
    // last edge was posedge, so determine the time
    if (pos > neg)
      return pos;
    else
      return (neg + simHdl->clocks[clk].low_phase_length);
  }
}

tTime bk_clock_combinational_time(tSimStateHdl simHdl, tClock clk)
{
  if (simHdl->queue && (clk < simHdl->clocks.size()))
    return simHdl->clocks[clk].combinational_at;

  return ((tTime) 0llu);
}

/* Simulation loop */

/* Quit simulation at the end of a given time slice */

void bk_quit_at(tSimStateHdl simHdl, tTime t)
{
  tEvent ev;
  ev.at       = t;
  ev.priority = make_priority(PG_FINAL, PS_UI);
  ev.fn       = quit_event;
  ev.data.ptr = NULL;
  simHdl->queue->schedule(ev);
}

tStatus bk_quit_after_edge(tSimStateHdl simHdl,
			   tClock clk, tEdgeDirection dir, tUInt64 cycle)
{
  if ((simHdl == NULL) || (simHdl->queue == NULL) ||
      (clk >= simHdl->clocks.size()))
    return BK_ERROR;

  if (dir == POSEDGE)
    simHdl->clocks[clk].posedge_limit = cycle;
  else
    simHdl->clocks[clk].negedge_limit = cycle;

  return BK_SUCCESS;
}

/* Test if simulation events are currently being executed.
 *
 * Returns 0 if the simulation is not running and non-zero if
 * it is running.
 */
tBool bk_is_running(tSimStateHdl simHdl)
{
  return simHdl->sim_running ? 1 : 0;
}

/* Execute the events in the simulation queue on the caller's
 * thread.  Shared body of bk_sync_run() and bk_sync_step(); the
 * caller has already validated the handle.
 */
static tStatus sync_run_events(tSimStateHdl simHdl)
{
  simHdl->sim_running = true;

  /* execute the events in the simulation queue, resetting the
   * transient halt flag first
   */
  simHdl->force_halt = false;
  simHdl->queue->execute(simHdl);

  /* already false if a yield event ended the run; clear it here
   * in case the queue drained instead
   */
  simHdl->sim_running = false;

  /* flush open file buffers once per return to the caller (unless
   * the embedder disabled it with bk_set_flush_on_pause())
   */
  if (simHdl->flush_on_pause)
    fflush(NULL);

  return BK_SUCCESS;
}

/* Execute simulation events on the caller's thread until a stopping
 * condition is encountered or the event queue drains.
 *
 * Returns BK_ERROR on error and BK_SUCCESS on success.
 */
tStatus bk_sync_run(tSimStateHdl simHdl)
{
  if ((simHdl == NULL) || (simHdl->queue == NULL))
    return BK_ERROR;

  /* check if the simulation is already running (not re-entrant) */
  if (bk_is_running(simHdl))
    return BK_ERROR;

  return sync_run_events(simHdl);
}

/* Execute simulation events on the caller's thread until one cycle
 * of the given clock has completed (bluetcl's 'sim step 1'), a
 * stopping condition is encountered, or the event queue drains.
 *
 * Returns BK_ERROR on error and BK_SUCCESS on success.
 */
tStatus bk_sync_step(tSimStateHdl simHdl, tClock clk)
{
  if ((simHdl == NULL) || (simHdl->queue == NULL))
    return BK_ERROR;

  /* check if the simulation is already running (not re-entrant) */
  if (bk_is_running(simHdl))
    return BK_ERROR;

  if (clk >= simHdl->clocks.size())
    return BK_ERROR;

  /* once $finish has been called there is nothing to step */
  if (bk_finished(simHdl))
    return BK_ERROR;

  /* One step runs until one more edge of clk in the direction which
   * returns the clock to its current value has executed -- one full
   * clock cycle -- matching bluetcl's 'sim step'.  Before any logic
   * has executed at time 0, step to the clock's first edge instead
   * of the next edge which returns to the initial clock value.
   */
  tEdgeDirection dir =
    (bk_clock_val(simHdl, clk) == CLK_LOW) ? NEGEDGE : POSEDGE;
  if ((bk_now(simHdl) == 0llu) &&
      (bk_clock_cycle_count(simHdl, clk) == 0llu))
    dir = (dir == POSEDGE) ? NEGEDGE : POSEDGE;

  /* arrange to yield after one more edge in that direction */
  tUInt64 old_limit = (dir == POSEDGE) ? simHdl->clocks[clk].posedge_limit
                                       : simHdl->clocks[clk].negedge_limit;
  tUInt64 limit = bk_clock_edge_count(simHdl, clk, dir) + 1llu;
  bk_quit_after_edge(simHdl, clk, dir, limit);

  tStatus status = sync_run_events(simHdl);

  /* Restore the edge limit for this clock and direction, so the
   * temporary one-cycle limit cannot linger and stop a later
   * bk_sync_run() early (e.g. after $stop or an abort ended the
   * step before the cycle completed), and so a pending limit set
   * with bk_quit_after_edge() is preserved.  Limits on other clocks
   * and on the other edge direction are never touched (though the
   * step will return early if one of them is reached mid-step).
   */
  bk_quit_after_edge(simHdl, clk, dir, old_limit);

  return status;
}

/* Control whether the sync path flushes open file buffers when it
 * returns control to the caller (default: enabled).
 */
void bk_set_flush_on_pause(tSimStateHdl simHdl, tBool enabled)
{
  if (simHdl == NULL)
    return;

  simHdl->flush_on_pause = (enabled != 0);
}

/* Test whether any events remain in the simulation queue. */
tBool bk_sync_pending(tSimStateHdl simHdl)
{
  if ((simHdl == NULL) || (simHdl->queue == NULL))
    return 0;

  return (simHdl->queue->size() > 0) ? 1 : 0;
}

/* Schedule a UI callback for the end of a given timeslice,
 * if there is not already one scheduled at or before that time.
 *
 * Returns BK_ERROR on error or BK_SUCCESS on success.
 */
tStatus bk_schedule_ui_event(tSimStateHdl simHdl, tTime at)
{
  if ((simHdl == NULL) || (simHdl->queue == NULL))
    return BK_ERROR;

  simHdl->target_yield_time = at;
  if (simHdl->queue->find(simHdl, isMatchingYieldEvent) == NULL)
  {
    tEvent event;
    event.at       = at;
    event.priority = make_priority(PG_FINAL, PS_UI);
    event.data.ptr = NULL;
    event.fn       = yield_event;

    simHdl->queue->schedule(event);
  }

  return BK_SUCCESS;
}

/* Remove a UI callback previously scheduled at the end of a given timeslice.
 *
 * Returns BK_ERROR on error or BK_SUCCESS on success.
 */
tStatus bk_remove_ui_event(tSimStateHdl simHdl, tTime at)
{
  if ((simHdl == NULL) || (simHdl->queue == NULL))
    return BK_ERROR;

  simHdl->target_yield_time = at;
  simHdl->queue->remove(simHdl, isMatchingYieldEvent);

  return BK_SUCCESS;
}

/* Control dumping of cycle counts */

void bk_enable_cycle_dumping(tSimStateHdl simHdl)
{
  if (!simHdl->call_dump_cycle_counts)
  {
    simHdl->call_dump_cycle_counts = true;
    setup_cycle_dump_events(simHdl);
  }
}

void bk_disable_cycle_dumping(tSimStateHdl simHdl)
{
  simHdl->call_dump_cycle_counts = false;
  if (simHdl->queue)
    simHdl->queue->remove(simHdl, isCycleDumpEvent);
}

tBool bk_is_cycle_dumping_enabled(tSimStateHdl simHdl)
{
  return simHdl->call_dump_cycle_counts ? 1 : 0;
}

void bk_dump_cycle_counts(tSimStateHdl simHdl, const char* label, tClock clk)
{
  unsigned int indent = 0;
  if (label)
  {
    printf("%s: ", label);
    indent = strlen(label) + 2;
  }
  if (clk >= simHdl->clocks.size())
  {
    for (tClock n = 0; n < simHdl->clocks.size(); ++n)
    {
      if (n > 0 && indent != 0)
        printf("%*s", indent, "");
      printf("%llu %s cycles\n",
             bk_clock_cycle_count(simHdl, n), simHdl->clocks[n].name);
    }
  }
  else
    printf("%llu %s cycles\n",
           bk_clock_cycle_count(simHdl, clk), simHdl->clocks[clk].name);
}

/* Call to enable clock edges without logic (for interactive stepping) */
void bk_set_interactive(tSimStateHdl simHdl)
{
  add_dummy_schedule_events(simHdl);
}

/* Stop simulation now */
void bk_stop_now(tSimStateHdl simHdl, tSInt32 status)
{
  simHdl->stop_called = true;
  simHdl->exit_status = status;
  bk_schedule_ui_event(simHdl, simHdl->sim_time);
}

/* End simulation */
void bk_finish_now(tSimStateHdl simHdl, tSInt32 status)
{
  simHdl->finish_called = true;
  simHdl->exit_status = status;
  bk_schedule_ui_event(simHdl, simHdl->sim_time);
}

void bk_fatal_now(tSimStateHdl simHdl, tSInt32 status)
{
  simHdl->fatal_called = true;
  bk_finish_now(simHdl, status);
}

tBool bk_stopped(tSimStateHdl simHdl)
{
  return simHdl->stop_called ? 1 : 0;
}

tBool bk_finished(tSimStateHdl simHdl)
{
  return simHdl->finish_called ? 1 : 0;
}

tSInt32 bk_exit_status(tSimStateHdl simHdl)
{
  return simHdl->exit_status;
}

tBool bk_fataled(tSimStateHdl simHdl)
{
  return simHdl->fatal_called ? 1 : 0;
}

/* Abort simulation (from outside, Ctrl-C, SIGPIPE, etc.) */
void bk_abort_now(tSimStateHdl simHdl)
{
  if (bk_is_running(simHdl))
  {
    simHdl->abort_called = true;
    simHdl->force_halt = true;
  }
}

tBool bk_aborted(tSimStateHdl simHdl)
{
  return simHdl->abort_called ? 1 : 0;
}

/* Routine which provides direct access to the top-level model.  This
 * should only be used by callers that know exactly what they are doing.
 */
void* bk_get_model_instance(tSimStateHdl simHdl)
{
  return simHdl->model->get_instance();
}

/* Get the symbol for the top module. */
tSymbol bk_top_symbol(tSimStateHdl simHdl)
{
  return &(simHdl->top_symbol);
}

/* Lookup a symbol by name.  Returns BAD_SYMBOL if the named
 * symbol is not found.
 */
tSymbol bk_lookup_symbol(tSymbol root, const char* name)
{
  tSymbol sym = root;

  if (name == NULL) return BAD_SYMBOL;

  while ((sym != BAD_SYMBOL) && bk_is_module(sym))
  {
    Module* mod = (Module*) bk_get_ptr(sym);
    const char* cptr = strchr(name,'.');
    unsigned int len = (cptr == NULL) ? strlen(name) : (cptr - name);
    sym = mod->lookup(name,len);
    name += len;
    if (*name == '\0') break;
    ++name;  // skip "."
  };

  return sym;
}

/* Test if a symbol represents a module */
tBool bk_is_module(tSymbol sym)
{
  return (get_symtag(sym) == SYM_MODULE) ? 1 : 0;
}

/* Test if a symbol represents a rule */
tBool bk_is_rule(tSymbol sym)
{
  return (get_symtag(sym) == SYM_RULE) ? 1 : 0;
}

/* Test if a symbol represents a value */
tBool bk_is_single_value(tSymbol sym)
{
  switch (get_symtag(sym))
  {
   case SYM_DEF:
   case SYM_PARAM:
   case SYM_PORT:
   case SYM_COMPUTED:
     return 1;
   default:
     return 0;
  }
}

/* Test if a symbol represents a range of values */
tBool bk_is_value_range(tSymbol sym)
{
  return (get_symtag(sym) == SYM_RANGE) ? 1 : 0;
}

/* Get a pointer to the value for a value symbol.
 * Returns NULL for other symbol types.
 */
const unsigned int* bk_peek_symbol_value(tSymbol sym)
{
  if (sym == BAD_SYMBOL) return NULL;

  switch (get_symtag(sym))
  {
    case SYM_DEF:
    case SYM_PARAM:
    case SYM_PORT:
    {
      unsigned int sz = bk_get_size(sym);
      void* ptr = bk_get_ptr(sym);
      return (symbol_value(ptr,sz));
    }
    default:
      return NULL;
  }
}

/* Get the minimum address for value range.
 * Returns NULL for other symbol types.
 */
tUInt64 bk_get_range_min_addr(tSymbol sym)
{
  if (bk_is_value_range(sym))
  {
    Range* range = (Range*) bk_get_ptr(sym);
    return range->lo;
  }

  return (tUInt64) 0;
}

/* Get the maximum address for a value range.
 * Returns NULL for other symbol types.
 */
tUInt64 bk_get_range_max_addr(tSymbol sym)
{
  if (bk_is_value_range(sym))
  {
    Range* range = (Range*) bk_get_ptr(sym);
    return range->hi;
  }

  return (tUInt64) 0;
}

/* Get a pointer to a value selected from a range.
 * Returns NULL for other symbol types, or if the address is out of bounds.
 */
const unsigned int* bk_peek_range_value(tSymbol sym, tUInt64 addr)
{
  if (bk_is_value_range(sym))
  {
    Range* range = (Range*) bk_get_ptr(sym);
    return range->fetch(range->base,addr);
  }

  return NULL;
}

/* Get the number of sub-symbols of a module.
 * Returns 0 for other symbol types.
 */
tUInt32 bk_num_symbols(tSymbol sym)
{
  if (sym == BAD_SYMBOL) return 0;

  if (bk_is_module(sym))
  {
    Module* mod = (Module*) bk_get_ptr(sym);
    return mod->num_symbols();
  }
  else
    return 0;
}

/* Get the Nth sub-symbol of a module (starting at 0).
 * Returns BAD_SYMBOL for other symbol types.
 */
tSymbol bk_get_nth_symbol(tSymbol sym, tUInt32 n)
{
  if (sym == BAD_SYMBOL) return BAD_SYMBOL;

  if (bk_is_module(sym))
  {
    Module* mod = (Module*) bk_get_ptr(sym);
    return mod->nth_symbol(n);
  }
  else
    return BAD_SYMBOL;
}

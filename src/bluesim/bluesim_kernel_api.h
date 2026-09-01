#ifndef __BLUESIM_KERNEL_H__
#define __BLUESIM_KERNEL_H__

#include "bluesim_types.h"
#include "bluesim_host_ops.h"
#include "bluesim_introspection.h"

/*
 * Declarations of all functions in the Bluesim kernel API.
 * All functions have C linkage.
 *
 * The kernel is synchronous: it creates no threads and installs no
 * signal handlers.  Simulation events are executed on the caller's
 * thread by bk_sync_run() and bk_sync_step(), which return when a
 * stopping condition is encountered.  An embedder that wants
 * asynchronous execution or Ctrl-C handling provides them itself
 * (as bluetcl does with its simulation worker thread).
 *
 * The kernel and model also perform no I/O of their own: all runtime
 * I/O ($display output, $fopen/$fwrite file access, memory-file
 * preloads, warnings, ...) goes through the host operations the
 * embedder passes to bk_sync_init() (see bluesim_host_ops.h).
 */

#if __cplusplus
extern "C" {
#endif

/*
 * Model construction.
 *
 * A generated design exports one constructor entry point,
 *
 *   void* new_MODEL_<top>(const struct bs_host_ops* ops, void* ctx,
 *                         void* state, void* inputs, void* outputs);
 *
 * which returns the design's model handle (the 'tModel' the functions
 * below take).  The model object itself lives in static storage
 * inside the loaded design; every call returns the same handle, and
 * each call re-records the five pointers:
 *
 *  - 'ops'/'ctx': the host operations the model performs its I/O
 *    through during construction (memory-file preloads) and the host
 *    context passed to each of them.  They must be the same table and
 *    context later passed to bk_sync_init().  Borrowed.
 *
 *  - 'state': storage for the model itself, at least
 *    bk_state_bytes(model) bytes, aligned like max_align_t (any
 *    malloc result qualifies).  bk_sync_init() placement-constructs
 *    the whole module tree in this buffer: the module objects at the
 *    front, and every published state element at its descriptor
 *    offset within the element sub-area that starts
 *    bk_state_elements_offset(model) bytes in (see
 *    bluesim_introspection.h).  Borrowed until bk_shutdown(), which
 *    tears the model down in place and frees nothing.
 *
 *  - 'inputs'/'outputs': storage for the top module's input and
 *    output port areas, at least bk_input_bytes(model) /
 *    bk_output_bytes(model) bytes, 8-byte aligned; either may be
 *    NULL when its area is empty.  Borrowed until bk_shutdown().
 *    The top module's ports are bound to these buffers at their
 *    published descriptor offsets: the host drives input ports
 *    (method arguments and enables) by writing 'inputs' before a
 *    clock edge, and reads output ports (method results and
 *    readies, refreshed by the schedule) from 'outputs' after it
 *    (see bluesim_introspection.h for the exact semantics).
 *
 * All five pointers may be NULL for a SIZING call: the returned
 * handle then supports the pre-initialization queries (the bk_*
 * introspection walkers, bk_max_event_queue_depth(),
 * bk_stack_depth_bound()) so the host can size the buffers, after
 * which it calls new_MODEL_<top>() again with real storage.
 * bk_sync_init() refuses (returns NULL) a model whose required
 * storage is still unbound.  Nothing in new_MODEL_<top>() or in model
 * construction calls the allocator.
 */

/*
 * Kernel resource management routines.
 */

/* Get the number of bytes of storage the kernel needs for its
 * simulation context: the simulation state and an event queue of the
 * given capacity.  The embedder allocates (or otherwise provides) a
 * buffer of at least this size, aligned like max_align_t (any malloc
 * result qualifies), and hands it to bk_sync_init(); the kernel
 * itself never allocates its context.  The size depends only on the
 * chosen event-queue capacity, so like bk_max_event_queue_depth()
 * and bk_stack_depth_bound() it can be queried before
 * bk_sync_init().  Returns 0 if 'event_queue_capacity' is 0 (an
 * invalid capacity).
 */
tUInt64 bk_context_bytes(tUInt32 event_queue_capacity);

/* This must be called before calling any other Bluesim
 * kernel API functions.
 * When master is non-zero, it indicates that the model
 * is its own master.
 *
 * The 'ops' argument supplies the host operations through which the
 * runtime performs all of its I/O, and 'ctx' is the host context
 * passed as the first argument of every operation (it may be NULL if
 * the operations need no context).  The initialization is rejected
 * (NULL is returned) if 'ops' is NULL, if any operation is missing,
 * or if the table is older (by size or version) than this kernel
 * requires; embedders that want the traditional stdio behavior pass
 * the implementation from bluesim_host_ops_default.h, as bluetcl
 * does.  Both 'ops' and 'ctx' are borrowed: they must remain valid
 * until bk_shutdown().
 *
 * Note: system tasks that are not passed a simulation handle (the
 * $fopen family) use the ops of the most recent bk_sync_init() in
 * the process, so when several models are loaded into one process
 * they must all be initialized with the same 'ops' and 'ctx'.
 *
 * 'event_queue_capacity' fixes the capacity of the kernel's event
 * queue: the storage lives in the context buffer and NEVER grows,
 * and scheduling an event into a full queue fails through the host's
 * noreturn event_queue_overflow operation.  The host chooses the
 * capacity; the intended budget is
 *
 *   bk_max_event_queue_depth(model) + headroom
 *
 * where the headroom covers the host's own event-enqueuing calls
 * (each documents its cost below).  bluetcl and the generated
 * SystemC wrappers use a headroom of 16, which generously covers
 * their usage (at most one extra pending UI yield event, one
 * bk_quit_at event and one host-triggered edge pair at a time).  A
 * capacity of 0 is rejected (NULL is returned).
 *
 * 'context_buffer' provides the storage for the kernel's simulation
 * context: at least bk_context_bytes(event_queue_capacity) bytes,
 * aligned like max_align_t.  The kernel constructs its state in this
 * buffer instead of allocating it.  The buffer is borrowed, not
 * owned: it must remain valid until bk_shutdown(), which tears the
 * context down in place and frees nothing, after which the buffer is
 * the caller's to reuse (including for another bk_sync_init()) or
 * release.  A NULL or misaligned buffer is rejected (NULL is
 * returned).
 *
 * Returns a handle to the simulation state, which is needed as an
 * argument to the other Bluesim kernel API functions.  The handle
 * points into 'context_buffer' and is invalidated by bk_shutdown().
 * Returns NULL on error.
 */
tSimStateHdl bk_sync_init(tModel model, tBool master,
                          const struct bs_host_ops* ops, void* ctx,
                          tUInt32 event_queue_capacity,
                          void* context_buffer);

/* Get the host operations / host context registered with
 * bk_sync_init().  A NULL simHdl returns the process-wide copy
 * (that of the most recent bk_sync_init()), which is what system
 * tasks without a simulation handle use.
 */
const struct bs_host_ops* bk_host_ops(tSimStateHdl simHdl);
void* bk_host_ctx(tSimStateHdl simHdl);

/* Report a fatal condition in the model through the corresponding
 * host operation and do not return.  These are called by the runtime
 * library and by generated model code, not by embedders.
 * bk_divide_by_zero uses the process-wide host operations (see
 * bk_host_ops above), since division is performed in contexts that
 * have no simulation handle at hand.
 */
BS_HOST_NORETURN void bk_divide_by_zero(const char* description);
BS_HOST_NORETURN void bk_out_of_bounds(tSimStateHdl simHdl,
                                       const char* prim,
                                       const char* instance,
                                       const char* access,
                                       tUInt64 addr,
                                       tUInt64 lo,
                                       tUInt64 hi);

/* Report that the kernel's event queue is full through the host's
 * event_queue_overflow operation and do not return.  This is called
 * by the event queue itself when an event is scheduled past the
 * fixed capacity chosen at bk_sync_init(); it is not for embedders
 * to call.  'capacity' is the fixed capacity that was exceeded.
 */
BS_HOST_NORETURN void bk_event_queue_overflow(tSimStateHdl simHdl,
                                              tUInt32 capacity);

/* Get the most events the kernel's event queue has ever held at
 * once.  This is a test/debug aid for validating event-queue
 * capacity budgets against bk_max_event_queue_depth(); it never
 * exceeds the capacity fixed at bk_sync_init().
 */
tUInt32 bk_event_queue_high_water(tSimStateHdl simHdl);

/* This should be called at the end of simulation to release the
 * resources controlled by the simulation kernel.  The simulation
 * context is torn down in place inside the caller-provided buffer;
 * the buffer itself is never freed here, and afterwards it is the
 * caller's to reuse or release.  After bk_shutdown() is called, the
 * handle is invalid and no other Bluesim kernel API functions may be
 * called unless bk_sync_init() has been called first.
 */
void bk_shutdown(tSimStateHdl simHdl);

/* Get version information about the Bluesim model */
void bk_version(tSimStateHdl simHdl, tBluesimVersionInfo* version);

/* Get the design's maximum event-queue depth: an upper bound on the
 * number of events the model can have live in the kernel's event
 * queue at any one time, ASSUMING NO HOST CALLS THAT ENQUEUE EVENTS.
 * The value is a static per-design constant computed at code
 * generation from the clocks and reset primitives the model
 * registers; it takes the model handle from new_MODEL_*() (not a
 * simulation handle) so that an embedder can query it before
 * bk_sync_init(), which is where the embedder chooses the actual
 * event-queue capacity.
 *
 * Host calls that enqueue events are NOT included in this bound and
 * must be budgeted by the embedder on top of it; each such call
 * documents its cost below.  They are: bk_quit_at(),
 * bk_schedule_ui_event() at times beyond the single included yield
 * event, bk_trigger_clock_edge() and bk_enqueue_initial_clock_edge()
 * when invoked by the embedder rather than by clock primitives,
 * bk_enable_cycle_dumping(), and bk_define_clock()/bk_alter_clock()
 * for clocks the embedder adds beyond those the model registers.
 *
 * Returns 0 if 'model' is NULL.
 */
tUInt32 bk_max_event_queue_depth(tModel model);

/* Get the design's static stack-depth bound: an upper bound, in
 * bytes, on the stack consumed on the simulation thread by a call to
 * bk_sync_run() or bk_sync_step(), measured from those functions'
 * own frames downward.  The value is computed at link time by
 * bluesim_stack_bound.py from per-function stack-usage data
 * (gcc -fstack-usage -fcallgraph-info=su) for every object linked
 * into the model, plus a hand-annotated table for the runtime's
 * indirect calls; see that tool for the full accounting, including
 * the declared assumptions on reset-chain depth, module hierarchy
 * depth ($display %m) and the width of values formatted or divided
 * through the wide-data VLA paths.
 *
 * The bound deliberately EXCLUDES:
 *   - everything on the far side of the bs_host_ops table: the
 *     embedder supplies those operations at bk_sync_init(), so their
 *     stack cost is the embedder's to add on top;
 *   - terminating/unwinding paths (abort, assertion failures,
 *     allocation-failure throws), which end the run rather than
 *     returning into it;
 *   - bk_sync_init(), new_MODEL_*() and everything else outside the
 *     bk_sync_run()/bk_sync_step() call trees.
 *
 * Returns 0 when no sound bound is available: the design imports
 * foreign (BDPI) functions, the link-time analysis could not run
 * (e.g. no python3, a non-GCC compiler without -fcallgraph-info, or
 * reused objects with no callgraph data), or the model was linked
 * outside bsc's Bluesim link step (e.g. a SystemC build).  Like
 * bk_max_event_queue_depth() it takes the model handle from
 * new_MODEL_*() so an embedder can size a stack before
 * bk_sync_init(); returns 0 if 'model' is NULL.
 */
tUInt64 bk_stack_depth_bound(tModel model);

/*
 * Non-allocating introspection of a model's state elements and of
 * its top-module input and output ports.
 *
 * These walk functions read static per-design descriptor tables
 * emitted by the code generator: they allocate nothing, and like
 * bk_max_event_queue_depth() they take the model handle from
 * new_MODEL_*() (not a simulation handle) so a host can size and
 * inspect a design before bk_sync_init().
 *
 * The descriptor types, the state-element kinds, and the documented
 * ordering, alignment and flat-layout rules (byte offsets within
 * planned contiguous state/input/output areas, and the total byte
 * size of each area) live in bluesim_introspection.h.
 *
 * The bk_get_* functions return a borrowed pointer -- NOT 'own' --
 * into 'static const' storage in the generated model: the caller
 * must not free it, and it remains valid for the lifetime of the
 * loaded model.  They return NULL if 'model' is NULL or the index is
 * out of range; the counting and sizing functions return 0 if
 * 'model' is NULL.
 */

/* Number of state elements (Bluesim primitive instances) in the
 * design's whole module tree.
 */
tUInt32 bk_num_state_elements(tModel model);

/* Descriptor of the nth state element (0-based), in the documented
 * table order.
 */
const tBkStateInfo* bk_get_state_element(tModel model, tUInt32 n);

/* Total byte size of the state area the host must provide to
 * new_MODEL_*(): the module-object region followed by the element
 * sub-area (see bluesim_introspection.h).
 */
tUInt64 bk_state_bytes(tModel model);

/* Byte offset of the element sub-area within the state area: element
 * descriptor offsets are relative to (state + this offset).  Always a
 * multiple of 16.
 */
tUInt64 bk_state_elements_offset(tModel model);

/* Number of top-module input ports (module argument ports, method
 * enables and method arguments; clock and reset ports are driven
 * through the kernel and are not included).
 */
tUInt32 bk_num_input_ports(tModel model);

/* Descriptor of the nth input port (0-based), in the documented
 * table order.
 */
const tBkPortInfo* bk_get_input_port(tModel model, tUInt32 n);

/* Total byte size of the planned contiguous input area. */
tUInt64 bk_input_bytes(tModel model);

/* Number of top-module output ports (method results, ready results
 * included).
 */
tUInt32 bk_num_output_ports(tModel model);

/* Descriptor of the nth output port (0-based), in the documented
 * table order.
 */
const tBkPortInfo* bk_get_output_port(tModel model, tUInt32 n);

/* Total byte size of the planned contiguous output area. */
tUInt64 bk_output_bytes(tModel model);

/*
 * Kernel clock definition
 */

/* Define a 2-phase clock waveform to be generated by the
 * Bluesim kernel.
 *
 *   name              - the name associated with the clock domain
 *   initial_value     - the value of the clock before the first edge
 *   has_initial_value - whether the clock gets the initial value or X
 *   first_edge        - the delay until the first edge
 *   high_duration     - the duration the clock remains CLK_HIGH
 *   low_duration      - the duration the clock remains CLK_LOW
 *
 * Returns the handle for the newly generated clock.
 *
 * Note: the total clock period is (low_duration + high_duration),
 * and a 50% duty-cycle is obtained when low_duration = high_duration.
 *
 * Note: when the total period is 0, it indicates that the clock is
 * to be managed explicitly by calling bk_trigger_clock_edge().
 *
 * Event-queue depth cost: this call enqueues no events itself, but a
 * clock with a waveform holds up to 5 live events once its schedule
 * callbacks are registered (see bk_set_clock_event_fn()).  Clocks the
 * model registers are counted in bk_max_event_queue_depth(); a clock
 * the HOST defines is not, and costs up to 5 further events.
 *
 * The clock table is fixed-capacity storage in the caller-provided
 * context buffer (nothing is allocated): at most 64 clocks can be
 * defined, and 'name' is copied into the entry's embedded buffer,
 * truncated to 127 characters if longer.  Defining a 65th clock
 * fails with BAD_CLOCK_HANDLE.
 */
tClock bk_define_clock(tSimStateHdl simHdl,
		       const char* name,
		       tClockValue initial_value,
		       tBool       has_initial_value,
		       tTime       first_edge,
		       tTime       high_duration,
		       tTime       low_duration);

/* Allow a clock definition to be altered (overridden from the UI, etc.)
 *
 * Returns BK_ERROR on error, BK_SUCCESS on success.
 *
 * Event-queue depth cost: re-derives the clock's schedule events
 * (replacing any it had), leaving up to 5 live events for a clock
 * with a waveform: the two edge events, the two post-edge
 * combinational events and possibly a time-0 initial edge.  This is
 * within the per-clock allotment of bk_max_event_queue_depth() for
 * clocks the model registers; for a host-defined clock it is host
 * cost (see bk_define_clock()).
 */
tStatus bk_alter_clock(tSimStateHdl simHdl,
		       tClock      handle,
		       tClockValue initial_value,
		       tBool       has_initial_value,
		       tTime       first_edge,
		       tTime       high_duration,
		       tTime       low_duration);

/* Associate a callback function with an event type for a particular
 * clock.
 *
 *   handle               - the handle of the clock
 *   on_edge_callback     - the function to call when the edge event occurs
 *   after_edge_callback  - the function to call after the edge event
 *   dir                  - direction of the clock edge
 *
 * Returns BK_ERROR on error, BK_SUCCESS on success.
 *
 * Event-queue depth cost: like bk_alter_clock(), re-derives the
 * clock's schedule events -- up to 5 live per clock with a waveform,
 * counted in bk_max_event_queue_depth() for model-registered clocks.
 */
tStatus bk_set_clock_event_fn(tSimStateHdl simHdl,
			      tClock handle,
			      tScheduleFn on_edge_callback,
			      tScheduleFn after_edge_callback,
			      tEdgeDirection dir);

/* Trigger a clock edge at a given simulation time.
 * This function is for use with clocks that have no defined
 * waveform (ie. high_duration == low_duration == 0).
 *
 * Returns BK_ERROR on error, or the number of events scheduled
 * for the clock edge on success.
 *
 * Event-queue depth cost: 2 events per call (the edge event and its
 * post-edge combinational event), consumed within the timeslice they
 * are scheduled for.  Calls made by the clock primitives inside the
 * model are counted in bk_max_event_queue_depth(); a call made by
 * the HOST is not, and costs 2 further events until they execute.
 */
tStatus bk_trigger_clock_edge(tSimStateHdl simHdl,
			      tClock handle, tEdgeDirection dir, tTime at);

/* Enqueue an initial clock edge (at time 0).
 * This function is for use with clocks that have no defined
 * waveform (ie. high_duration == low_duration == 0).
 *
 * Returns BK_ERROR on error, or the number of events scheduled for the
 * clock edge on success.
 *
 * Event-queue depth cost: 1 event, live until time 0 executes.
 * Calls made by clock primitives inside the model are counted in
 * bk_max_event_queue_depth(); a HOST call is not, and costs 1
 * further event.
 */
tStatus bk_enqueue_initial_clock_edge(tSimStateHdl simHdl,
				      tClock handle, tEdgeDirection dir);

/* Get the clock handle associated with a clock domain name.
 *
 * Returns the clock handle for the domain, or BAD_CLOCK_HANDLE
 * if there is no clock domain with the given name.
 */
tClock bk_get_clock_by_name(tSimStateHdl simHdl, const char* name);

/* If there is already a clock domain with the given name,
 * return the handle for it.  If there is no clock domain with
 * this name yet, then create one and return the handle of the
 * new domain.  The domain characteristics can be set with
 * a subsequent call to bk_alter_clock().
 */
tClock bk_get_or_define_clock(tSimStateHdl simHdl, const char* name);

/* Get the number of clocks defined in the kernel */
tUInt32 bk_num_clocks(tSimStateHdl simHdl);

/* Get the clock handle for the nth clock.
 *
 * Returns the clock handle on success or BAD_CLOCK_HANDLE on error.
 */
tClock bk_get_nth_clock(tSimStateHdl simHdl, tUInt32 n);

/* Get various information for a clock */
const char* bk_clock_name(tSimStateHdl simHdl, tClock handle);
tClockValue bk_clock_initial_value(tSimStateHdl simHdl, tClock handle);
tTime bk_clock_first_edge(tSimStateHdl simHdl, tClock handle);
tTime bk_clock_duration(tSimStateHdl simHdl, tClock handle, tClockValue value);
tClockValue bk_clock_val(tSimStateHdl simHdl, tClock handle);
tUInt64 bk_clock_cycle_count(tSimStateHdl simHdl, tClock handle);
tUInt64 bk_clock_edge_count(tSimStateHdl simHdl,
			    tClock handle, tEdgeDirection dir);

/*
 * Setup a default reset waveform (asserted at time 0, deasserted at time 2).
 * This should be called before the first bk_sync_run() call.
 *
 * Event-queue depth cost: 2 events (the assert and deassert), live
 * until they execute at times 0 and 2.  The generated model calls
 * this from create_model() when it is the master, and the 2 events
 * are counted in bk_max_event_queue_depth(); a further HOST call
 * costs 2 more.
 */
void bk_use_default_reset(tSimStateHdl simHdl);

/*
 * Simulation control
 */

/* Get the current simulation time */
tTime bk_now(tSimStateHdl simHdl);

/* Set simulation timescale - reporting scale factor and time unit for $time.
 *
 * Returns BK_ERROR on error, BK_SUCCESS on success.
 *
 * Errors include passing an invalid timescale unit and setting the timescale
 * after the beginning of the simulation.
 */
tStatus bk_set_timescale(tSimStateHdl simHdl, const char* scale_unit, tTime scale_factor);

/* Test if a given simulation time is still ongoing.
 * WARNING: This is a specialized function for use by
 * Bluesim primitives to facilitate connections to
 * event-driven simulation.  FOR EXPERT USE ONLY!
 */
tBool bk_is_same_time(tSimStateHdl simHdl, tTime t);

/* Test if we are currently executing within a combinational
 * schedule.  FOR EXPERT USE ONLY!
 */
tBool bk_is_combo_sched(tSimStateHdl simHdl);

/* Get information on the clock event queue */
tTime bk_clock_last_edge(tSimStateHdl simHdl, tClock handle);
tTime bk_clock_combinational_time(tSimStateHdl simHdl, tClock handle);

/* Quit simulation at the end of the current time slice.
 *
 * Event-queue depth cost: 1 event per call, live until time t
 * executes.  This is a HOST call: it is NOT counted in
 * bk_max_event_queue_depth() and each call must be budgeted on top
 * of that bound.
 */
void bk_quit_at(tSimStateHdl simHdl, tTime t);

/* Quit simulation at the end of the given time slice.
 *
 * Returns BK_ERROR on error and BK_SUCCESS on success.
 *
 * Event-queue depth cost: none at call time (it only sets a limit),
 * but when the limit is reached the kernel schedules the
 * deduplicated UI yield event for the current time -- the single
 * yield event that IS counted in bk_max_event_queue_depth() (see
 * bk_schedule_ui_event()).
 */
tStatus bk_quit_after_edge(tSimStateHdl simHdl,
			   tClock handle, tEdgeDirection dir, tUInt64 cycle);

/* Test if simulation events are currently being executed.
 *
 * Returns 0 if the simulation is not running and non-zero if
 * it is running.
 */
tBool bk_is_running(tSimStateHdl simHdl);

/* Execute simulation events on the caller's thread.
 *
 * Returns when the event queue drains or at the end of a stopping
 * timeslice: $stop/$finish/$fatal, bk_abort_now(), an edge limit
 * set with bk_quit_after_edge(), or a UI event scheduled with
 * bk_schedule_ui_event() (which is how running to a target time is
 * composed).  The cause can be distinguished using bk_stopped(),
 * bk_finished(), bk_fataled(), bk_aborted() and bk_sync_pending().
 * Calling it again resumes the simulation.  Note that the kernel
 * installs no signal handlers, so Ctrl-C is not converted into
 * bk_abort_now() unless the embedder arranges it.
 *
 * Returns BK_ERROR on error (including a call while the simulation
 * is running) and BK_SUCCESS on success.
 */
tStatus bk_sync_run(tSimStateHdl simHdl);

/* Like bk_sync_run(), but runs at most one cycle of the given
 * clock, with the same semantics as bluetcl's 'sim step': the
 * simulation runs until one more edge of the clock in the direction
 * which returns it to its current value has executed (one full
 * clock cycle), except before any logic has executed at time 0,
 * when it runs to the clock's first edge instead.
 *
 * As with bk_sync_run(), it returns earlier if another stopping
 * condition is encountered ($stop/$finish/$fatal, bk_abort_now(),
 * an edge limit on another clock or edge direction, a scheduled UI
 * event) or if the event queue drains; the cause can be
 * distinguished with the same predicates.  The edge limit for the
 * stepped clock and direction is saved and restored around the
 * step, so no one-cycle limit is left behind to stop a later
 * bk_sync_run() early, and a pending bk_quit_after_edge() limit
 * survives (limits on other clocks are never modified).
 *
 * Returns BK_ERROR on error (including an invalid clock, a call
 * while the simulation is running, or a call after $finish) and
 * BK_SUCCESS on success.
 */
tStatus bk_sync_step(tSimStateHdl simHdl, tClock clk);

/* Test whether any events remain in the simulation queue.
 *
 * Returns 0 if the queue is empty and non-zero otherwise.
 */
tBool bk_sync_pending(tSimStateHdl simHdl);

/* Control whether bk_sync_run() and bk_sync_step() flush open file
 * buffers each time they return control to the caller, by calling
 * the host ops flush entry with a NULL stream (the equivalent of
 * fflush(NULL) in the default host implementation).
 *
 * The default is enabled.  Embedders whose host ops do not buffer
 * can disable it to reduce per-step overhead; with flushing
 * disabled, pending $display output stays in the host's buffers
 * until the embedder flushes them itself.
 */
void bk_set_flush_on_pause(tSimStateHdl simHdl, tBool enabled);

/* Schedule a UI callback for the end of a given timeslice,
 * unless there is already one scheduled at that time.
 *
 * Returns BK_ERROR on error or BK_SUCCESS on success.
 *
 * Event-queue depth cost: 1 event per distinct target time (a repeat
 * for the same time is deduplicated).  bk_max_event_queue_depth()
 * includes exactly ONE yield event -- the one the model itself
 * schedules for the current time via $stop/$finish/$fatal or a
 * reached edge limit.  Each ADDITIONAL pending yield event at some
 * other time is a HOST cost on top of the bound.
 */
tStatus bk_schedule_ui_event(tSimStateHdl simHdl, tTime at);

/* Remove a UI callback previously scheduled at the end of a given timeslice.
 *
 * Returns BK_ERROR on error or BK_SUCCESS on success.
 */
tStatus bk_remove_ui_event(tSimStateHdl simHdl, tTime at);

/*
 * Routines to control debugging functionality.
 */

/* Event-queue depth cost of bk_enable_cycle_dumping(): one recurring
 * cycle-dump event per live schedule event (so up to 4 per clock with
 * a waveform, plus initial-edge dumps at time 0).  This is a HOST
 * call, NOT counted in bk_max_event_queue_depth(); budget up to 5
 * events per clock on top of the bound while dumping is enabled.
 */
void bk_enable_cycle_dumping(tSimStateHdl simHdl);
void bk_disable_cycle_dumping(tSimStateHdl simHdl);
tBool bk_is_cycle_dumping_enabled(tSimStateHdl simHdl);
void bk_dump_cycle_counts(tSimStateHdl simHdl,
			  const char* label, tClock handle);

/* Call to enable clock edges without logic (for interactive stepping)
 *
 * Event-queue depth cost: none beyond the per-clock allotment already
 * counted in bk_max_event_queue_depth() -- it re-derives each clock's
 * schedule events (keeping edges that have no logic), never exceeding
 * the 5 live events a clock with a waveform is budgeted for.
 */
void bk_set_interactive(tSimStateHdl simHdl);

/*
 * Callbacks to stop simulation within a schedule or model.
 *
 * Event-queue depth cost of bk_stop_now(), bk_finish_now() and
 * bk_fatal_now(): each schedules the deduplicated UI yield event for
 * the current time -- the single yield event already counted in
 * bk_max_event_queue_depth() (these are called by the model's $stop,
 * $finish and $fatal).
 */

/* Pause the simulation and return to the UI at the end of this
 * simulation cycle.  The status value is made available to
 * callers of bk_exit_status().
 */
void bk_stop_now(tSimStateHdl simHdl, tSInt32 status);

/* Abort the simulation and return to the UI at the end of this
 * simulation cycle.  The status value is made available to
 * callers of bk_exit_status().
 */
void bk_finish_now(tSimStateHdl simHdl, tSInt32 status);

/* Report a fatal error and end simulation. */
void bk_fatal_now(tSimStateHdl simHdl, tSInt32 status);

/* Test if $stop was called. */
tBool bk_stopped(tSimStateHdl simHdl);

/* Test if $finish was called. */
tBool bk_finished(tSimStateHdl simHdl);

/* Retrieve the status value of the last call to bk_stop_now()
 * or bk_finish_now().
 */
tSInt32 bk_exit_status(tSimStateHdl simHdl);

/* Test if $fatal was called. */
tBool bk_fataled(tSimStateHdl simHdl);


/*
 * Callbacks to stop simulation from outside a schedule or model.
 */

/* Abort the simulation and return to the UI at the end of the
 * current simulation cycle.
 */
void bk_abort_now(tSimStateHdl simHdl);

/* Test if bk_abort_now() was called. */
tBool bk_aborted(tSimStateHdl simHdl);


/*
 * Routines for setting and testing arguments (eg., plusargs).
 */

/* Add an argument string.  The string is copied into fixed-capacity
 * storage in the simulation context (nothing is allocated): at most
 * 64 arguments of at most 127 characters each are recorded, and an
 * argument beyond either limit is silently ignored.
 */
void bk_append_argument(tSimStateHdl simHdl, const char* arg);

/* Retrieve the trailing portion of the first matching argument */
const char* bk_match_argument(tSimStateHdl simHdl, const char* name);

/* Routine which provides direct access to the top-level model.  This
 * should only be used by callers that know exactly what they are doing.
 */
void* bk_get_model_instance(tSimStateHdl simHdl);

/*
 * API routines for finding and working with symbols
 */

/* Get the symbol for the top module. */
tSymbol bk_top_symbol(tSimStateHdl simHdl);

/* Lookup a symbol by name.  Returns BAD_SYMBOL if the named
 * symbol is not found.
 */
tSymbol bk_lookup_symbol(tSymbol root, const char* name);

/* Get the key for a symbol */
const char* bk_get_key(tSymbol sym);

/* Test if a symbol represents a module */
tBool bk_is_module(tSymbol sym);

/* Test if a symbol represents a rule */
tBool bk_is_rule(tSymbol sym);

/* Test if a symbol represents a value */
tBool bk_is_single_value(tSymbol sym);

/* Test if a symbol represents a range of values */
tBool bk_is_value_range(tSymbol sym);

/* Get the size for a symbol (for value and value range symbols) */
tUInt32 bk_get_size(tSymbol sym);

/* Get the value for a symbol (as a void*) */
void* bk_get_ptr(tSymbol sym);

/* Get a pointer to the value for a value symbol.
 * Returns NULL for other symbol types.
 */
const unsigned int* bk_peek_symbol_value(tSymbol sym);

/* Get the minimum address for a value range.
 * Returns NULL for other symbol types.
 */
tUInt64 bk_get_range_min_addr(tSymbol sym);

/* Get the maximum address for a value range.
 * Returns NULL for other symbol types.
 */
tUInt64 bk_get_range_max_addr(tSymbol sym);

/* Get a pointer to a value selected from a range.
 * Returns NULL for other symbol types, or if the address is out of bounds.
 */
const unsigned int* bk_peek_range_value(tSymbol sym, tUInt64 addr);

/* Get the number of sub-symbols of a module.
 * Returns 0 for other symbol types.
 */
tUInt32 bk_num_symbols(tSymbol sym);

/* Get the Nth sub-symbol of a module (starting at 0).
 * Returns BAD_SYMBOL for other symbol types.
 */
tSymbol bk_get_nth_symbol(tSymbol sym, tUInt32 n);

#if __cplusplus
} /* extern "C" */
#endif

#endif /* __BLUESIM_KERNEL_H__ */

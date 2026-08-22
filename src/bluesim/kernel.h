#ifndef __KERNEL_H__
#define __KERNEL_H__

#include "bluesim_kernel_api.h"
#include "bs_model.h"
#include "bs_symbol.h"
#include "event_queue.h"

/* The kernel keeps no growable containers: everything lives in the
 * caller-provided context buffer, in fixed-capacity storage embedded
 * in tSimState (bk_context_bytes() covers it automatically because
 * it is computed from sizeof(tSimState)).  The capacities below are
 * documented limits of the kernel:
 *
 *  - BK_MAX_CLOCKS bounds the number of clocks a simulation can
 *    define (the design's clock domains plus any host-defined
 *    clocks); bk_define_clock() returns BAD_CLOCK_HANDLE when full.
 *  - BK_CLOCK_NAME_MAX bounds a clock's name; longer names are
 *    truncated.
 *  - BK_MAX_LABELS bounds the pending hierarchy labels of the
 *    rule-firing dump; it comfortably exceeds any realistic module
 *    nesting depth, and on overflow the oldest pending label is
 *    dropped (only dump formatting is affected).
 *  - BK_MAX_PLUS_ARGS / BK_PLUS_ARG_MAX bound the number and length
 *    of recorded simulator plus-args; bk_append_argument() ignores
 *    arguments past those limits.
 */
#define BK_MAX_CLOCKS     64u
#define BK_CLOCK_NAME_MAX 128u
#define BK_MAX_LABELS     64u
#define BK_MAX_PLUS_ARGS  64u
#define BK_PLUS_ARG_MAX   128u

/* A tLabel provides the information for creating a label when
 * dumping rule firing information.
 */
typedef struct {
  unsigned int indent;
  const char*  text;
} tLabel;

/* The pending rule-firing labels, kept in a fixed-capacity ring
 * (used like a deque: pushed and popped at the back as the module
 * hierarchy is walked, drained from the front when a rule is
 * printed).
 */
typedef struct {
  tLabel       items[BK_MAX_LABELS];
  unsigned int head;    /* index of the front element */
  unsigned int count;   /* number of live elements */
} tLabelQueue;

/* A tClockInfo is a complete description a clock waveform
 * and the schedules which execute on its edges.
 */
typedef struct
{
  char name[BK_CLOCK_NAME_MAX];     /* clock name (truncated to fit) */
  tClockValue current_value;        /* current clock value */
  tClockValue initial_value;        /* initial clock value */
  bool has_initial_value;           /* whether the initial value is set */
  tTime initial_delay;              /* when is the first edge */
  tTime low_phase_length;           /* duration of low clock phase */
  tTime high_phase_length;          /* duration of high clock phase */
  tTime period;                     /* clock period (sum of low + high) */
  tTime negedge_at;                 /* time of last negedge */
  tTime posedge_at;                 /* time of last posedge */
  tTime combinational_at;           /* time of last combinational update */
  tScheduleFn on_posedge;           /* posedge schedule function */
  tScheduleFn after_posedge;        /* post-posedge schedule function */
  tScheduleFn on_negedge;           /* negedge schedule function */
  tScheduleFn after_negedge;        /* post-negedge schedule function */
  tUInt64 posedge_count;            /* count of number of posedges */
  tUInt64 negedge_count;            /* count of number of negedges */
  tUInt64 posedge_limit;            /* call UI on posedge count */
  tUInt64 negedge_limit;            /* call UI on negedge count */
} tClockInfo;

/*
 * Simulation kernel state
 */
struct tSimState {
  // handle to the design
  Model* model;

  // host operations through which all runtime I/O is performed,
  // and the host context passed to every operation (bk_sync_init)
  const struct bs_host_ops* host_ops;
  void* host_ctx;

  // current simulation time
  tTime sim_time;
  // scaling factor used for $time/$stime
  tTime sim_timescale;

  // a priority queue of locally-defined clock edges
  EventQueue* queue;

  // flag controlling whether the kernel flushes open file buffers
  // each time it returns control to the caller (bk_set_flush_on_pause)
  bool flush_on_pause;

  // flag set while simulation events are being executed
  volatile bool sim_running;

  // flag to record when executing a combinational logic schedule
  bool in_combo_schedule;

  // flags marking when $stop, $finish, or $fatal has been executed
  bool stop_called;
  bool finish_called;
  bool fatal_called;
  bool abort_called;
  tSInt32 exit_status;
  volatile bool force_halt;

  // flag that records current cycle dump setting
  bool call_dump_cycle_counts;

  // all clock definitions (fixed capacity, see BK_MAX_CLOCKS)
  tClockInfo clocks[BK_MAX_CLOCKS];
  unsigned int num_clocks;

  // a symbol for the top module
  tSym top_symbol;

  // the current dummy edge status
  unsigned int need_dummy_edges;

  // for managing event callbacks
  tTime target_yield_time;
  unsigned int data_to_match;

  // for dumping rule firings (fixed capacity, see BK_MAX_LABELS)
  tLabelQueue labels;
  unsigned int rule_name_indent;

  // simulator arguments (fixed capacity, see BK_MAX_PLUS_ARGS)
  char plus_args[BK_MAX_PLUS_ARGS][BK_PLUS_ARG_MAX];
  unsigned int num_plus_args;

  // Count the number of primitives that have requested reset ticks
  unsigned int reset_tick_requests;

  // Count the reset sources whose output reset is currently asserted
  // (see set_reset_output() in bs_reset.h), and the recorded level of
  // the kernel's own default reset waveform (one such source).
  unsigned int resets_asserted;
  bool default_reset_asserted;

};

typedef struct tSimState tSimState;

#endif /* __KERNEL_H__ */

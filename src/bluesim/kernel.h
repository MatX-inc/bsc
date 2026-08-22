#ifndef __KERNEL_H__
#define __KERNEL_H__

#include <deque>

#include "bluesim_kernel_api.h"
#include "bs_model.h"
#include "bs_symbol.h"
#include "event_queue.h"
#include "portability.h"


/* A tLabel provides the information for creating a label when
 * dumping rule firing information.
 */
typedef struct {
  unsigned int indent;
  const char*  text;
} tLabel;

/* A tClockInfo is a complete description a clock waveform
 * and the schedules which execute on its edges.
 */
typedef struct
{
  char* name;                       /* clock name */
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

  // current simulation time
  tTime sim_time;
  // scaling factor used for $time/$stime
  tTime sim_timescale;

  // a priority queue of locally-defined clock edges
  EventQueue* queue;

  // flag set when the kernel runs events on the caller's thread
  // (bk_sync_init) instead of on a separate simulation thread (bk_init)
  bool sync_mode;

  // flag controlling whether the sync path flushes open file buffers
  // each time it returns control to the caller (bk_set_flush_on_pause)
  bool flush_on_pause;

  // semaphores, etc. used for synchronization between API and sim_thread
  volatile bool sim_running;
  volatile bool sim_shutting_down;
  tSemaphore* start_semaphore; /* used to trigger simulation start */
  tSemaphore* stop_semaphore;  /* used to indicate simulation stop */
  pthread_mutex_t sim_mutex;   /* used to protect sim_running, etc. */
  pthread_t sim_thread_id;

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

  // an array of all clock definitions
  std::vector<tClockInfo> clocks;

  // a symbol for the top module
  tSym top_symbol;

  // the current dummy edge status
  unsigned int need_dummy_edges;

  // for managing event callbacks
  tTime target_yield_time;
  unsigned int data_to_match;

  // for dumping rule firings
  std::deque<tLabel> labels;
  unsigned int rule_name_indent;

  // simulator arguments
  std::vector<const char*> plus_args;

  // Count the number of primitives that have requested reset ticks
  unsigned int reset_tick_requests;

};

typedef struct tSimState tSimState;

#endif /* __KERNEL_H__ */

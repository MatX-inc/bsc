/* Host-side test harness for the fixed-capacity Bluesim event queue.
 *
 * The harness plays the role of an embedder: it dlopen()s a Bluesim
 * model shared object, installs the default host operations and
 * chooses the event-queue capacity at bk_sync_init(), exactly as
 * bluetcl and the generated SystemC wrappers do.
 *
 * Usage: host_eventqueue <model.so> <top-module> <mode>
 *
 *   mode "exact":    initialize with the event-queue capacity set
 *                    EXACTLY to the model's exposed maximum depth
 *                    (bk_max_event_queue_depth) -- no headroom -- and
 *                    run the simulation to completion.  The harness
 *                    makes no event-enqueuing host calls, so if the
 *                    codegen formula under-estimates the model's
 *                    needs, the queue overflows and the run aborts.
 *                    Also checks that a capacity of 0 is rejected and
 *                    that the high-water mark stayed within the
 *                    exposed bound.
 *
 *   mode "overflow": initialize with that same capacity, then (as a
 *                    misbehaving host) schedule bk_quit_at events at
 *                    distinct future times until the queue is
 *                    overfull.  The event_queue_overflow host
 *                    operation must fire: the default implementation
 *                    prints a message to stderr and abort()s, so the
 *                    harness must die with SIGABRT.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <dlfcn.h>

#include "bluesim_types.h"
#include "bluesim_host_ops.h"
#include "bluesim_host_ops_default.h"

typedef void* (*tNewModelFn)(const struct bs_host_ops*, void*,
                              void*, void*, void*);
typedef tUInt64 (*tBytesFn)(tModel);
typedef tUInt32      (*tMaxDepthFn)(tModel);
typedef tUInt64      (*tCtxBytesFn)(tUInt32);
typedef tSimStateHdl (*tSyncInitFn)(tModel, tBool,
                                    const struct bs_host_ops*, void*,
                                    tUInt32, void*);
typedef tStatus      (*tSyncRunFn)(tSimStateHdl);
typedef void         (*tQuitAtFn)(tSimStateHdl, tTime);
typedef tUInt32      (*tHighWaterFn)(tSimStateHdl);
typedef tBool        (*tFinishedFn)(tSimStateHdl);
typedef tSInt32      (*tExitStatusFn)(tSimStateHdl);
typedef void         (*tShutdownFn)(tSimStateHdl);

static void* find_sym(void* dl, const char* name)
{
  void* sym = dlsym(dl, name);
  if (sym == NULL)
  {
    fprintf(stderr, "harness: cannot find symbol '%s': %s\n",
            name, dlerror());
    exit(1);
  }
  return sym;
}

int main(int argc, char** argv)
{
  if (argc != 4)
  {
    fprintf(stderr,
            "usage: host_eventqueue <model.so> <top-module> exact|overflow\n");
    return 1;
  }
  const char* so_name  = argv[1];
  const char* top_name = argv[2];
  const char* mode     = argv[3];

  setvbuf(stdout, NULL, _IOLBF, 0);

  /* a bare filename would be looked up on the library search path;
   * anchor it to the current directory instead
   */
  char so_path[1024];
  if (strchr(so_name, '/') == NULL)
    snprintf(so_path, sizeof(so_path), "./%s", so_name);
  else
    snprintf(so_path, sizeof(so_path), "%s", so_name);

  void* dl = dlopen(so_path, RTLD_NOW);
  if (dl == NULL)
  {
    fprintf(stderr, "harness: cannot load '%s': %s\n", so_path, dlerror());
    return 1;
  }

  char new_model_name[256];
  snprintf(new_model_name, sizeof(new_model_name), "new_MODEL_%s", top_name);

  tNewModelFn   new_model   = (tNewModelFn)   find_sym(dl, new_model_name);
  tMaxDepthFn   max_depth   = (tMaxDepthFn)   find_sym(dl, "bk_max_event_queue_depth");
  tCtxBytesFn   ctx_bytes   = (tCtxBytesFn)   find_sym(dl, "bk_context_bytes");
  tSyncInitFn   sync_init   = (tSyncInitFn)   find_sym(dl, "bk_sync_init");
  tSyncRunFn    sync_run    = (tSyncRunFn)    find_sym(dl, "bk_sync_run");
  tQuitAtFn     quit_at     = (tQuitAtFn)     find_sym(dl, "bk_quit_at");
  tHighWaterFn  high_water  = (tHighWaterFn)  find_sym(dl, "bk_event_queue_high_water");
  tFinishedFn   finished    = (tFinishedFn)   find_sym(dl, "bk_finished");
  tExitStatusFn exit_status = (tExitStatusFn) find_sym(dl, "bk_exit_status");
  tShutdownFn   shutdown_fn = (tShutdownFn)   find_sym(dl, "bk_shutdown");

  tModel model = new_model(NULL, NULL, NULL, NULL, NULL);
  if (model == NULL)
  {
    fprintf(stderr, "harness: new_%s returned NULL\n", top_name);
    return 1;
  }

  tUInt32 max = max_depth(model);
  if (max == 0)
  {
    fprintf(stderr, "harness: bk_max_event_queue_depth returned 0\n");
    return 1;
  }
  printf("harness: model exposes a maximum event-queue depth\n");

  const struct bs_host_ops* ops = bs_default_host_ops();
  void* ctx = bs_default_host_ctx();

  /* the model's caller-provided storage (constructor ABI) */
  tBytesFn state_bytes = (tBytesFn) find_sym(dl, "bk_state_bytes");
  tBytesFn in_bytes    = (tBytesFn) find_sym(dl, "bk_input_bytes");
  tBytesFn out_bytes   = (tBytesFn) find_sym(dl, "bk_output_bytes");
  void* state_buf = malloc(state_bytes(model));
  void* in_buf  = (in_bytes(model) > 0)  ? malloc(in_bytes(model))  : NULL;
  void* out_buf = (out_bytes(model) > 0) ? malloc(out_bytes(model)) : NULL;
  model = new_model(ops, ctx, state_buf, in_buf, out_buf);

  void* ctx_buf = malloc(ctx_bytes(max));

  if (strcmp(mode, "exact") == 0)
  {
    /* a capacity of 0 must be rejected (and needs no context bytes) */
    if ((ctx_bytes(0) != 0) ||
        (sync_init(model, 1, ops, ctx, 0, ctx_buf) != NULL))
    {
      fprintf(stderr, "harness: capacity 0 was not rejected\n");
      return 1;
    }
    printf("harness: zero capacity rejected\n");

    /* a missing context buffer must be rejected */
    if (sync_init(model, 1, ops, ctx, max, NULL) != NULL)
    {
      fprintf(stderr, "harness: NULL context buffer was not rejected\n");
      return 1;
    }
    printf("harness: null context buffer rejected\n");

    /* run the whole simulation in a queue of exactly the exposed
     * bound: any under-estimate in the formula aborts the run
     */
    tSimStateHdl sim = sync_init(model, 1, ops, ctx, max, ctx_buf);
    if (sim == NULL)
    {
      fprintf(stderr, "harness: bk_sync_init failed\n");
      return 1;
    }
    printf("harness: running with capacity == exposed bound\n");
    sync_run(sim);
    if (!finished(sim))
    {
      fprintf(stderr, "harness: simulation did not run to $finish\n");
      return 1;
    }
    printf("harness: simulation finished with status %d\n",
           (int) exit_status(sim));
    printf("harness: high water within bound: %s\n",
           (high_water(sim) <= max) ? "yes" : "NO");
    shutdown_fn(sim);
    free(ctx_buf);
    return 0;
  }
  else if (strcmp(mode, "overflow") == 0)
  {
    tSimStateHdl sim = sync_init(model, 1, ops, ctx, max, ctx_buf);
    if (sim == NULL)
    {
      fprintf(stderr, "harness: bk_sync_init failed\n");
      return 1;
    }
    /* schedule host events at distinct future times, past the
     * capacity: the event_queue_overflow operation must fire (the
     * default implementation aborts) before this loop completes
     */
    printf("harness: scheduling past the capacity\n");
    fflush(stdout);
    for (tUInt32 i = 0; i <= max; ++i)
      quit_at(sim, 1000000llu + i);
    fprintf(stderr, "harness: overflow did not fire\n");
    return 1;
  }

  fprintf(stderr, "harness: unknown mode '%s'\n", mode);
  return 1;
}

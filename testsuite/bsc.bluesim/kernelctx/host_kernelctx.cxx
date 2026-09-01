/* Host-side harness for the kernel's caller-provided simulation
 * context.
 *
 * The kernel does not allocate its context: the embedder asks
 * bk_context_bytes() for the size implied by its chosen event-queue
 * capacity, provides a buffer of that size (aligned like
 * max_align_t), and bk_sync_init() constructs the simulation state
 * and the event queue inside it.  bk_shutdown() tears the context
 * down in place and frees nothing, so the same buffer can be reused
 * for another initialization.
 *
 * The harness checks that:
 *   - bk_context_bytes() rejects a zero capacity and grows with the
 *     capacity (the event storage lives in the buffer);
 *   - bk_sync_init() rejects a NULL and a misaligned buffer;
 *   - the returned handle points into the provided buffer;
 *   - a full init/run-to-$finish/shutdown cycle works in the buffer;
 *   - after bk_shutdown() the same buffer hosts a second, fresh
 *     initialization that produces identical results.
 *
 * Usage: host_kernelctx <model.so> <top-module>
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
typedef tTime        (*tNowFn)(tSimStateHdl);
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

static tNewModelFn   new_model;
static tMaxDepthFn   max_depth;
static tCtxBytesFn   ctx_bytes;
static tSyncInitFn   sync_init;
static tSyncRunFn    sync_run;
static tNowFn        now_fn;
static tFinishedFn   finished;
static tExitStatusFn exit_status;
static tShutdownFn   shutdown_fn;

/* the model's caller-provided storage, shared by every run */
static void* g_state_buf;
static void* g_in_buf;
static void* g_out_buf;

/* one full simulation inside 'ctx_buf': init, run to $finish, tear
 * down in place.  Returns 0 on success and fills in the final status
 * and time.
 */
static int run_once(void* ctx_buf, tUInt32 capacity,
                    tSInt32* status, tTime* end_time)
{
  tModel model = new_model(bs_default_host_ops(), bs_default_host_ctx(),
                           g_state_buf, g_in_buf, g_out_buf);
  if (model == NULL)
  {
    fprintf(stderr, "harness: new_MODEL returned NULL\n");
    return 1;
  }
  tSimStateHdl sim = sync_init(model, 1,
                               bs_default_host_ops(), bs_default_host_ctx(),
                               capacity, ctx_buf);
  if (sim == NULL)
  {
    fprintf(stderr, "harness: bk_sync_init failed\n");
    return 1;
  }
  if (((char*) sim < (char*) ctx_buf) ||
      ((char*) sim >= ((char*) ctx_buf) + ctx_bytes(capacity)))
  {
    fprintf(stderr, "harness: handle does not point into the buffer\n");
    return 1;
  }
  sync_run(sim);
  if (!finished(sim))
  {
    fprintf(stderr, "harness: simulation did not run to $finish\n");
    return 1;
  }
  *status = exit_status(sim);
  *end_time = now_fn(sim);
  shutdown_fn(sim);
  return 0;
}

int main(int argc, char** argv)
{
  if (argc != 3)
  {
    fprintf(stderr, "usage: host_kernelctx <model.so> <top-module>\n");
    return 1;
  }
  const char* so_name  = argv[1];
  const char* top_name = argv[2];

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

  new_model   = (tNewModelFn)   find_sym(dl, new_model_name);
  max_depth   = (tMaxDepthFn)   find_sym(dl, "bk_max_event_queue_depth");
  ctx_bytes   = (tCtxBytesFn)   find_sym(dl, "bk_context_bytes");
  sync_init   = (tSyncInitFn)   find_sym(dl, "bk_sync_init");
  sync_run    = (tSyncRunFn)    find_sym(dl, "bk_sync_run");
  now_fn      = (tNowFn)        find_sym(dl, "bk_now");
  finished    = (tFinishedFn)   find_sym(dl, "bk_finished");
  exit_status = (tExitStatusFn) find_sym(dl, "bk_exit_status");
  shutdown_fn = (tShutdownFn)   find_sym(dl, "bk_shutdown");

  tModel probe = new_model(NULL, NULL, NULL, NULL, NULL);
  if (probe == NULL)
  {
    fprintf(stderr, "harness: new_%s returned NULL\n", top_name);
    return 1;
  }
  tUInt32 capacity = max_depth(probe) + 16;

  /* the model's caller-provided storage (constructor ABI), reused
   * across every re-initialization below
   */
  tBytesFn state_bytes = (tBytesFn) find_sym(dl, "bk_state_bytes");
  tBytesFn in_bytes    = (tBytesFn) find_sym(dl, "bk_input_bytes");
  tBytesFn out_bytes   = (tBytesFn) find_sym(dl, "bk_output_bytes");
  g_state_buf = malloc(state_bytes(probe));
  g_in_buf  = (in_bytes(probe) > 0)  ? malloc(in_bytes(probe))  : NULL;
  g_out_buf = (out_bytes(probe) > 0) ? malloc(out_bytes(probe)) : NULL;

  /* the size accessor rejects the invalid capacity and accounts for
   * the event storage
   */
  printf("harness: zero capacity needs no context: %s\n",
         (ctx_bytes(0) == 0) ? "yes" : "NO");
  if (ctx_bytes(0) != 0)
    return 1;
  printf("harness: context size grows with the capacity: %s\n",
         (ctx_bytes(capacity + 100) > ctx_bytes(capacity)) ? "yes" : "NO");
  if (ctx_bytes(capacity + 100) <= ctx_bytes(capacity))
    return 1;

  tUInt64 bytes = ctx_bytes(capacity);
  fprintf(stderr, "harness: context is %llu bytes for capacity %u\n",
          (unsigned long long) bytes, (unsigned) capacity);

  /* allocate one extra byte so a deliberately misaligned pointer
   * still points at owned storage
   */
  char* ctx_buf = (char*) malloc(bytes + 1);
  if (ctx_buf == NULL)
  {
    fprintf(stderr, "harness: cannot allocate the context buffer\n");
    return 1;
  }

  /* a missing or misaligned buffer is rejected */
  const struct bs_host_ops* ops = bs_default_host_ops();
  void* hctx = bs_default_host_ctx();
  printf("harness: null context buffer rejected: %s\n",
         (sync_init(probe, 1, ops, hctx, capacity, NULL) == NULL)
             ? "yes" : "NO");
  printf("harness: misaligned context buffer rejected: %s\n",
         (sync_init(probe, 1, ops, hctx, capacity, ctx_buf + 1) == NULL)
             ? "yes" : "NO");

  /* first full run in the buffer (with a fresh model instance, since
   * the probe above was never initialized into a simulation)
   */
  tSInt32 status1, status2;
  tTime   time1, time2;
  if (run_once(ctx_buf, capacity, &status1, &time1) != 0)
    return 1;
  printf("harness: first run finished with status %d\n", (int) status1);

  /* bk_shutdown tore the context down in place; the buffer is ours
   * again.  Scribble over it to show the second initialization
   * depends on nothing left behind, then run again in the same
   * buffer.
   */
  memset(ctx_buf, 0xA5, (size_t) bytes);
  if (run_once(ctx_buf, capacity, &status2, &time2) != 0)
    return 1;
  printf("harness: second run in the same buffer finished with "
         "status %d\n", (int) status2);

  printf("harness: runs behave identically: %s\n",
         ((status1 == status2) && (time1 == time2)) ? "yes" : "NO");

  free(ctx_buf);

  return ((status1 == status2) && (time1 == time2)) ? 0 : 1;
}

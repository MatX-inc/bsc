/* Host-side harness demonstrating that steady-state evaluation of a
 * wide-heavy Bluesim design makes no allocator calls for wide
 * temporaries.
 *
 * Wide temporaries in generated code (function-local wide defs, wide
 * concatenation intermediates, nested wide primitive results, wide
 * method returns and wide method arguments) are backed by fixed stack
 * arrays or non-owning views of module members, so once the model is
 * constructed, running cycles must not call the Bluesim word
 * allocator (alloc_mem/free_mem) or the C allocator at all.
 *
 * The harness dlopen()s the model shared object and drives it one
 * clock cycle at a time with bk_sync_step().  It observes
 *
 *   - the Bluesim allocator, through the bs_mem_alloc_counters()
 *     test-support accessor of the kernel library, and
 *
 *   - malloc/free/calloc/realloc, by interposing counting wrappers
 *     from this executable: the dynamic linker resolves the model
 *     shared object's allocator references (including those made
 *     inside libstdc++'s operator new) to the definitions here.
 *
 * After a warm-up period it runs several fixed-length segments and
 * requires every counter delta to be exactly zero, then lets the
 * design run to its own $finish.
 *
 * Usage: host_stacktemps <model.so> <top-module>
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <dlfcn.h>

#include "bluesim_types.h"
#include "bluesim_host_ops.h"
#include "bluesim_host_ops_default.h"

/* ---- C allocator interposition ---- */

extern "C" void* __libc_malloc(size_t size);
extern "C" void  __libc_free(void* ptr);
extern "C" void* __libc_calloc(size_t nmemb, size_t size);
extern "C" void* __libc_realloc(void* ptr, size_t size);

static volatile unsigned long long g_c_alloc_calls = 0;
static volatile unsigned long long g_c_free_calls = 0;

extern "C" void* malloc(size_t size)
{
  ++g_c_alloc_calls;
  return __libc_malloc(size);
}

extern "C" void free(void* ptr)
{
  ++g_c_free_calls;
  __libc_free(ptr);
}

extern "C" void* calloc(size_t nmemb, size_t size)
{
  ++g_c_alloc_calls;
  return __libc_calloc(nmemb, size);
}

extern "C" void* realloc(void* ptr, size_t size)
{
  ++g_c_alloc_calls;
  return __libc_realloc(ptr, size);
}

/* ---- kernel entry points ---- */

typedef void* (*tNewModelFn)(const struct bs_host_ops*, void*,
                              void*, void*, void*);
typedef tUInt64 (*tBytesFn)(tModel);
typedef tUInt32      (*tMaxDepthFn)(tModel);
typedef tUInt64      (*tCtxBytesFn)(tUInt32);
typedef tSimStateHdl (*tSyncInitFn)(tModel, tBool,
                                    const struct bs_host_ops*, void*,
                                    tUInt32, void*);
typedef tStatus      (*tSyncRunFn)(tSimStateHdl);
typedef tStatus      (*tSyncStepFn)(tSimStateHdl, tClock);
typedef tClock       (*tGetClockFn)(tSimStateHdl, const char*);
typedef tTime        (*tNowFn)(tSimStateHdl);
typedef tBool        (*tFinishedFn)(tSimStateHdl);
typedef tSInt32      (*tExitStatusFn)(tSimStateHdl);
typedef void         (*tShutdownFn)(tSimStateHdl);
typedef void         (*tMemCountersFn)(unsigned long long*,
                                       unsigned long long*);

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

struct tCounts
{
  unsigned long long mem_allocs;   /* alloc_mem() calls  */
  unsigned long long mem_frees;    /* free_mem() calls   */
  unsigned long long c_allocs;     /* malloc/calloc/realloc calls */
  unsigned long long c_frees;      /* free calls         */
};

static tMemCountersFn g_mem_counters = NULL;

static void sample(tCounts* c)
{
  g_mem_counters(&c->mem_allocs, &c->mem_frees);
  c->c_allocs = g_c_alloc_calls;
  c->c_frees = g_c_free_calls;
}

int main(int argc, char** argv)
{
  if (argc != 3)
  {
    fprintf(stderr, "usage: host_stacktemps <model.so> <top-module>\n");
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

  tNewModelFn   new_model   = (tNewModelFn)   find_sym(dl, new_model_name);
  tMaxDepthFn   max_depth   = (tMaxDepthFn)   find_sym(dl, "bk_max_event_queue_depth");
  tCtxBytesFn   ctx_bytes   = (tCtxBytesFn)   find_sym(dl, "bk_context_bytes");
  tSyncInitFn   sync_init   = (tSyncInitFn)   find_sym(dl, "bk_sync_init");
  tSyncRunFn    sync_run    = (tSyncRunFn)    find_sym(dl, "bk_sync_run");
  tSyncStepFn   sync_step   = (tSyncStepFn)   find_sym(dl, "bk_sync_step");
  tGetClockFn   get_clock   = (tGetClockFn)   find_sym(dl, "bk_get_clock_by_name");
  tNowFn        now_fn      = (tNowFn)        find_sym(dl, "bk_now");
  tFinishedFn   finished    = (tFinishedFn)   find_sym(dl, "bk_finished");
  tExitStatusFn exit_status = (tExitStatusFn) find_sym(dl, "bk_exit_status");
  tShutdownFn   shutdown_fn = (tShutdownFn)   find_sym(dl, "bk_shutdown");
  g_mem_counters = (tMemCountersFn) find_sym(dl, "bs_mem_alloc_counters");

  tCounts before_init;
  sample(&before_init);

  tModel model = new_model(NULL, NULL, NULL, NULL, NULL);
  if (model == NULL)
  {
    fprintf(stderr, "harness: new_%s returned NULL\n", top_name);
    return 1;
  }

  /* the model's caller-provided storage (constructor ABI); taken
   * straight from __libc_malloc so the harness's own allocations
   * stay out of the counters
   */
  tBytesFn state_bytes = (tBytesFn) find_sym(dl, "bk_state_bytes");
  tBytesFn in_bytes    = (tBytesFn) find_sym(dl, "bk_input_bytes");
  tBytesFn out_bytes   = (tBytesFn) find_sym(dl, "bk_output_bytes");
  void* state_buf = __libc_malloc(state_bytes(model));
  void* in_buf  = (in_bytes(model) > 0)
                      ? __libc_malloc(in_bytes(model))  : NULL;
  void* out_buf = (out_bytes(model) > 0)
                      ? __libc_malloc(out_bytes(model)) : NULL;
  model = new_model(bs_default_host_ops(), bs_default_host_ctx(),
                    state_buf, in_buf, out_buf);

  tUInt32 capacity = max_depth(model) + 16;
  void* ctx_buf = __libc_malloc(ctx_bytes(capacity));
  tSimStateHdl sim = sync_init(model, 1,
                               bs_default_host_ops(), bs_default_host_ctx(),
                               capacity, ctx_buf);
  if (sim == NULL)
  {
    fprintf(stderr, "harness: bk_sync_init failed\n");
    return 1;
  }

  tCounts after_init;
  sample(&after_init);
  /* the model constructs into caller-provided storage and the
   * kernel's bookkeeping is fixed storage in the caller-provided
   * context, so the whole of construction and initialization must
   * touch no allocator at all
   */
  printf("harness: construction and init are allocation-free: %s\n",
         ((after_init.c_allocs == before_init.c_allocs) &&
          (after_init.c_frees == before_init.c_frees) &&
          (after_init.mem_allocs == before_init.mem_allocs) &&
          (after_init.mem_frees == before_init.mem_frees)) ? "yes" : "NO");

  tClock clk = get_clock(sim, "CLK");

  /* warm up: run past reset and into steady state */
  for (unsigned i = 0; i < 200; ++i)
  {
    if (sync_step(sim, clk) != 0 /* BK_SUCCESS */ || finished(sim))
    {
      fprintf(stderr, "harness: warm-up ended early\n");
      return 1;
    }
  }
  printf("harness: warmed up (200 cycles, now at time %llu)\n",
         (unsigned long long) now_fn(sim));

  /* measured segments: every allocator counter must stay flat */
  unsigned bad_segments = 0;
  tCounts prev;
  sample(&prev);
  for (unsigned seg = 0; seg < 5; ++seg)
  {
    for (unsigned i = 0; i < 100; ++i)
    {
      if (sync_step(sim, clk) != 0 /* BK_SUCCESS */ || finished(sim))
      {
        fprintf(stderr, "harness: segment %u ended early\n", seg);
        return 1;
      }
    }
    tCounts cur;
    sample(&cur);   /* sample before any printing */
    unsigned long long d_ma = cur.mem_allocs - prev.mem_allocs;
    unsigned long long d_mf = cur.mem_frees - prev.mem_frees;
    unsigned long long d_ca = cur.c_allocs - prev.c_allocs;
    unsigned long long d_cf = cur.c_frees - prev.c_frees;
    printf("harness: segment %u (100 cycles): "
           "alloc_mem +%llu, free_mem +%llu, malloc +%llu, free +%llu\n",
           seg, d_ma, d_mf, d_ca, d_cf);
    if (d_ma != 0 || d_mf != 0 || d_ca != 0 || d_cf != 0)
      ++bad_segments;
    prev = cur;
  }
  printf("harness: steady state is allocation-free: %s\n",
         (bad_segments == 0) ? "yes" : "NO");

  /* let the design run to its own $finish */
  sync_run(sim);
  if (!finished(sim))
  {
    fprintf(stderr, "harness: simulation did not run to $finish\n");
    return 1;
  }
  printf("harness: simulation finished with status %d\n",
         (int) exit_status(sim));
  shutdown_fn(sim);
  __libc_free(ctx_buf);

  return (bad_segments == 0) ? 0 : 1;
}

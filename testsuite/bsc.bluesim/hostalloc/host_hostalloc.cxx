/* Host-side harness proving that a Bluesim model makes ZERO allocator
 * calls end to end: through the new_MODEL sizing call, the (host-side)
 * buffer allocation, construction in the caller-provided buffers,
 * bk_sync_init(), hundreds of busy cycles (including $display traffic
 * with strings, wide values and reals, RegFile and BRAM traffic and a
 * $swrite), the run to the design's own $finish, and bk_shutdown().
 *
 * Two observations cover every allocator:
 *
 *   - the Bluesim word allocator, through the bs_mem_alloc_counters()
 *     test-support accessor: its counters must still read 0/0 after
 *     shutdown (not even arena-served calls are allowed);
 *
 *   - malloc/calloc/realloc/free and operator new/new[], interposed
 *     here with wrappers that record their callers' return addresses
 *     while the watch window is open.  The harness itself prints
 *     (stdio may allocate internally) and the host side of the host
 *     operations is allowed to allocate -- those callers live in
 *     libc -- so a recorded caller only counts as a violation when it
 *     lies OUTSIDE ld.so and libc: with this harness the only such
 *     code that runs is the model shared object itself (directly or
 *     through libstdc++ helpers).  The harness's own buffers are
 *     taken straight from __libc_malloc, which bypasses the counting
 *     wrappers.
 *
 * The window also hosts the state write-through validation in both
 * directions: the harness writes the design's 'poke' register through
 * the state buffer at its published offset and the simulation
 * observes it (a rule copies it into 'mirror' and the design displays
 * both), and the harness reads 'cnt', 'mirror' and the wide register
 * back at their published offsets and checks/prints the simulated
 * values.
 *
 * Usage: host_hostalloc <model.so> <top-module>
 */

#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <dlfcn.h>

#include "bluesim_types.h"
#include "bluesim_host_ops.h"
#include "bluesim_host_ops_default.h"
#include "bluesim_introspection.h"

/* ---- interposition and attribution machinery ---- */

extern "C" void* __libc_malloc(size_t size);
extern "C" void  __libc_free(void* ptr);
extern "C" void* __libc_calloc(size_t nmemb, size_t size);
extern "C" void* __libc_realloc(void* ptr, size_t size);

/* while non-zero, allocator callers are recorded */
static volatile int g_watch = 0;

#define MAX_RECORDED 65536
static void*         g_alloc_callers[MAX_RECORDED];
static unsigned int  g_alloc_count = 0;

static void record_alloc_caller(void* ra)
{
  if (g_watch && (g_alloc_count < MAX_RECORDED))
    g_alloc_callers[g_alloc_count++] = ra;
}

extern "C" void* malloc(size_t size)
{
  record_alloc_caller(__builtin_return_address(0));
  return __libc_malloc(size);
}

extern "C" void free(void* ptr)
{
  __libc_free(ptr);
}

extern "C" void* calloc(size_t nmemb, size_t size)
{
  record_alloc_caller(__builtin_return_address(0));
  return __libc_calloc(nmemb, size);
}

extern "C" void* realloc(void* ptr, size_t size)
{
  record_alloc_caller(__builtin_return_address(0));
  return __libc_realloc(ptr, size);
}

void* operator new(size_t size)
{
  record_alloc_caller(__builtin_return_address(0));
  void* p = __libc_malloc(size);
  if (p == NULL)
  {
    fprintf(stderr, "harness: operator new failed\n");
    abort();
  }
  return p;
}

void* operator new[](size_t size)
{
  record_alloc_caller(__builtin_return_address(0));
  void* p = __libc_malloc(size);
  if (p == NULL)
  {
    fprintf(stderr, "harness: operator new[] failed\n");
    abort();
  }
  return p;
}

void operator delete(void* p) throw() { __libc_free(p); }
void operator delete[](void* p) throw() { __libc_free(p); }
void operator delete(void* p, size_t) throw() { __libc_free(p); }
void operator delete[](void* p, size_t) throw() { __libc_free(p); }

/* Count the recorded allocation callers that are NOT inside ld.so or
 * libc (see the file comment for why those are the excused origins).
 * Each stray caller is reported to stderr with the object dladdr
 * attributes it to.
 */
static unsigned int count_strays(void** addrs, unsigned int n)
{
  unsigned int strays = 0;
  for (unsigned int i = 0; i < n; ++i)
  {
    const char* fname = "(unknown)";
    Dl_info info;
    if (dladdr(addrs[i], &info) != 0 && info.dli_fname != NULL)
    {
      const char* base = strrchr(info.dli_fname, '/');
      fname = (base == NULL) ? info.dli_fname : base + 1;
      if ((strncmp(fname, "ld-", 3) == 0) ||
          (strncmp(fname, "libc.", 5) == 0) ||
          (strncmp(fname, "libc-", 5) == 0))
        continue;
    }
    ++strays;
    fprintf(stderr, "harness: stray allocation from %p (%s)\n",
            addrs[i], fname);
  }
  return strays;
}

/* ---- kernel entry points ---- */

typedef void*               (*tNewModelFn)(const struct bs_host_ops*, void*,
                                           void*, void*, void*);
typedef tUInt64             (*tBytesFn)(tModel);
typedef tUInt32             (*tMaxDepthFn)(tModel);
typedef tUInt64             (*tCtxBytesFn)(tUInt32);
typedef tSimStateHdl        (*tSyncInitFn)(tModel, tBool,
                                           const struct bs_host_ops*, void*,
                                           tUInt32, void*);
typedef tStatus             (*tSyncRunFn)(tSimStateHdl);
typedef tStatus             (*tSyncStepFn)(tSimStateHdl, tClock);
typedef tClock              (*tGetClockFn)(tSimStateHdl, const char*);
typedef tBool               (*tFinishedFn)(tSimStateHdl);
typedef tSInt32             (*tExitStatusFn)(tSimStateHdl);
typedef void                (*tShutdownFn)(tSimStateHdl);
typedef void                (*tMemCountersFn)(unsigned long long*,
                                              unsigned long long*);
typedef tUInt32             (*tCountFn)(tModel);
typedef const tBkStateInfo* (*tGetStateFn)(tModel, tUInt32);

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

static unsigned failures = 0;

static void check(int ok, const char* what)
{
  if (!ok)
  {
    fprintf(stderr, "harness: CHECK FAILED: %s\n", what);
    ++failures;
  }
}

/* ---- state element access through the state buffer ---- */

static tModel         g_model;
static tCountFn       g_num_state;
static tGetStateFn    g_get_state;
static unsigned char* g_elem_base;   /* start of the element sub-area */

static const tBkStateInfo* find_state(const char* name)
{
  for (tUInt32 i = 0; i < g_num_state(g_model); ++i)
  {
    const tBkStateInfo* e = g_get_state(g_model, i);
    if ((e != NULL) && (strcmp(e->name, name) == 0))
      return e;
  }
  fprintf(stderr, "harness: no state element named '%s'\n", name);
  exit(1);
}

/* read a state element of up to 64 bits at its published offset */
static tUInt64 read_elem(const char* name)
{
  const tBkStateInfo* e = find_state(name);
  unsigned char* base = g_elem_base + e->offset;
  if (e->bits <= 8)  return *(tUInt8*)  base;
  if (e->bits <= 32) return *(tUInt32*) base;
  return *(tUInt64*) base;
}

/* write a state element of up to 64 bits at its published offset */
static void write_elem(const char* name, tUInt64 value)
{
  const tBkStateInfo* e = find_state(name);
  unsigned char* base = g_elem_base + e->offset;
  if (e->bits <= 8)       *(tUInt8*)  base = (tUInt8)  value;
  else if (e->bits <= 32) *(tUInt32*) base = (tUInt32) value;
  else                    *(tUInt64*) base = (tUInt64) value;
}

int main(int argc, char** argv)
{
  if (argc != 3)
  {
    fprintf(stderr, "usage: host_hostalloc <model.so> <top-module>\n");
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

  tNewModelFn    new_model    = (tNewModelFn)    find_sym(dl, new_model_name);
  tBytesFn       state_bytes  = (tBytesFn)       find_sym(dl, "bk_state_bytes");
  tBytesFn       in_bytes     = (tBytesFn)       find_sym(dl, "bk_input_bytes");
  tBytesFn       out_bytes    = (tBytesFn)       find_sym(dl, "bk_output_bytes");
  tBytesFn       elems_offset = (tBytesFn)       find_sym(dl, "bk_state_elements_offset");
  tMaxDepthFn    max_depth    = (tMaxDepthFn)    find_sym(dl, "bk_max_event_queue_depth");
  tCtxBytesFn    ctx_bytes    = (tCtxBytesFn)    find_sym(dl, "bk_context_bytes");
  tSyncInitFn    sync_init    = (tSyncInitFn)    find_sym(dl, "bk_sync_init");
  tSyncRunFn     sync_run     = (tSyncRunFn)     find_sym(dl, "bk_sync_run");
  tSyncStepFn    sync_step    = (tSyncStepFn)    find_sym(dl, "bk_sync_step");
  tGetClockFn    get_clock    = (tGetClockFn)    find_sym(dl, "bk_get_clock_by_name");
  tFinishedFn    finished     = (tFinishedFn)    find_sym(dl, "bk_finished");
  tExitStatusFn  exit_status  = (tExitStatusFn)  find_sym(dl, "bk_exit_status");
  tShutdownFn    shutdown_fn  = (tShutdownFn)    find_sym(dl, "bk_shutdown");
  tMemCountersFn mem_counters = (tMemCountersFn) find_sym(dl, "bs_mem_alloc_counters");
  g_num_state = (tCountFn)    find_sym(dl, "bk_num_state_elements");
  g_get_state = (tGetStateFn) find_sym(dl, "bk_get_state_element");

  /* prime lazy libc state (stdio buffers, float formatting) before
   * opening the watch window, so the window sees only what the model
   * itself causes
   */
  printf("harness: watching the whole model lifecycle\n");
  fprintf(stderr, "harness: primed %e\n", 0.25);

  /* ================= the watched window opens ================= */
  g_watch = 1;

  /* sizing call, then bind the real storage (taken from
   * __libc_malloc: host-side allocation is the host's business and
   * must not be attributed to the model)
   */
  tModel model = new_model(NULL, NULL, NULL, NULL, NULL);
  if (model == NULL)
  {
    fprintf(stderr, "harness: %s returned NULL\n", new_model_name);
    return 1;
  }
  g_model = model;

  void* state_buf = __libc_malloc(state_bytes(model));
  void* in_buf  = (in_bytes(model) > 0)
                      ? __libc_malloc(in_bytes(model))  : NULL;
  void* out_buf = (out_bytes(model) > 0)
                      ? __libc_malloc(out_bytes(model)) : NULL;
  model = new_model(bs_default_host_ops(), bs_default_host_ctx(),
                    state_buf, in_buf, out_buf);
  g_elem_base = (unsigned char*) state_buf + elems_offset(model);

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

  unsigned long long ma, mf;
  mem_counters(&ma, &mf);
  printf("harness: bluesim allocator after construction and init: "
         "%llu allocs, %llu frees\n", ma, mf);
  check((ma == 0) && (mf == 0), "no bluesim allocator calls at init");

  tClock clk = get_clock(sim, "CLK");

  /* busy cycles: $display with strings, wide values and reals,
   * RegFile/BRAM/FIFO traffic, $swrite -- all inside the window
   */
  for (unsigned i = 0; i < 100; ++i)
  {
    if (sync_step(sim, clk) != 0 /* BK_SUCCESS */ || finished(sim))
    {
      fprintf(stderr, "harness: busy cycles ended early\n");
      return 1;
    }
  }

  /* ---- state write-through, simulation -> host: values the rules
   * computed are read back at the published offsets
   */
  /* the first step's edge (time 0) lands during reset, so the
   * counting rule has fired on 99 of the 100 edges
   */
  tUInt64 cnt_val = read_elem("top.cnt");
  printf("harness: top.cnt element reads %llu after 100 cycles\n",
         (unsigned long long) cnt_val);
  check(cnt_val == 99, "cnt element shows the simulated value");

  const tBkStateInfo* wide_e = find_state("top.wide");
  check(wide_e->bits == 96, "top.wide is 96 bits");
  {
    tUInt32* w = (tUInt32*) (g_elem_base + wide_e->offset);
    printf("harness: top.wide element reads %08x%08x%08x\n",
           (unsigned) w[2], (unsigned) w[1], (unsigned) w[0]);
  }

  /* ---- state write-through, host -> simulation: poke is written by
   * no rule; write it through the state buffer, let one edge run,
   * and observe it both in the mirror register a rule copies it
   * into, and in the design's own $display output at the next report
   */
  write_elem("top.poke", 0xdeadbeefull);
  if (sync_step(sim, clk) != 0 || finished(sim))
  {
    fprintf(stderr, "harness: post-poke step failed\n");
    return 1;
  }
  tUInt64 mirror_val = read_elem("top.mirror");
  printf("harness: top.mirror element reads %08llx after the poke\n",
         (unsigned long long) mirror_val);
  check(mirror_val == 0xdeadbeefull, "simulation observed the poked value");

  /* run to the design's own $finish (its reports along the way show
   * poke=deadbeef), then tear down -- still inside the window
   */
  sync_run(sim);
  if (!finished(sim))
  {
    fprintf(stderr, "harness: simulation did not run to $finish\n");
    return 1;
  }
  printf("harness: simulation finished with status %d\n",
         (int) exit_status(sim));
  shutdown_fn(sim);

  mem_counters(&ma, &mf);

  g_watch = 0;
  /* ================= the watched window is closed ================= */

  printf("harness: bluesim allocator after shutdown: "
         "%llu allocs, %llu frees\n", ma, mf);
  check((ma == 0) && (mf == 0), "no bluesim allocator calls end to end");

  fprintf(stderr, "harness: %u allocator calls recorded in the window "
          "(all origins)\n", g_alloc_count);
  unsigned int strays = count_strays(g_alloc_callers, g_alloc_count);
  printf("harness: allocator calls beyond the host's own: %u\n", strays);
  check(strays == 0, "no allocator calls from the model");

  printf("harness: model lifecycle is allocation-free: %s\n",
         (failures == 0) ? "yes" : "NO");

  __libc_free(ctx_buf);
  __libc_free(state_buf);
  if (in_buf)  __libc_free(in_buf);
  if (out_buf) __libc_free(out_buf);

  return (failures == 0) ? 0 : 1;
}

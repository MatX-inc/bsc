/* Host-side harness demonstrating that LOADING a Bluesim model
 * performs no allocator calls.
 *
 * The generated code's wide and string literals are plain
 * constant-initialized arrays, and the runtime library's static
 * tables are constant-initialized POD, so dlopen() of a model shared
 * object must:
 *
 *   - make no calls to the Bluesim allocator: the model's
 *     bs_mem_alloc_counters() must read 0/0 right after dlopen();
 *
 *   - register no static destructors: __cxa_atexit is interposed
 *     here, and no registration at all may happen while dlopen()
 *     runs -- one would mean a C++ object with a destructor is
 *     statically initialized in the model (the destructor itself may
 *     live in libstdc++, as std::string's does, so registrations are
 *     forbidden outright rather than attributed);
 *
 *   - perform no C allocator or operator new calls on behalf of
 *     model initialization: malloc/calloc/realloc and operator
 *     new/new[] are interposed here and record their callers' return
 *     addresses while dlopen() runs.  The dynamic linker itself
 *     allocates while loading any shared object, so callers inside
 *     ld.so and libc are expected; the only code beyond those that
 *     can run during dlopen() is the model's own initialization
 *     (directly, or through libstdc++ helpers), so every caller
 *     outside ld.so/libc counts as a violation.
 *
 * The raw counts are reported to stderr for inspection.
 *
 * After the load-time checks the model is initialized and run to its
 * own $finish, to show that a model loaded this way still works.
 *
 * Usage: host_loadalloc <model.so> <top-module>
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

/* ---- interposition and attribution machinery ---- */

extern "C" void* __libc_malloc(size_t size);
extern "C" void  __libc_free(void* ptr);
extern "C" void* __libc_calloc(size_t nmemb, size_t size);
extern "C" void* __libc_realloc(void* ptr, size_t size);

/* while non-zero, allocator callers and atexit registrations are recorded */
static volatile int g_watch = 0;

#define MAX_RECORDED 65536
static void*         g_alloc_callers[MAX_RECORDED];
static unsigned int  g_alloc_count = 0;
static unsigned int  g_atexit_count = 0;

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

/* interposed __cxa_atexit: counts registrations made while the watch
 * window is open, then forwards to the real registration
 */
typedef int (*tCxaAtexitFn)(void (*)(void*), void*, void*);

extern "C" int __cxa_atexit(void (*fn)(void*), void* arg, void* dso)
{
  static tCxaAtexitFn real_cxa_atexit = NULL;
  if (g_watch)
  {
    ++g_atexit_count;
    fprintf(stderr, "harness: load-time static destructor %p (dso %p)\n",
            (void*) fn, dso);
  }
  if (real_cxa_atexit == NULL)
    real_cxa_atexit = (tCxaAtexitFn) dlsym(RTLD_NEXT, "__cxa_atexit");
  return real_cxa_atexit(fn, arg, dso);
}

/* Count the recorded allocation callers that are NOT the dynamic
 * linker's own bookkeeping (ld.so or libc).  Each stray caller is
 * reported to stderr with the object dladdr attributes it to.
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
    fprintf(stderr, "harness: stray load-time allocation from %p (%s)\n",
            addrs[i], fname);
  }
  return strays;
}

/* ---- kernel entry points ---- */

typedef void*        (*tNewModelFn)(void);
typedef tUInt32      (*tMaxDepthFn)(tModel);
typedef tUInt64      (*tCtxBytesFn)(tUInt32);
typedef tSimStateHdl (*tSyncInitFn)(tModel, tBool,
                                    const struct bs_host_ops*, void*,
                                    tUInt32, void*);
typedef tStatus      (*tSyncRunFn)(tSimStateHdl);
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

int main(int argc, char** argv)
{
  if (argc != 3)
  {
    fprintf(stderr, "usage: host_loadalloc <model.so> <top-module>\n");
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

  /* prime lazy initialization inside the wrappers (dlsym) before
   * opening the watch window, so the window records only dlopen()
   */
  void* self = dlopen(NULL, RTLD_NOW);
  if (self != NULL)
    dlclose(self);

  /* ---- the measured load ---- */
  g_watch = 1;
  void* dl = dlopen(so_path, RTLD_NOW);
  g_watch = 0;

  if (dl == NULL)
  {
    fprintf(stderr, "harness: cannot load '%s': %s\n", so_path, dlerror());
    return 1;
  }

  fprintf(stderr, "harness: %u allocator calls and %u atexit "
          "registrations while loading (all origins)\n",
          g_alloc_count, g_atexit_count);

  unsigned int bad = 0;

  /* the model's own Bluesim allocator must be untouched */
  tMemCountersFn mem_counters =
    (tMemCountersFn) find_sym(dl, "bs_mem_alloc_counters");
  unsigned long long mem_allocs, mem_frees;
  mem_counters(&mem_allocs, &mem_frees);
  printf("harness: bluesim allocator calls at load: %llu allocs, "
         "%llu frees\n", mem_allocs, mem_frees);
  if ((mem_allocs != 0) || (mem_frees != 0))
    ++bad;

  /* nothing that runs during dlopen() may register a static
   * destructor (only model initialization could)
   */
  printf("harness: static destructors registered at load: %u\n",
         g_atexit_count);
  if (g_atexit_count != 0)
    ++bad;

  /* every load-time allocation must be the dynamic linker's own */
  unsigned int stray_allocs = count_strays(g_alloc_callers, g_alloc_count);
  printf("harness: allocator calls beyond the dynamic linker's at "
         "load: %u\n", stray_allocs);
  if (stray_allocs != 0)
    ++bad;

  printf("harness: model loads without allocator calls: %s\n",
         (bad == 0) ? "yes" : "NO");

  /* ---- the model still works: initialize and run it to $finish ---- */
  char new_model_name[256];
  snprintf(new_model_name, sizeof(new_model_name), "new_MODEL_%s", top_name);

  tNewModelFn   new_model   = (tNewModelFn)   find_sym(dl, new_model_name);
  tMaxDepthFn   max_depth   = (tMaxDepthFn)   find_sym(dl, "bk_max_event_queue_depth");
  tCtxBytesFn   ctx_bytes   = (tCtxBytesFn)   find_sym(dl, "bk_context_bytes");
  tSyncInitFn   sync_init   = (tSyncInitFn)   find_sym(dl, "bk_sync_init");
  tSyncRunFn    sync_run    = (tSyncRunFn)    find_sym(dl, "bk_sync_run");
  tFinishedFn   finished    = (tFinishedFn)   find_sym(dl, "bk_finished");
  tExitStatusFn exit_status = (tExitStatusFn) find_sym(dl, "bk_exit_status");
  tShutdownFn   shutdown_fn = (tShutdownFn)   find_sym(dl, "bk_shutdown");

  tModel model = new_model();
  if (model == NULL)
  {
    fprintf(stderr, "harness: new_%s returned NULL\n", top_name);
    return 1;
  }

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

  return (bad == 0) ? 0 : 1;
}

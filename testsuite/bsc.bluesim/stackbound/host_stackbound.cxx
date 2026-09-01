/* Host-side harness validating the static stack-depth bound a
 * Bluesim model exposes through bk_stack_depth_bound().
 *
 * In 'measure' mode (a design with no BDPI) the harness requires the
 * exposed bound to be positive, and validates its soundness
 * empirically: the model runs on a thread whose stack the harness
 * allocated itself; after construction, initialization and a warm-up
 * period, everything below the thread's stack pointer is painted
 * with a byte pattern, many busy cycles are stepped (wide
 * arithmetic, wide division and $display included), and the painted
 * high-water mark must stay within the exposed bound.
 *
 * The bound deliberately excludes the far side of the bs_host_ops
 * table (see bk_stack_depth_bound() in bluesim_kernel_api.h), so the
 * harness installs host operations built directly on read(2)/
 * write(2), keeping the host-side share of the measured high-water a
 * few hundred bytes instead of pulling stdio's buffering machinery
 * into the measurement.
 *
 * In 'nobound' mode (a design importing a BDPI function, which is
 * compiled outside the stack-usage scheme) the harness requires the
 * exposed bound to be the documented "no bound available" value 0,
 * and simply runs the design to $finish.
 *
 * Usage: host_stackbound <model.so> <top-module> measure|nobound
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <dlfcn.h>
#include <pthread.h>
#include <unistd.h>

#include "bluesim_types.h"
#include "bluesim_host_ops.h"

/* ---- host operations built on raw file-descriptor syscalls ---- */

/* fake stream handles: fd + 1, so stdin(0) is not NULL */
static struct bs_host_file* fd_handle(int fd)
{
  return (struct bs_host_file*)(intptr_t)(fd + 1);
}

static int handle_fd(struct bs_host_file* f)
{
  return ((int)(intptr_t)f) - 1;
}

static struct bs_host_file* raw_std_stream(void* /*ctx*/,
                                           tHostStdStream which)
{
  switch (which)
  {
    case BS_HOST_STDIN:  return fd_handle(0);
    case BS_HOST_STDOUT: return fd_handle(1);
    case BS_HOST_STDERR: return fd_handle(2);
  }
  return NULL;
}

static struct bs_host_file* raw_open(void* /*ctx*/,
                                     const char* /*filename*/,
                                     const char* /*mode*/)
{
  return NULL;   /* the designs under test open no files */
}

static void raw_close(void* /*ctx*/, struct bs_host_file* /*file*/)
{
}

static tBool raw_write(void* /*ctx*/, struct bs_host_file* file,
                       const char* data, size_t len)
{
  int fd = handle_fd(file);
  while (len > 0)
  {
    ssize_t n = write(fd, data, len);
    if (n <= 0)
      return 0;
    data += n;
    len -= (size_t)n;
  }
  return 1;
}

static tSInt64 raw_read(void* /*ctx*/, struct bs_host_file* file,
                        char* buf, size_t len)
{
  return (tSInt64)read(handle_fd(file), buf, len);
}

static tSInt32 raw_unget_char(void* /*ctx*/, struct bs_host_file* /*file*/,
                              char /*c*/)
{
  return -1;   /* not used by the designs under test */
}

static void raw_flush(void* /*ctx*/, struct bs_host_file* /*file*/)
{
  /* raw writes are unbuffered */
}

static tSInt32 raw_format_real(void* /*ctx*/, char* buf, size_t buf_size,
                               const char* format, double value)
{
  return (tSInt32)snprintf(buf, buf_size, format, value);
}

static void raw_divide_by_zero(void* /*ctx*/, const char* description)
{
  fprintf(stderr, "harness: %s by zero\n", description);
  abort();
}

static void raw_out_of_bounds(void* /*ctx*/, const char* prim,
                              const char* instance, const char* access,
                              tUInt64 addr, tUInt64 lo, tUInt64 hi)
{
  fprintf(stderr, "harness: %s %s out of bounds on %s "
          "(0x%llx not in [0x%llx,0x%llx])\n",
          prim, access, instance,
          (unsigned long long)addr, (unsigned long long)lo,
          (unsigned long long)hi);
  abort();
}

static void raw_event_queue_overflow(void* /*ctx*/, tUInt32 capacity)
{
  fprintf(stderr, "harness: event queue overflow (capacity %u)\n",
          (unsigned)capacity);
  abort();
}

static const struct bs_host_ops raw_ops = {
  sizeof(struct bs_host_ops),
  BS_HOST_OPS_VERSION,
  raw_std_stream,
  raw_open,
  raw_close,
  raw_write,
  raw_read,
  raw_unget_char,
  raw_flush,
  raw_format_real,
  raw_divide_by_zero,
  raw_out_of_bounds,
  raw_event_queue_overflow,
};

/* ---- kernel entry points ---- */

typedef void* (*tNewModelFn)(const struct bs_host_ops*, void*,
                              void*, void*, void*);
typedef tUInt64 (*tModelBytesFn)(tModel);
typedef tUInt32      (*tMaxDepthFn)(tModel);
typedef tUInt64      (*tStackBoundFn)(tModel);
typedef tUInt64      (*tCtxBytesFn)(tUInt32);
typedef tSimStateHdl (*tSyncInitFn)(tModel, tBool,
                                    const struct bs_host_ops*, void*,
                                    tUInt32, void*);
typedef tStatus      (*tSyncRunFn)(tSimStateHdl);
typedef tStatus      (*tSyncStepFn)(tSimStateHdl, tClock);
typedef tClock       (*tGetClockFn)(tSimStateHdl, const char*);
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

/* ---- the simulation thread ---- */

#define PAINT_BYTE     0xa5u
#define PAINT_MARGIN   1024      /* skipped just below the current SP */
#define STACK_BYTES    (1024u * 1024u)
#define WARMUP_CYCLES  200
#define MEASURE_CYCLES 1000

static struct
{
  /* in */
  tNewModelFn   new_model;
  tMaxDepthFn   max_depth;
  tCtxBytesFn   ctx_bytes;
  tSyncInitFn   sync_init;
  tSyncRunFn    sync_run;
  tSyncStepFn   sync_step;
  tGetClockFn   get_clock;
  tFinishedFn   finished;
  tExitStatusFn exit_status;
  tShutdownFn   shutdown_fn;
  tModelBytesFn state_bytes;
  tModelBytesFn in_bytes;
  tModelBytesFn out_bytes;
  char*         stack_low;     /* base of the thread's stack region */
  /* out */
  int           ok;
  tSInt32       final_status;
  unsigned long long measured; /* painted bytes consumed below the SP */
} g;

static void* sim_thread(void* /*arg*/)
{
  g.ok = 0;

  tModel model = g.new_model(NULL, NULL, NULL, NULL, NULL);
  if (model == NULL)
  {
    fprintf(stderr, "harness: new_MODEL returned NULL\n");
    return NULL;
  }

  /* the model's caller-provided storage (constructor ABI) */
  void* state_buf = malloc(g.state_bytes(model));
  void* in_buf  = (g.in_bytes(model) > 0)  ? malloc(g.in_bytes(model))  : NULL;
  void* out_buf = (g.out_bytes(model) > 0) ? malloc(g.out_bytes(model)) : NULL;
  model = g.new_model(&raw_ops, NULL, state_buf, in_buf, out_buf);

  tUInt32 capacity = g.max_depth(model) + 16;
  void* ctx_buf = malloc(g.ctx_bytes(capacity));
  tSimStateHdl sim = g.sync_init(model, 1, &raw_ops, NULL,
                                 capacity, ctx_buf);
  if (sim == NULL)
  {
    fprintf(stderr, "harness: bk_sync_init failed\n");
    return NULL;
  }

  tClock clk = g.get_clock(sim, "CLK");

  /* warm up: past reset and into steady state */
  for (unsigned i = 0; i < WARMUP_CYCLES; ++i)
  {
    if (g.sync_step(sim, clk) != 0 /* BK_SUCCESS */ || g.finished(sim))
    {
      fprintf(stderr, "harness: warm-up ended early\n");
      return NULL;
    }
  }

  /* paint everything below the current stack pointer (minus a margin
   * protecting this frame and its red zone)
   */
  volatile char sp_probe = 0;
  char* paint_top = (char*)((((uintptr_t)&sp_probe) - PAINT_MARGIN)
                            & ~(uintptr_t)15);
  memset(g.stack_low, PAINT_BYTE, (size_t)(paint_top - g.stack_low));

  /* the measured busy cycles */
  for (unsigned i = 0; i < MEASURE_CYCLES; ++i)
  {
    if (g.sync_step(sim, clk) != 0 /* BK_SUCCESS */ || g.finished(sim))
    {
      fprintf(stderr, "harness: measured run ended early\n");
      return NULL;
    }
  }

  /* high-water scan: first byte from the bottom no longer painted.
   * The reported figure is measured from the probe SP, so the
   * unpainted margin counts as used -- a conservative over-count of
   * at most PAINT_MARGIN bytes, which only makes the within-bound
   * check stricter.  A fully clean painted region reports 0.
   */
  char* p = g.stack_low;
  while (p < paint_top && *(unsigned char*)p == PAINT_BYTE)
    ++p;
  g.measured = (p == paint_top)
      ? 0llu
      : (unsigned long long)((char*)&sp_probe - p);

  /* run the design to its own $finish */
  g.sync_run(sim);
  if (!g.finished(sim))
  {
    fprintf(stderr, "harness: simulation did not run to $finish\n");
    return NULL;
  }
  g.final_status = g.exit_status(sim);
  g.shutdown_fn(sim);
  free(ctx_buf);

  g.ok = 1;
  return NULL;
}

int main(int argc, char** argv)
{
  if (argc != 4 ||
      (strcmp(argv[3], "measure") != 0 && strcmp(argv[3], "nobound") != 0))
  {
    fprintf(stderr,
            "usage: host_stackbound <model.so> <top-module> "
            "measure|nobound\n");
    return 1;
  }
  const char* so_name  = argv[1];
  const char* top_name = argv[2];
  int measure = (strcmp(argv[3], "measure") == 0);

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

  g.new_model   = (tNewModelFn)   find_sym(dl, new_model_name);
  g.state_bytes = (tModelBytesFn) find_sym(dl, "bk_state_bytes");
  g.in_bytes    = (tModelBytesFn) find_sym(dl, "bk_input_bytes");
  g.out_bytes   = (tModelBytesFn) find_sym(dl, "bk_output_bytes");
  g.max_depth   = (tMaxDepthFn)   find_sym(dl, "bk_max_event_queue_depth");
  g.ctx_bytes   = (tCtxBytesFn)   find_sym(dl, "bk_context_bytes");
  g.sync_init   = (tSyncInitFn)   find_sym(dl, "bk_sync_init");
  g.sync_run    = (tSyncRunFn)    find_sym(dl, "bk_sync_run");
  g.sync_step   = (tSyncStepFn)   find_sym(dl, "bk_sync_step");
  g.get_clock   = (tGetClockFn)   find_sym(dl, "bk_get_clock_by_name");
  g.finished    = (tFinishedFn)   find_sym(dl, "bk_finished");
  g.exit_status = (tExitStatusFn) find_sym(dl, "bk_exit_status");
  g.shutdown_fn = (tShutdownFn)   find_sym(dl, "bk_shutdown");
  tStackBoundFn stack_bound =
      (tStackBoundFn) find_sym(dl, "bk_stack_depth_bound");

  /* the bound is a property of the loaded model: readable before any
   * simulation state exists (a NULL model handle reads as 0)
   */
  tModel probe_model = g.new_model(NULL, NULL, NULL, NULL, NULL);
  tUInt64 bound = stack_bound(probe_model);
  fprintf(stderr, "harness: exposed stack bound: %llu bytes\n",
          (unsigned long long)bound);
  printf("harness: NULL model handle reads as no bound: %s\n",
         (stack_bound(NULL) == 0) ? "yes" : "NO");

  if (!measure)
  {
    printf("harness: BDPI design exposes the no-bound value 0: %s\n",
           (bound == 0) ? "yes" : "NO");
    if (bound != 0)
      return 1;

    /* the design still simulates normally */
    void* state_buf = malloc(g.state_bytes(probe_model));
    void* in_buf = (g.in_bytes(probe_model) > 0)
                       ? malloc(g.in_bytes(probe_model)) : NULL;
    void* out_buf = (g.out_bytes(probe_model) > 0)
                        ? malloc(g.out_bytes(probe_model)) : NULL;
    probe_model = g.new_model(&raw_ops, NULL, state_buf, in_buf, out_buf);
    tUInt32 capacity = g.max_depth(probe_model) + 16;
    void* ctx_buf = malloc(g.ctx_bytes(capacity));
    tSimStateHdl sim = g.sync_init(probe_model, 1, &raw_ops, NULL,
                                   capacity, ctx_buf);
    if (sim == NULL)
    {
      fprintf(stderr, "harness: bk_sync_init failed\n");
      return 1;
    }
    g.sync_run(sim);
    if (!g.finished(sim))
    {
      fprintf(stderr, "harness: simulation did not run to $finish\n");
      return 1;
    }
    printf("harness: simulation finished with status %d\n",
           (int)g.exit_status(sim));
    g.shutdown_fn(sim);
    free(ctx_buf);
    return 0;
  }

  printf("harness: exposed stack bound is positive: %s\n",
         (bound > 0) ? "yes" : "NO");
  if (bound == 0)
    return 1;

  /* run the simulation on a thread whose stack we own, so its usage
   * can be painted and measured (the thread constructs its own model
   * handle; the probe handle above was only for reading the bound)
   */
  void* stack_mem = NULL;
  if (posix_memalign(&stack_mem, 4096, STACK_BYTES) != 0)
  {
    fprintf(stderr, "harness: cannot allocate the thread stack\n");
    return 1;
  }
  g.stack_low = (char*)stack_mem;

  pthread_attr_t attr;
  pthread_attr_init(&attr);
  if (pthread_attr_setstack(&attr, stack_mem, STACK_BYTES) != 0)
  {
    fprintf(stderr, "harness: pthread_attr_setstack failed\n");
    return 1;
  }

  pthread_t tid;
  if (pthread_create(&tid, &attr, sim_thread, NULL) != 0)
  {
    fprintf(stderr, "harness: pthread_create failed\n");
    return 1;
  }
  pthread_join(tid, NULL);
  if (!g.ok)
    return 1;

  fprintf(stderr, "harness: measured stack high-water: %llu bytes "
          "over %u cycles\n", g.measured, (unsigned)MEASURE_CYCLES);

  printf("harness: busy cycles touch the painted stack: %s\n",
         (g.measured > 0) ? "yes" : "NO");
  printf("harness: measured high-water within the exposed bound: %s\n",
         (g.measured <= bound) ? "yes" : "NO");
  printf("harness: simulation finished with status %d\n",
         (int)g.final_status);

  return ((g.measured > 0) && (g.measured <= bound)) ? 0 : 1;
}

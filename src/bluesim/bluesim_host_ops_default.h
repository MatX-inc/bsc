#ifndef __BLUESIM_HOST_OPS_DEFAULT_H__
#define __BLUESIM_HOST_OPS_DEFAULT_H__

/*
 * Default implementation of the bs_host_ops interface, backed by the
 * C library's stdio: streams are FILE*s, files are opened with
 * fopen(), and real numbers are formatted with snprintf().  Output
 * streams are made line-buffered, which is what Bluesim's output has
 * traditionally been.
 *
 * This header is for EMBEDDERS of a Bluesim model (it is what bluetcl
 * installs, and what the generated SystemC wrappers use).  It must
 * not be included by code that is linked into a Bluesim model shared
 * object: the point of bs_host_ops is that the model itself contains
 * no I/O.
 *
 * Everything here is declared static inline so that the header can
 * be included from multiple translation units of a host program.
 */

#include <stdio.h>
#include <stdlib.h>

#include "bluesim_host_ops.h"

static inline struct bs_host_file* bs_default_std_stream(void* ctx,
                                                  tHostStdStream which)
{
  (void) ctx;
  switch (which)
  {
    case BS_HOST_STDIN:  return (struct bs_host_file*) stdin;
    case BS_HOST_STDOUT: return (struct bs_host_file*) stdout;
    case BS_HOST_STDERR: return (struct bs_host_file*) stderr;
  }
  return NULL;
}

static inline own struct bs_host_file* bs_default_open(void* ctx,
                                                const char* filename,
                                                const char* mode)
{
  (void) ctx;
  FILE* file = fopen(filename, mode);
  if (file != NULL)
    setvbuf(file, NULL, _IOLBF, 0); /* line-buffered, as it always was */
  return (struct bs_host_file*) file;
}

static inline void bs_default_close(void* ctx, own struct bs_host_file* file)
{
  (void) ctx;
  if (file != NULL)
    fclose((FILE*) file);
}

static inline tBool bs_default_write(void* ctx, struct bs_host_file* file,
                              const char* data, size_t len)
{
  (void) ctx;
  return (fwrite(data, 1, len, (FILE*) file) == len) ? 1 : 0;
}

static inline tSInt64 bs_default_read(void* ctx, struct bs_host_file* file,
                               char* buf, size_t len)
{
  (void) ctx;
  size_t count = fread(buf, 1, len, (FILE*) file);
  if ((count == 0) && ferror((FILE*) file))
    return -1;
  return (tSInt64) count;
}

static inline tSInt32 bs_default_unget_char(void* ctx, struct bs_host_file* file,
                                     char c)
{
  (void) ctx;
  return ungetc((unsigned char) c, (FILE*) file);
}

static inline void bs_default_flush(void* ctx, struct bs_host_file* file)
{
  (void) ctx;
  fflush((FILE*) file); /* a NULL file flushes all open output streams */
}

static inline tSInt32 bs_default_format_real(void* ctx, char* buf, size_t buf_size,
                                      const char* format, double value)
{
  (void) ctx;
  return (tSInt32) snprintf(buf, buf_size, format, value);
}

static inline void bs_default_divide_by_zero(void* ctx,
                                             const char* description)
    BS_HOST_NORETURN;
static inline void bs_default_divide_by_zero(void* ctx,
                                             const char* description)
{
  (void) ctx;
  fprintf(stderr, "Error: %s by zero.\n", description);
  fflush(stderr);
  abort();
}

static inline void bs_default_out_of_bounds(void* ctx,
                                            const char* prim,
                                            const char* instance,
                                            const char* access,
                                            tUInt64 addr,
                                            tUInt64 lo,
                                            tUInt64 hi)
    BS_HOST_NORETURN;
static inline void bs_default_out_of_bounds(void* ctx,
                                            const char* prim,
                                            const char* instance,
                                            const char* access,
                                            tUInt64 addr,
                                            tUInt64 lo,
                                            tUInt64 hi)
{
  (void) ctx;
  fprintf(stderr,
          "Error: %s '%s' -- %s is out of bounds: 0x%llx"
          " (valid range: 0x%llx to 0x%llx)\n",
          prim, instance, access,
          (unsigned long long) addr,
          (unsigned long long) lo,
          (unsigned long long) hi);
  fflush(stderr);
  abort();
}

static inline void bs_default_event_queue_overflow(void* ctx,
                                                   tUInt32 capacity)
    BS_HOST_NORETURN;
static inline void bs_default_event_queue_overflow(void* ctx,
                                                   tUInt32 capacity)
{
  (void) ctx;
  fprintf(stderr,
          "Error: Bluesim event queue overflow (capacity %u events).\n",
          (unsigned int) capacity);
  fflush(stderr);
  abort();
}

/* Get the default host operations table (constructed on first use).
 * The table is static, so it satisfies bk_sync_init()'s requirement
 * of remaining valid until bk_shutdown().
 */
static inline const struct bs_host_ops* bs_default_host_ops(void)
{
  static struct bs_host_ops ops;
  static int ops_ready = 0;
  if (!ops_ready)
  {
    ops.size        = sizeof(struct bs_host_ops);
    ops.version     = BS_HOST_OPS_VERSION;
    ops.std_stream  = bs_default_std_stream;
    ops.open        = bs_default_open;
    ops.close       = bs_default_close;
    ops.write       = bs_default_write;
    ops.read        = bs_default_read;
    ops.unget_char  = bs_default_unget_char;
    ops.flush       = bs_default_flush;
    ops.format_real = bs_default_format_real;
    ops.divide_by_zero = bs_default_divide_by_zero;
    ops.out_of_bounds  = bs_default_out_of_bounds;
    ops.event_queue_overflow = bs_default_event_queue_overflow;

    /* Bluesim's standard output has always been line-buffered (the
     * runtime used to setlinebuf() every stream it wrote to).
     */
    setvbuf(stdout, NULL, _IOLBF, 0);
    setvbuf(stderr, NULL, _IOLBF, 0);

    ops_ready = 1;
  }
  return &ops;
}

/* Get the host context that goes with the default host operations
 * (they keep no state, so it is NULL).
 */
static inline void* bs_default_host_ctx(void)
{
  return NULL;
}

#endif /* __BLUESIM_HOST_OPS_DEFAULT_H__ */

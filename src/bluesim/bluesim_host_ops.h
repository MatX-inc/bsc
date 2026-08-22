#ifndef __BLUESIM_HOST_OPS_H__
#define __BLUESIM_HOST_OPS_H__

#include <stddef.h>

#include "bluesim_types.h"

/*
 * Host operations for the Bluesim runtime.
 *
 * A Bluesim model performs no I/O of its own: every byte it reads or
 * writes (including $display output, $fopen/$fwrite file access and
 * memory-file preloads) goes through a table of function pointers
 * supplied by the embedder -- the "host" -- when the kernel is
 * initialized with bk_sync_init().  The host also supplies a single
 * shared context pointer, which is passed back as the first argument
 * of every operation; the runtime never interprets it.
 *
 * The structure leads with 'size' and 'version' fields so that new
 * operations can be appended over time: a host fills in the fields it
 * was compiled against and the kernel checks at initialization that
 * the table is at least as new as the runtime requires.
 *
 * A ready-made implementation backed by the C library is provided in
 * bluesim_host_ops_default.h for embedders that just want the
 * traditional stdio behavior (bluetcl installs it by default).
 */

#if __cplusplus
extern "C" {
#endif

/* Documentation marker for ownership transfer (an idea borrowed from
 * wasm-c-api): a pointer parameter or return type marked 'own' means
 * that ownership of the pointed-to object is transferred by the call.
 * A value received as 'own' must eventually be released by handing it
 * back to a call whose parameter is marked 'own' (e.g. a stream
 * returned by the 'open' operation is released by passing it to the
 * 'close' operation).  The macro expands to nothing; it exists purely
 * to document the ownership contract in the type.
 */
#ifndef own
#define own
#endif

/* Marker for operations that must not return: after reporting the
 * condition, the host must terminate execution (as the default
 * implementation does with abort()) or unwind past the runtime by a
 * mechanism of its own (e.g. longjmp to a point outside the kernel;
 * the simulation state may not be used again afterwards).  The
 * runtime traps if such an operation returns.
 */
#if defined(__GNUC__)
#define BS_HOST_NORETURN __attribute__((noreturn))
#else
#define BS_HOST_NORETURN
#endif

/* An opaque handle to a host output/input stream.  The host owns the
 * underlying object (a FILE* in the default implementation); the
 * runtime only stores and forwards these handles.
 */
struct bs_host_file;

/* Selector for the standard streams of the host */
typedef enum { BS_HOST_STDIN  = 0
             , BS_HOST_STDOUT = 1
             , BS_HOST_STDERR = 2
             } tHostStdStream;

/* The current version of the bs_host_ops structure.  This is bumped
 * whenever operations are appended to the structure; the kernel
 * refuses a table whose version (or size) is older than the one it
 * was compiled against.
 */
#define BS_HOST_OPS_VERSION 3u

struct bs_host_ops {
  /* The size in bytes of the structure as compiled into the host,
   * i.e. sizeof(struct bs_host_ops).  Must be filled in by the host.
   */
  size_t size;

  /* The interface version as compiled into the host,
   * i.e. BS_HOST_OPS_VERSION.  Must be filled in by the host.
   */
  tUInt32 version;

  /* Get the handle for one of the host's standard streams.  The
   * returned handle is NOT owned by the caller (the runtime never
   * closes it) and remains valid until bk_shutdown().
   */
  struct bs_host_file* (*std_stream)(void* ctx, tHostStdStream which);

  /* Open the named file, in the manner of fopen(): 'mode' is a C
   * stdio mode string ("r", "w", "a", "r+", ...).  Returns a stream
   * handle owned by the caller, to be released with 'close', or NULL
   * on failure (with errno set, as fopen() does).
   */
  own struct bs_host_file* (*open)(void* ctx,
                                   const char* filename,
                                   const char* mode);

  /* Close a stream previously returned by 'open', releasing it.
   * Must not be used on a handle from 'std_stream'.  A NULL handle
   * is ignored.
   */
  void (*close)(void* ctx, own struct bs_host_file* file);

  /* Write 'len' bytes to a stream.  Returns non-zero if all bytes
   * were written and 0 on failure (with errno set).
   */
  tBool (*write)(void* ctx,
                 struct bs_host_file* file,
                 const char* data,
                 size_t len);

  /* Read up to 'len' bytes from a stream into 'buf'.  Returns the
   * number of bytes read, which is 0 at end-of-file and less than
   * 'len' only at end-of-file or on error; returns a negative value
   * on error.
   */
  tSInt64 (*read)(void* ctx,
                  struct bs_host_file* file,
                  char* buf,
                  size_t len);

  /* Push a byte back onto a stream, in the manner of ungetc(): the
   * byte is returned by the next read.  Returns the byte pushed
   * back, or a negative value on failure.
   */
  tSInt32 (*unget_char)(void* ctx, struct bs_host_file* file, char c);

  /* Flush any buffered output for a stream.  A NULL 'file' requests
   * that all of the host's open output streams be flushed (in the
   * manner of fflush(NULL)); this is what the kernel uses when
   * returning control to the embedder (see bk_set_flush_on_pause()).
   */
  void (*flush)(void* ctx, struct bs_host_file* file);

  /* Format one real (double) value using a printf-style format
   * string containing exactly one %e/%f/%g conversion (this is how
   * $display and friends print reals; the integer, string and time
   * formats are printed by the runtime itself).  Behaves like
   * snprintf(buf, buf_size, format, value): the output is truncated
   * to fit 'buf_size' bytes including the terminating NUL, and the
   * return value is the untruncated length (excluding the NUL), or a
   * negative value on error.
   */
  tSInt32 (*format_real)(void* ctx,
                         char* buf,
                         size_t buf_size,
                         const char* format,
                         double value);

  /* -- Operations below were appended in version 2: the noreturn
   *    fatal-condition reports (divide_by_zero, out_of_bounds) -- */

  /* Report that the model attempted to divide by zero, and terminate
   * execution: this operation must not return (see BS_HOST_NORETURN
   * above).  'description' is a static string describing the
   * operation, phrased to fit a message of the form
   * "<description> by zero" (e.g. "wide integer division").
   */
  void (*divide_by_zero)(void* ctx,
                         const char* description) BS_HOST_NORETURN;

  /* Report that the model accessed a memory primitive (a RegFile or
   * BRAM) outside of its address bounds, and terminate execution:
   * this operation must not return (see BS_HOST_NORETURN above).
   * 'prim' is the primitive kind (e.g. "RegFile" or "BRAM") and
   * 'access' describes the offending access (e.g. "Read address" or
   * "Write address on port A"); both are static strings.  'instance'
   * is the full dotted instance name of the primitive (owned by the
   * caller; it is only valid for the duration of the call, which
   * never returns).  'addr' is the out-of-bounds address and
   * 'lo'/'hi' are the valid (inclusive) address bounds.
   */
  void (*out_of_bounds)(void* ctx,
                        const char* prim,
                        const char* instance,
                        const char* access,
                        tUInt64 addr,
                        tUInt64 lo,
                        tUInt64 hi) BS_HOST_NORETURN;

  /* -- Operations below were appended in version 3: the noreturn
   *    event-queue overflow report -- */

  /* Report that the kernel's event queue is full and cannot accept
   * another event, and terminate execution: this operation must not
   * return (see BS_HOST_NORETURN above).  The event queue has the
   * fixed capacity the host chose at bk_sync_init() (normally the
   * model's bk_max_event_queue_depth() plus headroom for the host's
   * own events), so hitting this means either the host under-budgeted
   * its own event-enqueuing calls or the model exceeded its computed
   * bound.  'capacity' is the fixed capacity that was exceeded.
   */
  void (*event_queue_overflow)(void* ctx,
                               tUInt32 capacity) BS_HOST_NORETURN;

  /* New operations are appended here in later versions; each
   * addition bumps BS_HOST_OPS_VERSION.
   */
};

#if __cplusplus
} /* extern "C" */
#endif

#endif /* __BLUESIM_HOST_OPS_H__ */

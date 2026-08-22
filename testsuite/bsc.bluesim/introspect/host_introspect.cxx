/* Host-side test harness for the non-allocating introspection of a
 * Bluesim model: the state-element and top-module input/output port
 * descriptor tables and their flat byte layout, as documented in
 * bluesim_introspection.h.
 *
 * The harness plays the role of an embedder: it dlopen()s a Bluesim
 * model shared object and walks the descriptor tables through the
 * bk_* kernel accessors BEFORE bk_sync_init(), exactly as a host
 * that wants to size storage for a design would.  It prints the
 * complete tables (pinning names, kinds, widths, geometry and the
 * layout in the expected output) and independently re-checks the
 * documented layout invariants:
 *
 *   - the storage unit and alignment rules for each bit width
 *   - offsets are aligned, non-overlapping and in table order
 *   - the sum of the element sizes fits in the total area size
 *   - each area's total is a multiple of 8 bytes
 *
 * It then initializes the kernel and checks that the descriptors are
 * the same static tables after initialization.
 *
 * Usage: host_introspect <model.so> <top-module>
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <dlfcn.h>

#include "bluesim_types.h"
#include "bluesim_host_ops.h"
#include "bluesim_host_ops_default.h"
#include "bluesim_introspection.h"

typedef void*               (*tNewModelFn)(const struct bs_host_ops*, void*,
                                            void*, void*, void*);
typedef tUInt32             (*tMaxDepthFn)(tModel);
typedef tUInt32             (*tCountFn)(tModel);
typedef const tBkStateInfo* (*tGetStateFn)(tModel, tUInt32);
typedef const tBkPortInfo*  (*tGetPortFn)(tModel, tUInt32);
typedef tUInt64             (*tBytesFn)(tModel);
typedef tUInt64             (*tCtxBytesFn)(tUInt32);
typedef tSimStateHdl        (*tSyncInitFn)(tModel, tBool,
                                           const struct bs_host_ops*, void*,
                                           tUInt32, void*);
typedef void                (*tShutdownFn)(tSimStateHdl);

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

static const char* kind_name(tBkStateKind kind)
{
  switch (kind)
  {
    case BK_STATE_REG:     return "reg";
    case BK_STATE_WIRE:    return "wire";
    case BK_STATE_REGFILE: return "regfile";
    case BK_STATE_BRAM:    return "bram";
    case BK_STATE_FIFO:    return "fifo";
    case BK_STATE_PROBE:   return "probe";
    case BK_STATE_COUNTER: return "counter";
    case BK_STATE_SYNC:    return "sync";
    case BK_STATE_CLOCK:   return "clock";
    case BK_STATE_RESET:   return "reset";
    default:               return "???";
  }
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

/* the documented storage-unit and alignment rules of
 * bluesim_introspection.h, re-implemented independently
 */
static tUInt64 unit_bytes(tUInt32 bits)
{
  if (bits <= 8)  return 1;
  if (bits <= 32) return 4;
  if (bits <= 64) return 8;
  return 4ull * ((bits + 31u) / 32u);
}

static tUInt64 align_bytes(tUInt32 bits)
{
  if (bits <= 8)  return 1;
  if (bits <= 32) return 4;
  if (bits <= 64) return 8;
  return 4;
}

/* shared layout checks for one element of any area; returns the
 * element's end offset
 */
static tUInt64 check_element(const char* name, tUInt32 bits,
                             tUInt64 entries, tUInt64 offset, tUInt64 size,
                             tUInt64 prev_end, tUInt64 total)
{
  char buf[256];
  snprintf(buf, sizeof(buf), "%s: size is entries * unit", name);
  check(size == entries * unit_bytes(bits), buf);
  snprintf(buf, sizeof(buf), "%s: offset is aligned", name);
  check((offset % align_bytes(bits)) == 0, buf);
  snprintf(buf, sizeof(buf), "%s: no overlap with previous element", name);
  check(offset >= prev_end, buf);
  snprintf(buf, sizeof(buf), "%s: element fits in the area", name);
  check(offset + size <= total, buf);
  return offset + size;
}

int main(int argc, char** argv)
{
  if (argc != 3)
  {
    fprintf(stderr, "usage: host_introspect <model.so> <top-module>\n");
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

  tNewModelFn new_model  = (tNewModelFn) find_sym(dl, new_model_name);
  tMaxDepthFn max_depth  = (tMaxDepthFn) find_sym(dl, "bk_max_event_queue_depth");
  tCountFn    num_state  = (tCountFn)    find_sym(dl, "bk_num_state_elements");
  tGetStateFn get_state  = (tGetStateFn) find_sym(dl, "bk_get_state_element");
  tBytesFn    state_bytes = (tBytesFn)   find_sym(dl, "bk_state_bytes");
  tBytesFn    elems_off  = (tBytesFn)    find_sym(dl, "bk_state_elements_offset");
  tCountFn    num_in     = (tCountFn)    find_sym(dl, "bk_num_input_ports");
  tGetPortFn  get_in     = (tGetPortFn)  find_sym(dl, "bk_get_input_port");
  tBytesFn    in_bytes   = (tBytesFn)    find_sym(dl, "bk_input_bytes");
  tCountFn    num_out    = (tCountFn)    find_sym(dl, "bk_num_output_ports");
  tGetPortFn  get_out    = (tGetPortFn)  find_sym(dl, "bk_get_output_port");
  tBytesFn    out_bytes  = (tBytesFn)    find_sym(dl, "bk_output_bytes");
  tCtxBytesFn ctx_bytes  = (tCtxBytesFn) find_sym(dl, "bk_context_bytes");
  tSyncInitFn sync_init  = (tSyncInitFn) find_sym(dl, "bk_sync_init");
  tShutdownFn shutdown_fn = (tShutdownFn) find_sym(dl, "bk_shutdown");

  /* a NULL model yields 0 counts / NULL descriptors */
  check(num_state(NULL) == 0, "NULL model has no state elements");
  check(get_state(NULL, 0) == NULL, "NULL model has no state descriptor");
  check(state_bytes(NULL) == 0, "NULL model has no state bytes");
  check(elems_off(NULL) == 0, "NULL model has no elements offset");
  check(num_in(NULL) == 0 && num_out(NULL) == 0,
        "NULL model has no ports");
  check(get_in(NULL, 0) == NULL && get_out(NULL, 0) == NULL,
        "NULL model has no port descriptors");
  check(in_bytes(NULL) == 0 && out_bytes(NULL) == 0,
        "NULL model has no port bytes");

  /* a sizing call: no storage yet, only the pre-init queries */
  tModel model = new_model(NULL, NULL, NULL, NULL, NULL);
  if (model == NULL)
  {
    fprintf(stderr, "harness: %s returned NULL\n", new_model_name);
    return 1;
  }

  /* everything below runs BEFORE bk_sync_init: the descriptors are
   * static per-design tables usable without any initialization
   */

  tUInt32 n_state = num_state(model);
  tUInt64 t_state = state_bytes(model);
  tUInt64 e_off   = elems_off(model);
  tUInt64 t_elems = t_state - e_off;
  printf("state elements: %u (%llu bytes)\n",
         (unsigned) n_state, (unsigned long long) t_elems);
  check(n_state > 0, "design has state elements");
  check(t_state > 0, "state area is nonzero");
  check(e_off > 0, "the module objects precede the element sub-area");
  check((e_off % 16) == 0, "element sub-area offset is 16-byte aligned");
  check(t_state == e_off + t_elems, "state bytes = objects + elements");
  check((t_elems % 8) == 0, "element sub-area is a multiple of 8 bytes");
  tUInt64 prev_end = 0;
  tUInt64 sum = 0;
  for (tUInt32 i = 0; i < n_state; ++i)
  {
    const tBkStateInfo* s = get_state(model, i);
    if (s == NULL)
    {
      fprintf(stderr, "harness: state element %u is NULL\n", (unsigned) i);
      return 1;
    }
    printf("  %-16s %-8s bits=%-3u entries=%-3llu offset=%-4llu size=%llu\n",
           s->name, kind_name(s->kind), (unsigned) s->bits,
           (unsigned long long) s->entries,
           (unsigned long long) s->offset, (unsigned long long) s->size);
    prev_end = check_element(s->name, s->bits, s->entries,
                             s->offset, s->size, prev_end, t_elems);
    sum += s->size;
  }
  check(sum <= t_elems, "state sizes sum within the area");
  check(get_state(model, n_state) == NULL,
        "out-of-range state index yields NULL");

  tUInt32 n_in = num_in(model);
  tUInt64 t_in = in_bytes(model);
  printf("input ports: %u (%llu bytes)\n",
         (unsigned) n_in, (unsigned long long) t_in);
  check(n_in > 0, "design has input ports");
  check(t_in > 0, "input area is nonzero");
  check((t_in % 8) == 0, "input area is a multiple of 8 bytes");
  prev_end = 0;
  sum = 0;
  for (tUInt32 i = 0; i < n_in; ++i)
  {
    const tBkPortInfo* p = get_in(model, i);
    if (p == NULL)
    {
      fprintf(stderr, "harness: input port %u is NULL\n", (unsigned) i);
      return 1;
    }
    printf("  %-16s bits=%-3u offset=%-4llu size=%llu\n",
           p->name, (unsigned) p->bits,
           (unsigned long long) p->offset, (unsigned long long) p->size);
    prev_end = check_element(p->name, p->bits, 1, p->offset, p->size,
                             prev_end, t_in);
    sum += p->size;
  }
  check(sum <= t_in, "input sizes sum within the area");
  check(get_in(model, n_in) == NULL,
        "out-of-range input index yields NULL");

  tUInt32 n_out = num_out(model);
  tUInt64 t_out = out_bytes(model);
  printf("output ports: %u (%llu bytes)\n",
         (unsigned) n_out, (unsigned long long) t_out);
  check(n_out > 0, "design has output ports");
  check(t_out > 0, "output area is nonzero");
  check((t_out % 8) == 0, "output area is a multiple of 8 bytes");
  prev_end = 0;
  sum = 0;
  for (tUInt32 i = 0; i < n_out; ++i)
  {
    const tBkPortInfo* p = get_out(model, i);
    if (p == NULL)
    {
      fprintf(stderr, "harness: output port %u is NULL\n", (unsigned) i);
      return 1;
    }
    printf("  %-16s bits=%-3u offset=%-4llu size=%llu\n",
           p->name, (unsigned) p->bits,
           (unsigned long long) p->offset, (unsigned long long) p->size);
    prev_end = check_element(p->name, p->bits, 1, p->offset, p->size,
                             prev_end, t_out);
    sum += p->size;
  }
  check(sum <= t_out, "output sizes sum within the area");
  check(get_out(model, n_out) == NULL,
        "out-of-range output index yields NULL");

  /* the input and output areas describe SEPARATE planned areas */
  check(n_in != 0 && n_out != 0 && get_in(model, 0) != get_out(model, 0),
        "input and output tables are separate");

  /* initializing the kernel does not change the static tables */
  const tBkStateInfo* s0 = get_state(model, 0);
  const struct bs_host_ops* ops = bs_default_host_ops();
  void* ctx = bs_default_host_ctx();
  tUInt32 capacity = max_depth(model) + 16;
  void* ctx_buf = malloc(ctx_bytes(capacity));

  /* the kernel refuses to construct a model with no storage bound */
  check(sync_init(model, 1, ops, ctx, capacity, ctx_buf) == NULL,
        "bk_sync_init refuses a model without storage");

  /* record the caller-provided storage with a second constructor call */
  void* state_buf = malloc(t_state);
  void* in_buf = (t_in > 0) ? malloc(t_in) : NULL;
  void* out_buf = (t_out > 0) ? malloc(t_out) : NULL;
  check(new_model(ops, ctx, state_buf, in_buf, out_buf) == model,
        "new_MODEL returns the same handle when storage is recorded");

  tSimStateHdl sim = sync_init(model, 1, ops, ctx, capacity, ctx_buf);
  if (sim == NULL)
  {
    fprintf(stderr, "harness: bk_sync_init failed\n");
    return 1;
  }
  check(num_state(model) == n_state, "state count unchanged by init");
  check(get_state(model, 0) == s0, "descriptors are static across init");
  check(state_bytes(model) == t_state, "state bytes unchanged by init");
  shutdown_fn(sim);
  free(ctx_buf);
  free(state_buf);
  free(in_buf);
  free(out_buf);

  if (failures != 0)
  {
    fprintf(stderr, "harness: %u check(s) failed\n", failures);
    return 1;
  }
  printf("harness: all checks passed\n");
  return 0;
}

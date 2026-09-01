/* Host-side test harness for the caller-provided port buffers: the
 * top module's input ports are read from, and its output ports
 * written to, the buffers the host passed to new_MODEL_*(), at the
 * byte offsets published by the introspection descriptor tables
 * (bluesim_introspection.h).
 *
 * The harness plays the role of an embedder driving a design purely
 * through port storage, the way a testbench drives Verilog ports:
 *
 *   - it sizes and allocates the state/input/output buffers, binds
 *     them, initializes the kernel as master (default clock and
 *     reset) and steps the clock with bk_sync_step();
 *   - it drives method arguments and enables by writing the input
 *     buffer at the published offsets before an edge;
 *   - it observes readies and method results by reading the output
 *     buffer at the published offsets after an edge;
 *   - it checks the full round trip: an enabled action method
 *     updates the design state, value-method results (narrow and
 *     wide) reflect it, an always-ready combinational method tracks
 *     its input-port argument cycle by cycle, and an ActionValue
 *     method both acts and returns a result.
 *
 * Usage: host_ports <model.so> <top-module>
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <dlfcn.h>

#include "bluesim_types.h"
#include "bluesim_host_ops.h"
#include "bluesim_host_ops_default.h"
#include "bluesim_introspection.h"

typedef void*              (*tNewModelFn)(const struct bs_host_ops*, void*,
                                          void*, void*, void*);
typedef tUInt32            (*tCountFn)(tModel);
typedef const tBkPortInfo* (*tGetPortFn)(tModel, tUInt32);
typedef tUInt64            (*tBytesFn)(tModel);
typedef tUInt32            (*tMaxDepthFn)(tModel);
typedef tUInt64            (*tCtxBytesFn)(tUInt32);
typedef tSimStateHdl       (*tSyncInitFn)(tModel, tBool,
                                          const struct bs_host_ops*, void*,
                                          tUInt32, void*);
typedef tClock             (*tGetClockFn)(tSimStateHdl, const char*);
typedef tStatus            (*tSyncStepFn)(tSimStateHdl, tClock);
typedef void               (*tShutdownFn)(tSimStateHdl);

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

/* ---------------- port access through the buffers ---------------- */

/* the port tables of the loaded design, captured once */
static tModel      g_model;
static tGetPortFn  g_get_in;
static tGetPortFn  g_get_out;
static tCountFn    g_num_in;
static tCountFn    g_num_out;
static unsigned char* g_in_buf;
static unsigned char* g_out_buf;

static const tBkPortInfo* find_port(int is_input, const char* name)
{
  tCountFn   num = is_input ? g_num_in : g_num_out;
  tGetPortFn get = is_input ? g_get_in : g_get_out;
  for (tUInt32 i = 0; i < num(g_model); ++i)
  {
    const tBkPortInfo* p = get(g_model, i);
    if ((p != NULL) && (strcmp(p->name, name) == 0))
      return p;
  }
  fprintf(stderr, "harness: no %s port named '%s'\n",
          is_input ? "input" : "output", name);
  exit(1);
}

/* read a port of up to 64 bits from its buffer at the published
 * offset, using the documented storage unit for its width
 */
static tUInt64 read_narrow(int is_input, const char* name)
{
  const tBkPortInfo* p = find_port(is_input, name);
  unsigned char* base = (is_input ? g_in_buf : g_out_buf) + p->offset;
  if (p->bits <= 8)  return *(tUInt8*)  base;
  if (p->bits <= 32) return *(tUInt32*) base;
  return *(tUInt64*) base;
}

/* write an input port of up to 64 bits into the input buffer */
static void write_narrow(const char* name, tUInt64 value)
{
  const tBkPortInfo* p = find_port(1, name);
  unsigned char* base = g_in_buf + p->offset;
  if (p->bits <= 8)       *(tUInt8*)  base = (tUInt8)  value;
  else if (p->bits <= 32) *(tUInt32*) base = (tUInt32) value;
  else                    *(tUInt64*) base = (tUInt64) value;
}

/* wide ports: arrays of 32-bit words at the published offset */
static tUInt32* wide_words(int is_input, const char* name)
{
  const tBkPortInfo* p = find_port(is_input, name);
  return (tUInt32*) ((is_input ? g_in_buf : g_out_buf) + p->offset);
}

int main(int argc, char** argv)
{
  if (argc != 3)
  {
    fprintf(stderr, "usage: host_ports <model.so> <top-module>\n");
    return 1;
  }
  const char* so_name  = argv[1];
  const char* top_name = argv[2];

  setvbuf(stdout, NULL, _IOLBF, 0);

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

  tNewModelFn new_model   = (tNewModelFn) find_sym(dl, new_model_name);
  tBytesFn    state_bytes = (tBytesFn)    find_sym(dl, "bk_state_bytes");
  tBytesFn    in_bytes    = (tBytesFn)    find_sym(dl, "bk_input_bytes");
  tBytesFn    out_bytes   = (tBytesFn)    find_sym(dl, "bk_output_bytes");
  tMaxDepthFn max_depth   = (tMaxDepthFn) find_sym(dl, "bk_max_event_queue_depth");
  tCtxBytesFn ctx_bytes   = (tCtxBytesFn) find_sym(dl, "bk_context_bytes");
  tSyncInitFn sync_init   = (tSyncInitFn) find_sym(dl, "bk_sync_init");
  tGetClockFn get_clock   = (tGetClockFn) find_sym(dl, "bk_get_clock_by_name");
  tSyncStepFn sync_step   = (tSyncStepFn) find_sym(dl, "bk_sync_step");
  tShutdownFn shutdown_fn = (tShutdownFn) find_sym(dl, "bk_shutdown");

  g_num_in  = (tCountFn)   find_sym(dl, "bk_num_input_ports");
  g_get_in  = (tGetPortFn) find_sym(dl, "bk_get_input_port");
  g_num_out = (tCountFn)   find_sym(dl, "bk_num_output_ports");
  g_get_out = (tGetPortFn) find_sym(dl, "bk_get_output_port");

  /* sizing call, then allocate and bind the real storage */
  tModel model = new_model(NULL, NULL, NULL, NULL, NULL);
  if (model == NULL)
  {
    fprintf(stderr, "harness: %s returned NULL\n", new_model_name);
    return 1;
  }
  g_model = model;

  void* state_buf = malloc(state_bytes(model));
  g_in_buf  = (unsigned char*) ((in_bytes(model) > 0)
                                    ? malloc(in_bytes(model)) : NULL);
  g_out_buf = (unsigned char*) ((out_bytes(model) > 0)
                                    ? malloc(out_bytes(model)) : NULL);
  check(g_in_buf != NULL, "design has input ports");
  check(g_out_buf != NULL, "design has output ports");

  /* pre-fill the buffers so 'initialized by construction' below is
   * a real observation, not leftover calloc zeros
   */
  memset(g_in_buf, 0xee, in_bytes(model));
  memset(g_out_buf, 0xee, out_bytes(model));

  model = new_model(bs_default_host_ops(), bs_default_host_ctx(),
                    state_buf, g_in_buf, g_out_buf);
  check(model == g_model, "rebinding returns the same handle");

  tUInt32 capacity = max_depth(model) + 16;
  void* ctx_buf = malloc(ctx_bytes(capacity));
  tSimStateHdl sim = sync_init(model, 1,
                               bs_default_host_ops(), bs_default_host_ctx(),
                               capacity, ctx_buf);
  if (sim == NULL)
  {
    fprintf(stderr, "harness: bk_sync_init failed\n");
    return 1;
  }

  /* construction writes every input port once: enables (and the
   * other inputs) start at 0, overwriting the 0xee fill
   */
  check(read_narrow(1, "EN_push") == 0, "EN_push initialized to 0");
  check(read_narrow(1, "EN_grab") == 0, "EN_grab initialized to 0");
  check(read_narrow(1, "push_x") == 0, "push_x initialized to 0");
  check(read_narrow(1, "push_y") == 0, "push_y initialized to 0");

  tClock clk = get_clock(sim, "CLK");
  check(clk != BAD_CLOCK_HANDLE, "design has a CLK clock");

  /* run through reset until the design is ready for a push */
  int settled = 0;
  for (int i = 0; i < 8 && !settled; ++i)
  {
    sync_step(sim, clk);
    settled = (read_narrow(0, "RDY_push") == 1);
  }
  check(settled, "RDY_push rises after reset");
  check(read_narrow(0, "RDY_headv") == 0, "empty: RDY_headv is 0");
  check(read_narrow(0, "RDY_suml") == 0, "empty: RDY_suml is 0");
  check(read_narrow(0, "RDY_grab") == 0, "empty: RDY_grab is 0");
  check(read_narrow(0, "RDY_widev") == 1, "RDY_widev is always 1");
  check(read_narrow(0, "RDY_echo") == 1, "RDY_echo is always 1");
  printf("reset done, RDY_push=1\n");

  /* the always-ready combinational method tracks its input-port
   * argument cycle by cycle (it appears in no schedule; its call is
   * appended to every edge)
   */
  write_narrow("echo_v", 17);
  sync_step(sim, clk);
  check(read_narrow(0, "echo") == 18, "echo(17) == 18");
  write_narrow("echo_v", 41);
  sync_step(sim, clk);
  check(read_narrow(0, "echo") == 42, "echo(41) == 42");
  printf("echo tracks its argument\n");

  /* drive an enabled action method: arguments (narrow 8-bit, wide
   * 100-bit, narrow 48-bit) plus its enable, all through the input
   * buffer
   */
  static const tUInt32 w_pattern[4] =
      { 0x00000005u, 0x11111111u, 0x22222222u, 0x0000000fu };
  write_narrow("push_x", 0xab);
  write_narrow("push_y", 0x123456789abcull);
  memcpy(wide_words(1, "push_w"), w_pattern, sizeof(w_pattern));
  write_narrow("EN_push", 1);
  sync_step(sim, clk);
  write_narrow("EN_push", 0);

  /* the value methods now see the pushed state */
  check(read_narrow(0, "RDY_push") == 0, "full: RDY_push is 0");
  check(read_narrow(0, "RDY_headv") == 1, "full: RDY_headv is 1");
  check(read_narrow(0, "headv") == 0xac, "headv == pushed x + 1");
  check(read_narrow(0, "RDY_suml") == 1, "full: RDY_suml is 1");
  check(read_narrow(0, "suml") == 0x123456789ac1ull,
        "suml == pushed y + 5");
  printf("push observed through narrow results\n");

  /* wide round trip: widev = pushed w + widev_sel */
  write_narrow("widev_sel", 3);
  sync_step(sim, clk);
  {
    const tUInt32* out = wide_words(0, "widev");
    check(out[0] == 0x00000008u, "widev word 0 == w[0] + 3");
    check(out[1] == 0x11111111u, "widev word 1 == w[1]");
    check(out[2] == 0x22222222u, "widev word 2 == w[2]");
    check(out[3] == 0x0000000fu, "widev word 3 == w[3]");
  }
  printf("wide argument and result round-trip\n");

  /* ActionValue: acts (clears full) and returns a result */
  write_narrow("grab_a", 0x5555);
  write_narrow("EN_grab", 1);
  sync_step(sim, clk);
  write_narrow("EN_grab", 0);
  check(read_narrow(0, "grab") == (0x00ab ^ 0x5555),
        "grab result == zeroExtend(x) ^ a");
  check(read_narrow(0, "RDY_push") == 1, "grab cleared full");
  check(read_narrow(0, "RDY_grab") == 0, "empty again: RDY_grab is 0");
  printf("ActionValue acts and returns through the buffers\n");

  shutdown_fn(sim);
  free(ctx_buf);
  free(state_buf);
  free(g_in_buf);
  free(g_out_buf);

  if (failures > 0)
  {
    fprintf(stderr, "harness: %u checks failed\n", failures);
    return 1;
  }
  printf("all checks passed\n");
  return 0;
}

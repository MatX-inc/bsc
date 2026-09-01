#ifndef __BLUESIM_INTROSPECTION_H__
#define __BLUESIM_INTROSPECTION_H__

#include "bluesim_types.h"

/*
 * Non-allocating introspection of a Bluesim model.
 *
 * A generated Bluesim design carries three static, code-generation
 * time descriptor tables: one for its state elements (the Bluesim
 * primitive instances of the whole module tree) and one each for the
 * input and output ports of the top module.  The tables are stored
 * as 'static const' data in the generated code; walking them
 * allocates nothing and needs only the model handle from
 * new_MODEL_*(), so a host can size and inspect a design before
 * bk_sync_init() (see the bk_num_* and bk_get_* walk functions in
 * bluesim_kernel_api.h).
 *
 * Alongside the descriptors, the code generator defines a flat
 * layout: every state element is assigned a byte offset within a
 * planned contiguous state area, and every top-module input (output)
 * port a byte offset within a planned contiguous input (output)
 * area.  The three areas are independent; each starts at offset 0.
 * In this version of the runtime the layout is DESCRIPTIVE only --
 * construction does not yet place storage at these offsets -- but
 * the layout is fixed per design and hosts may size buffers from it.
 *
 * Layout rules (identical for all three areas):
 *
 *  - Every element stores 'entries' entries of 'bits' bits each
 *    (ports always have entries == 1).  One entry occupies its
 *    storage unit:
 *        bits <=  8:  1 byte
 *        bits <= 32:  4 bytes
 *        bits <= 64:  8 bytes
 *        bits >  64:  4 * ceil(bits/32) bytes (an array of 32-bit
 *                     words, as Bluesim wide data)
 *    and the element's size is entries * unit.
 *
 *  - The element's alignment is the size of its storage unit (1, 4
 *    or 8 bytes), except wide (> 64 bit) elements, which are aligned
 *    to 4 bytes (word array).
 *
 *  - Offsets are assigned by walking the elements in table order
 *    with a running offset that starts at 0: round the offset up to
 *    the element's alignment, assign it, advance by the element's
 *    size.  Elements therefore never overlap, and the table is
 *    sorted by offset.
 *
 *  - The total byte size of an area is the final running offset
 *    rounded up to a multiple of 8, so an area can itself be placed
 *    at any 8-byte-aligned address (e.g. from malloc) with every
 *    element correctly aligned.
 *
 * Table ordering (deterministic per design):
 *
 *  - State elements appear in a depth-first pre-order walk of the
 *    module instance tree starting at the top module, taking the
 *    sub-instances of each module in the order the code generator
 *    records them (alphabetical by instance name within a module).
 *    Names are full dotted instance names rooted at "top" (the
 *    runtime name of the top-module instance), e.g.
 *    "top.sub.theReg".
 *
 *  - Input ports are the top module's argument ports (in declaration
 *    order) followed by its method enable and argument ports in
 *    ascending case-sensitive name order.  Output ports are the top
 *    module's method result ports (value-method and ActionValue
 *    results, including the RDY_* ready results of separately
 *    generated ready methods) in ascending case-sensitive name
 *    order.  Port names are the Verilog port names (e.g. "EN_push",
 *    "push_x", "RDY_first").
 *
 *  - Clock and reset ports of the top module appear in NEITHER area:
 *    clocks are driven through the kernel's clock machinery
 *    (bk_trigger_clock_edge() and friends) and resets through
 *    reset_model()/bk_use_default_reset(), not through port storage.
 */

#if __cplusplus
extern "C" {
#endif

/* The kind of a state element: which Bluesim primitive family
 * implements it.  The values are fixed and new kinds are only ever
 * appended.
 */
typedef enum
{
  /* register: RegN/RegUN/RegA, ConfigReg*, RegTwo*, CReg*,
   * CrossingReg*, RevertReg, RegAligned (bs_prim_mod_reg.h) */
  BK_STATE_REG     = 0,
  /* wire: RWire(0), BypassWire(0), the crossing wires
   * (bs_prim_mod_wire.h) */
  BK_STATE_WIRE    = 1,
  /* register file: RegFile, RegFileLoad and DualPortRam;
   * 'entries' is the number of addressable entries
   * (bs_prim_mod_regfile.h, MOD_DualPortRam) */
  BK_STATE_REGFILE = 2,
  /* block RAM: the BRAM1/BRAM2 families; 'entries' is the memory
   * size (bs_prim_mod_bram.h) */
  BK_STATE_BRAM    = 3,
  /* FIFO: the FIFO/SizedFIFO/FIFOL families and the SyncFIFO
   * families; 'entries' is the FIFO depth (bs_prim_mod_fifo.h,
   * MOD_SyncFIFO) */
  BK_STATE_FIFO    = 4,
  /* probe: Probe and ProbeWire (bs_prim_mod_probe.h) */
  BK_STATE_PROBE   = 5,
  /* counter: Counter (bs_prim_mod_counter.h) */
  BK_STATE_COUNTER = 6,
  /* clock-domain synchronizer: SyncBit*, SyncPulse, SyncHandshake,
   * SyncRegister, LatchCrossingReg (bs_prim_mod_synchronizers.h) */
  BK_STATE_SYNC    = 7,
  /* clock generator/manipulator: ClockGen, MakeClock, GatedClock,
   * ClockInverter, ClockDiv, ClockSelect, ClockMux and their
   * (un)gated variants; 'bits' and 'entries' are nominally 1
   * (bs_prim_mod_clockgen.h, bs_prim_mod_gatedclock.h,
   * bs_prim_mod_clockmux.h) */
  BK_STATE_CLOCK   = 8,
  /* reset generator/manipulator: MakeReset(A/0), SyncReset(A/0),
   * InitialReset, ResetMux, ResetEither, ResetToBool; 'bits' and
   * 'entries' are nominally 1 (bs_prim_mod_resets.h) */
  BK_STATE_RESET   = 9
} tBkStateKind;

/* Descriptor of one state element.  Returned pointers point at
 * 'static const' storage inside the generated model: they are NOT
 * owned by the caller (never free them) and remain valid for the
 * lifetime of the loaded model.
 */
typedef struct
{
  const char*  name;    /* full dotted instance name, e.g. "top.sub.theReg" */
  tBkStateKind kind;    /* primitive family */
  tUInt32      bits;    /* bit width of one entry */
  tUInt64      entries; /* number of entries (1 unless a regfile/BRAM/FIFO) */
  tUInt64      offset;  /* byte offset within the planned state area */
  tUInt64      size;    /* byte size within the planned state area */
} tBkStateInfo;

/* Descriptor of one top-module input or output port.  Ownership and
 * lifetime are as for tBkStateInfo.
 */
typedef struct
{
  const char* name;    /* Verilog port name, e.g. "EN_push" or "push_x" */
  tUInt32     bits;    /* bit width of the port */
  tUInt64     offset;  /* byte offset within its (input or output) area */
  tUInt64     size;    /* byte size within its (input or output) area */
} tBkPortInfo;

#if __cplusplus
} /* extern "C" */
#endif

#endif /* __BLUESIM_INTROSPECTION_H__ */

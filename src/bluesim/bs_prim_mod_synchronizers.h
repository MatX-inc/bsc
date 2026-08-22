#ifndef __BS_PRIM_MOD_SYNCHRONIZERS_H__
#define __BS_PRIM_MOD_SYNCHRONIZERS_H__

#include "bluesim_kernel_api.h"
#include "bs_module.h"
#include "bs_wide_data.h"
#include "bs_prim_storage.h"

/* aux storage words the wide synchronizer families need for their
 * secondary values (see bs_prim_storage.h) */
#define BS_SYNCREG_AUX_WORDS(b)   (3u * BS_AUX_WORDS(b)) /* SyncVar x2, reset */
#define BS_SYNCFIFO_AUX_WORDS(b)  (1u * BS_AUX_WORDS(b)) /* dDoutReg */
#define BS_DPRAM_AUX_WORDS(b)     (1u * BS_AUX_WORDS(b)) /* prev_value */
#define BS_LATCHXREG_AUX_WORDS(b) (3u * BS_AUX_WORDS(b)) /* latch, prev, reset */

// This is a helper class we use manage race conditions at
// clock domain crossings.  A SyncVar allows us to read a
// variable in one domain that was written from a different
// domain at the same time, and see the previous value.
// This mimics the Verilog behavior of a non-blocking write
// that updates at the of the simulation cycle.
template<typename T>
class SyncVar
{
 public:
  // 'aux_io' supplies (and advances over) auxiliary word storage for
  // wide values; a narrow SyncVar ignores it and may pass NULL
  SyncVar(tSimStateHdl simHdl, unsigned int width,
          unsigned int** aux_io = NULL)
    : sim_hdl(simHdl), bits(width)
  {
    if (aux_io != NULL)
    {
      bs_bind_aux(prev_value, aux_io, bits);
      bs_bind_aux(value, aux_io, bits);
    }
    write_undet(&prev_value, bits);
    write_undet(&value, bits);
    written_at = ~bk_now(sim_hdl);
  }
 public:
  const T& read() const
  {
    if (bk_is_same_time(sim_hdl, written_at))
      return prev_value;
    else
      return value;
  }
  const T& probe() const { return value; }
  void write(const T& x)
  {
    prev_value = value;
    value = x;
    written_at = bk_now(sim_hdl);
  }
  void force(const T& x)
  {
    prev_value = x;
    value = x;
  }
 public:
  SyncVar<T>& operator=(const SyncVar<T>& sv)
  {
    prev_value = sv.prev_value;
    value = sv.value;
    written_at = sv.written_at;
    return (*this);
  }
 private:
  tSimStateHdl sim_hdl;
  const unsigned int bits;
  T prev_value;
  T value;
  tTime written_at;
};

// Simple uni-directional synchronizers

// This is the definition used for 1-bit synchronizer primitives
// with a 2-destination-clock-cycle delay.
class MOD_Sync2 : public Module
{
 public:
  MOD_Sync2(tSimStateHdl simHdl, const char* name, Module* parent,
            tStateLayout* sto, tUInt8 v)
    : Module(simHdl, name, parent),
      dSyncReg2(*(tUInt8*)sto->claim()),
      sSyncReg(simHdl, 1), reset_value(v)
  {
    write_undet(&dSyncReg1, 1);
    write_undet(&dSyncReg2, 1);
    in_reset = false;
  }
 public:
  const tUInt8 METH_read() const  { return dSyncReg2; }

  void METH_send(tUInt8 x) { if (!in_reset) sSyncReg.write(x); }

  void clk_dst(tUInt8 /* clock_value */, tUInt8 gate_value = 1)
  {
    if (gate_value == 0) return;

    dSyncReg2 = dSyncReg1;
    dSyncReg1 = sSyncReg.read();
  }

  void reset_sRST(tUInt8 rst_in)
  {
    in_reset = (rst_in == 0);
    if (in_reset)
    {
      dSyncReg1 = dSyncReg2 = reset_value;
      sSyncReg.force(reset_value);
    }
  }
  void rst_tick_clk_src(tUInt8 /* clock_gate */) { /* unused */ }
 public:
 private:
  tUInt8 dSyncReg1;
  tUInt8& dSyncReg2;      // the published element value
  SyncVar<tUInt8> sSyncReg;
  tUInt8 reset_value;
  bool in_reset;
};

// This is the definition used for synchronizer primitives
// with a 1.5-destination-clock-cycle delay.
class MOD_Sync15 : public Module
{
 public:
  MOD_Sync15(tSimStateHdl simHdl, const char* name, Module* parent,
             tStateLayout* sto, tUInt8 v)
    : Module(simHdl, name, parent),
      dSyncReg2(*(tUInt8*)sto->claim()),
      sSyncReg(simHdl, 1), reset_value(v)
  {
    write_undet(&dSyncReg1, 1);
    write_undet(&dSyncReg2, 1);
    in_reset = false;
  }
 public:
  const tUInt8 METH_read() const  { return dSyncReg2; }
  void METH_send(tUInt8 x) { if (!in_reset) sSyncReg.write(x); }
  void clk_dst(tUInt8 clock_value, tUInt8 gate_value = 1)
  {
    if (gate_value == 0) return;

    if (clock_value != 0)
      dSyncReg2 = dSyncReg1;
    else
      dSyncReg1 = sSyncReg.read();
  }
  void reset_sRST(tUInt8 rst_in)
  {
    in_reset = (rst_in == 0);
    if (in_reset)
    {
      dSyncReg2 = dSyncReg1 = reset_value;
      sSyncReg.force(reset_value);
    }
  }
  void rst_tick_clk_src(tUInt8 /* clock_gate */) { /* unused */ }
 public:
 private:
  tUInt8 dSyncReg1;
  tUInt8& dSyncReg2;      // the published element value
  SyncVar<tUInt8> sSyncReg;
  tUInt8 reset_value;
  bool in_reset;
};

// This is the definition used for synchronizer primitives
// with a 1-destination-clock-cycle delay.  It is also used
// for 0.5-destination-clock-cycle delay, since the only difference
// is whether clk_dst() is called on posedge or negedge.
class MOD_Sync1 : public Module
{
 public:
  MOD_Sync1(tSimStateHdl simHdl, const char* name, Module* parent,
            tStateLayout* sto, tUInt8 v)
    : Module(simHdl, name, parent),
      dSyncReg1(*(tUInt8*)sto->claim()),
      sSyncReg(simHdl, 1), reset_value(v)
  {
    write_undet(&dSyncReg1, 1);
    in_reset = false;
  }
 public:
  const tUInt8 METH_read() const  { return dSyncReg1; }

  void METH_send(tUInt8 x) { if (!in_reset) sSyncReg.write(x); }

  void clk_dst(tUInt8 /* clock_value */, tUInt8 gate_value = 1)
  {
    if (gate_value == 0) return;

    dSyncReg1 = sSyncReg.read();
  }

  void reset_sRST(tUInt8 rst_in)
  {
    in_reset = (rst_in == 0);
    if (in_reset)
    {
      dSyncReg1 = reset_value;
      sSyncReg.force(reset_value);
    }
  }
  void rst_tick_clk_src(tUInt8 /* clock_gate */) { /* unused */ }
 public:
 private:
  tUInt8& dSyncReg1;      // the published element value
  SyncVar<tUInt8> sSyncReg;
  tUInt8 reset_value;
  bool in_reset;
};

// Pulse synchronizers

// A pulse synchronizer is a uni-directional synchronizer
// based on transmitting a pulse across clock domains.
class MOD_SyncPulse : public Module
{
 public:
  MOD_SyncPulse(tSimStateHdl simHdl, const char* name, Module* parent,
                tStateLayout* sto)
    : Module(simHdl, name, parent),
      dSyncReg2(*(tUInt8*)sto->claim()),
      sSyncReg(simHdl, 1)
  {
    write_undet(&dSyncReg1, 1);
    write_undet(&dSyncReg2, 1);
    write_undet(&dSyncPulse, 1);
    in_reset = false;
  }
 public:
  tUInt8 METH_pulse() const  { return (dSyncReg2 ^ dSyncPulse) ? 0x1 : 0x0; }
  void METH_send()
  {
    if (!in_reset)
      sSyncReg.write((sSyncReg.read() == 0) ? 0x1 : 0x0);
  }
  void clk_dst(tUInt8 /* clock_value */, tUInt8 gate_value = 1)
  {
    if (gate_value == 0) return;

    dSyncPulse = dSyncReg2;
    dSyncReg2 = dSyncReg1;
    dSyncReg1 = sSyncReg.read();
  }
  void reset_sRST(tUInt8 rst_in)
  {
    in_reset = (rst_in == 0);
    if (in_reset)
    {
      dSyncPulse = dSyncReg2 = dSyncReg1 = 0;
      sSyncReg.force(0);
    }
  }
  void rst_tick_clk_src(tUInt8 /* clock_gate */) { /* unused */ }
 public:
 private:
  tUInt8 dSyncReg1;
  tUInt8& dSyncReg2;      // the published element value
  tUInt8 dSyncPulse;
  SyncVar<tUInt8> sSyncReg;
  bool in_reset;
};

// Handshaking synchronizers

// A handshake synchronizer is a pulse synchronizer which feeds
// synchronization information back from the destination domain
// to ensure that the next pulse cannot be transmitted until the
// previous pulse has become visible in the destination domain.
class MOD_SyncHandshake : public Module
{
 public:
  // 'sto' is the element-storage cursor when this instance is a
  // published state element; an embedded instance (inside SyncReg or
  // SyncFIFO) passes NULL and keeps its ready bit in the object
  MOD_SyncHandshake(tSimStateHdl simHdl, const char* name, Module* parent
                    ,tStateLayout* sto = NULL
                    ,bool init = 0, bool delayreturn = false)
    : Module(simHdl, name, parent),
      dSyncReg2(simHdl, 1)
    , dLastState(simHdl, 1)
    , sToggleReg(simHdl, 1)
    , sRDY(sto ? *(tUInt8*)sto->claim() : sRDY_stg)
    , param_init(init)
    , param_delayreturn(delayreturn)
    , __clk_handle_0(BAD_CLOCK_HANDLE)
    , __clk_handle_1(BAD_CLOCK_HANDLE)
  {
    write_undet(&sSyncReg1, 1);
    write_undet(&sSyncReg2, 1);
    sRDY = 0;

    init_val(dSyncReg1, 1);
    write_undet(&dSyncReg1, 1);

    sSyncReg1 = 1;
    sSyncReg2 = 1;

    en = false;
    did_send = false;
    pulsing = false;

    in_reset = false;
  }
 public:
  tUInt8 METH_pulse() const  { return (dSyncReg2.read() != dLastState.read()) ? 0x1 : 0x0; }
  void METH_send() { en = true; }
  bool METH_RDY_send() const { return (!in_reset && (sRDY != 0)); }
  void clk_src(tUInt8 /* clock_value */, tUInt8 gate_value = 1)
  {
    if (gate_value == 0) return;

    if (!in_reset) {
      sSyncReg2 = sSyncReg1;
      sSyncReg1 = param_delayreturn ? dLastState.read() : dSyncReg2.read();
    }

    if (en)
    {
      sToggleReg.write((sToggleReg.read() == 0) ? 0x1 : 0x0);
      sRDY = 0;
    }
    else
      sRDY = (sSyncReg2 == sToggleReg.read());

    did_send = en;
    en = false;
  }
  void clk_dst(tUInt8 /* clock_value */, tUInt8 gate_value = 1)
  {
    if (gate_value == 0) return;

    dLastState.write (dSyncReg2.read());
    dSyncReg2.write(dSyncReg1);
    dSyncReg1 = sToggleReg.read();
    pulsing = dLastState.probe() != dSyncReg2.probe();
  }

  void set_clk_0(const char* s)
  {
    __clk_handle_0 = bk_get_or_define_clock(sim_hdl, s);
  }

  void set_clk_1(const char* s)
  {
    __clk_handle_1 = bk_get_or_define_clock(sim_hdl, s);
  }

  void reset_sRST(tUInt8 rst_in)
  {
    in_reset = (rst_in == 0);
    if (in_reset)
    {
      dSyncReg2.force(param_init ? 1 : 0);
      sToggleReg.force(param_init ? 1 : 0);
      dSyncReg1 = param_init ? 1 : 0;
      dLastState.force (param_init ? 1 : 0);
      sSyncReg1 = param_init ? 0 : 1; /* ! init */
      sSyncReg2 = param_init ? 0 : 1; /* ! init */
      sRDY = 0;
      en = false;
      pulsing = false;
    }
  }
  void rst_tick_clk_src(tUInt8 /* clock_gate */) { /* unused */ }
 public:
 private:
  tUInt8 dSyncReg1;
  SyncVar<tUInt8> dSyncReg2;
  SyncVar<tUInt8> dLastState;
  SyncVar<tUInt8> sToggleReg;
  tUInt8 sSyncReg1;
  tUInt8 sSyncReg2;
  tUInt8 sRDY_stg;        // storage for an embedded (non-element) instance
  tUInt8& sRDY;           // the published element value when claimed
  bool en;
  bool in_reset;
  bool pulsing;
  bool param_init;
  bool param_delayreturn;

  tClock __clk_handle_0; // sCLK
  tClock __clk_handle_1; // dCLK
  bool did_send;
};

// Synchronized registers

template<typename T>
class MOD_SyncReg : public Module
{
 public:
  MOD_SyncReg(tSimStateHdl simHdl, const char* name, Module* parent,
	      tStateLayout* sto, unsigned int* aux,
	      unsigned int width, const T& v)
    : Module(simHdl, name, parent), sDataSyncIn(simHdl, width, &aux),
      dD_OUT(bs_bind_elem(dD_OUT_stg_, sto->claim(), width)),
      sync(simHdl, "sync", this, NULL, false, true), bits(width)
  {
    bs_bind_aux(reset_value, &aux, bits);
    reset_value = v;
    write_undet(&dD_OUT, bits);
    in_reset = false;
  }
  MOD_SyncReg(tSimStateHdl simHdl, const char* name, Module* parent,
	      tStateLayout* sto, unsigned int* aux,
	      unsigned int width)
    : Module(simHdl, name, parent), sDataSyncIn(simHdl, width, &aux),
      dD_OUT(bs_bind_elem(dD_OUT_stg_, sto->claim(), width)),
      sync(simHdl, "sync", this), bits(width)
  {
    bs_bind_aux(reset_value, &aux, bits);
    write_undet(&dD_OUT, bits);
    write_undet(&reset_value, bits);
    in_reset = false;
  }
 public:
  const T& METH_read() const  { return dD_OUT; }
  bool METH_RDY_write() const { return sync.METH_RDY_send(); }
  void METH_write(const T& x)
  {
    sDataSyncIn.write(x);
    sync.METH_send();
  }
  void clk_src(tUInt8 clock_value, tUInt8 gate_value = 1)
  {
    if (gate_value == 0) return;

    sync.clk_src(clock_value, gate_value);
  }
  void clk_dst(tUInt8 clock_value, tUInt8 gate_value = 1)
  {
    if (gate_value == 0) return;

    if (sync.METH_pulse())
      dD_OUT = sDataSyncIn.read();
    sync.clk_dst(clock_value, gate_value);
  }
  void reset_sRST(tUInt8 rst_in)
  {
    in_reset = (rst_in == 0);
    sync.reset_sRST(rst_in);
    if (in_reset)
    {
      sDataSyncIn.force(reset_value);
      dD_OUT = reset_value;
    }
  }
  void rst_tick_clk_src(tUInt8 /* clock_gate */) { /* unused */ }
 public:
 private:
  SyncVar<T> sDataSyncIn; // aux-bound when wide
  T dD_OUT_stg_;          // wide: the view object behind 'dD_OUT'
  T& dD_OUT;              // the live element value, in caller storage
  T reset_value;          // aux-bound when wide
  MOD_SyncHandshake sync;
  bool in_reset;
  const unsigned int bits;
};


// Synchronized FIFOs

template<typename T, typename I>
const unsigned int* index_fn_syncfifo(void* base, tUInt64 addr);

static unsigned int index_size(unsigned int d)
{
    unsigned int sz = 0;
    while (d != 0)
    {
      ++sz;
      d = d >> 1;
    }
    return sz;
}

template<typename T, typename I>
class MOD_SyncFIFO : public Module
{
  // embedded symbol-table storage (bound to Module::symbols;
  // symbol tables never allocate)
 private:
  tSym __symbols[3];
 public:
  MOD_SyncFIFO(tSimStateHdl simHdl, const char* name, Module* parent,
	       tStateLayout* sto, unsigned int* aux,
	       unsigned int width, unsigned int depth, unsigned int hasClr)
    : Module(simHdl, name, parent), width(width), depth(depth),
      src_hi(simHdl, index_size(depth)+1), dst_lo(simHdl, index_size(depth)+1),
      hasClear(hasClr),
      sClrSync(simHdl, "sClrSync", this), dClrSync(simHdl, "dClrSync", this)
  {
    data.bind(sto->claim(), width);
    data.init_undet(depth);
    bs_bind_aux(dDoutReg, &aux, width);
    write_undet(&dDoutReg, width);

    idx_bits = index_size(depth);
    mask = (1 << idx_bits) - 1;

    src_lo = 0;
    dst_hi = 0;
    src_hi_plus_1 = 1;
    dst_lo_plus_1 = 1;
    dSyncReg1 = 0;
    sSyncReg1 = 0;

    init_val(sCountReg, idx_bits);
    sCountReg = 0;
    init_val(dCountReg, idx_bits);
    dCountReg = 0;

    not_empty = false;
    not_full = true;
    in_reset = false;
    s_reset = false;
    d_reset = false;
    did_enq = false;
    did_deq = false;
    did_sclear = false;
    did_dclear = false;

    symbol_count = 3;
    symbols = __symbols;

    range.lo = 0;
    range.hi = depth - 1;
    range.base = (void*) this;
    range.fetch = index_fn_syncfifo<T,I>;

    symbols[0].key = "";
    symbols[0].info = SYM_RANGE | width << 4;
    symbols[0].value = (void*)(&range);

    symbols[1].key = "depth";
    symbols[1].info = SYM_PARAM | (8*sizeof(unsigned int)) << 4;
    symbols[1].value = (void*)(&depth);

    symbols[2].key = "level";
    symbols[2].info = SYM_DEF | idx_bits << 4;
    symbols[2].value = (void*)(&dCountReg);
  }
 public:
  bool METH_notEmpty()
  {
    // true when non-empty and not in reset
    return (!d_reset && ((depth != 1) ? not_empty : dst_hi != dst_lo.probe()));
    //    return (!d_reset && not_empty);
  }
  // support the alternate naming used by the SyncFIFOLevel import-BVI
  bool METH_dNotEmpty()
  {
    return METH_notEmpty();
  }
  bool METH_RDY_first()
  {
    return METH_notEmpty();
  }
  T METH_first()
  {
    return bs_value_view(dDoutReg, width);
  }
  bool METH_RDY_deq()
  {
    return METH_notEmpty();
  }
  void METH_deq()
  {
    did_deq = true;
  }
  bool METH_notFull()
  {
    // true when non-full and not in reset
    // (note: depth assumed to be power of 2)
    return (!s_reset && not_full);
  }
  // support the alternate naming used by the SyncFIFOLevel import-BVI
  bool METH_sNotFull()
  {
    return METH_notFull();
  }
  bool METH_RDY_enq()
  {
    return METH_notFull();
  }
  void METH_enq(const T& x)
  {
    if (depth == 1)
      dDoutReg = x;
    data.put(src_hi.read() % depth, x);
    did_enq = true;
  }
  // For zero-width variants
  void METH_enq()
  {
    did_enq = true;
  }
  I METH_sCount()
  {
    return sCountReg;
  }
  I METH_dCount()
  {
    return dCountReg;
  }
  bool METH_RDY_sClear()
  {
    return sClrSync.METH_RDY_send();
  }
  void METH_sClear()
  {
    sClrSync.METH_send();
    did_sclear = true;
  }
  bool METH_RDY_dClear()
  {
    return dClrSync.METH_RDY_send();
  }
  void METH_dClear()
  {
    dClrSync.METH_send();
    did_dclear = true;
  }

  void clk_src(tUInt8 clock_value, tUInt8 gate_value = 1)
  {
    if (gate_value == 0) return;

    // update source-side reset
    s_reset = in_reset;

    // update not full reg and count
    if (s_reset ||
        (hasClear && (did_sclear || !sClrSync.METH_RDY_send() || dClrSync.METH_pulse())))
    {
      src_hi.force(0);
      src_hi_plus_1 = 1;
      not_full = false;
      sCountReg = 0;
    }
    else if (did_enq)
    {
      not_full = ((src_hi_plus_1 ^ depth) != src_lo);
      if (src_hi_plus_1 > src_lo)
	sCountReg = src_hi_plus_1 - src_lo;
      else
	sCountReg = (src_hi_plus_1 + (2*depth) - src_lo) & mask;
      src_hi.write(src_hi_plus_1);
      src_hi_plus_1 = (src_hi_plus_1 + 1) % (2*depth);
    }
    else
    {
      not_full = ((src_hi.read() ^ depth) != src_lo);
      if (src_hi.read() > src_lo)
	sCountReg = src_hi.read() - src_lo;
      else
	sCountReg = (src_hi.read() + (2*depth) - src_lo) & mask;
    }
    did_sclear = false;
    did_enq = false;

    // synchronize index from destination side
    src_lo = sSyncReg1;
    sSyncReg1 = dst_lo.read();

    if (depth == 1) {
      not_full = src_hi.probe() == src_lo;
      sCountReg = not_full ? 0 : 1;
    }

    sClrSync.clk_src(clock_value, gate_value);
    dClrSync.clk_dst(clock_value, gate_value);
  }
  void clk_dst(tUInt8 clock_value, tUInt8 gate_value = 1)
  {
    if (gate_value == 0) return;

    // update destination-side reset
    d_reset = in_reset;

    // update not empty reg and count
    if (d_reset ||
        (hasClear && (did_dclear || !dClrSync.METH_RDY_send() || sClrSync.METH_pulse())))
    {
      dst_lo.force(0);
      dst_lo_plus_1 = 1;
      not_empty = false;
      dCountReg = 0;
    }
    else if (did_deq)
    {
      not_empty = (dst_hi != dst_lo.read());
      if (dst_hi > dst_lo_plus_1)
	dCountReg = dst_hi - dst_lo.read();
      else
	dCountReg = (dst_hi + (2*depth) - dst_lo.read()) & mask;
      //not_empty = dCountReg != 0;
      if (not_empty) {
        if (depth != 1)
          dDoutReg = data.get(dst_lo.read() % depth);
        dst_lo.write(dst_lo_plus_1);
        dst_lo_plus_1 = (dst_lo_plus_1 + 1) % (2*depth);
      }
    }
    else
    {
      if (dst_hi > dst_lo.read())
	dCountReg = dst_hi - dst_lo.read();
      else
	dCountReg = (dst_hi + (2*depth) - dst_lo.read()) & mask;

      if ((depth != 1) && !not_empty && (dst_hi != dst_lo.read())) {
        dDoutReg = data.get(dst_lo.read() % depth);
        dst_lo.write(dst_lo_plus_1);
        dst_lo_plus_1 = (dst_lo_plus_1 + 1) % (2*depth);
        not_empty = true;
      }

    }
    did_dclear = false;
    did_deq = false;

    // synchronize index from source side
    dst_hi = dSyncReg1;
    dSyncReg1 = src_hi.read();

    if (depth == 1) {
      not_empty = dst_lo.probe() == dst_hi;
      dCountReg = not_empty ? 1 : 0;
    }

    sClrSync.clk_dst(clock_value, gate_value);
    dClrSync.clk_src(clock_value, gate_value);
  }
  void reset_sRST(tUInt8 rst_in)
  {
    in_reset = (rst_in == 0);
    sClrSync.reset_sRST(rst_in);
    dClrSync.reset_sRST(rst_in);
    if (in_reset)
    {
      src_lo = dst_hi = 0;
      src_hi.force(0);
      dst_lo.force(0);
      src_hi_plus_1 = dst_lo_plus_1 = 1;
      dSyncReg1 = sSyncReg1 = 0;
      sCountReg = dCountReg = 0;
      s_reset = d_reset = true;
      did_enq = did_deq = false;
      did_sclear = did_dclear = false;
    }
  }
  void rst_tick_clk_src(tUInt8 /* clock_gate */) { /* unused */ }
 private:
  bool occupied(unsigned int idx)
  {
    unsigned int l = dst_lo.probe();
    unsigned int h = src_hi.probe();
    // when l == h the FIFO is empty
    if (l == h)
      return false;
    // map l and h onto data array indexes
    l = l % depth;
    h = h % depth;
    return (l < h) ? (idx >= l && idx < h) : (idx >= l || idx < h);
  }
 public:
  const unsigned int* data_index(tUInt64 addr) const
  {
    if (addr < ((tUInt64) dCountReg))
      return data.sym_value((dst_lo.read() + addr) % depth);
    else
      return NULL;
  }

 private:
  const unsigned int width;
  const unsigned int depth;

  unsigned int idx_bits;
  unsigned int mask;

  tStateArray<T> data;  // flat entries, in caller-provided storage
  T  dDoutReg;           // aux-bound when wide
  unsigned int src_lo;
  SyncVar<unsigned int> src_hi;
  SyncVar<unsigned int> dst_lo;
  unsigned int dst_hi;
  unsigned int dSyncReg1;
  unsigned int sSyncReg1;
  unsigned int src_hi_plus_1;
  unsigned int dst_lo_plus_1;
  I sCountReg;
  I dCountReg;
  bool not_empty;
  bool not_full;
  bool in_reset;
  bool s_reset;
  bool d_reset;
  bool did_enq;
  bool did_deq;
  bool did_sclear;
  bool did_dclear;
  bool hasClear;
  MOD_SyncHandshake sClrSync;
  MOD_SyncHandshake dClrSync;

  // range structure for symbolic access to FIFO data
  Range range;
};

// Function to index into FIFO data array
template<typename T, typename I>
const unsigned int* index_fn_syncfifo(void* base, tUInt64 addr)
{
  MOD_SyncFIFO<T,I>* fifo = (MOD_SyncFIFO<T,I>*) base;
  return fifo->data_index(addr);
}

// Synchronized RAM Model

template<typename AT, typename DT>
class MOD_DualPortRam : public Module
{
 public:
  MOD_DualPortRam(tSimStateHdl simHdl, const char* name, Module* parent,
		  tStateLayout* sto, unsigned int* aux,
		  unsigned int addr_width, unsigned int data_width)
    : Module(simHdl, name, parent), addr_bits(addr_width),
      data_bits(data_width), written_at(~bk_now(sim_hdl))
  {
    nWords = 1llu << addr_width;
    data.bind(sto->claim(), data_bits);
    data.init_undet(nWords);
    init_val(write_addr, addr_bits);
    bs_bind_aux(prev_value, &aux, data_bits);
  }
 public:
  // Note: the read and write methods of a DualPortRam are
  // conflict free.  When the edges coincide and a simultaneous
  // read and write occur the same address, we want the read to
  // return the value from the beginning of the cycle.
  const DT METH_read(const AT& addr) const
  {
    if ((write_addr == addr) && bk_is_same_time(sim_hdl, written_at))
      return bs_value_view(prev_value, data_bits);
    else
      return data.get((tUInt64) addr);
  }
  void METH_write(const AT& addr, const DT& val)
  {
    written_at = bk_now(sim_hdl);
    write_addr = addr;
    prev_value = data.get((tUInt64) addr);
    data.put((tUInt64) addr, val);
  }

 public:
 private:
  tStateArray<DT> data;  // flat entries, in caller-provided storage
  unsigned int addr_bits;
  unsigned int data_bits;
  unsigned long long nWords;
  tTime written_at;
  AT write_addr;
  DT prev_value;         // aux-bound when wide
};


// This is a synchronization primitive that combines a register
// with a latch. The register is written and read in the source
// domain, and its output is latched to be read in the destination
// domain.  The purpose of this primitive is to align data to a shifted
// clock domain.
template<typename T>
class MOD_LatchCrossingReg : public Module
{
 public:
  MOD_LatchCrossingReg(tSimStateHdl simHdl, const char* name, Module* parent,
		       tStateLayout* sto, unsigned int* aux,
		       unsigned int width, const T& v)
    : Module(simHdl, name, parent),
      sFlop(bs_bind_elem(sFlop_stg_, sto->claim(), width)), bits(width)
  {
    bs_bind_aux(dLatch, &aux, width);
    bs_bind_aux(prev_value, &aux, width);
    bs_bind_aux(reset_value, &aux, width);
    reset_value = v;
    write_undet(&dLatch, width);
    write_undet(&sFlop, width);
    write_undet(&prev_value, width);
    writtenAt = ~bk_now(sim_hdl);
    in_reset = false;
    prev_transparent = false;
    transparent = false;
  }
 public:
  const T& METH__read() const { return sFlop; }

  void METH__write(const T& x) {
    if (!in_reset) {
      prev_value = sFlop;
      sFlop = x;
      writtenAt = bk_now(sim_hdl);
      if (transparent) dLatch = x;
    }
  }

  const T& METH_crossed() const {
    if (transparent) {
      if (writtenAt == bk_now(sim_hdl))
	return prev_value;
      else
	return sFlop;
    } else {
      return dLatch;
    }
  }

  void dstClk(tUInt8 clock_value, tUInt8 gate_value = 1)
  {
    prev_transparent = transparent;
    transparent = (gate_value != 0 && clock_value != 0);

    if (transparent) {
      dLatch = sFlop;
    } else if (prev_transparent) {
      if (writtenAt == bk_now(sim_hdl))
	dLatch = prev_value;
      else
	dLatch = sFlop;
    }
  }

  void reset_SRST(tUInt8 rst_in)
  {
    in_reset = (rst_in == 0);
    if (in_reset)
    {
      dLatch = reset_value;
      sFlop = reset_value;
      prev_value = reset_value;
      prev_transparent = false;
      transparent = false;
    }
  }

  void rst_tick_clk(tUInt8 /* clock_gate */) { /* unused */ }
 public:
 private:
  T dLatch;              // aux-bound when wide
  T sFlop_stg_;          // wide: the view object behind 'sFlop'
  T& sFlop;              // the live element value, in caller storage
  T prev_value;          // aux-bound when wide
  T reset_value;         // aux-bound when wide
  unsigned int bits;
  tTime writtenAt;
  bool in_reset;
  bool prev_transparent;
  bool transparent;
};


#endif /* __BS_PRIM_MOD_SYNCHRONIZERS_H__ */

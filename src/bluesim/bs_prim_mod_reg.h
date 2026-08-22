#ifndef __BS_PRIM_MOD_REG_H__
#define __BS_PRIM_MOD_REG_H__

#include "bluesim_kernel_api.h"
#include "bs_module.h"
#include "bs_wide_data.h"
#include "bs_prim_storage.h"
#include "bs_reset.h"

/* aux storage words each register family needs for its wide
 * secondary values (see bs_prim_storage.h; 0 when the width is not
 * wide, in which case the constructors accept a NULL aux pointer) */
#define BS_REG_AUX_WORDS(b)        (2u * BS_AUX_WORDS(b)) /* prev, reset */
#define BS_REGALIGNED_AUX_WORDS(b) (2u * BS_AUX_WORDS(b)) /* reset, next */
#define BS_CONFIGREG_AUX_WORDS(b)  (2u * BS_AUX_WORDS(b)) /* old, reset */
#define BS_REGTWO_AUX_WORDS(b)     (2u * BS_AUX_WORDS(b)) /* old, reset */
#define BS_CREG_AUX_WORDS(b)      (12u * BS_AUX_WORDS(b)) /* reset, rec,
                                                             read[5], write[5] */

#define NO_RESET_REG    0
#define SYNC_RESET_REG  1
#define ASYNC_RESET_REG 2

// This is the definition of the Reg register primitive.  It is used for both
// normal register and clock-crossing registers (so it has duplicate methods
// to cover all the required names and behaviors).
template<typename T>
class MOD_Reg : public Module
{
  // embedded symbol-table storage (bound to Module::symbols;
  // symbol tables never allocate)
 private:
  tSym __symbols[1];
 public:
  // RegN, RegA, CrossingRegN, CrossingRegA
  MOD_Reg(tSimStateHdl simHdl, const char* name, Module* parent,
	  tStateLayout* sto, unsigned int* aux,
	  unsigned int width, const T& v, unsigned int async)
    : Module(simHdl, name, parent),
      value(bs_bind_elem(value_stg_, sto->claim(), width)), bits(width),
      reset_type(async ? ASYNC_RESET_REG : SYNC_RESET_REG)
  {
    bs_bind_aux(prev_value, &aux, bits);
    bs_bind_aux(reset_value, &aux, bits);
    reset_value = v;
    write_undet(&prev_value, bits);
    write_undet(&value, bits);
    written_at = ~bk_now(sim_hdl);

    in_reset = false;
    suppress_write = false;

    symbol_count = 1;
    symbols = __symbols;

    symbols[0].key = "";
    symbols[0].info = SYM_DEF | bits << 4;
    symbols[0].value = (void*)(&value);
  }
  // RevertReg
  MOD_Reg(tSimStateHdl simHdl, const char* name, Module* parent,
	  tStateLayout* sto, unsigned int* aux,
	  unsigned int width, const T& v)
    : Module(simHdl, name, parent),
      value(bs_bind_elem(value_stg_, sto->claim(), width)), bits(width),
      reset_type(NO_RESET_REG)
  {
    bs_bind_aux(prev_value, &aux, bits);
    bs_bind_aux(reset_value, &aux, bits);
    value = v;
    in_reset = false;
    suppress_write = false;

    symbol_count = 1;
    symbols = __symbols;

    symbols[0].key = "";
    symbols[0].info = SYM_DEF | bits << 4;
    symbols[0].value = (void*)(&value);
  }
  // RegUN, CrossingRegUN
  MOD_Reg(tSimStateHdl simHdl, const char* name, Module* parent,
	  tStateLayout* sto, unsigned int* aux,
	  unsigned int width)
    : Module(simHdl, name, parent),
      value(bs_bind_elem(value_stg_, sto->claim(), width)), bits(width),
      reset_type(NO_RESET_REG)
  {
    bs_bind_aux(prev_value, &aux, bits);
    bs_bind_aux(reset_value, &aux, bits);
    write_undet(&prev_value, bits);
    write_undet(&value, bits);
    write_undet(&reset_value, bits);
    written_at = ~bk_now(sim_hdl);

    in_reset = false;
    suppress_write = false;

    symbol_count = 1;
    symbols = __symbols;

    symbols[0].key = "";
    symbols[0].info = SYM_DEF | bits << 4;
    symbols[0].value = (void*)(&value);
  }
  
 public:
  // read method for single-domain register
  const T& METH_read()    const { return value; }
  // read methods for clock-crossing register
  const T& METH__read()   const { return value; }
  const T& METH_crossed() const
  {
    if (bk_is_same_time(sim_hdl, written_at) && !bk_is_combo_sched(sim_hdl))
      return prev_value;
    else
      return value;
  }
  // write method for single-domain register
  void METH_write(const T& x)
  {
    if ((reset_type != ASYNC_RESET_REG) || !suppress_write)
      value = x;
  }
  // write method for clock-crossing register
  void METH__write(const T& x)
  {
    if ((reset_type != ASYNC_RESET_REG) || !suppress_write)
    {
      prev_value = value;
      value = x;
      written_at = bk_now(sim_hdl);
    }
  }
  // clock and reset infrastructure
  void reset_RST(tUInt8 rst_in)
  {
    in_reset = (rst_in == 0);
    if (in_reset)
    {
      if (reset_type == ASYNC_RESET_REG)
	rst_tick__clk__1(1);
      else if (reset_type == SYNC_RESET_REG)
	start_reset_ticks(sim_hdl); /* request rst_tick() on the next clock edge */
    }
    else
    {
      suppress_write = false;
      if (reset_type == SYNC_RESET_REG)
	stop_reset_ticks(sim_hdl); /* stop rst_tick() when reset is not asserted */
    }
  }
  void rst_tick__clk__1(tUInt8 clock_gate)
  {
    if (in_reset && (clock_gate != 0))
    {
      value = reset_value;
      suppress_write = true;
    }
  }
  void rst_tick_sClk(tUInt8 clock_gate)
  {
    if (in_reset && (clock_gate != 0))
    {
      prev_value = value;
      value = reset_value;
      written_at = bk_now(sim_hdl);
      suppress_write = true;
    }
  }
 public:
 // register data members
 private:
  T prev_value;          // aux-bound when wide
  T value_stg_;          // wide: the view object behind 'value'
  T& value;              // the live element value, in caller storage
  const unsigned int bits;
  T reset_value;         // aux-bound when wide
  const unsigned int reset_type;
  tTime written_at;
  bool suppress_write;
  bool in_reset;

};


// This is the definition of the RegAligned register primitive.
// It is the same as Reg, but has different module, clock, etc.
// names, because it is imported differently.
template<typename T>
class MOD_RegAligned : public Module
{
  // embedded symbol-table storage (bound to Module::symbols;
  // symbol tables never allocate)
 private:
  tSym __symbols[1];
 public:
  MOD_RegAligned(tSimStateHdl simHdl, const char* name, Module* parent,
		 tStateLayout* sto, unsigned int* aux,
		 unsigned int width, const T& v, unsigned int async)
    : Module(simHdl, name, parent),
      value(bs_bind_elem(value_stg_, sto->claim(), width)), bits(width),
      reset_type(async ? ASYNC_RESET_REG : SYNC_RESET_REG),
      __clk_handle_0(BAD_CLOCK_HANDLE), __clk_handle_1(BAD_CLOCK_HANDLE),
      written_at(~bk_now(sim_hdl))
  {
    bs_bind_aux(reset_value, &aux, bits);
    bs_bind_aux(next_value, &aux, bits);
    reset_value = v;
    write_undet(&value, bits);
    write_undet(&next_value, bits);
    in_reset = false;
    suppress_write = false;
    did_write = false;

    symbol_count = 1;
    symbols = __symbols;

    symbols[0].key = "";
    symbols[0].info = SYM_DEF | bits << 4;
    symbols[0].value = (void*)(&value);
  }
  MOD_RegAligned(tSimStateHdl simHdl, const char* name, Module* parent,
		 tStateLayout* sto, unsigned int* aux,
		 unsigned int width)
    : Module(simHdl, name, parent),
      value(bs_bind_elem(value_stg_, sto->claim(), width)), bits(width),
      tick_at(~bk_now(sim_hdl)),
      reset_type(NO_RESET_REG), __clk_handle_0(BAD_CLOCK_HANDLE),
      __clk_handle_1(BAD_CLOCK_HANDLE), written_at(~bk_now(sim_hdl))
  {
    bs_bind_aux(reset_value, &aux, bits);
    bs_bind_aux(next_value, &aux, bits);
    write_undet(&value, bits);
    write_undet(&reset_value, bits);
    in_reset = false;
    suppress_write = false;
    did_write = false;

    symbol_count = 1;
    symbols = __symbols;

    symbols[0].key = "";
    symbols[0].info = SYM_DEF | bits << 4;
    symbols[0].value = (void*)(&value);
  }
  
 public:
  const T& METH__read() const  { return value; }
  void METH__write(const T& x)
  {
    if ((reset_type != ASYNC_RESET_REG) || !suppress_write)
    {
      next_value = x;
      if (tick_at == bk_now(sim_hdl))
      {
	value = next_value;
	written_at = bk_now(sim_hdl);
      }
    }
  }

  void realClock(tUInt8 /* clk */, tUInt8 gate = true )
  {
    tick_at = bk_now(sim_hdl);
    if ((reset_type != ASYNC_RESET_REG) || !suppress_write)
      value = next_value;
  }

  void set_clk_0(const char* s)
  {
    __clk_handle_0 = bk_get_or_define_clock(sim_hdl, s);
  }

  void set_clk_1(const char* s)
  {
    __clk_handle_1 = bk_get_or_define_clock(sim_hdl, s);
  }

  void reset_RST(tUInt8 rst_in)
  {
    in_reset = (rst_in == 0);
    if (in_reset)
    {
      if (reset_type == ASYNC_RESET_REG)
	rst_tick_realClock(1);
      else if (reset_type == SYNC_RESET_REG)
	start_reset_ticks(sim_hdl); /* request rst_tick() on the next clock edge */
    }
    else
    {
      suppress_write = false;
      if (reset_type == SYNC_RESET_REG)
	stop_reset_ticks(sim_hdl); /* stop rst_tick() when reset is not asserted */
    }
  }
  void rst_tick_realClock(tUInt8 clock_gate)
  {
    if (in_reset && (clock_gate != 0))
    {
      value = reset_value;
      next_value = reset_value;
      suppress_write = true;
    }
  }
 public:
 // RegAligned data members
 private:
  T value_stg_;          // wide: the view object behind 'value'
  T& value;              // the live element value, in caller storage
  const unsigned int bits;
  T reset_value;         // aux-bound when wide
  T next_value;          // aux-bound when wide
  tTime tick_at;
  const unsigned int reset_type;
  bool suppress_write;
  bool in_reset;

  tClock __clk_handle_0;  // clock for reg updates (realClock)
  tClock __clk_handle_1;  // clock for inputs      (sClkIn)
  bool did_write;
  tTime written_at;

};


// This is the definition of the ConfigReg register primitive.
// It differs from Reg by always returning the value at the beginning
// of the simulation cycle.
template<typename T>
class MOD_ConfigReg : public Module
{
  // embedded symbol-table storage (bound to Module::symbols;
  // symbol tables never allocate)
 private:
  tSym __symbols[1];
 public:
  MOD_ConfigReg(tSimStateHdl simHdl, const char* name, Module* parent,
		tStateLayout* sto, unsigned int* aux,
		unsigned int width, const T& v, unsigned int async)
    : Module(simHdl, name, parent),
      value(bs_bind_elem(value_stg_, sto->claim(), width)), bits(width),
      written(~bk_now(sim_hdl)),
      reset_type(async ? ASYNC_RESET_REG : SYNC_RESET_REG)
  {
    bs_bind_aux(old_value, &aux, bits);
    bs_bind_aux(reset_value, &aux, bits);
    reset_value = v;
    write_undet(&value, bits);
    write_undet(&old_value, bits);
    in_reset = false;
    suppress_write = false;

    symbol_count = 1;
    symbols = __symbols;

    symbols[0].key = "";
    symbols[0].info = SYM_DEF | bits << 4;
    symbols[0].value = (void*)(&value);
  }
  MOD_ConfigReg(tSimStateHdl simHdl, const char* name, Module* parent,
		tStateLayout* sto, unsigned int* aux,
		unsigned int width)
    : Module(simHdl, name, parent),
      value(bs_bind_elem(value_stg_, sto->claim(), width)), bits(width),
      written(~bk_now(sim_hdl)),
      reset_type(NO_RESET_REG)
  {
    bs_bind_aux(old_value, &aux, bits);
    bs_bind_aux(reset_value, &aux, bits);
    write_undet(&value, bits);
    write_undet(&old_value, bits);
    write_undet(&reset_value, bits);
    in_reset = false;
    suppress_write = false;

    symbol_count = 1;
    symbols = __symbols;

    symbols[0].key = "";
    symbols[0].info = SYM_DEF | bits << 4;
    symbols[0].value = (void*)(&value);
  }
  
 public:
  const T& METH_read() const
  {
    if (bk_is_same_time(sim_hdl, written))
      return old_value;
    else
      return value;
  }
  void METH_write(const T& x)
  {
    // suppress writes when async reset is active
    if ((reset_type == ASYNC_RESET_REG) && suppress_write)
      return;

    // only the first write in a cycle should update old_value
    if (written != bk_now(sim_hdl))
    {
      old_value = value;
      written = bk_now(sim_hdl);
    }
    value = x;
  }
  void reset_RST(tUInt8 rst_in)
  {
    in_reset = (rst_in == 0);
    if (in_reset)
    {
      if (reset_type == ASYNC_RESET_REG)
	rst_tick__clk__1(1);
      else if (reset_type == SYNC_RESET_REG)
	start_reset_ticks(sim_hdl); /* request rst_tick() on the next clock edge */
    }
    else
    {
      suppress_write = false;
      if (reset_type == SYNC_RESET_REG)
	stop_reset_ticks(sim_hdl); /* stop rst_tick() when reset is not asserted */
    }
  }
  void rst_tick__clk__1(tUInt8 clock_gate)
  {
    if (in_reset && (clock_gate != 0))
    {
      value = reset_value;
      old_value = reset_value;
      suppress_write = true;
    }
  }

 public:
 // ConfigReg data members
 private:
  T value_stg_;          // wide: the view object behind 'value'
  T& value;              // the live element value, in caller storage
  unsigned int bits;
  tTime written;
  T old_value;           // aux-bound when wide
  T reset_value;         // aux-bound when wide
  const unsigned int reset_type;
  bool suppress_write;
  bool in_reset;

};

// This is the definition of the RegTwo register primitive.
// It has two set methods, where setA has priority over setB.
// Like ConfigReg, the get method can occur after the sets but
// should return the original value.
template<typename T>
class MOD_RegTwo : public Module
{
 public:
  MOD_RegTwo(tSimStateHdl simHdl, const char* name, Module* parent,
	     tStateLayout* sto, unsigned int* aux,
	     unsigned int width, const T& v, unsigned int async)
    : Module(simHdl, name, parent),
      value(bs_bind_elem(value_stg_, sto->claim(), width)), bits(width),
      written(~bk_now(sim_hdl)), a_at(~bk_now(sim_hdl)),
      reset_type(async ? ASYNC_RESET_REG : SYNC_RESET_REG)
  {
    bs_bind_aux(old_value, &aux, bits);
    bs_bind_aux(reset_value, &aux, bits);
    reset_value = v;
    write_undet(&value, bits);

    in_reset = false;
    suppress_write = false;
  }
  MOD_RegTwo(tSimStateHdl simHdl, const char* name, Module* parent,
	     tStateLayout* sto, unsigned int* aux,
	     unsigned int width)
    : Module(simHdl, name, parent),
      value(bs_bind_elem(value_stg_, sto->claim(), width)), bits(width),
      written(~bk_now(sim_hdl)), a_at(~bk_now(sim_hdl)),
      reset_type(NO_RESET_REG)
  {
    bs_bind_aux(old_value, &aux, bits);
    bs_bind_aux(reset_value, &aux, bits);
    write_undet(&value, bits);

    write_undet(&reset_value, bits);
    in_reset = false;
    suppress_write = false;
  }
  
 public:
  const T& METH_get() const
  {
    if (bk_is_same_time(sim_hdl, written))
      return old_value;
    else
      return value;
  }
  void METH_setA(const T& x)
  {
    if ((reset_type != ASYNC_RESET_REG) || !suppress_write)
    {
      if (written != bk_now(sim_hdl))
      {
        old_value = value;
        written = bk_now(sim_hdl);
      }
      a_at = bk_now(sim_hdl);
      value = x;
    }
  }
  void METH_setB(const T& x)
  {
    if ((reset_type != ASYNC_RESET_REG) || !suppress_write)
    {
      if (written != bk_now(sim_hdl))
      {
        old_value = value;
        written = bk_now(sim_hdl);
      }
      if (a_at != bk_now(sim_hdl))
        value = x;
    }
  }
  void reset_RST(tUInt8 rst_in)
  {
    in_reset = (rst_in == 0);
    if (in_reset)
    {
      if (reset_type == ASYNC_RESET_REG)
	rst_tick__clk__1(1);
      else if (reset_type == SYNC_RESET_REG)
	start_reset_ticks(sim_hdl); /* request rst_tick() on the next clock edge */
    }
    else
    {
      suppress_write = false;
      if (reset_type == SYNC_RESET_REG)
	stop_reset_ticks(sim_hdl); /* stop rst_tick() when reset is not asserted */
    }
  }
  void rst_tick__clk__1(tUInt8 clock_gate)
  {
    if (in_reset && (clock_gate != 0))
    {
      value = reset_value;
      suppress_write = true;
    }
  }
 public:
 // RegTwo data members
 private:
  T value_stg_;          // wide: the view object behind 'value'
  T& value;              // the live element value, in caller storage
  const unsigned int bits;
  tTime written;
  tTime a_at;
  T old_value;           // aux-bound when wide
  T reset_value;         // aux-bound when wide
  const unsigned int reset_type;
  bool suppress_write;
  bool in_reset;

};

// This is the definition of the CReg concurrent register primitive.
// It has multiple Reg interfaces that schedule in sequence.
template<typename T>
class MOD_CReg : public Module
{
  // embedded symbol-table storage (bound to Module::symbols;
  // symbol tables never allocate)
 private:
  tSym __symbols[1];
 public:
  // CRegN, CRegA
  MOD_CReg(tSimStateHdl simHdl, const char* name, Module* parent,
	   tStateLayout* sto, unsigned int* aux,
	   unsigned int width, const T& v, unsigned int async)
    : Module(simHdl, name, parent),
      ports(max_ports), // this should eventually be a parameter
      __clk_handle_0(BAD_CLOCK_HANDLE),
      value(bs_bind_elem(value_stg_, sto->claim(), width)), bits(width),
      reset_type(async ? ASYNC_RESET_REG : SYNC_RESET_REG)
  {
    bs_bind_aux(reset_value, &aux, bits);
    bs_bind_aux(value_rec, &aux, bits);
    reset_value = v;
    write_undet(&value, bits);

    in_reset = false;
    suppress_write = false;

    write_undet(&value_rec, bits);

    for (unsigned int i = 0; i < max_ports; i++) {
      bs_bind_aux(read_val[i], &aux, bits);
      write_undet(&(read_val[i]), bits);
      did_write[i] = false;
      did_write_rec[i] = false;
      bs_bind_aux(write_val[i], &aux, bits);
      write_undet(&(write_val[i]), bits);
    }

    symbol_count = 1;
    symbols = __symbols;

    symbols[0].key = "";
    symbols[0].info = SYM_DEF | bits << 4;
    symbols[0].value = (void*)(&value);
  }
  // CRegUN
  MOD_CReg(tSimStateHdl simHdl, const char* name, Module* parent,
	   tStateLayout* sto, unsigned int* aux,
	   unsigned int width)
    : Module(simHdl, name, parent),
      ports(max_ports), // this should eventually be a parameter
      __clk_handle_0(BAD_CLOCK_HANDLE),
      value(bs_bind_elem(value_stg_, sto->claim(), width)), bits(width),
      reset_type(NO_RESET_REG)
  {
    bs_bind_aux(reset_value, &aux, bits);
    bs_bind_aux(value_rec, &aux, bits);
    write_undet(&value, bits);
    write_undet(&reset_value, bits);

    in_reset = false;
    suppress_write = false;

    write_undet(&value_rec, bits);

    for (unsigned int i = 0; i < max_ports; i++) {
      bs_bind_aux(read_val[i], &aux, bits);
      write_undet(&(read_val[i]), bits);
      did_write[i] = false;
      did_write_rec[i] = false;
      bs_bind_aux(write_val[i], &aux, bits);
      write_undet(&(write_val[i]), bits);
    }

    symbol_count = 1;
    symbols = __symbols;

    symbols[0].key = "";
    symbols[0].info = SYM_DEF | bits << 4;
    symbols[0].value = (void*)(&value);
  }
  
 public:
  const T& METH_port0__read()    const { return value; }
  const T& METH_port1__read()    const { return value; }
  const T& METH_port2__read()    const { return value; }
  const T& METH_port3__read()    const { return value; }
  const T& METH_port4__read()    const { return value; }
  void METH_port0__write(const T& x)
  {
    if ((reset_type != ASYNC_RESET_REG) || !suppress_write) {
      value = x;
      did_write[0] = true;
      write_val[0] = x;
    }
  }
  void METH_port1__write(const T& x)
  {
    if ((reset_type != ASYNC_RESET_REG) || !suppress_write) {
      value = x;
      did_write[1] = true;
      write_val[1] = x;
    }
  }
  void METH_port2__write(const T& x)
  {
    if ((reset_type != ASYNC_RESET_REG) || !suppress_write) {
      value = x;
      did_write[2] = true;
      write_val[2] = x;
    }
  }
  void METH_port3__write(const T& x)
  {
    if ((reset_type != ASYNC_RESET_REG) || !suppress_write) {
      value = x;
      did_write[3] = true;
      write_val[3] = x;
    }
  }
  void METH_port4__write(const T& x)
  {
    if ((reset_type != ASYNC_RESET_REG) || !suppress_write) {
      value = x;
      did_write[4] = true;
      write_val[4] = x;
    }
  }
 public:
  void set_clk_0(const char* s)
  {
    __clk_handle_0 = bk_get_or_define_clock(sim_hdl, s);
  }
  void clk(tUInt8 /* clock_value */, tUInt8 gate_value = 1)
  {
    // compute Q_OUTs starting with the registered value
    // (before the writes to "value" this cycle)
    read_val[0] = value_rec;
    // record the registered value for the next clock cycle
    value_rec = value;
    // record the EN signals and clear them for the next clock cycle
    for (unsigned int i = 0; i < ports; i++) {
      did_write_rec[i] = did_write[i];
      did_write[i] = false;
    }
  }
  void reset_RST(tUInt8 rst_in)
  {
    in_reset = (rst_in == 0);
    if (in_reset)
    {
      if (reset_type == ASYNC_RESET_REG)
	rst_tick_clk(1);
      else if (reset_type == SYNC_RESET_REG)
	start_reset_ticks(sim_hdl); /* request rst_tick() on the next clock edge */
    }
    else
    {
      suppress_write = false;
      if (reset_type == SYNC_RESET_REG)
	stop_reset_ticks(sim_hdl); /* stop rst_tick() when reset is not asserted */
    }
  }
  void rst_tick_clk(tUInt8 clock_gate)
  {
    if (in_reset && (clock_gate != 0))
    {
      value = reset_value;
      value_rec = reset_value;
      suppress_write = true;
    }
  }
 public:
 // register data members
 private:
  static const unsigned int max_ports = 5;
  const unsigned int ports;
  tClock __clk_handle_0;
  T value_stg_;          // wide: the view object behind 'value'
  T& value;              // the live element value, in caller storage
  const unsigned int bits;
  T reset_value;         // aux-bound when wide
  const unsigned int reset_type;
  bool suppress_write;
  bool in_reset;
  T value_rec;
  T read_val[max_ports];
  bool did_write[max_ports];
  bool did_write_rec[max_ports];
  T write_val[max_ports];

};

#endif /* __BS_PRIM_MOD_REG_H__ */

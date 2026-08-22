#ifndef __BS_PRIM_MOD_WIRE_H__
#define __BS_PRIM_MOD_WIRE_H__

#include "bluesim_kernel_api.h"
#include "bs_module.h"
#include "bs_prim_storage.h"

// This is the definition of the Wire primitive for <= 64 bits
template<typename T>
class MOD_Wire : public Module
{
  // embedded symbol-table storage (bound to Module::symbols;
  // symbol tables never allocate)
 private:
  tSym __symbols[3];
 public:
  MOD_Wire(tSimStateHdl simHdl, const char* name, Module* parent,
           tStateLayout* sto,
           unsigned int width, const T& v, bool is_sync_wire)
    : Module(simHdl, name, parent), __clk_handle_0(BAD_CLOCK_HANDLE),
      bits(width),
      value(bs_bind_elem(value_stg_, sto->claim(), width)),
      isValid(false), written(false)
  {
    value = v;
    symbol_count = 3;
    symbols = __symbols;

    symbols[0].key = "";
    symbols[0].info = SYM_DEF | bits << 4;
    symbols[0].value = (void*)(&value);

    symbols[1].key = "isValid";
    symbols[1].info = SYM_DEF | 1 << 4;
    symbols[1].value = (void*)(&isValid);

    symbols[2].key = "value";
    symbols[2].info = SYM_DEF | bits << 4;
    symbols[2].value = (void*)(&value);
  }
  MOD_Wire(tSimStateHdl simHdl, const char* name, Module* parent,
           tStateLayout* sto,
           unsigned int width, bool is_sync_wire=false)
    : Module(simHdl, name, parent), __clk_handle_0(BAD_CLOCK_HANDLE),
      bits(width),
      value(bs_bind_elem(value_stg_, sto->claim(), width)),
      isValid(false), written(false)
  {
    write_undet(&value, width);

    symbol_count = 3;
    symbols = __symbols;

    symbols[0].key = "";
    symbols[0].info = SYM_DEF | bits << 4;
    symbols[0].value = (void*)(&value);

    symbols[1].key = "isValid";
    symbols[1].info = SYM_DEF | 1 << 4;
    symbols[1].value = (void*)(&isValid);

    symbols[2].key = "value";
    symbols[2].info = SYM_DEF | bits << 4;
    symbols[2].value = (void*)(&value);
  }
 public:
  bool METH_whas() const     { return isValid; }
  const T& METH_wget() const { return value; }
  void METH_wset(const T& x) {
    value = x;
    isValid = true;
  }
  // for zero-width wires
  void METH_wset()           { isValid = true; }
 public:
  void set_clk_0(const char* s)
  {
    __clk_handle_0 = bk_get_or_define_clock(sim_hdl, s);
  }
  void clk(tUInt8 /* clock_value */, tUInt8 gate_value = 1)
  {
    written = isValid;
    isValid = false;
  }
 private:
  tClock __clk_handle_0;
  unsigned int bits;
  T value_stg_;          // wide: the view object behind 'value'
  T& value;              // the live element value, in caller storage
  bool isValid;
  bool written;
};

#endif /* __BS_PRIM_MOD_WIRE_H__ */

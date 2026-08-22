#ifndef __BS_PRIM_MOD_WIRE_H__
#define __BS_PRIM_MOD_WIRE_H__

#include <iostream>

#include "bluesim_kernel_api.h"
#include "bs_module.h"

// This is the definition of the Wire primitive for <= 64 bits
template<typename T>
class MOD_Wire : public Module
{
 public:
  MOD_Wire(tSimStateHdl simHdl, const char* name, Module* parent,
           unsigned int width, const T& v, bool is_sync_wire)
    : Module(simHdl, name, parent), __clk_handle_0(BAD_CLOCK_HANDLE),
      bits(width), value(v), isValid(false), written(false)
  {
    symbol_count = 3;
    symbols = new tSym[symbol_count];

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
           unsigned int width, bool is_sync_wire=false)
    : Module(simHdl, name, parent), __clk_handle_0(BAD_CLOCK_HANDLE),
      bits(width), isValid(false), written(false)
  {
    init_val(value, width);
    write_undet(&value, width);

    symbol_count = 3;
    symbols = new tSym[symbol_count];

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
  void dump_state(unsigned int indent)
  {
    printf("%*s%s = ", indent, "", inst_name);
    if (!written)
      printf("Not set");
    else
      dump_val(value, bits);
    putchar('\n');
  }
 private:
  tClock __clk_handle_0;
  unsigned int bits;
  T value;
  bool isValid;
  bool written;
};

#endif /* __BS_PRIM_MOD_WIRE_H__ */

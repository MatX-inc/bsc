#ifndef __BS_PRIM_MOD_PROBE_H__
#define __BS_PRIM_MOD_PROBE_H__

#include "bluesim_kernel_api.h"
#include "bs_module.h"
#include "bs_wide_data.h"

// This is the definition of the Probe primitive.
template<typename T>
class MOD_Probe : public Module
{
  // embedded symbol-table storage (bound to Module::symbols;
  // symbol tables never allocate)
 private:
  tSym __symbols[1];
 public:
  MOD_Probe(tSimStateHdl simHdl, const char* name, Module* parent,
	    unsigned int width)
    : Module(simHdl, name, parent), __clk_handle_0(BAD_CLOCK_HANDLE),
      bits(width)
  {
    init_val(value, bits);
    write_undet(&value, bits);

    symbol_count = 1;
    symbols = __symbols;

    symbols[0].key = "";
    symbols[0].info = SYM_DEF | bits << 4;
    symbols[0].value = (void*)(&value);
  }
 public:
  void METH__write(const T& x) { value = x; }
 public:
  void set_clk_0(const char* s)
  {
    __clk_handle_0 = bk_get_or_define_clock(sim_hdl, s);
  }
 // Probe data members
 private:
  tClock __clk_handle_0;
  T value;
  unsigned int bits;

};

// This is the definition of the ProbeWire primitive.
template<typename T>
class MOD_ProbeWire : public Module
{
 public:
  MOD_ProbeWire(tSimStateHdl simHdl, const char* name, Module* parent,
		unsigned int width)
    : Module(simHdl, name, parent)
  {
    symbol_count = 0;
    symbols = NULL;
  }
 public:
  const T& METH_id(const T& x) const { return x; }
 public:
  void set_clk_0(const char* s)
  {
    //__clk_handle_0 = bk_get_or_define_clock(sim_hdl, s);
  }
 // ProbeWire data members
 private:
};

#endif /* __BS_PRIM_MOD_PROBE_H__ */

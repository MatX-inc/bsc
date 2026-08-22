#ifndef __BS_PRIM_MOD_COUNTER_H__
#define __BS_PRIM_MOD_COUNTER_H__

#include "bluesim_kernel_api.h"
#include "bs_module.h"
#include "bs_reset.h"

// This is the definition of the Probe primitive.
template<typename T>
class MOD_Counter : public Module
{
 public:
  MOD_Counter(tSimStateHdl simHdl, const char* name, Module* parent,
	      unsigned int width, const T& init)
    : Module(simHdl, name, parent),
      saved_at(~bk_now(sim_hdl)),
      a_at(~bk_now(sim_hdl)), b_at(~bk_now(sim_hdl)),
      c_at(~bk_now(sim_hdl)), f_at(~bk_now(sim_hdl)),

      bits(width), reset_val(init)
  {
    init_val(val, bits);
    write_undet(&val, bits);
    init_val(a, bits);
    init_val(b, bits);
    init_val(c, bits);
    init_val(f, bits);
    init_val(saved_val, bits);
    in_reset = false;
    suppress_write = false;
  }

 public:
  const T& METH_value() const
  {
    if (bk_is_same_time(sim_hdl, saved_at))
      return saved_val;
    else
      return val;
  }
  void METH_setC(const T& x)
  {
    if (!suppress_write)
    {
      if (saved_at != bk_now(sim_hdl))
      {
        saved_at = bk_now(sim_hdl);
        saved_val = val;
      }
      c_at = bk_now(sim_hdl);
      c    = x;
      val  = x;
      if (a_at == bk_now(sim_hdl))
        val += a;
      if (b_at == bk_now(sim_hdl))
        val += b;
      mask_high_bits(&val,bits);
    }
  }
  void METH_addA(const T& x)
  {
    if (!suppress_write)
    {
      if (saved_at != bk_now(sim_hdl))
      {
        saved_at = bk_now(sim_hdl);
        saved_val = val;
      }
      a_at = bk_now(sim_hdl);
      a    = x;
      val += x;
      mask_high_bits(&val,bits);
    }
  }
  void METH_addB(const T& x)
  {
    if (!suppress_write)
    {
      if (saved_at != bk_now(sim_hdl))
      {
        saved_at = bk_now(sim_hdl);
        saved_val = val;
      }
      b_at = bk_now(sim_hdl);
      b    = x;
      val += x;
      mask_high_bits(&val,bits);
    }
  }
  // Note: addA, addB, setC < setF
  void METH_setF(const T& x)
  {
    if (!suppress_write)
    {
      if (saved_at != bk_now(sim_hdl))
      {
        saved_at = bk_now(sim_hdl);
        saved_val = val;
      }
      f_at = bk_now(sim_hdl);
      f    = x;
      val  = x;
    }
  }
  const T& METH__read() const
  {
    return METH_value();
  }
  void METH_incrA (const T& x)
  {
    METH_addA(x);
  }
  void METH_incrB (const T& x)
  {
    METH_addB(x);
  }
  void METH_update (const T& x)
  {
    METH_setC(x);
  }
  void METH__write (const T& x)
  {
    METH_setF(x);
  }

 public:
  // Setting the clock
  void set_clk_0(const char* s)
  {
    __clk_handle_0 = bk_get_or_define_clock(sim_hdl, s);
  }

  // Handle reset
  void reset_RST(tUInt8 rst_in)
  {
    in_reset = (rst_in == 0);
    if (in_reset)
    {
      start_reset_ticks(sim_hdl); /* request rst_tick() on the next clock edge */
    }
    else
    {
      suppress_write = false;
      stop_reset_ticks(sim_hdl); /* stop rst_tick() when reset is not asserted */
    }
  }
  void rst_tick__clk__1(tUInt8 clock_gate)
  {
    if (in_reset && (clock_gate != 0))
    {
      val = reset_val;
      suppress_write = true;
    }
  }
  void rst_tick__clk__(tUInt8 clock_gate)
  {
    rst_tick__clk__1(clock_gate);
  }

 public:
 private:
  T val;
  T saved_val;
  T a;
  T b;
  T c;
  T f;
  tTime saved_at;
  tTime a_at;
  tTime b_at;
  tTime c_at;
  tTime f_at;
  const unsigned int bits;
  T reset_val;
  bool suppress_write;
  bool in_reset;

  tClock __clk_handle_0;
  bool did_adda;
  bool did_addb;
  bool did_setc;
  bool did_setf;
};

#endif /* __BS_PRIM_MOD_COUNTER_H__ */

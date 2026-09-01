#include "bs_prim_mod_resets.h"

/* Constructor */
MOD_MakeReset::MOD_MakeReset(tSimStateHdl simHdl,
			     const char* name, Module* parent_mod,
			     tStateLayout* sto,
			     unsigned int cycles, tUInt8 init, tUInt8 async)
    : Module(simHdl, name, parent_mod),
      sync(simHdl, "rstSync", this, NULL, cycles, async),
      rst_reset_value(init),
      written(~bk_now(sim_hdl))
{
  // clock/reset primitives keep no data in the element area: claim
  // (and ignore) the published entry so later elements stay aligned
  sto->claim();
  reset_fn = NULL;
  rst = 1;
  old_rst = rst;
  in_reset = false;
}

void MOD_MakeReset::static_reset_syncRst$rst(void *my_this, tUInt8 rst_in)
{
  ((MOD_MakeReset *)(my_this))->reset_syncRst$rst(rst_in);
}

void MOD_MakeReset::static_reset_syncRst$gen_rst(void *my_this, tUInt8 rst_in)
{
  ((MOD_MakeReset *)(my_this))->reset_syncRst$gen_rst(rst_in);
}


void MOD_ResetMux::static_do_select(void *my_this, tUInt8 /* rst_in */)
{
  ((MOD_ResetMux *)(my_this))->do_select();
}


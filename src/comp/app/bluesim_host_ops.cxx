/* Host operations that bluetcl installs into a dynamically-loaded
 * Bluesim model.
 *
 * The Bluesim runtime performs all of its I/O through the bs_host_ops
 * table passed to bk_sync_init() (see bluesim_host_ops.h).  bluetcl
 * uses the default C-library-backed implementation, which lives in
 * the header bluesim_host_ops_default.h shared with the generated
 * SystemC wrappers.  These entry points expose that implementation
 * to BluesimLoader.hs through the Haskell FFI.
 */

#include "bluesim_host_ops_default.h"

extern "C" const struct bs_host_ops* bluesim_default_host_ops(void)
{
  return bs_default_host_ops();
}

extern "C" void* bluesim_default_host_ctx(void)
{
  return bs_default_host_ctx();
}

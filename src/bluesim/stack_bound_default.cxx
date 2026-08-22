/*
 * Weak default for the design's static stack-depth bound.
 *
 * When bsc links a Bluesim model it runs bluesim_stack_bound.py and
 * compiles a small generated TU carrying the strong definition of
 * bs_stack_depth_bound; that definition satisfies the kernel's
 * reference first, so this archive member is never extracted.  A
 * link done without that TU -- a SystemC build, or any link outside
 * bsc's Bluesim link step -- extracts this weak 0 instead, which is
 * the documented "no bound available" value of
 * bk_stack_depth_bound().
 *
 * This lives in its own translation unit deliberately: were the
 * default defined next to the accessor in kernel.cxx, the compiler
 * would be entitled to constant-fold the load to 0 there.
 */

#include "bluesim_types.h"

extern "C" const tUInt64 bs_stack_depth_bound
#if defined(__GNUC__)
    __attribute__((weak))
#endif
    = 0llu;

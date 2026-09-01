#include <cstring>

#include "bluesim_kernel_api.h"
#include "kernel.h"
#include "plusargs.h"

/* The recorded simulator arguments live in fixed-capacity storage
 * embedded in the simulation context (see BK_MAX_PLUS_ARGS and
 * BK_PLUS_ARG_MAX in kernel.h); recording and matching them
 * allocates nothing.  An argument beyond the capacity, or longer
 * than BK_PLUS_ARG_MAX - 1 characters, is ignored (documented with
 * bk_append_argument() in bluesim_kernel_api.h).
 */

void clear_plusargs(tSimStateHdl simHdl)
{
  simHdl->num_plus_args = 0;
}

void bk_append_argument(tSimStateHdl simHdl, const char* arg)
{
  if (arg == NULL)
    return;
  if (simHdl->num_plus_args >= BK_MAX_PLUS_ARGS)
    return;  /* table full: the argument is ignored */
  if (strlen(arg) >= BK_PLUS_ARG_MAX)
    return;  /* too long to record: the argument is ignored */
  strcpy(simHdl->plus_args[simHdl->num_plus_args], arg);
  ++(simHdl->num_plus_args);
}

const char* bk_match_argument(tSimStateHdl simHdl, const char* name)
{
  if (name == NULL)
    return NULL;

  unsigned int len = strlen(name);
  for (unsigned int n = 0; n < simHdl->num_plus_args; ++n)
  {
    const char* arg = simHdl->plus_args[n];
    if (!strncmp(name, arg, len))
      return arg + len; // return trailing portion
  }

  return NULL;  // no match
}

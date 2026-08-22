/* Foreign functions for the sysElfBdpi freestanding-ELF probe.
 * The strlen call resolves against the model's own vendored
 * definition (the checker proves there is no strlen import).
 */
#include <string.h>

typedef unsigned int u32;

u32 elfcheck_strlen(const char* s)
{
  return (u32) strlen(s);
}

void elfcheck_incr(u32* out, const u32* x)
{
  for (int i = 0; i < 4; ++i)
    out[i] = x[i] + 1u;
}

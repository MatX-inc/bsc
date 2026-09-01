#include <stdint.h>

#include "rand32.h"

/* The pseudo-random generator behind the Randomizable library's
 * rand32 BDPI import.
 *
 * This is splitmix64 (Steele/Lea/Flood, "Fast splittable
 * pseudorandom number generators", OOPSLA 2014; public-domain
 * reference implementation), truncated to the top 32 bits of each
 * output.  It lives in the model shared object so that random
 * numbers need no C library: the previous implementation called
 * random(), which was a dynamic libc import.
 *
 * Seeding semantics are unchanged from that implementation: there is
 * no seeding interface at all, so every run of a model produces the
 * same sequence.  (The old code never called srandom() either -- it
 * ran on the C library generator's default seed of 1 -- so runs were
 * already deterministic; the values themselves differ from glibc's,
 * which were never portable to begin with and, being random()'s,
 * never had their top bit set.)
 */

static uint64_t rand32_state = 1u; /* mirrors random()'s default seed */

extern "C" unsigned int rand32 ()
{
  uint64_t z = (rand32_state += 0x9e3779b97f4a7c15ull);
  z = (z ^ (z >> 30)) * 0xbf58476d1ce4e5b9ull;
  z = (z ^ (z >> 27)) * 0x94d049bb133111ebull;
  z = z ^ (z >> 31);
  return (unsigned int)(z >> 32);
}

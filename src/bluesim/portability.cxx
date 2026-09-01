/* This file contains definitions of utility functions
 * which enhance code portability.
 */
#include "portability.h"

/* exponentiation on unsigned ints */
unsigned long long powll(unsigned int base, unsigned int exp)
{
  if (exp == 0)  return 1llu;
  if (base == 0) return 0llu;

  unsigned long long ret = 1;
  unsigned long long m = base;
  while (exp > 0)
  {
    if (exp & 1) ret *= m;
    exp = exp >> 1;
    m = m * m;
  }

  return ret;
}



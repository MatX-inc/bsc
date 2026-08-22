/* This file contains definitions of utility functions
 * which enhance code portability.
 */
#include <cstdarg>
#include <cstdlib>
#include <cstdio>

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


// re-implement asprintf using only C99 features
// must be specifically marked as not throwing exceptions
// in order to not conflict with glibc's asprintf prototype
int new_asprintf(char **strp, const char* fmt, ...) throw()
{
  va_list ap1, ap2;

  va_start(ap1,fmt);
  va_copy(ap2,ap1);

  size_t output_chars = vsnprintf(NULL, 0, fmt, ap1);
  va_end(ap1);

  // add space for the terminating null
  size_t output_size = output_chars + 1;

  char* output_buffer = (char*) malloc(output_size);

  int result = vsnprintf(output_buffer, output_size, fmt, ap2);
  va_end(ap2);

  if (result != -1) {
    *strp = output_buffer;
  }
  else {
    free(strp);
  }

  return(result);
}


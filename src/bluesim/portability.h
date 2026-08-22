#ifndef __PORTABILITY_H__
#define __PORTABILITY_H__

/* This file contains declarations of utility functions
 * which enhance code portability.
 */

#include <sys/types.h>
#include <limits.h>

/* The defines for limits of long long values change across versions */
#ifndef LLONG_MAX
#define LLONG_MAX    9223372036854775807LL
#endif

#ifndef LLONG_MIN
#define LLONG_MIN    (-LLONG_MAX - 1LL)
#endif

#ifndef LONG_LONG_MIN
#define LONG_LONG_MIN LLONG_MIN
#endif

#ifndef LONG_LONG_MAX
#define LONG_LONG_MAX LLONG_MAX
#endif

extern "C" {

/* exponentiation on integers */
unsigned long long powll(unsigned int base, unsigned int exp);

} /* extern "C" */

#endif /* __PORTABILITY_H__ */

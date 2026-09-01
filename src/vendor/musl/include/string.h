#ifndef BS_MUSL_STRING_H
#define BS_MUSL_STRING_H

/* Minimal <string.h> for compiling the vendored musl string routines
 * (src/string/) outside of a musl source tree.  It stands in for both
 * musl's public include/string.h and its internal src/include/string.h:
 * it declares exactly the functions this vendored subset defines or
 * calls, plus the weak_alias() macro the sources use (in musl it comes
 * from the internal features.h).
 *
 * This header is NOT part of musl; see ../README.md.  It is only on
 * the include path when the vendored sources themselves are compiled.
 */

#include <stddef.h> /* size_t (provided by the compiler) */

void *memcpy(void *restrict, const void *restrict, size_t);
void *memmove(void *, const void *, size_t);
void *memset(void *, int, size_t);

size_t strlen(const char *);
int strcmp(const char *, const char *);
int strncmp(const char *, const char *, size_t);
char *strchr(const char *, int);
char *strcpy(char *restrict, const char *restrict);
char *strncpy(char *restrict, const char *restrict, size_t);

/* musl-internal entry points (used by strchr/strcpy/strncpy) */
char *__strchrnul(const char *, int);
char *__stpcpy(char *restrict, const char *restrict);
char *__stpncpy(char *restrict, const char *restrict, size_t);

#ifndef weak_alias
#define weak_alias(old, new) \
	extern __typeof(old) new __attribute__((__weak__, __alias__(#old)))
#endif

#endif

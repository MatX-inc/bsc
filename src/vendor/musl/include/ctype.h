#ifndef BS_MUSL_CTYPE_H
#define BS_MUSL_CTYPE_H

/* Minimal <ctype.h> for compiling the vendored musl character
 * classification routines (src/ctype/) outside of a musl source
 * tree.  It declares exactly the functions this vendored subset
 * defines or calls, the locale_t type their (vestigial here) _l
 * variants take, and the weak_alias() macro the sources use.
 *
 * This header is NOT part of musl; see ../README.md.  It is only on
 * the include path when the vendored sources themselves are compiled.
 */

typedef struct __locale_struct *locale_t;

int isblank(int);
int isdigit(int);
int isupper(int);
int isxdigit(int);
int tolower(int);

int __isblank_l(int, locale_t);
int __isdigit_l(int, locale_t);
int __isupper_l(int, locale_t);
int __isxdigit_l(int, locale_t);
int __tolower_l(int, locale_t);

#ifndef weak_alias
#define weak_alias(old, new) \
	extern __typeof(old) new __attribute__((__weak__, __alias__(#old)))
#endif

#endif

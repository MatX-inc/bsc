#ifndef BS_MUSL_ENDIAN_H
#define BS_MUSL_ENDIAN_H

/* Minimal <endian.h> for compiling the vendored musl string routines
 * (memcpy.c tests __BYTE_ORDER) outside of a musl source tree,
 * defined in terms of the compiler's own byte-order macros.
 *
 * This header is NOT part of musl; see ../README.md.  It is only on
 * the include path when the vendored sources themselves are compiled.
 */

#define __LITTLE_ENDIAN 1234
#define __BIG_ENDIAN    4321

#if defined(__BYTE_ORDER__) && (__BYTE_ORDER__ == __ORDER_BIG_ENDIAN__)
#define __BYTE_ORDER __BIG_ENDIAN
#else
#define __BYTE_ORDER __LITTLE_ENDIAN
#endif

#endif

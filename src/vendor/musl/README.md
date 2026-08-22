# Vendored musl string and ctype routines

This directory holds a small subset of [musl libc](https://musl.libc.org/)
**version 1.2.6**, vendored so that Bluesim model shared objects can
provide the C string/memory/ctype routines they need locally instead of
importing them from the host's C library.  The compiler emits calls to
`memcpy`/`memset`/`memmove` on its own (it does so even under
`-ffreestanding`), so a model that must have *no* dynamic dependencies
needs local, hidden-visibility definitions of them; the runtime's few
`str*`/`is*`/`tolower` uses are covered from the same source for
consistency.  The routines are compiled into `libbsstring.a` by
`src/bluesim/Makefile` and linked into every generated model `.so`.

## License

musl is MIT-licensed.  The complete license text and author list are in
[`COPYRIGHT`](./COPYRIGHT), copied verbatim from the top of the musl
1.2.6 source tree.  Per that file, the string/ctype implementation
sources vendored here carry no per-file copyright notices and are
covered by the standard MIT license reproduced there; the aarch64
memcpy/memset assembly additionally carries its own MIT
`SPDX-License-Identifier` and Arm Limited copyright header in-file.

## Provenance

Source: `musl-1.2.6.tar.gz` from https://musl.libc.org/releases/,
SHA-256:

    d585fd3b613c66151fc3249e8ed44f77020cb5e6c1e635a616d3f9f82460512a

The following files are copied **verbatim** (byte-for-byte) from that
tarball, keeping their paths under `src/`:

| Here                        | In musl-1.2.6                    |
|-----------------------------|----------------------------------|
| `COPYRIGHT`                 | `COPYRIGHT`                      |
| `src/string/memcpy.c`       | `src/string/memcpy.c`            |
| `src/string/memmove.c`      | `src/string/memmove.c`           |
| `src/string/memset.c`       | `src/string/memset.c`            |
| `src/string/strlen.c`       | `src/string/strlen.c`            |
| `src/string/strcmp.c`       | `src/string/strcmp.c`            |
| `src/string/strncmp.c`      | `src/string/strncmp.c`           |
| `src/string/strchr.c`       | `src/string/strchr.c`            |
| `src/string/strchrnul.c`    | `src/string/strchrnul.c`         |
| `src/string/strcpy.c`       | `src/string/strcpy.c`            |
| `src/string/stpcpy.c`       | `src/string/stpcpy.c`            |
| `src/string/strncpy.c`      | `src/string/strncpy.c`           |
| `src/string/stpncpy.c`      | `src/string/stpncpy.c`           |
| `src/string/x86_64/memcpy.s`| `src/string/x86_64/memcpy.s`     |
| `src/string/x86_64/memmove.s`| `src/string/x86_64/memmove.s`   |
| `src/string/x86_64/memset.s`| `src/string/x86_64/memset.s`     |
| `src/string/aarch64/memcpy.S`| `src/string/aarch64/memcpy.S`   |
| `src/string/aarch64/memset.S`| `src/string/aarch64/memset.S`   |
| `src/ctype/isblank.c`       | `src/ctype/isblank.c`            |
| `src/ctype/isdigit.c`       | `src/ctype/isdigit.c`            |
| `src/ctype/isupper.c`       | `src/ctype/isupper.c`            |
| `src/ctype/isxdigit.c`      | `src/ctype/isxdigit.c`           |
| `src/ctype/tolower.c`       | `src/ctype/tolower.c`            |

Architecture-specific assembly: musl 1.2.6 provides string assembly for
two 64-bit little-endian architectures -- x86_64 (memcpy, memmove,
memset) and aarch64 (memcpy, memset) -- and both are vendored in full.
The generic C implementations serve every routine that has no assembly
for the build architecture (and all routines on architectures or object
formats without any).

The headers under `include/` (`string.h`, `ctype.h`, `endian.h`) are
**not** from musl: they are minimal local stand-ins for the musl
public/internal headers the sources include, declaring exactly the
functions in this subset plus musl's `weak_alias()` macro.  They are
only on the include path while the vendored sources themselves are
compiled.

## Updating

To move to a newer musl release: verify the release tarball's SHA-256
against https://musl.libc.org/, re-copy the files listed above
verbatim, update the version and hash in this README, and diff the new
sources for added internal dependencies (anything newly referenced must
be declared in the `include/` stand-ins or vendored as well).

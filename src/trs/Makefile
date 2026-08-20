PWD:=$(shell pwd)
TOP:=$(PWD)/../..

INSTALL ?= install

PREFIX?=$(TOP)/inst
BINDIR=$(PREFIX)/bin

CARGO ?= cargo

.PHONY: all
all: install

# The trs executable is a self-contained Rust binary (the interpreter
# needs no BLUESPECDIR or LD_LIBRARY_PATH), so it is installed directly
# in "bin" with no wrapper script.  libtrs_capi.a (the bk_* C API for
# `trs link --interactive` models) installs beside it — the linker
# searches next to its own binary — and `trs capi-so` turns it into
# the ONE shared libtrs_capi.so the fast link's per-design capi shims
# link against (docs/TCL-CAPI.md).
#
# The hybrid JIT needs llvm-sys (LLVM 18 dev headers).  The prefix is
# AUTO-DETECTED via llvm-config-18 so a plain build produces the full
# product (a lean install silently downgraded every artifact to the
# interpreter until the perf fence caught it); LLVM_SYS_181_PREFIX in
# the environment overrides the probe.  Without either, the lean
# interpreter-only fallback builds (no LLVM required).
LLVM_SYS_181_PREFIX ?= $(shell llvm-config-18 --prefix 2>/dev/null)
ifneq ($(LLVM_SYS_181_PREFIX),)
export LLVM_SYS_181_PREFIX
TRS_FLAGS = --features jit
CAPI_FLAGS  =
else
# lean fallback: no compile tier, but artifact LOADING stays (aot
# needs no LLVM) — artifacts built elsewhere run at full speed
TRS_FLAGS = --features aot
CAPI_FLAGS  = --no-default-features --features aot
endif

# Strip debug info from a static archive.  GNU binutils spells this
# --strip-debug; the macOS strip does not accept it and uses -S.
ifeq ($(shell uname -s),Darwin)
STRIP_DEBUG = strip -S
else
STRIP_DEBUG = strip --strip-debug
endif

.PHONY: build
build:
	$(CARGO) build --release --locked -p trs $(TRS_FLAGS)
	# slim artifact runtime (no LLVM): what `trs link --exe` binaries
	# load — the full capi lib carries statically-linked LLVM whose
	# constructors cost ~5ms at every exec.  Built FIRST and stashed
	# as libtrs_rt.a before the full capi build overwrites the .a.
	$(CARGO) build --release --locked -p trs-capi --no-default-features --features aot
	cp target/release/libtrs_capi.a target/release/libtrs_rt.a
	$(CARGO) build --release --locked -p trs-capi $(CAPI_FLAGS)

.PHONY: install
install: build
	$(INSTALL) -m 755 -d $(BINDIR)
	$(INSTALL) -m 755 target/release/trs $(BINDIR)/trs
	$(INSTALL) -m 644 target/release/libtrs_capi.a $(BINDIR)/libtrs_capi.a
	$(INSTALL) -m 644 target/release/libtrs_rt.a $(BINDIR)/libtrs_rt.a
	# debuginfo in the staticlib is dead weight at .so link time
	# (~420MB -> tens of MB); the interactive link strips the model.
	# GNU strip spells this --strip-debug; the macOS one only has -S.
	$(STRIP_DEBUG) $(BINDIR)/libtrs_capi.a
	$(STRIP_DEBUG) $(BINDIR)/libtrs_rt.a
	# the shared capi: linked ONCE here instead of once per design;
	# without it the fast wrapper's -c/-f script tier is absent (the
	# artifact contract is unchanged), so a cc/LLVM-less environment
	# degrades with a note rather than failing the install
	$(BINDIR)/trs capi-so -o $(BINDIR)/libtrs_capi.so \
	  || echo "trs: note: libtrs_capi.so not built; artifacts will lack the -c/-f script tier"
	# the slim runtime .so `trs link --exe` binaries link against;
	# LLVM-free, so it builds in lean environments too
	$(BINDIR)/trs capi-so --rt -o $(BINDIR)/libtrs_rt.so \
	  || echo "trs: note: libtrs_rt.so not built; trs link --exe will fall back to libtrs_capi.so"

.PHONY: clean full_clean
clean full_clean:
	$(CARGO) clean

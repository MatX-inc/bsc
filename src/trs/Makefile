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
# searches next to its own binary.
#
# The hybrid JIT needs llvm-sys (LLVM 18 dev headers): when the
# environment provides LLVM_SYS_181_PREFIX, build the full product;
# otherwise the lean interpreter-only fallback (no LLVM required).
ifdef LLVM_SYS_181_PREFIX
TRS_FLAGS = --features jit
CAPI_FLAGS  =
else
TRS_FLAGS =
CAPI_FLAGS  = --no-default-features
endif

.PHONY: build
build:
	$(CARGO) build --release --locked -p trs $(TRS_FLAGS)
	$(CARGO) build --release --locked -p trs-capi $(CAPI_FLAGS)

.PHONY: install
install: build
	$(INSTALL) -m 755 -d $(BINDIR)
	$(INSTALL) -m 755 target/release/trs $(BINDIR)/trs
	$(INSTALL) -m 644 target/release/libtrs_capi.a $(BINDIR)/libtrs_capi.a
	# debuginfo in the staticlib is dead weight at .so link time
	# (~420MB -> tens of MB); the interactive link strips the model
	strip --strip-debug $(BINDIR)/libtrs_capi.a

.PHONY: clean full_clean
clean full_clean:
	$(CARGO) clean

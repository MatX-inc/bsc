PWD := $(shell pwd)
TOP := $(PWD)/..

include $(TOP)/platform.mk

PREFIX?=$(TOP)/inst

ifndef NO_DEPS_CHECKS
CC_TOOLS=$(CC) $(CXX) $(LD)
# The Haskell toolchain, driven by Cabal (which also builds the vendored
# SMT solvers, via SetupHooks)
CABAL_TOOLS=cabal ghc
YICES_TOOLS=gperf autoconf
STP_TOOLS=flex bison

TOOLS=$(CC_TOOLS) \
      $(CABAL_TOOLS) \
      $(YICES_TOOLS) \
      $(STP_TOOLS) \

SUBMODS=vendor/yices/v2.6/yices2

# Check all the necessary build tools are present
pathsearch=$(firstword $(wildcard $(addsuffix /$(1),$(subst :, ,$(PATH)))))
checktool=$(if $(call pathsearch,$(1)),,$(error Tool $(1) could not be found))
$(foreach tool,$(TOOLS),$(call checktool,$(tool)))

# Check all submodules are initialized
submodexists=$(shell git submodule status $(1) 2> /dev/null )
in_git_repo:=$(foreach submod,$(SUBMODS),$(call submodexists,$(submod)))
ifneq ($(in_git_repo),)
define SUBMOD_MSG
Submodule $(1) missing. Initialize with
    git submodule update --init --recursive
endef
checksubmod=$(if $(wildcard $(1)/.git),,$(error $(SUBMOD_MSG)))
else
define SUBMOD_MSG
Submodule $(1) missing.
    This archive was exported from Git without the source files for
    submodules. You will need to acquire those files or, preferably,
    build instead from a clone of the BSC Git repository
endef
checksubmod=$(if $(wildcard $(1)/*),,$(error $(SUBMOD_MSG)))
endif
$(foreach submod,$(SUBMODS),$(call checksubmod,$(submod)))
endif # NO_DEPS_CHECKS

.PHONY: all
all: install

# -------------------------
# The Haskell tools (and the SMT solvers statically linked into them) are
# built by Cabal, from the package description at the top of the repo.

# The binaries installed into $(PREFIX)/bin by default, and the extra
# (developer) binaries installed only by 'install-extra'
BIN_TARGETS=bsc bluetcl
EXTRA_BIN_TARGETS=bsc2bsv bsv2bsc dumpbo dumpba showrules vcdcheck

# GHCJOBS: how many modules GHC compiles in parallel;
# GHCRTSFLAGS: RTS options for GHC itself (e.g. heap sizing, "+RTS -M5G -RTS").
# Both were options of the pre-Cabal build and are still accepted (CI uses
# them); they become -j/RTS flags passed to GHC through Cabal.
GHCJOBS ?=
GHCRTSFLAGS ?=
CABAL_GHC_OPTIONS := $(strip $(if $(GHCJOBS),-j$(GHCJOBS)) $(GHCRTSFLAGS))
CABAL_BUILD_FLAGS ?=
ifneq ($(CABAL_GHC_OPTIONS),)
override CABAL_BUILD_FLAGS += --ghc-options="$(CABAL_GHC_OPTIONS)"
endif

.PHONY: cabal-build
cabal-build:
	cd $(TOP) && cabal build $(CABAL_BUILD_FLAGS) \
		$(addprefix exe:,$(BIN_TARGETS) $(EXTRA_BIN_TARGETS))

.PHONY: install-bin
install-bin: cabal-build
	install -m 755 -d $(PREFIX)/bin
	cd $(TOP) && for exe in $(BIN_TARGETS); do \
		install -m 755 "`cabal list-bin exe:$$exe`" $(abspath $(PREFIX)/bin) || exit 1; \
	done

.PHONY: install-extra
install-extra: cabal-build
	install -m 755 -d $(PREFIX)/bin
	cd $(TOP) && for exe in $(EXTRA_BIN_TARGETS); do \
		install -m 755 "`cabal list-bin exe:$$exe`" $(abspath $(PREFIX)/bin) || exit 1; \
	done

# When BSC_BUILD_PKGINFO is set (the release flow does), record the licenses
# of the GHC packages statically linked into the installed binaries.  The
# direct dependencies are read from cabal's build plan (and
# make-ghc-pkg-info.sh follows the transitive ones); packages that cabal
# built from Hackage are found via the store package db.
BSC_BUILD_PKGINFO ?=

.PHONY: install-pkginfo
install-pkginfo: cabal-build
ifneq ($(BSC_BUILD_PKGINFO),)
	install -m 755 -d $(PREFIX)/LICENSES
	cd $(TOP) && \
		store_db="`cabal path --store-dir`/ghc-`ghc --numeric-version`/package.db"; \
		if [ -d "$$store_db" ]; then export GHC_PACKAGE_PATH="$$store_db:"; fi; \
		src/comp/make-ghc-pkg-info.sh \
			`python3 src/comp/list-cabal-plan-deps.py` \
			> $(abspath $(PREFIX))/LICENSES/LICENSE.ghc_pkgs
endif

# -------------------------
# Everything else (the Bluespec standard library, the Bluesim kernel, the
# Verilog primitives, ...) is built by the subdirectory Makefiles.
#
# Order matters: Libraries compiles the standard library with the
# newly-installed $(PREFIX)/bin/bsc.
#
# 'install-runtime' assembles just these runtime pieces, assuming the
# binaries are already in $(PREFIX)/bin; the Nix build uses it (with
# NO_DEPS_CHECKS=1, as cabal is not available there) after the Haskell
# builder has installed the executables.

.PHONY: install
install: install-bin install-pkginfo install-runtime

.PHONY: install-runtime
install-runtime:
	$(MAKE)  -C Libraries  PREFIX=$(PREFIX)  install
	$(MAKE)  -C exec       PREFIX=$(PREFIX)  install
	$(MAKE)  -C VPI        PREFIX=$(PREFIX)  install
	$(MAKE)  -C Verilog    PREFIX=$(PREFIX)  install
	$(MAKE)  -C Verilog.Quartus  PREFIX=$(PREFIX)  install
	$(MAKE)  -C Verilog.Vivado   PREFIX=$(PREFIX)  install
	$(MAKE)  -C bluetcl    PREFIX=$(PREFIX)  install
	$(MAKE)  -C bluesim    PREFIX=$(PREFIX)  install
	$(MAKE)  -C Verilator  PREFIX=$(PREFIX)  install

.PHONY: clean full_clean
clean full_clean:
	$(MAKE)  -C vendor/stp   PREFIX=$(PREFIX)  $@
	$(MAKE)  -C vendor/yices PREFIX=$(PREFIX)  $@
	$(MAKE)  -C Libraries  PREFIX=$(PREFIX)  $@
	$(MAKE)  -C exec       PREFIX=$(PREFIX)  $@
	$(MAKE)  -C VPI        PREFIX=$(PREFIX)  $@
	$(MAKE)  -C Verilog    PREFIX=$(PREFIX)  $@
	$(MAKE)  -C Verilog.Quartus  PREFIX=$(PREFIX)  $@
	$(MAKE)  -C Verilog.Vivado   PREFIX=$(PREFIX)  $@
	$(MAKE)  -C bluetcl    PREFIX=$(PREFIX)  $@
	$(MAKE)  -C bluesim    PREFIX=$(PREFIX)  $@
	$(MAKE)  -C Verilator  PREFIX=$(PREFIX)  $@

TOP:=$(shell pwd)/../..

# -----
# System tools

INSTALL ?= install
CABAL ?= cabal

RM = rm -f

FIND = find

# -----
# Options

# Set this to 1 if the source files are not in Git or the Git tool
# is not available.  The variable is used by "update-build-version.sh"
# which will not try running Git if the value is 1.
#
NOGIT ?= 0
export NOGIT

# -----
# Paths and environment

# PREFIX is where things are installed
PREFIX?=$(TOP)/inst
BINDIR=$(PREFIX)/bin

# Where package information is installed
LICDIR=$(PREFIX)/LICENSES

# The Haskell sources beyond this directory (tags)
PARSEC_HS = ../Parsec
STP_HS = ../vendor/stp/include_hs
YICES_HS = ../vendor/yices/include_hs
HTCL_HS = ../vendor/htcl

# -----
# GHC

GHC ?= ghc
# A versioned GHC uses the corresponding ghc-pkg; wrappers may override it.
GHCPKG ?= $(if $(findstring /,$(GHC)),$(dir $(GHC)))ghc-pkg$(patsubst ghc%,%,$(notdir $(GHC)))
# The compile parallelism, passed as cabal's -j: from GHC 9.12, with
# cabal.project's semaphore, the number of GHC slots in all; below it the
# number of packages building at once (doc/cabal.md section 2). Default it
# from the machine. Measured on a 228-module build (16-core/32-thread EPYC, GHC
# 9.14.1): wall time is 253.7s at -j1, 82.9s at -j4, 67.7s at -j12, and
# flat (within ~1%) from -j12 through -j32 -- the module graph's average
# ready-set width (~5-6 compiles) bounds useful parallelism well below the
# core count, and SMT threads add nothing. 16 captures the plateau with
# margin.
GHCJOBS ?= $(shell n=`nproc 2>/dev/null || sysctl -n hw.ncpu 2>/dev/null || echo 1`; if [ "$$n" -gt 16 ]; then echo 16; else echo "$$n"; fi)

GHCVERSION=$(shell $(GHC) --numeric-version)
$(info Building with GHC $(GHCVERSION))

# GHC's RTS options for the build, as CI sets them to cap the heap on its
# runners (+RTS -M5G -A128m -RTS): passed to cabal as GHC options, which
# reach every GHC the build runs and nothing else, and given to list-bin as
# well so that both see one configuration. Unset, GHC's defaults; the full
# build's high-water mark on four cores was under 6 GB.
GHCRTSFLAGS ?=
CABAL_COMPILER_OPTIONS = --with-compiler="$(GHC)" --with-hc-pkg="$(GHCPKG)"
CABAL_GHC_OPTIONS = $(CABAL_COMPILER_OPTIONS) $(if $(strip $(GHCRTSFLAGS)),--ghc-options='$(GHCRTSFLAGS)')

# -----
# GHC package info

# If BSC_BUILD_PKGINFO is non-empty,
# generate and install a LICENSES/LICENSE.ghc_pkgs file

ifeq ($(BSC_BUILD_PKGINFO),)
PKGINFO=
else
PKGINFO=$(LICDIR)/LICENSE.ghc_pkgs

# The packages the installed programs were linked against: the in-place
# library, the dependencies cabal-install built into its store, and GHC's
# own (the trailing colon), walked from the library.
CABAL_COMPILER_STORE = $(shell cd $(TOP) && $(CABAL) path $(CABAL_COMPILER_OPTIONS) --compiler-info | sed -n 's/^compiler-store-path: //p')
CABAL_PKG_PATH = $(TOP)/dist-newstyle/packagedb/ghc-$(GHCVERSION):$(CABAL_COMPILER_STORE)/package.db:

.PHONY: $(PKGINFO)
$(PKGINFO):
	mkdir -p -m 755 $(LICDIR)
	GHCPKG="$(GHCPKG)" GHC_PACKAGE_PATH="$(CABAL_PKG_PATH)" ./make-ghc-pkg-info.sh bsc > $(PKGINFO)
endif

# -----
# Install targets

# `install` is the cabal build: `cabal build all` at the repository root
# compiles the library once and links every program against its archive,
# which is what makes the programs the same bytes from build to build
# (doc/cabal.md section 5). Every executable stanza of bsc.cabal is
# installed, so a step that adds a program adds nothing here. The build's
# flags are bsc.cabal's and cabal.project's; a variant (unoptimized, debug,
# profiling) is a cabal.project.local, as INSTALL.md describes.
CABAL_EXES = $(shell sed -n 's/^executable[[:space:]]*\([^[:space:]]*\).*/\1/p' $(TOP)/bsc.cabal)

# BuildVersion names the commit, and its rule runs when cabal-install builds
# the package, which it does when a declared source changed. A commit that
# touches no source of the package (a testsuite or workflow change) would
# leave the previous commit's version in the programs, so `install` writes
# HEAD into a file bsc.cabal declares as a source: a new commit is then a
# changed source, and the version follows (doc/cabal.md section 2). This is
# a rebuild input, not a Cabal configuration file.
CABAL_HEAD = $(TOP)/cabal-head.project

# The cabal build links the solvers by name and is told no directory
# (SetupHooks.hs, solverSetupHooks): src/Makefile installed them under
# $(PREFIX)/lib/SAT before descending here, and LIBRARY_PATH names that
# directory to the linker for every link of the build.

.PHONY: install
install:
	mkdir -p -m 755 $(BINDIR)
	(./update-build-version.sh --head 2>/dev/null || echo no-repository) > $(CABAL_HEAD).tmp && \
	  { cmp -s $(CABAL_HEAD).tmp $(CABAL_HEAD) || mv $(CABAL_HEAD).tmp $(CABAL_HEAD); } && \
	  $(RM) -f $(CABAL_HEAD).tmp
	cd $(TOP) && LIBRARY_PATH="$(abspath $(PREFIX))/lib/SAT$${LIBRARY_PATH:+:$$LIBRARY_PATH}" \
	  $(CABAL) build -j$(GHCJOBS) $(CABAL_GHC_OPTIONS) all
	@set -e; for exe in $(CABAL_EXES); do \
	  bin=$$(cd $(TOP) && $(CABAL) list-bin $(CABAL_GHC_OPTIONS) bsc:exe:$$exe); \
	  echo "$(INSTALL) -m 755 $$bin $(BINDIR)/$$exe"; \
	  $(INSTALL) -m 755 "$$bin" $(BINDIR)/$$exe; \
	done
ifneq ($(PKGINFO),)
	$(MAKE) $(PKGINFO)
endif

# Everything is one build; the target remains for callers that run both.
.PHONY: install-extra
install-extra: install

# ----
# Other targets

tags:   *hs */*hs bluewish.hs
	$(FIND) . $(PARSEC_HS) $(STP_HS) $(YICES_HS) $(HTCL_HS) \
		-type f -name \*hs | xargs hasktags
	mv tags tags.tmpfile
	sort tags.tmpfile > tags
	$(RM) tags.tmpfile

# -----
# Clean targets

# The environment file names dist-newstyle's package database, so it goes
# with the build it describes.
.PHONY: clean
clean:
	$(RM) -rf $(TOP)/dist-newstyle $(CABAL_HEAD) $(TOP)/.ghc.environment.*

# The three generated modules an older checkout's make build left in this
# directory, which the hooks refuse to build over.
.PHONY: full_clean
full_clean: clean
	$(RM) -f BuildSystem.hs BuildVersion.hs Warmup.hs
	$(RM) tags TAGS

# -----

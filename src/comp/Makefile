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
# The ghc-pkg of the selected GHC, for make-ghc-pkg-info.sh (cabal finds
# its own): ghc-9.4.7 has ghc-pkg-9.4.7 beside it; a wrapper may set GHCPKG.
GHCPKG ?= $(if $(findstring /,$(GHC)),$(dir $(GHC)))ghc-pkg$(patsubst ghc%,%,$(notdir $(GHC)))
# GHCJOBS is the parallelism of the cabal build, passed as `cabal build -jN`.
# What N bounds depends on the GHC (doc/cabal.md section 2): from 9.12,
# cabal.project puts cabal-install and every GHC it runs under one job
# semaphore of N slots, so N is the number of modules compiling at once over
# the whole build; below 9.12 a parallel compile is not deterministic (no
# -fobject-determinism), so each package compiles serially and N is only the
# number of packages building at once.  The default is the machine's
# processor count, capped at 16: measured, the build gains nothing past
# -j12, since only 5 or 6 modules are ready to compile at any moment.
GHCJOBS ?= $(shell n=`nproc 2>/dev/null || sysctl -n hw.ncpu 2>/dev/null || echo 1`; if [ "$$n" -gt 16 ]; then echo 16; else echo "$$n"; fi)

GHCVERSION=$(shell $(GHC) --numeric-version 2>/dev/null)
# A GHC that prints no version (one named by GHC= and not on PATH, or none
# at all) stops here, named, as the make build's check stopped.
ifeq ($(GHCVERSION),)
$(error GHC not found: '$(GHC) --version' printed no version (INSTALL.md))
endif
$(info Building with GHC $(GHCVERSION))
# The floor bsc.cabal's bounds set (INSTALL.md), checked here as the make
# build checked its own, so that an older GHC is named rather than reported
# by cabal's solver; a newer series than CI tests, which bsc.cabal's upper
# bounds refuse, is a warning naming the remedy.
GHCMAJOR=$(shell echo $(GHCVERSION) | cut -d. -f1)
GHCMINOR=$(shell echo $(GHCVERSION) | cut -d. -f2)
GHCPATCH=$(shell echo $(GHCVERSION) | cut -d. -f3)
ifeq ($(shell expr $(GHCMAJOR) \< 9 \| $(GHCMAJOR) = 9 \& \( $(GHCMINOR) \< 4 \| $(GHCMINOR) = 4 \& $(GHCPATCH) \< 7 \)),1)
$(error Unsupported GHC $(GHCVERSION): 9.4.7 or newer is required (INSTALL.md))
endif
ifeq ($(shell expr $(GHCMAJOR) \> 9 \| $(GHCMAJOR) = 9 \& $(GHCMINOR) \> 14),1)
$(warning GHC $(GHCVERSION) is newer than the series bsc.cabal's upper bounds admit (through 9.14): raise them, or allow-newer in cabal.project.local (INSTALL.md))
endif

# GHC's RTS options for every GHC that compiles the package, as the make
# build set them: a heap cap of 3 GB plus 1 GB per job and a 256 MB
# allocation area, the measured plateau for parallel builds; CI sets its own
# (+RTS -M5G -A128m -RTS). Passed to cabal as GHC options, which reach the
# package's compiles and nothing else (the dependencies, built once into the
# store, take GHC's defaults), and given to list-bin as well so that both
# see one configuration.
GHCMAXHEAP ?= $(shell expr 3 + $(GHCJOBS))G
GHCRTSFLAGS ?= +RTS -M$(GHCMAXHEAP) -A256m -RTS

# BSC_BUILD selects a variant of the build, as it did for the make build:
# PROF (profiling, cost centres on every function, and the larger stack cap
# their frames need), DEBUG (Core lint and the debugging runtime) or NOOPT
# (unoptimized); unset, the project's settings. A cabal.project.local can
# set the same (INSTALL.md).
ifeq ($(BSC_BUILD),PROF)
$(info ----- Profiling build options -----)
BSC_BUILD_FLAGS = --enable-profiling --profiling-detail=all-functions \
	--ghc-options='"-with-rtsopts=-H256m -K256m -i1"'
else ifeq ($(BSC_BUILD),DEBUG)
$(info ----- Debug build options -----)
BSC_BUILD_FLAGS = --ghc-options='-dcore-lint -debug'
else ifeq ($(BSC_BUILD),NOOPT)
$(info ----- Unoptimized build options -----)
BSC_BUILD_FLAGS = --disable-optimization
else
BSC_BUILD_FLAGS =
endif

# A cabal.project.local at the root (INSTALL.md) is read by every cabal
# command here, so its settings apply to this build as well.
ifneq ($(wildcard $(TOP)/cabal.project.local),)
$(info ----- cabal.project.local present: its settings apply to this build -----)
endif

# What the cabal commands here take beyond the project's settings: the GHC
# make is given, as the make build took it, the variant and the RTS options;
# list-bin takes the same, since the build directory follows them.
CABAL_COMPILER_OPTIONS = --with-compiler=$(GHC)
CABAL_FLAGS = $(CABAL_COMPILER_OPTIONS) $(BSC_BUILD_FLAGS) $(if $(strip $(GHCRTSFLAGS)),--ghc-options='$(GHCRTSFLAGS)')

# -----
# GHC package info

# If BSC_BUILD_PKGINFO is non-empty,
# generate and install a LICENSES/LICENSE.ghc_pkgs file

ifeq ($(BSC_BUILD_PKGINFO),)
PKGINFO=
else
PKGINFO=$(LICDIR)/LICENSE.ghc_pkgs

# The packages the installed programs were linked against: the dependencies
# cabal-install built into its store and GHC's own (the trailing colon),
# walked from the in-place library's dependencies.
CABAL_COMPILER_STORE := $(shell cd $(TOP) && $(CABAL) path $(CABAL_COMPILER_OPTIONS) --compiler-info | sed -n 's/^compiler-store-path: //p')
ifeq ($(CABAL_COMPILER_STORE),)
$(error cabal path reports no compiler-store-path for $(GHC))
endif
CABAL_PKG_PATH = $(TOP)/dist-newstyle/packagedb/ghc-$(GHCVERSION):$(CABAL_COMPILER_STORE)/package.db:

.PHONY: $(PKGINFO)
$(PKGINFO):
	mkdir -p -m 755 $(LICDIR)
	GHCPKG="$(GHCPKG)" GHC_PACKAGE_PATH="$(CABAL_PKG_PATH)" ./make-ghc-pkg-info.sh --deps-of bsc > $(PKGINFO)
endif

# -----
# Install targets

# The build is `cabal build all` at the repository root: it compiles the
# library once and links every program against its archive, which is what
# makes the programs the same bytes from build to build (doc/cabal.md
# section 5). `install` copies bsc and bluetcl into $(BINDIR) and
# `install-extra` every other executable stanza of bsc.cabal, as the make
# build's targets did. The build's flags are bsc.cabal's and cabal.project's,
# with BSC_BUILD's variant and GHCRTSFLAGS on top (CABAL_FLAGS above).
CABAL_EXES = $(shell sed -n 's/^executable[[:space:]]*\([^[:space:]]*\).*/\1/p' $(TOP)/bsc.cabal)
# What `install` copies; `install-extra` copies every other stanza.
INSTALLEXES = $(BSCEXES) $(TCLEXES)
EXTRAEXES = $(filter-out $(INSTALLEXES),$(CABAL_EXES))

# BuildVersion names the commit, and its rule runs when cabal-install builds
# the package, which it does when a declared source changed. A commit that
# touches no source of the package (a testsuite or workflow change) would
# leave the previous commit's version in the programs, so `install` writes
# HEAD and the version name into a file bsc.cabal declares as a source: a
# new commit, or a tag at the built one, is then a changed source, and the
# version follows (doc/cabal.md section 2).
CABAL_HEAD = $(TOP)/build/cabal-head

# The cabal build links the solvers by name and is told no directory
# (doc/cabal.md section 2): src/Makefile installed them under
# $(PREFIX)/lib/SAT before descending here, and LIBRARY_PATH names that
# directory to the linker for every link of the build.

# The programs, each built with cabal and linked beside this Makefile, as the
# make build's targets built them (the link resolves the solvers through the
# build tree's lib/SAT, which a copy here would not); `all` is every
# program. The solvers are linked from $(PREFIX)/lib/SAT, where
# src/Makefile installs them (`make install-solvers` at the top).
CABAL_ENV = LIBRARY_PATH="$(abspath $(PREFIX))/lib/SAT$${LIBRARY_PATH:+:$$LIBRARY_PATH}"
BSCEXES = bsc
TCLEXES = bluetcl
UTILEXES = bsc2bsv bsv2bsc dumpbo dumpba vcdcheck
SHOWRULESEXES = showrules

# The generated modules an older checkout's make build left here, which the
# hooks would otherwise refuse to build over (doc/cabal.md section 3).
.PHONY: no-stale-modules
no-stale-modules:
	$(RM) -f BuildSystem.hs BuildVersion.hs Warmup.hs

.PHONY: all
all: $(CABAL_EXES)

.PHONY: $(CABAL_EXES)
$(CABAL_EXES): no-stale-modules
	cd $(TOP) && $(CABAL_ENV) $(CABAL) build -j$(GHCJOBS) $(CABAL_FLAGS) exe:$@
	ln -sf "$$(cd $(TOP) && $(CABAL) list-bin $(CABAL_FLAGS) bsc:exe:$@)" $@

.PHONY: install install-extra
install: CABAL_INSTALL = $(INSTALLEXES)
install-extra: CABAL_INSTALL = $(EXTRAEXES)
install install-extra: no-stale-modules
	mkdir -p -m 755 $(TOP)/build $(BINDIR)
	{ ./update-build-version.sh --head 2>/dev/null || echo no-repository; ./update-build-version.sh --name; } > $(CABAL_HEAD).tmp && \
	  { cmp -s $(CABAL_HEAD).tmp $(CABAL_HEAD) || mv $(CABAL_HEAD).tmp $(CABAL_HEAD); } && \
	  $(RM) -f $(CABAL_HEAD).tmp
	cd $(TOP) && $(CABAL_ENV) $(CABAL) build -j$(GHCJOBS) $(CABAL_FLAGS) all
	@set -e; for exe in $(CABAL_INSTALL); do \
	  bin=$$(cd $(TOP) && $(CABAL) list-bin $(CABAL_FLAGS) bsc:exe:$$exe); \
	  echo "$(INSTALL) -m 755 $$bin $(BINDIR)/$$exe"; \
	  $(INSTALL) -m 755 "$$bin" $(BINDIR)/$$exe; \
	done
ifneq ($(PKGINFO),)
	$(MAKE) $(PKGINFO)
endif

# The make build's group installs, over the one build.
.PHONY: install-bsc install-bluetcl install-utils install-showrules
install-bsc: ; $(MAKE) install CABAL_INSTALL="$(BSCEXES)"
install-bluetcl: ; $(MAKE) install CABAL_INSTALL="$(TCLEXES)"
install-utils: ; $(MAKE) install CABAL_INSTALL="$(UTILEXES)"
install-showrules: ; $(MAKE) install CABAL_INSTALL="$(SHOWRULESEXES)"

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
	$(RM) -rf $(CABAL_EXES) $(TOP)/dist-newstyle $(CABAL_HEAD) $(TOP)/.ghc.environment.*

.PHONY: full_clean
full_clean: clean no-stale-modules
	$(RM) tags TAGS

# -----

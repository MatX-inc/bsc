PWD:=$(shell pwd)
TOP:=$(PWD)/../..

INSTALL ?= install

PREFIX?=$(TOP)/inst
INSTALLDIR=$(PREFIX)/lib/Bluesim

COMPDIR=$(TOP)/src/comp

.PHONY: all
all: install

# -------------------------

# These get copied to the inst/lib/Bluesim area
HEADERS = bluesim_kernel_api.h \
          bluesim_host_ops.h \
          bluesim_host_ops_default.h \
          bluesim_introspection.h \
          bluesim_types.h \
          bluesim_primitives.h \
          bluesim_systemc.h \
          bs_wide_data.h \
          bs_prim_ops.h \
          bs_prim_storage.h \
          bs_prim_mod_reg.h bs_prim_mod_wire.h bs_prim_mod_probe.h \
          bs_prim_mod_fifo.h bs_prim_mod_regfile.h bs_prim_mod_bram.h \
          bs_prim_mod_counter.h \
          bs_prim_mod_clockgen.h \
          bs_prim_mod_synchronizers.h \
          bs_prim_mod_gatedclock.h \
          bs_prim_mod_clockmux.h \
          bs_prim_mod_resets.h \
          bs_reset.h \
          bs_str.h \
          bs_symbol.h \
          bs_system_tasks.h \
          bs_mem_defines.h bs_mem_file.h bs_range_tracker.h \
          bs_module.h bs_target.h \
          bs_model.h

# These ld export maps get copies to the inst/lib/Bluesim area
LINKFILES = bs_elf_export_map.txt \
            bs_mach-o_export_map.txt

# The link-time stack-depth analysis tool (see bk_stack_depth_bound()
# in bluesim_kernel_api.h); installed alongside the libraries so that
# bsc can run it when linking a model.
SCRIPTS = bluesim_stack_bound.py

# -------------------------

CFLAGS += -Wall -Wextra -Werror \
	-Wmissing-prototypes \
	-Wstrict-prototypes \
	-Wpointer-arith \
	-Wshadow \
	-Wcast-qual \
	-Wno-unused-parameter \
	-g \
	-std=c99 \
        -D_FILE_OFFSET_BITS=64 \
	-fPIC
# The runtime is linked into the model shared object, which must be
# freestanding: no exceptions or RTTI (their tables would import the
# unwinder and type-info machinery from libgcc/libstdc++), no stack
# protector (its failure path -- and on some hosts its canary -- lives
# in libc), and hidden visibility (the public entry points carry
# BS_EXPORT explicitly; see bluesim_types.h).
CXXFLAGS += -Wall -Wextra -Werror \
	-Wpointer-arith \
	-Wshadow \
	-Wcast-qual \
	-Wno-unused-parameter \
	-g \
	-D_ISOC99_SOURCE \
	-std=c++11 \
        -D_FILE_OFFSET_BITS=64 \
        -fno-rtti \
        -fno-exceptions \
        -fno-stack-protector \
        -fvisibility=hidden \
	-fPIC

CLANG_MAJVER=$(shell echo | $(CXX) -dM -E - | grep __clang_major__ | awk '{print $$3}')
ifneq ($(CLANG_MAJVER),)
ifeq ($(shell test $(CLANG_MAJVER) -ge 17; echo $$?),0)
CXXFLAGS += -Wno-vla-cxx-extension
endif
endif

# Compares two dotted numeric strings (e.g 2.3.16.1) for $1 >= $2
define version_ge
$(findstring TRUE,$(shell bash -c 'sort -cu -t. -k1,1nr -k2,2nr -k3,3nr -k4,4nr <(echo -e "$2\n$1") 2>&1 || echo TRUE'))
endef

glibcversion := $(shell ldd --version | head -1 | awk '{print $$NF}')
ifeq ($(call version_ge,$(glibcversion),2.20),TRUE)
CFLAGS += -D_DEFAULT_SOURCE
CXXFLAGS += -D_DEFAULT_SOURCE
else
CFLAGS += -D_SVID_SOURCE
CXXFLAGS += -D_SVID_SOURCE
endif

COPT ?= -O3
CDEBUG ?= -UUSE_ENTER
CFLAGS += $(PROF) $(COPT) $(CDEBUG)
CXXFLAGS += $(PROF) $(COPT) $(CDEBUG)

# Emit per-function stack usage and a call graph (.su/.ci files)
# alongside each object, for the link-time stack-depth analysis
# (bluesim_stack_bound.py).  These are pure diagnostics: they do not
# change code generation.  Clang lacks -fcallgraph-info, so probe for
# support; without it no .su/.ci ship and bk_stack_depth_bound()
# reports 0 (no bound available).
STACK_USAGE_FLAGS = -fstack-usage -fcallgraph-info=su
STACK_USAGE_OK := $(shell t=`mktemp -d` && \
	{ echo 'int bs_su_probe;' > $$t/p.cxx; \
	  $(CXX) $(STACK_USAGE_FLAGS) -c -o $$t/p.o $$t/p.cxx \
	    >/dev/null 2>&1 && echo yes; }; rm -rf $$t)
ifeq ($(STACK_USAGE_OK),yes)
CXXFLAGS += $(STACK_USAGE_FLAGS)
endif

# The vendored musl string/ctype routines (see src/vendor/musl/README.md),
# compiled into their own archive and linked into every generated model
# shared object so that the model provides these symbols locally (with
# hidden visibility) instead of importing them from the host's C library.
# The compiler emits memcpy/memset/memmove calls on its own, so they are
# built exactly as musl builds them: freestanding, with the builtins off
# (otherwise GCC can turn their own loops back into calls to themselves).
MUSLDIR = ../vendor/musl
MUSL_CFLAGS = -std=c99 -g $(COPT) -fPIC \
	-ffreestanding -fno-builtin -fno-stack-protector \
	-fvisibility=hidden \
	-Wall -Wno-parentheses -Wno-unused-parameter \
	-I$(MUSLDIR)/include
ifeq ($(STACK_USAGE_OK),yes)
MUSL_SU_FLAGS = $(STACK_USAGE_FLAGS)
endif

# Use the assembly implementations where musl has them for the build
# architecture (ELF only; the assembly uses ELF directives).  Every
# routine without assembly here comes from the generic C sources.
UNAME_S := $(shell uname -s)
UNAME_M := $(shell uname -m)
MUSL_MEM_ASM =
ifeq ($(UNAME_S),Linux)
ifeq ($(UNAME_M),x86_64)
MUSL_MEM_ASM = x86_64/memcpy.s x86_64/memmove.s x86_64/memset.s
endif
ifeq ($(UNAME_M),aarch64)
MUSL_MEM_ASM = aarch64/memcpy.S aarch64/memset.S
endif
endif

MUSL_MEM_ALL = memcpy memmove memset
MUSL_MEM_C = $(filter-out $(basename $(notdir $(MUSL_MEM_ASM))),$(MUSL_MEM_ALL))
MUSL_STR_C = strlen strcmp strncmp strchr strchrnul strcpy stpcpy \
             strncpy stpncpy
MUSL_CTYPE_C = isblank isdigit isupper isxdigit tolower

# Objects are prefixed to keep them apart from the runtime's own; the
# .su/.ci stack-usage data of the C-compiled members (the objects that
# are actually linked) ships with the runtime libraries below, while
# the assembly members are covered by the stack-depth analysis's
# conservative per-call allowances for string primitives.
STRING_C_OBJS = $(addprefix musl_,$(addsuffix .o,\
                    $(MUSL_MEM_C) $(MUSL_STR_C) $(MUSL_CTYPE_C)))
STRING_ASM_OBJS = $(addprefix musl_asm_,$(addsuffix .o,\
                    $(basename $(notdir $(MUSL_MEM_ASM)))))
STRING_OBJS = $(STRING_C_OBJS) $(STRING_ASM_OBJS)
LIB_STRING = libbsstring.a

vpath %.c $(MUSLDIR)/src/string $(MUSLDIR)/src/ctype

musl_%.o: %.c
	$(CC) $(MUSL_CFLAGS) $(MUSL_SU_FLAGS) -c -o $@ $<

# one assembly rule per architecture, so the same stem (e.g. memcpy)
# can never resolve to another architecture's source
ifeq ($(UNAME_M),x86_64)
musl_asm_%.o: $(MUSLDIR)/src/string/x86_64/%.s
	$(CC) -fPIC -Wa,--noexecstack -c -o $@ $<
endif
ifeq ($(UNAME_M),aarch64)
musl_asm_%.o: $(MUSLDIR)/src/string/aarch64/%.S
	$(CC) -fPIC -Wa,--noexecstack -c -o $@ $<
endif

$(LIB_STRING): $(STRING_OBJS)
	$(RM) $(LIB_STRING)
	$(AR) cr $(LIB_STRING) $(STRING_OBJS)

# Description of library of Bluesim primitives
LIB_PRIM = libbsprim.a
PRIM_OBJS = prim_ops.o wide_data.o mem_alloc.o target.o \
            prim_mod_reg.o prim_mod_wire.o prim_mod_probe.o \
            prim_mod_fifo.o prim_mod_regfile.o prim_mod_bram.o \
            prim_mod_counter.o \
            prim_mod_clockgen.o \
            prim_mod_synchronizers.o \
            prim_mod_gatedclock.o \
            prim_mod_resets.o \
            dollar_display.o dollar_plusargs.o \
            dollar_stop_finish.o dollar_time.o \
            mem_file.o module.o portability.o \
            rand32.o

# Description of Bluesim kernel library
LIB_KERNEL = libbskernel.a
KERNEL_OBJS = kernel.o event_queue.o priority.o symbol.o \
              reset.o plusargs.o portability.o stack_bound_default.o

LIBS = $(LIB_PRIM) $(LIB_KERNEL) $(LIB_STRING)

vpath %.cxx ../

%.o:%.cxx
	$(CXX) -E -MM $< > $*.p
	cp $*.p $*.d; \
	sed -e 's/#.*//' -e 's/^[^:]*: *//' -e 's/ *\\$$//' \
	    -e '/^$$/ d' -e 's/$$/ :/' < $*.p >> $*.d; \
	rm -f $*.p
	$(CXX) $(CXXFLAGS) -c -o $@ $<

# -------------------------

$(LIB_PRIM): version $(PRIM_OBJS)
	$(RM) $(LIB_PRIM)
	$(AR) cr $(LIB_PRIM) $(PRIM_OBJS)

$(LIB_KERNEL): version $(KERNEL_OBJS)
	$(RM) $(LIB_KERNEL)
	$(AR) cr $(LIB_KERNEL) $(KERNEL_OBJS)

# BuildVersion.hs is (re)generated in the source tree here, since this
# Makefile can run before (or without) a cabal build having done so.
.PHONY: version
version:
	cd $(COMPDIR) && \
		NOGIT=$${NOGIT:-0} NOUPDATEBUILDVERSION=$${NOUPDATEBUILDVERSION:-0} \
		./update-build-version.sh
	./gen_version_h $(COMPDIR)/BuildVersion.hs > version.h

# Static check of the header files before releasing them.
# Don't include headers which require systemC.
.PHONY: test-headers
test-headers:
	$(RM) test_hdr.cxx
	for hdr in $(HEADERS); do \
	  if [ $$hdr != "bluesim_systemc.h" ]; then \
	    echo "#include \"$$hdr\"" >> test_hdr.cxx ; \
	  fi ; \
	done
	$(CXX) -std=c++11 -c -o /dev/null test_hdr.cxx

.PHONY: install
install: $(LIBS) test-headers
	$(INSTALL) -m 755 -d $(INSTALLDIR)
	$(INSTALL) -m 644 $(HEADERS) $(INSTALLDIR)
	$(INSTALL) -m 644 $(LINKFILES) $(INSTALLDIR)
	$(INSTALL) -m 644 $(LIBS) $(INSTALLDIR)
	$(INSTALL) -m 755 $(SCRIPTS) $(INSTALLDIR)
ifeq ($(STACK_USAGE_OK),yes)
	$(INSTALL) -m 755 -d $(INSTALLDIR)/stack-usage
	$(INSTALL) -m 644 $(sort $(SU_SHIP_OBJS:.o=.su) $(SU_SHIP_OBJS:.o=.ci)) \
		$(INSTALLDIR)/stack-usage
endif

.PHONY: clean
clean:
	$(RM) test_hdr.cxx version.h
	$(RM) *.o *.p *.d *.su *.ci $(LIBS)

.PHONY: full_clean
full_clean: clean

TAGS:	*.h *.c *.cxx
	etags *.h *.c *.cxx

# the objects whose stack-usage data (.su/.ci) ships with the runtime
SU_SHIP_OBJS = $(PRIM_OBJS) $(KERNEL_OBJS) $(STRING_C_OBJS)

# include dependency information
DEPEND_OBJS = $(PRIM_OBJS) $(KERNEL_OBJS)

-include $(DEPEND_OBJS:.o=.d)

# -------------------------

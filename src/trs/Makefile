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
# in "bin" with no wrapper script.
.PHONY: build
build:
	$(CARGO) build --release --locked -p trs

.PHONY: install
install: build
	$(INSTALL) -m 755 -d $(BINDIR)
	$(INSTALL) -m 755 target/release/trs $(BINDIR)/trs

.PHONY: clean full_clean
clean full_clean:
	$(CARGO) clean

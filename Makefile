.PHONY: all build install uninstall doc test clean

ifneq ($(PREFIX),)
INSTALL_ARGS := $(INSTALL_ARGS) --prefix $(PREFIX)
endif

ifneq ($(LIBDIR),)
INSTALL_ARGS := $(INSTALL_ARGS) --libdir $(LIBDIR)
endif

all: build

build:
	dune build @install

install:
	dune install $(INSTALL_ARGS)

uninstall:
	dune uninstall $(INSTALL_ARGS)

doc:
	dune build @doc
	ln -sf _build/default/_doc doc

test:
	dune test

clean:
	dune clean
	rm -f doc

headers:
	headache -h .header $(shell find src/ tests/ -regex '.*\.ml[ily]?')

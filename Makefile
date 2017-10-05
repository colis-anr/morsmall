
all:
	$(MAKE) -C src
	mkdir -p lib
	cp src/_build/libmorsmall.* lib

install:
	@ if [ x$(PREFIX) = x ]; then                          \
	    printf '\nPlease use the following command:\n\n';  \
	    printf '    PREFIX=... make install\n\n';          \
	    printf 'to install morsmall at $PREFIX/lib';       \
	    exit 1;                                            \
	  fi
	ocamlfind install -destdir $(PREFIX)/lib libmorsmall META || true
	cp lib/* $(PREFIX)/lib/libmorsmall

clean:
	$(MAKE) -C src clean
	[ ! -d lib ] || rm -rf lib

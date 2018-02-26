.PHONY: all install remove clean

EXPORTED_SOURCES=                                                              \
        src/AST.ml                                                             \
        src/API.mli

build:
	jbuilder build @install

install:
	jbuilder install

uninstall:
	jbuilder uninstall

clean:
	jbuilder clean

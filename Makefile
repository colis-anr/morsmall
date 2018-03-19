.PHONY: build install uninstall doc clean tests

EXPORTED_SOURCES=                                                              \
        src/AST.ml                                                             \
        src/API.mli

build:
	jbuilder build @install

install:
	jbuilder install

uninstall:
	jbuilder uninstall

doc:
	jbuilder build @doc
	ln -s _build/default/_doc doc

tests:
	jbuilder build tests/run.exe
	_build/default/tests/run.exe

clean:
	jbuilder clean
	rm -f doc
	rm -f morsmall_test_report_*.md

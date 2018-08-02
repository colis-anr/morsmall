.PHONY: build install uninstall doc tests clean

build:
	dune build @install
	ln -sf _build/install/default/bin bin
	ln -sf _build/install/default/lib lib

install:
	dune install

uninstall:
	dune uninstall

doc:
	dune build @doc
	ln -sf _build/default/_doc doc

tests:
	dune build tests/run.exe
	_build/default/tests/run.exe

clean:
	dune clean
	rm -f bin lib doc
	rm -f morsmall_test_report_*.md

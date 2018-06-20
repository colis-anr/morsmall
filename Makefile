.PHONY: build install uninstall doc tests clean

build:
	jbuilder build @install
	ln -sf _build/install/default/bin bin
	ln -sf _build/install/default/lib lib

install:
	jbuilder install

uninstall:
	jbuilder uninstall

doc:
	jbuilder build @doc
	ln -sf _build/default/_doc doc

tests:
	jbuilder build tests/run.exe
	_build/default/tests/run.exe

clean:
	jbuilder clean
	rm -f bin lib doc
	rm -f morsmall_test_report_*.md

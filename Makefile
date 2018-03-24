.PHONY: build install uninstall doc tests clean

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

EMACS ?= emacs

.PHONY: compile test check

compile:
	$(EMACS) -Q --batch -L . -L test -f batch-byte-compile \
		packlet-runtime.el \
		packlet-source.el \
		packlet-parse.el \
		packlet-expand.el \
		packlet.el \
		test/packlet-test.el

test:
	$(EMACS) -Q --batch -L . -L test -l test/packlet-test.el -f ert-run-tests-batch-and-exit

check: compile test

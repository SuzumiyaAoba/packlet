EMACS ?= emacs

.PHONY: compile test check

TEST_FILES = \
	test/packlet-test-support.el \
	test/packlet-test-parse.el \
	test/packlet-test-source.el \
	test/packlet-test-loading.el \
	test/packlet-test-autoload.el \
	test/packlet-test-bindings.el \
	test/packlet-test-keywords.el \
	test/packlet-test-hooks.el \
	test/packlet-test.el

compile:
	$(EMACS) -Q --batch -L . -L test -f batch-byte-compile \
		packlet-runtime.el \
		packlet-source.el \
		packlet-parse.el \
		packlet-expand.el \
		packlet.el \
		$(TEST_FILES)

test:
	$(EMACS) -Q --batch -L . -L test -l test/packlet-test.el -f ert-run-tests-batch-and-exit

check: compile test

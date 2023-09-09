EMACS = emacs

.PHONY: test test-debug

test:
	$(EMACS) -batch -L . -l t/init.el -l t/vzi.el -f ert-run-tests-batch-and-exit

test-debug: SELECTOR =
test-debug:
	$(EMACS) -Q -L . -l t/init.el -l t/vzi.el --eval '(ert-run-tests-interactively "$(SELECTOR)")'

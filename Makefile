EMACS ?= emacs
# The Clojure project that provides replique, for the tests that need a
# process.  Without it they are skipped.
REPLIQUE_PROJECT ?=

SRC = replique-common.el replique-edn.el replique-conn.el replique-exception.el \
      replique-process.el replique-repl.el replique-eval.el replique.el

.PHONY: all compile test lint clean

all: compile test

compile:
	$(EMACS) -Q -batch -L . -L test -f batch-byte-compile $(SRC) test/replique-test.el

test:
	REPLIQUE_PROJECT=$(REPLIQUE_PROJECT) $(EMACS) -Q -batch -L . -L test \
	  -l replique-test -f ert-run-tests-batch-and-exit

lint:
	$(EMACS) -Q -batch -L . --eval "(progn (require 'checkdoc) \
	  (dolist (f (list $(patsubst %,\"%\",$(SRC)))) (checkdoc-file f)))"

clean:
	rm -f *.elc test/*.elc

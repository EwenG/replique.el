EMACS ?= emacs
# The Clojure project that provides replique, for the tests that need a
# process.  Without it they are skipped.
REPLIQUE_PROJECT ?=

SRC = replique-common.el replique-clojure-mode.el replique-edn.el \
      replique-conn.el replique-exception.el replique-process.el \
      replique-repl.el replique-locals.el replique-deps.el replique-eval.el \
      replique-pprint.el replique-completion.el replique-forms.el replique.el

# replique-clojure-mode.el is left out: it is master's file, carried over as
# it was, and its checkdoc warnings are not this tree's to answer
LINT = replique-common.el replique-edn.el replique-conn.el replique-exception.el \
       replique-process.el replique-repl.el replique-locals.el replique-deps.el \
       replique-eval.el replique-pprint.el replique-completion.el \
       replique-forms.el replique.el

.PHONY: all compile test lint clean

all: compile test

compile:
	$(EMACS) -Q -batch -L . -L test -f batch-byte-compile $(SRC) \
	  test/replique-test.el test/replique-locals-test.el test/replique-deps-test.el \
	  test/replique-pprint-test.el test/replique-completion-test.el \
	  test/replique-forms-test.el

test:
	REPLIQUE_PROJECT=$(REPLIQUE_PROJECT) $(EMACS) -Q -batch -L . -L test \
	  -l replique-test -l replique-locals-test -l replique-deps-test \
	  -l replique-pprint-test -l replique-completion-test -l replique-forms-test \
	  -f ert-run-tests-batch-and-exit

lint:
	$(EMACS) -Q -batch -L . --eval "(progn (require 'checkdoc) \
	  (dolist (f (list $(patsubst %,\"%\",$(LINT)))) (checkdoc-file f)))"

clean:
	rm -f *.elc test/*.elc

EMACS ?= emacs

# The macOS version the native compiler builds for.  gcc's driver works it out
# from the kernel's version when nothing says otherwise - Darwin 27 as macOS
# 18.0 - which was true until macOS renumbered itself 26, and clang then
# refuses the -mmacosx-version-min=18.0 it is handed: "invalid version
# number".  Naming it here is what the driver reads first, so nothing
# computes it.
#
# It matters for the suite and not only for `native', because a test that
# redefines a primitive - `message', say, to read what was said - makes emacs
# build a subr trampoline for it, and a trampoline is natively compiled.  A
# native compiler that cannot run takes those tests down with it.
ifeq ($(shell uname -s),Darwin)
MACOSX_DEPLOYMENT_TARGET ?= $(shell sw_vers -productVersion)
export MACOSX_DEPLOYMENT_TARGET
endif

# The Clojure project that provides replique, for the tests that need a
# process.  Without it they are skipped.
REPLIQUE_PROJECT ?=

SRC = replique-common.el replique-clojure-mode.el replique-edn.el \
      replique-parse.el \
      replique-conn.el replique-exception.el replique-process.el \
      replique-repl.el replique-locals.el replique-deps.el replique-eval.el \
      replique-pprint.el replique-forms.el replique-name.el \
      replique-fresh.el \
      replique-completion.el replique-symbol.el replique-lint.el \
      replique-main-js.el replique-css.el replique-reload.el \
      replique-stale.el replique.el

LINT = replique-common.el replique-parse.el replique-clojure-mode.el \
       replique-edn.el replique-conn.el \
       replique-exception.el \
       replique-process.el replique-repl.el replique-locals.el replique-deps.el \
       replique-eval.el replique-pprint.el replique-forms.el replique-name.el \
       replique-fresh.el \
       replique-completion.el replique-symbol.el replique-lint.el \
       replique-main-js.el replique-css.el replique-reload.el \
       replique-stale.el replique.el

.PHONY: all compile native test lint clean

all: compile test

compile:
	$(EMACS) -Q -batch -L . -L test -f batch-byte-compile $(SRC) \
	  test/replique-test.el test/replique-locals-test.el test/replique-deps-test.el \
	  test/replique-parse-test.el test/replique-clojure-mode-test.el \
	  test/replique-pprint-test.el test/replique-completion-test.el \
	  test/replique-forms-test.el test/replique-symbol-test.el \
	  test/replique-fresh-test.el test/replique-stale-test.el \
	  test/replique-main-js-test.el test/replique-css-test.el \
	  test/replique-reload-test.el \
	  test/replique-dialect-test.el \
	  test/replique-repl-choice-test.el \
	  test/replique-connect-test.el \
	  test/replique-name-fuzz-test.el \
	  test/replique-lint-test.el

# Natively compiled, into the eln cache this Emacs reads.
#
# Loading a .elc queues this on its own - see `native-comp-jit-compilation',
# which says "compile loaded .elc files asynchronously" and means the .elc
# rather than the .el: a file loaded as source is never natively compiled and
# never byte compiled either, it is interpreted.  So a checkout with no .elc
# in it runs interpreted however many cores are sitting idle.
#
# Doing it here rather than waiting for that queue is worth the seconds it
# takes, because the queue only runs after the file has been loaded once:
# the first session of the day is the one that would run byte compiled.  And
# what is being compiled is a reader written to be compiled - it reads four
# hundred kilobytes of Clojure in 19 ms byte compiled and 6.9 ms natively,
# against tree-sitter's 24, so byte compiled it is a wash and natively it is
# three and a half times quicker.  The same shows up a level up: laying out a
# 113 KB value costs 41.8 ms byte compiled and 24.9 ms natively.
native: compile
	$(EMACS) -Q -batch -L . -L test \
	  --eval "(mapc (lambda (f) (native-compile f)) \
	                (list $(patsubst %,\"%\",$(SRC))))"

# Compiled first, because that is what is then loaded.  `load-prefer-newer'
# is nil by default, so emacs loads the .elc whether or not the .el beside it
# is newer - which makes a suite run after an edit a suite run against the
# code as it was before the edit.  The first defeat test written against this
# tree found it by coming back inert thirteen times in a row.
test: compile
	REPLIQUE_PROJECT=$(REPLIQUE_PROJECT) $(EMACS) -Q -batch -L . -L test \
	  -l replique-test -l replique-locals-test -l replique-deps-test \
	  -l replique-parse-test -l replique-clojure-mode-test \
	  -l replique-pprint-test -l replique-completion-test \
	  -l replique-forms-test -l replique-symbol-test \
	  -l replique-fresh-test -l replique-stale-test \
	  -l replique-main-js-test -l replique-css-test \
	  -l replique-reload-test \
	  -l replique-dialect-test \
	  -l replique-repl-choice-test \
	  -l replique-connect-test \
	  -l replique-name-fuzz-test \
	  -l replique-lint-test \
	  -f ert-run-tests-batch-and-exit

lint:
	$(EMACS) -Q -batch -L . --eval "(progn (require 'checkdoc) \
	  (dolist (f (list $(patsubst %,\"%\",$(LINT)))) (checkdoc-file f)))"

clean:
	rm -f *.elc test/*.elc

# Makefile for the tp library.
#
# Usage:
#   make test                 # run all ERT test suites
#   make doctest              # execute README examples against the code
#   make compile              # byte-compile all modules
#   make clean                # remove compiled files
#
# If dash.el is not on the default load-path, point LOAD_EXTRA at it:
#   make test LOAD_EXTRA="-L ~/.emacs.d/elpa/dash-20240510.1327"

EMACS ?= emacs
LOAD_EXTRA ?=
LOADPATH = -L . $(LOAD_EXTRA)

SRC = tp-core.el tp-reactive.el tp-layer.el tp-ops.el tp-search.el \
      tp-render.el tp-stack.el tp-palette.el tp-builtins.el tp.el
TESTS = $(wildcard *-tests.el)

.PHONY: test doctest compile clean

test:
	$(EMACS) -Q --batch $(LOADPATH) -l tp.el $(patsubst %,-l %,$(TESTS)) \
	  -f ert-run-tests-batch-and-exit

doctest:
	$(EMACS) -Q --batch $(LOADPATH) -l tp-doctest.el

compile: clean
	$(EMACS) -Q --batch $(LOADPATH) -f batch-byte-compile $(SRC)

clean:
	rm -f *.elc

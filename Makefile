# Makefile for the tp library.
#
# Usage:
#   make test                 # run all ERT test suites
#   make test-shuffled        # run the suite in a random order (SHUFFLE_SEED=n reproduces)
#   make doctest              # execute README examples against the code
#   make compile              # byte-compile the library modules
#   make compile-all          # byte-compile modules + tests + dev scripts
#   make clean                # remove compiled files
#
# WERROR=t turns byte-compile warnings into errors (used in CI).
# If dash.el is not on the default load-path, point LOAD_EXTRA at it:
#   make test LOAD_EXTRA="-L ~/.emacs.d/elpa/dash-20240510.1327"

EMACS ?= emacs
LOAD_EXTRA ?=
WERROR ?= nil
LOADPATH = -L . $(LOAD_EXTRA)

SRC = tp-core.el tp-reactive.el tp-layer.el tp-ops.el tp-search.el \
      tp-render.el tp-stack.el tp-palette.el tp-builtins.el tp.el
TESTS = $(wildcard *-tests.el)
DEV = tp-doctest.el tp-run-shuffled.el

.PHONY: test test-shuffled doctest compile compile-all clean

test:
	$(EMACS) -Q --batch $(LOADPATH) -l tp.el $(patsubst %,-l %,$(TESTS)) \
	  -f ert-run-tests-batch-and-exit

test-shuffled:
	$(EMACS) -Q --batch $(LOADPATH) -l tp.el $(patsubst %,-l %,$(TESTS)) \
	  -l tp-run-shuffled.el

doctest:
	$(EMACS) -Q --batch $(LOADPATH) -l tp-doctest.el

compile: clean
	$(EMACS) -Q --batch $(LOADPATH) \
	  --eval "(setq byte-compile-error-on-warn $(WERROR))" \
	  -f batch-byte-compile $(SRC)

compile-all: clean
	$(EMACS) -Q --batch $(LOADPATH) \
	  --eval "(setq byte-compile-error-on-warn $(WERROR))" \
	  -f batch-byte-compile $(SRC) $(TESTS) $(DEV)

clean:
	rm -f *.elc

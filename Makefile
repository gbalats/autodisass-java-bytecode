EMACS ?= emacs
CASK  ?= cask

CASKED_EMACS = $(CASK) exec $(EMACS)
DISTDIR := dist

project         := autodisass-java-bytecode
project_version := $(shell $(CASK) version)
project_pkgdir  := $(shell $(CASK) package-directory)

export EMACS


all: $(project)-pkg.el compile

deps: | $(project_pkgdir)

compile: deps
	$(CASK) build

run:
	$(CASKED_EMACS) -Q -l $(project).el


# Cleaning targets

clean: clean-elc clean-dist
distclean: clean clean-deps

clean-dist:
	rm -rf $(DISTDIR)/

clean-elc:
	$(CASK) clean-elc

clean-deps:
	rm -rf .cask/

dist:
	$(CASK) package $(DISTDIR)

.PHONY: all compile run \
	clean clean-deps clean-elc clean-dist \
	dist distclean deps


# Package descriptor file

$(project)-pkg.el: $(project).el Cask
	$(CASK) pkg-file


# Package directory

$(project_pkgdir):
	$(CASK) install

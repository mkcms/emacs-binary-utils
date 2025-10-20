emacs ?= emacs

SHELL := /bin/bash

FILES :=                        \
        asm-data-test.el        \
        asm-data.el             \
        asm-jump.el             \
        asm-x86.el              \
        asm2src.el              \
        bdx-test.el             \
        bdx.el                  \
        binfile-test.el         \
        binfile.el              \
        compdb.el               \
        untemplatize-cxx.el     \

ELC := $(FILES:.el=.elc)

PACKAGE_INIT := -f package-initialize

INSTALL_DEPENDENCIES := ${PACKAGE_INIT} --eval '(progn                             \
	(load "seq" nil t)                                                         \
	(load "project" nil t)                                                     \
	(unless (and (fboundp `seq-contains-p)                                     \
	             (fboundp `project-name)                                       \
	             (package-installed-p (quote package-lint)))                   \
	  (push (quote ("melpa" . "https://melpa.org/packages/")) package-archives)\
	  (package-refresh-contents)                                               \
	  (package-install (quote package-lint))                                   \
	  (package-install (cadr (assoc `project package-archive-contents)))       \
	  (package-install (cadr (assoc `map package-archive-contents)))           \
	  (package-install (cadr (assoc `seq package-archive-contents))))          \
	(unless (package-installed-p `ivy)                                         \
	  (package-refresh-contents)                                               \
	  (package-install `ivy)))'

IGNORED_LINT_WARNINGS := \
	-e "You should depend on (emacs \"29\\.1\") or the compat package if you need \`take'\\." \
	-e "warning: \`with-eval-after-load' is for use in configurations," \

LIBS := $(patsubst %.el,-l %,${FILES})

SELECTOR ?= .*

deps:
	${emacs} -Q --batch ${INSTALL_DEPENDENCIES}

compile: deps ${ELC}

check: compile
	${emacs} -Q --batch ${PACKAGE_INIT} -L . ${LIBS}                      \
	    --eval '(ert-run-tests-batch-and-exit "${SELECTOR}")'

%.elc: %.el
	${emacs} -Q --batch ${PACKAGE_INIT} -L .                              \
	    --eval '(setq byte-compile-error-on-warn t)'                      \
	    -f batch-byte-compile $<

%.lint-checkdoc: %.el
	@lint=$$(mktemp);                                                     \
	${emacs} -Q --batch $<                                                \
		--eval '(checkdoc-file (buffer-file-name))' 2>&1 | tee $$lint \
        && test -z "$$(cat $$lint)"

%.lint-long-lines: %.el
	@sed '1{s/.*//}' $< | grep -n -E "^.{80,}" `# Catch long lines`       \
	    | sed  -r 's/^([0-9]+).*/'$<':\1: Too long/;q1';

%.lint-package: %.el
	@file=$$(mktemp);result=$$(mktemp);                              \
	${emacs} -Q --batch ${PACKAGE_INIT}                              \
	  -f 'package-lint-batch-and-exit' $<  2>$$file || true          \
	&& sed -i "/^Entering directory/d" $$file                        \
	&& grep -v ${IGNORED_LINT_WARNINGS} $$file | tee $$result        \
	&& test -z "$$(cat $$result)"

%.lint: %.el %.lint-checkdoc %.lint-long-lines %.lint-package
	@true

lint: $(patsubst %.el,%.lint,$(filter-out %-test.el,$(FILES)))

clean:
	rm -f *.elc

update-copyright-years:
	year=`date +%Y`;                                                      \
	sed -i *.el *.md -r                                                   \
	  -e 's/Copyright \(C\) ([0-9]+)(-[0-9]+)?/Copyright (C) \1-'$$year'/'

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
        compiled-file.el        \
        untemplatize-cxx.el     \

ELC := $(FILES:.el=.elc)

PACKAGE_INIT := -f package-initialize

INSTALL_DEPENDENCIES := ${PACKAGE_INIT} --eval '(progn                        \
	(load "seq" nil t)                                                    \
	(load "project" nil t)                                                \
	(unless (and (fboundp `seq-contains-p) (fboundp `project-name))       \
	  (package-refresh-contents)                                          \
	  (package-install (cadr (assoc `project package-archive-contents)))  \
	  (package-install (cadr (assoc `map package-archive-contents)))      \
	  (package-install (cadr (assoc `seq package-archive-contents))))     \
	(unless (package-installed-p `ivy)                                    \
	  (package-refresh-contents)                                          \
	  (package-install `ivy)))'

LIBS := $(patsubst %.el,-l %,${FILES})

SELECTOR ?= .*

.PHONY: deps
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

lint:
	set -e;                                                               \
	files=(                                                               \
	  asm-data.el                                                         \
	  asm-jump.el                                                         \
	  asm-x86.el                                                          \
	  asm2src.el                                                          \
	  bdx.el                                                              \
	  binfile.el                                                          \
	  compdb.el                                                           \
	  compiled-file.el                                                    \
	  untemplatize-cxx.el                                                 \
	);                                                                    \
	for f in $${files[@]}; do                                             \
	    lint=$$(mktemp)                                                   \
	    && ${emacs} -Q --batch $$f                                        \
		--eval '(checkdoc-file (buffer-file-name))' 2>&1 | tee $$lint \
	    && test -z "$$(cat $$lint)";                                      \
	done;                                                                 \
	for f in $${files[@]}; do                                             \
	    sed '1{s/.*//}' $$f | grep -n -E "^.{80,}" `# Catch long lines`   \
	    | sed  -r 's/^([0-9]+).*/'$$f':\1: Too long/;q1';                 \
	done

clean:
	rm -f *.elc

update-copyright-years:
	year=`date +%Y`;                                                      \
	sed -i *.el *.md -r                                                   \
	  -e 's/Copyright \(C\) ([0-9]+)(-[0-9]+)?/Copyright (C) \1-'$$year'/'

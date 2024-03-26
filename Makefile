emacs ?= emacs
FILES :=                        \
        asm-data-test.el        \
        asm-data.el             \
        asm-jump.el             \
        asm-x86.el              \
        binfile-test.el         \
        binfile.el              \
        compdb.el               \
        compiled-file.el        \
        objdump-test.el         \
        objdump.el              \

ELC := $(FILES:.el=.elc)

LIBS := $(patsubst %.el,-l %,${FILES})

SELECTOR ?= .*

check: compile
	${emacs} -Q -L . ${LIBS} --batch --eval '(ert-run-tests-batch-and-exit "${SELECTOR}")'

%.elc: %.el
	${emacs} --batch -L . -f batch-byte-compile $<

clean:
	rm -f *.elc

update-copyright-years:
	year=`date +%Y`;                                                      \
	sed -i *.el *.md -r                                                   \
	  -e 's/Copyright \(C\) ([0-9]+)(-[0-9]+)?/Copyright (C) \1-'$$year'/'

compile: ${ELC}

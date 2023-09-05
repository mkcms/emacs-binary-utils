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

compile: ${ELC}

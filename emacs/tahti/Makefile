OSTYPE=$(shell echo $$OSTYPE)
ifeq (,$(findstring darwin,$(OSTYPE)))
  EMACS=emacs
else
  EMACS=/Applications/Emacs.app/Contents/MacOS/Emacs
endif

BATCH=$(EMACS) -batch -q -no-site-file \
  -L lisp -L lisp/modes -L el-get/el-get \
  -eval "(setq max-specpdl-size 2000 max-lisp-eval-depth 1000)" \

ELC= $(BATCH) -f batch-byte-compile

ELFILES=$(wildcard lisp/*.el) $(wildcard lisp/modes/*.el)
ELCFILES=$(ELFILES:%.el=%.elc)

.SUFFIXES: .el .elc

compile: $(ELCFILES)

.el.elc:
	$(ELC) $<

info:
	echo $(ELCFILES)

clean:
	rm -rf $(ELCFILES)


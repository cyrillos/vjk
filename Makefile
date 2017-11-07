.PHONY: all .FORCE
.DEFAULT_GOAL := all

ifeq ($(strip $(V)),)
        E := @echo
        Q := @
else
        E := @\#
        Q :=
endif

export E Q

define msg-gen
        $(E) "  GEN     " $(1)
endef

define msg-clean
        $(E) "  CLEAN   " $(1)
endef

export msg-gen msg-clean

MAKEFLAGS += --no-print-directory
export MAKEFLAGS

RM		?= rm -f
MAKE		?= make
GIT		?= git
CP		?= cp -f
CTAGS		?= ctags
PYTHON		?= python3
SBCL		?= sbcl

export RM MAKE GIT CP CTAGS PYTHON SBCL

srv-y	+= src/server/vjk.lisp
cli-y	+= src/client/vjk.py

srv: $(srv-y)
	$(Q) $(SBCL) --load $(srv-y) --conf conf/vjk.json
.PHONY: srv

cli-start: $(cli-y)
	$(Q) $(PYTHON) $(cli-y) --conf conf/vjk.json start psmb@vz,sas32
.PHONY: cli-start

cli-stop: $(cli-y)
	$(Q) $(PYTHON) $(cli-y) --conf conf/vjk.json stop
.PHONY: cli-start

.SUFFIXES:

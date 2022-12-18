# Compile
CC := clang
CFLAGS += -Wall -Wextra -Wno-unused-parameter -fsanitize=address,undefined
CPPFLAGS += -I..

# Assemble
AS := nasm

UNAME_S := $(shell uname -s)
ifeq ($(UNAME_S), Darwin)
  CPPFLAGS += -D'GLOBAL(name)=name'
  ASFLAGS += -fmacho64
else ifeq ($(UNAME_S), Linux)
  CPPFLAGS += -D'GLOBAL(name)=_\#\#name'
  ASFLAGS += -felf64
else
  $(error $(UNAME_S) unsupported)
endif

# Link
LDFLAGS += -fsanitize=address,undefined

# Check
FILECHECK := $(shell command -v FileCheck 2> /dev/null)
ifeq ($(FILECHECK),)
  FILECHECK := ../filecheck.sh
endif

.PRECIOUS: %.s
%.s: compile.scm %.input
	./tora $^ > $@

.PHONY: check
check:
	for x in $^; do ./$$x | $(FILECHECK) $$x.input; done

.PHONY: clean
clean::
	$(RM) *.s *.o

.PHONY: all

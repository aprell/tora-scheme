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

.PRECIOUS: %.s
%.s: compile.scm %.input
	./tora $^ > $@

.PHONY: clean
clean::
	rm -f *.s *.o

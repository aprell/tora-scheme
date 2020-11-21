# Compile
CC := clang
CFLAGS += -Wall -Wextra -Wno-unused-parameter -fsanitize=address,undefined

# Assemble
AS := nasm
ASFLAGS += -fmacho64 # requires macOS

# Link
LDFLAGS += -fsanitize=address,undefined

.PRECIOUS: %.s
%.s: compile.scm %.input
	./tora $^ > $@

.PHONY: clean
clean::
	rm -f *.s *.o

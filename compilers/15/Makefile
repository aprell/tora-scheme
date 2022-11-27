all: test fib Y

test: test.o runtime.o
fib: fib.o runtime.o
Y: Y.o runtime.o

check: test fib Y

clean::
	$(RM) test fib Y

include ../common.mk

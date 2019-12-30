#include <assert.h>
#include <inttypes.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#define MASK_BITS 3
#define MASK ((1 << MASK_BITS) - 1)

#define IMM_MASK_BITS (MASK_BITS + 2)
#define IMM_MASK ((1 << IMM_MASK_BITS) - 1)

#define HEAP_SIZE 1000000

extern int64_t entry(void *);
void print(int64_t);

enum type {
	immediate = 0, // 0b000
	box,           // 0b001
	pair,          // 0b010
	fun            // 0b011
};

enum immediate_type {
	number = 0,         // 0b00000
	boolean_true = 8,   // 0b01000
	boolean_false = 16, // 0b10000
	empty = 24          // 0b11000
};

void error(void)
{
	printf("err\n");
	exit(EXIT_FAILURE);
}

void print_immediate(int64_t a)
{
	switch (a & IMM_MASK) {
	case number:
		printf("%" PRId64, a >> IMM_MASK_BITS);
		break;
	case boolean_true:
		printf("#t");
		break;
	case boolean_false:
		printf("#f");
		break;
	case empty:
		printf("()");
		break;
	default:
		assert(0 && "Internal error");
		break;
	}
}

void print_box(int64_t a)
{
	printf("#&");
	print(*((int64_t *)(a ^ box)));
}

void print_pair(int64_t a)
{
	int64_t car = *((int64_t *)((a + 0) ^ pair));
	int64_t cdr = *((int64_t *)((a + 8) ^ pair));

	print(car);

	if ((cdr & IMM_MASK) == empty) return;

	if ((cdr & MASK) == pair) {
		printf(" ");
		print_pair(cdr);
	} else {
		printf(" . ");
		print(cdr);
	}
}

void print_fun(int64_t a)
{
	printf("#&(fun %p)", (int64_t *)(a ^ fun));
}

void print(int64_t result)
{
	switch (result & MASK) {
	case immediate:
		print_immediate(result);
		break;
	case box:
		print_box(result);
		break;
	case pair:
		printf("(");
		print_pair(result);
		printf(")");
		break;
	case fun:
		print_fun(result);
		break;
	default:
		assert(0 && "Internal error");
		break;
	}
}

int main(int argc, char *argv[])
{
	void *heap = malloc(HEAP_SIZE);
	int64_t result = entry((assert(heap), heap));
	print(result);
	printf("\n");
	free(heap);
	return 0;
}

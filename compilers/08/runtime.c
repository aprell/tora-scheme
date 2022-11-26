#include <inttypes.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include "common.h"

#define MASK_BITS 1
#define MASK ((1 << MASK_BITS) - 1)

extern int64_t GLOBAL(entry)();

enum type {
    number = 0,
    boolean
};

void GLOBAL(error)(void) {
    printf("err\n");
    exit(EXIT_FAILURE);
}

int main(int argc, char *argv[]) {
    int64_t result = GLOBAL(entry)();
    switch (result & MASK) {
    case number:
        printf("%" PRId64 "\n", result >> MASK_BITS);
        break;
    case boolean:
        printf("#%c\n", result >> MASK_BITS ? 't' : 'f');
        break;
    }
    return 0;
}

#include <inttypes.h>
#include <stdint.h>
#include <stdio.h>
#include "common.h"

#define MASK_BITS 1
#define MASK ((1 << MASK_BITS) - 1)

enum type {
    number = 0,
    boolean
};

extern int64_t GLOBAL(entry)();

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

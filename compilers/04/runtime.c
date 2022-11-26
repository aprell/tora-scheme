#include <inttypes.h>
#include <stdint.h>
#include <stdio.h>
#include "common.h"

extern int64_t GLOBAL(entry)();

int main(int argc, char *argv[]) {
    int64_t result = GLOBAL(entry)();
    printf("%" PRId64 "\n", result);
    return 0;
}

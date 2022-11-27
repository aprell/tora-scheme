#!/bin/sh

set -eu

for compiler in $(seq -f "%02.0f" 3 15); do
    printf "%s: " "$compiler"
    if make -sC "$compiler" check 2> /dev/null; then
        printf "\e[32mPASS\e[0m\n"
    else
        printf "\e[31mFAIL\e[0m\n"
    fi
done

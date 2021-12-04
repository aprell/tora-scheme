#!/usr/bin/env bash
# ShellChecked

set -eu
set -o pipefail

for test in test/test.scm; do
    OUTPUT="test/$(basename "${test%.*}").out"
    sed -n "s/.*;;\s*CHECK:\s*\(.*\)$/\1/p" "$test" > "$OUTPUT.expect"
    ./tora "$test" > "$OUTPUT.actual"
    if git diff --no-index "$OUTPUT.expect" "$OUTPUT.actual"; then
        rm "$OUTPUT.actual"
    fi
done

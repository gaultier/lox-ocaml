#!/bin/sh

if command -v patdiff
then
    export DIFFTOOL=patdiff
else
    export DIFFTOOL="diff"
fi
printf "test/arithmetic.lox\ntest/boolean_logic.lox" | parallel  --shuf --timeout 4 "awk -F '// expect: ' '/expect/{print $2}' < {} > {.}.expected; ./lox run {} > {.}.output 2>&1; $DIFFTOOL {.}.expected {.}.output && (echo {} '✓' || echo {} '✗')"

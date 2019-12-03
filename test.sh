#!/bin/sh

if command -v patdiff
then
    export DIFFTOOL=patdiff
else
    export DIFFTOOL="diff"
fi

if [ -z "$LOXBIN" ]
then
    LOXBIN='./lox'
fi

find test -name '*.lox' -type f | parallel  --shuf --timeout 4 "awk -F '// expect: ' '/expect/{print \$2}' < {} > {.}.expected; $LOXBIN run {} > {.}.output 2>&1; $DIFFTOOL {.}.expected {.}.output && (echo {} '✓' || echo {} '✗')"

#!/bin/sh

if command -v patdiff
then
    export DIFFTOOL=patdiff
else
    export DIFFTOOL="diff"
fi

find test -name '*.lox' -type f | parallel  --shuf --timeout 4 "./lox run {} > {.}.output 2>&1; $DIFFTOOL {.}.expected {.}.output && (echo {} '✓' || echo {} '✗')"

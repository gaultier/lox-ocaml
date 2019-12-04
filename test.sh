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

find test -name '*.lox' -type f | sort | parallel -k --timeout 4 " \
    awk -F '// expect: ' '/expect/{print \$2}' < {} > $TMPDIR/{/}.expected; \
    $LOXBIN run {} > $TMPDIR/{/}.output 2>&1; \
    $DIFFTOOL $TMPDIR/{/}.expected $TMPDIR/{/}.output && \
    printf '\033[32m✓ {}\033[0m\n'  || (printf '\033[31m✗ {}\033[0m\n'; exit 1)"

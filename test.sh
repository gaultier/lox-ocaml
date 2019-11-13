#!/bin/sh

find test -name '*.lox' -type f | parallel  --shuf --timeout 2 './lox run {} > {.}.output 2>&1; patdiff {.}.expected {.}.output && (echo {} "✓" || echo {} "✗")'

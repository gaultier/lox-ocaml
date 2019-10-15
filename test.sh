#!/bin/sh
set -ex

if command -v patdiff
then
    DIFFTOOL=patdiff
else
    DIFFTOOL="diff"
fi

for i in test/*.lox
do
    printf "Testing [%s]" "$i"
    ./lox run "$i" > "${i%.lox}.output" 2>&1
    $DIFFTOOL "${i%.lox}.expected" "${i%.lox}.output"
done

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
    ./_build/default/bin/main.exe build "$i" > "${i%.lox}.output"
    $DIFFTOOL "${i%.lox}.expected" "${i%.lox}.output"
done

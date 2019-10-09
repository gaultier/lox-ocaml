#!/bin/sh
set -ex

for i in test/*.lox
do
    printf "Testing %s" "$i"
    ./_build/default/bin/main.exe build "$i" > "${i%.lox}.output"
    diff "${i%.lox}.expected" "${i%.lox}.output"
done

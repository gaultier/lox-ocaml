#!/bin/sh

if command -v patdiff
then
    DIFFTOOL=patdiff
else
    DIFFTOOL="diff"
fi

RET=0

for i in $(find test -name "*.lox" -type f | sort -R)
do
    printf "%s" "$i"
    ./lox run "$i" > "${i%.lox}.output" 2>&1
    LOX_RET=$?
    DIFF=$($DIFFTOOL "${i%.lox}.expected" "${i%.lox}.output")
    DIFF_RET=$?

    if [ "$LOX_RET" = 0 ] && [ "$DIFF_RET" = 0 ]
    then
        printf " ✓\n"
    else 
        printf " x\n"
        RET=1
        echo "$DIFF"
    fi
done

exit $RET

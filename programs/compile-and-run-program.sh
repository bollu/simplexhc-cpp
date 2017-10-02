#!/usr/bin/env bash
set -e
SIMPLEXHC=../build/simplexhc
LIBSTGPATH=../build/
LLC=llc
CC=clang

$SIMPLEXHC < $1  out.ll >/dev/null
ret=$?
if [ $ret -ne 0 ]; then
    cat out.ll
    rm out.ll
else
    $LLC out.ll -filetype=obj -o out.o
    rm out.ll
    $CC out.o -L $LIBSTGPATH -lstgruntime -o $1.out
    rm out.o
fi;

./$1.out

#!/usr/bin/env bash
set -e
SIMPLEXHC=../build/simplexhc
LIBSTGPATH=../build/
LLC=llc
CC=clang

OUTLL=$1".out.ll"
OUTO=$1".out.o"

$SIMPLEXHC < $1  $OUTLL >/dev/null
ret=$?
if [ $ret -ne 0 ]; then
    cat $OUTLL
    rm $OUTLL
else
    $LLC $OUTLL -filetype=obj -o $OUTO
    rm $OUTLL
    $CC $OUTO -L $LIBSTGPATH -lstgruntime -o $1.out
    rm $OUTO
fi;

if [  $(dirname $1)  == $(dirname $0) ]; then
    # need to figure out how to set STG programs's exit code :)
    ./$1.out || true
else
    $1.out || true
fi;

exit 0


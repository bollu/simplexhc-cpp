#!/usr/bin/env bash
set -o xtrace
set -e
SIMPLEXHC=../build/sxhc
LIBSTGPATH=../build/
# LLC=llc
LLVMEXTRACT=/Users/bollu/work/LLVM-all/polly/llvm_build_ninja/bin/llvm-extract
LLC=/Users/bollu/work/LLVM-all/polly/llvm_build_ninja/bin/llc
OPT=/Users/bollu/work/LLVM-all/polly/llvm_build_ninja/bin/opt
CC=gcc

OUTO=$1".out.o"
OUTLL=$1".out.ll"

$SIMPLEXHC  $1 --emit-llvm -o $OUTLL ${@:2}  -O 3
$OPT  -instnamer  $OUTLL -S -o temp; mv temp $OUTLL

$LLC $OUTLL -o $OUTO -filetype=obj -O 3

$CC $OUTO -L $LIBSTGPATH -lstgruntime -o $1.out
rm $OUTO

echo "----SUCCESSFULLY COMPILED----"

# If this is at the CWD, then we need to execute it with `./` prepended
# to the path. Otherwise, we can directly execute it.
if [  $(dirname $1)  == $(dirname $0) ]; then
    # need to figure out how to set STG programs's exit code :)
    time ./$1.out || true
else
   time  $1.out || true
fi;

exit 0


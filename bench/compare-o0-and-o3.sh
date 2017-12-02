#!/usr/bin/env bash
set -o xtrace
set -e
SIMPLEXHC=sxhc
LIBSTGPATH=/home/bollu/work/sxhc/build/
# LLC=llc
LLVMEXTRACT=/home/bollu/build/llvm-5.0/build/bin/llvm-extract
LLC=/home/bollu/build/llvm-5.0/build/bin/llc
OPT=/home/bollu/build/llvm-5.0/build/bin/opt
CC=gcc

OUTO0=$1".out.o0"
OUTO3=$1".out.o3"
OUTO3_FROM_OUTO0=$1".out.o3.from.o0"

$SIMPLEXHC  $1 --emit-llvm -o $OUTO0".ll"  -O 0
$LLVMEXTRACT -func=main -S -o $OUTO0".main.ll" $OUTO0".ll"
$OPT -dot-cfg $OUTO0".main.ll"
mv cfg.main.dot $OUTO0".main.dot"

$OPT -O3 -S $OUTO0".ll" -o $OUTO3_FROM_OUTO0".ll"
$LLVMEXTRACT -func=main -S -o $OUTO3_FROM_OUTO0".main.ll" $OUTO3_FROM_OUTO0".ll"
$OPT -dot-cfg $OUTO3_FROM_OUTO0".main.ll"
mv cfg.main.dot $OUTO3_FROM_OUTO0".main.dot"


$SIMPLEXHC  $1 --emit-llvm -o $OUTO3".ll"  -O 3
$LLVMEXTRACT -func=main -S -o $OUTO3".main.ll" $OUTO3".ll"
$OPT -dot-cfg $OUTO3".main.ll"
mv cfg.main.dot $OUTO3".main.dot"

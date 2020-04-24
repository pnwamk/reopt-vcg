#!/bin/bash

deps="/home/vagrant/reopt-vcg/lean4/deps"

# LEAN_PATH wants LIB_NAME=PATH_TO_LIB entries
GALOIS_LIB="Galois=$deps/galois_stdlib/src/Galois"
X86SEM_LIB="X86Semantics=$deps/x86_semantics/src/X86Semantics"
DECODEX86_LIB="DecodeX86=llvm-tablegen-support/lean"
MAIN_LIB="Main:$deps/../simulator"

export LEAN_PATH="$LEAN_PATH:$GALOIS_LIB:$X86SEM_LIB:$DECODEX86_LIB:$MAIN_LIB"

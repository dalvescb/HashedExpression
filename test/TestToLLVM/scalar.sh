#!/bin/zsh
#
#
# test libm functions
# compile Haskell test function converting basic math functions in HashedExpression to LLVM IR
stack ghc -- TestScalar.hs -main-is TestToLLVM.TestScalar.main -o GenTestScalar.exe
# generate LLVM IR functions for all libm functions as TestScalar.ll
./GenTestScalar.exe
# compile TestScalar.ll together with TestScalarMain.c
clang Test1.ll Test2.ll Test3.ll TestScalarMain.c -O3 -o TestScalar.exe
# Run the TestScalar.exe to verify the output from llvm function.
./TestScalar.exe
rm GenTestScalar.exe TestScalar.exe Test*.ll TestScalar.o TestScalar.hi
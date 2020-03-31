#!/bin/zsh
#
#
# test libm functions
# compile Haskell test function converting basic math functions in HashedExpression to LLVM IR
stack ghc -- TestScalar.hs -main-is TestToLLVM.TestScalar.main -o GenTestScalar.exe
# generate LLVM IR functions for all libm functions as TestScalar.ll
./GenTestScalar.exe
# compile TestScalar.ll together with TestScalarMain.c
clang Test16.ll TestScalarMain.c -O3 -o TestScalar.exe
#Test9.ll Test10.ll Test11.ll Test12.ll Test13.ll Test14.ll Test15.ll Test16.ll Test17.ll Test18.ll
# Run the TestScalar.exe to verify the output from llvm function.
./TestScalar.exe
#rm GenTestScalar.exe TestScalar.exe Test*.ll TestScalar.o TestScalar.hi
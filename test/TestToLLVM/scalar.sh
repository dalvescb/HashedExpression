#!/bin/zsh
#
#
# test libm functions
rm *.exe Test*.ll *.o *.hi
# compile Haskell test function converting basic math functions in HashedExpression to LLVM IR
stack ghc -- TestScalar.hs -main-is TestToLLVM.TestScalar.main -o GenTestScalar.exe
# generate LLVM IR functions for all libm functions as TestScalar.ll
./GenTestScalar.exe
# compile TestScalar.ll together with TestScalarMain.c
clang Test1.ll Test2.ll Test3.ll Test4.ll Test5.ll Test6.ll Test7.ll Test8.ll Test9.ll Test10.ll Test11.ll Test12.ll Test13.ll Test14.ll Test15.ll Test16.ll Test17.ll Test18.ll Test19.ll Test20.ll Test21.ll TestScalarMain.c -O3 -o TestScalar.exe
#Test9.ll Test10.ll Test11.ll Test12.ll Test13.ll Test14.ll Test15.ll Test16.ll Test17.ll Test18.ll
# Run the TestScalar.exe to verify the output from llvm function.
./TestScalar.exe
rm *.exe Test*.ll *.o *.hi
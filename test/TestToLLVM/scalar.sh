#!/bin/zsh
#
#
#start_time=`date +%s`
# test libm functions
rm *.exe Test*.ll *.o *.hi *.bc
# compile Haskell test function converting basic math functions in HashedExpression to LLVM IR
#clang TestScalarMain.c
times
stack ghc -- TestScalar.hs -main-is TestToLLVM.TestScalar.main -o GenTestScalar.exe
# generate LLVM IR functions for all libm functions as TestScalar.ll
./GenTestScalar.exe
#end_time=`date +%s`
#echo execution time was `expr $end_time - $start_time` s.

# compile TestScalar.ll together with TestScalarMain.c
clang Test1.ll Test2.ll Test3.ll Test4.ll Test5.ll Test6.ll Test7.ll Test8.ll Test9.ll Test10.ll Test11.ll Test12.ll Test13.ll Test14.ll Test15.ll Test16.ll Test17.ll Test18.ll Test19.ll Test20.ll Test21.ll Test22.ll Test23.ll Test24.ll Test25.ll Test26.ll Test27.ll Test28.ll Test29.ll Test30.ll Test31.ll Test32.ll Test33.ll TestScalarMain.c -O3 -o TestScalar.exe
#--clang Test1.bc Test2.bc Test3.bc Test4.bc Test5.bc Test6.bc Test7.bc Test8.bc Test9.bc Test10.bc Test11.bc Test12.bc Test13.bc Test14.bc Test15.bc Test16.bc Test17.bc Test18.bc Test19.bc Test20.bc Test21.bc Test22.bc Test23.bc Test24.bc Test25.bc Test26.bc Test27.bc Test28.bc Test29.bc Test30.bc Test31.bc Test32.bc Test33.bc TestScalarMain.c -O3 -o TestScalar.exe

times

# Run the TestScalar.exe to verify the output from llvm function.
./TestScalar.exe
times
#rm *.exe Test*.ll *.o *.hi

#echo "Time taken for running the test:"
#times
: '
clang Test1.ll -emit-llvm -c -o Test1.bc
clang Test2.ll -emit-llvm -c -o Test2.bc
clang Test3.ll -emit-llvm -c -o Test3.bc
clang Test4.ll -emit-llvm -c -o Test4.bc
clang Test5.ll -emit-llvm -c -o Test5.bc
clang Test6.ll -emit-llvm -c -o Test6.bc
clang Test7.ll -emit-llvm -c -o Test7.bc
clang Test8.ll -emit-llvm -c -o Test8.bc
clang Test9.ll -emit-llvm -c -o Test9.bc
clang Test10.ll -emit-llvm -c -o Test10.bc
clang Test11.ll -emit-llvm -c -o Test11.bc
clang Test12.ll -emit-llvm -c -o Test12.bc
clang Test13.ll -emit-llvm -c -o Test13.bc
clang Test14.ll -emit-llvm -c -o Test14.bc
clang Test15.ll -emit-llvm -c -o Test15.bc
clang Test16.ll -emit-llvm -c -o Test16.bc
clang Test17.ll -emit-llvm -c -o Test17.bc
clang Test18.ll -emit-llvm -c -o Test18.bc
clang Test19.ll -emit-llvm -c -o Test19.bc
clang Test20.ll -emit-llvm -c -o Test20.bc
clang Test21.ll -emit-llvm -c -o Test21.bc
clang Test22.ll -emit-llvm -c -o Test22.bc
clang Test23.ll -emit-llvm -c -o Test23.bc
clang Test24.ll -emit-llvm -c -o Test24.bc
clang Test25.ll -emit-llvm -c -o Test25.bc
clang Test26.ll -emit-llvm -c -o Test26.bc
clang Test27.ll -emit-llvm -c -o Test27.bc
clang Test28.ll -emit-llvm -c -o Test28.bc
clang Test29.ll -emit-llvm -c -o Test29.bc
clang Test30.ll -emit-llvm -c -o Test30.bc
clang Test31.ll -emit-llvm -c -o Test31.bc
clang Test32.ll -emit-llvm -c -o Test32.bc
clang Test33.ll -emit-llvm -c -o Test33.bc
#clang Test33.ll -S -O3 -o Test33.bc
'
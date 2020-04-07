#!/bin/zsh
#
#
#start_time=`date +%N`
rm Test*.c
times
stack ghc -- TestScalar.hs -main-is TestToC.TestScalar.main -o GenTestScalar.exe

./GenTestScalar.exe

clang Test1.c Test2.c Test3.c Test4.c Test5.c Test6.c Test7.c Test8.c Test9.c Test10.c Test11.c Test12.c Test13.c Test14.c Test15.c Test16.c Test17.c Test18.c Test19.c Test20.c Test21.c Test22.c Test23.c Test24.c Test25.c Test26.c Test27.c Test28.c Test29.c Test30.c Test31.c Test32.c Test33.c Main.c -O3 -o Test.exe

#clang Test1.c Test2.c Test3.c Main.c -O3 -o Test.exe
times
./Test.exe
times
: '
clang Test1.c -O3 -o Test1.exe
#./a.out

clang Test2.c -O3 -o Test2.exe
#./a.out

clang Test3.c -O3 -o Test3.exe
#./a.out

clang Test4.c -O3 -o Test4.exe
#./a.out

clang Test5.c -O3 -o Test5.exe
#./a.out

clang Test6.c -O3 -o Test6.exe
#./a.out

clang Test7.c  -O3 -o Test7.exe
#./a.out

clang Test8.c -O3 -o Test8.exe
#./a.out

clang Test9.c -O3 -o Test9.exe
#./a.out

clang Test10.c -O3
#./a.out

clang Test11.c -O3
#./a.out

clang Test12.c -O3
#./a.out

clang Test13.c -O3
#./a.out

clang Test14.c -O3
#./a.out

clang Test15.c -O3
#./a.out

clang Test16.c -O3
#./a.out

clang Test17.c -O3
#./a.out

clang Test18.c -O3
#./a.out

clang Test19.c -O3
#./a.out

clang Test20.c -O3
#./a.out

clang Test21.c -O3
#./a.out

clang Test22.c -O3
#./a.out

clang Test23.c -O3
#./a.out

clang Test24.c -O3
#./a.out

clang Test25.c -O3
#./a.out

clang Test26.c -O3
#./a.out

clang Test27.c -O3
#./a.out

clang Test28.c -O3
#./a.out

clang Test29.c -O3
#./a.out

clang Test30.c -O3
#./a.out

clang Test31.c -O3
#./a.out

clang Test32.c -O3
#./a.out

clang Test33.c -O3
#./a.out

#end_time=`date +%N`
#echo $start_time
#echo $end_time
#echo execution time was `expr $end_time - $start_time` s.
'

#./Test1.exe
#./Test2.exe
#echo "\n Time taken for running the test:"
#times
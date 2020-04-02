# HashedExpression to LLVM IR Test Procedure
Test procedures for HashedExpression to LLVM IR

## Overview

For testing purposes, main function is written in C to insert the initial values, i.e. x and y in the example, and to print out the answer.
 
Upon executing the ToLLVM.hs module through TestScalar.hs(/Main.hs) with an expression as input, its corresponding LLVM code will be generated and is stored in a separate file.
 
This ".ll" LLVM output file is compiled and linked with the "TestScalarMain.c" file in order to ensure that the generated LLVM code gives the same output as other program files.

This Directory contains:

**scalar.sh**, a bash script file to 
- execute the TestScalar.hs file and generate ".ll" files.
- Run TestScalarMain.c file with generated ".ll" files to generate an .exe file
- Run the exe file to check if both output generated from C code and LLVM code are same.
- Removes all `.ll` files everytime when it is executes to generate new `.ll` files.
- Times the overall execution time for running the test cases

**TestScalar.hs**, a haskell file containing HashedExpression and generates LLVM code.

**TestScalar.c**, a C file which has same operation as HashedExpression implemented in C and compares with LLVM output. It has 31 test scenarios and 10 different test data for each test scenario. So there are 310 test cases in this Test suite.

### Usage
Below is an example of generating LLVM code from HashedExpression and checking it with a C file.  

```haskell
main = do
  let x = HashedExpression.variable "x"
  let y = HashedExpression.variable "y"
  let lcode1 = mkModule "test1" $ ( x + y )
  toLLVM "Test1.ll" lcode1

toLLVM :: String -> LLVM.AST.Module -> IO ()
toLLVM filename mod = withContext $ \ctx -> do
  llvm <- M.withModuleFromAST ctx mod M.moduleLLVMAssembly
  BS.putStrLn llvm
  BS.writeFile filename llvm
```

Will generate `Test1.ll` as shown:

```llvm
; ModuleID = 'basic'
source_filename = "Main.hs"

define double @test1(double %x, double %y) {
entry:
  %t888871445490635 = fadd double %x, %y
  ret double %t888871445490635
}
```

Now, this `Test1.ll` file is executed with `TestScalarMain.c` with the following interface:

```c
#include<stdio.h>
#include<stdbool.h>
#include<math.h>
double test1(double a, double b);
int test(int,double,double);
int main()
{
  double af = 10, bf = 2;
  int pass=0,fail=0,total;
  bool ret;
  printf(" %f + %f = %f\n",af,bf,test1(af,bf));
  ret=test(1,af,bf);
  ret?pass++:fail++;
  . . . 
 return 0;
}
int test(int n, double a, double b)
{
 double resultVal, trueVal;
 bool ret = true;
 resultVal = test1(a,b);
 trueVal = a + b;
 if (fabs(resultVal-trueVal)<1e-10) {
  } else {
    printf("Failed Test%d: (%f,%f) Expected: %f, Returned: %f \n",n,a,b,trueVal,resultVal);
      ret = false;
  }
  return ret;
 }

```
The execution on terminal should be:
```terminal
$ stack ghc -- TestScalar.hs -main-is TestToLLVM.TestScalar.main -o GenTestScalar.exe
$ ./GenTestScalar.exe
$ clang Test1.ll TestScalarMain.c -O3 -o TestScalar.exe
$ ./TestScalar.exe
```

This gives us an output as shown:
```terminal
 10.000000 + 2.000000 = 12.000000
```
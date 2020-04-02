# HashedExpression to LLVM IR 


## Overview

`ToLLVM.hs` is used to convert the HashedExpressions given in `Main.hs` file to a LLVM IR `.ll` file.

###Important Directories and Files:

- **ToLLVM.hs** can be found in the path `HashedExpression/src/HashedExpression/Internal/ToLLVM.hs`  
- **Main.hs** can be found in the path `HashedExpression/app/Main.hs`. Corresponding `.ll` file will be generated in this path upon executing this file.
- **testMain.c** can be found in the path `HashedExpression/app/testMain.c`
- **Test suite** for this module can be found in the path `HashedExpression/test/TestToLLVM/`. Test scripts can be executed by running the file `scalar.sh`  file in this folder. For more details about running Test scripts and evaluation, refer to README.md file in the test folder.
- **Documentation** for this module can be found in the path `HashedExpression/dist/docs/HashedExpression-Internal-ToLLVM.html`. All other files listed in this folder are the documentation related for the other modules in this library.

## Steps to execute:

To execute the Main.hs using stack, from the project folder run the following commands:
```terminal
$ stack build
$ stack run 
```

To execute the Main.hs using ghci in stack, go to `HashedExpression/app/` folder and run the following commands:
```terminal
$ stack ghci Main.hs
$ Prelude> main
$ Prelude> :quit
```

Above methods executes the `Main.hs` file and generates its corresponding `.ll` file in the same folder.

To verify the `.ll` file, test it with the .c file `testMain.c` in the same folder as shown:
```terminal
$ clang sampleMod.ll testMain.c -O3 -o test.exe
$ ./test.exe
```
This takes the LLVM function and input parameters are passed in .c file and the result printed on the screen.
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
--Example for adding two integers
module Main where

import Data.Text.Lazy.IO as T

import LLVM.AST hiding (function)
import LLVM.AST.Type as AST
import qualified LLVM.AST.Float as F
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.IntegerPredicate as P
import qualified LLVM.AST.Operand as O
import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad
import LLVM.IRBuilder.Instruction

import LLVM.AST.Global
import LLVM.Context
import qualified LLVM.Module as M

import Control.Monad.Except
import Data.ByteString.Char8 as BS

sampleMod :: Module
sampleMod = buildModule "exampleModule" $ mdo
  function "add" [(AST.i32, "x"), (AST.i32, "y")] AST.i32 $ \[x, y] -> do
    _entry <- block `named` "entry"
    y <- sub (ConstantOperand (C.Int 32 0)) x --Negate
    x <- add y y
    r <- mul x y -- multiplication
    ret r
  function "swap" [(AST.i32, "x"), (AST.i32, "y")] AST.i32 $ \[x, y] -> do
    _entry <- block `named` "entry"
    x <- add x y -- addition
    y <- sub x y -- subtraction two variables
    x <- sub x y -- swapping two variables
    ret (ConstantOperand (C.Int 32 0)) 

toLLVM :: Module -> IO ()
toLLVM mod = withContext $ \ctx -> do
  llvm <- M.withModuleFromAST ctx mod M.moduleLLVMAssembly
  BS.putStrLn llvm
  BS.writeFile "sampleMod.ll" llvm

main :: IO ()
main = toLLVM sampleMod
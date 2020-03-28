{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Array
import Data.Complex
import qualified Data.IntMap.Strict as IM
import Data.Map (empty, fromList, union)
import qualified Data.Set as Set
import HashedExpression.Derivative

import HashedExpression.Interp
import HashedExpression.Operation
import qualified HashedExpression.Operation
import HashedExpression.Prettify
import Data.List (intercalate)
import Data.Maybe (fromJust)
import Data.STRef.Strict
import Graphics.EasyPlot
import HashedExpression.Internal.CollectDifferential
import HashedExpression.Internal.ToLLVM
import HashedExpression.Internal.Utils
import HashedExpression
import LLVM.AST.Global
import LLVM.Context
import LLVM.AST
import qualified LLVM.Module as M
import HashedExpression.Internal.Expression
import Control.Monad.Except
import Data.ByteString.Char8 as BS
main :: IO ()
main = do
    -- Encode and represent expressions
    let x = HashedExpression.variable "x"
    let y = HashedExpression.variable "y"
    let z = constant 49
    --let lcode = mkModule $ normalize (x + y + z)
    let lcode = mkModule $ (x * y)
    
    let a = mkModule (negate z)
    let b = mkModule (sin x)
    let c = exp y
    let sqrtFun = mkModule (c + sqrt z)
    toLLVM sqrtFun 
    --toLLVM c
    
toLLVM :: LLVM.AST.Module -> IO ()
toLLVM mod = withContext $ \ctx -> do
  llvm <- M.withModuleFromAST ctx mod M.moduleLLVMAssembly
  BS.putStrLn llvm
  BS.writeFile "sampleMod.ll" llvm

 























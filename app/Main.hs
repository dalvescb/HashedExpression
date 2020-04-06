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
    let x1 = HashedExpression.variable "x1"
    let x2 = HashedExpression.variable "x2"
    let x3 = HashedExpression.variable "x3"
    let x4 = HashedExpression.variable "x4"
    let x5 = HashedExpression.variable "x5"
    let x6 = HashedExpression.variable "x6"
    let x7 = HashedExpression.variable "x7"
    let x8 = HashedExpression.variable "x8"
    let x9 = HashedExpression.variable "x9"
    let x0 = HashedExpression.variable "x0"
    let lcode1 =  x0 + x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 
    let lcode2 =  x0 * x1 * x2 * x3 * x4 * x5 * x6 * x7 *  x8 * x9 
    let lcode3 = lcode2 - lcode1 + x0 / (x + y) 
    let lcode4 = mkModule "test1.ll" $ lcode3 HashedExpression.^ 2
    --let z = constant 1
    --let lcode2 = mkModule "test2" $  x HashedExpression.Internal.Expression.^ 2
    --let lcode3 = mkModule "test3" $ tan z
    toLLVM "Test1.ll" lcode4
     
toLLVM :: String -> LLVM.AST.Module -> IO ()
toLLVM filename mod = withContext $ \ctx -> do
  llvm <- M.withModuleFromAST ctx mod M.moduleLLVMAssembly
  BS.putStrLn llvm
  BS.writeFile filename llvm




module TestToLLVM.TestScalar where
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeApplications #-}


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
import qualified HashedExpression.Internal.Expression as E
import Control.Monad.Except
import Data.ByteString.Char8 as BS
main :: IO ()
main = do
    -- Encode and represent expressions
    let x = HashedExpression.variable "x"
    let y = HashedExpression.variable "y"
    let z = constant 49.0
    let a = constant 1.0
    let lcode1 = mkModule "test1" $ ( x + y )
    toLLVM "Test1.ll" lcode1
    let lcode2 = mkModule "test2" $ ( x - y )
    toLLVM "Test2.ll" lcode2
    let lcode3 = mkModule "test3" $ ( x * y )
    toLLVM "Test3.ll" lcode3
  {--  let lcode4 = mkModule "test4" $ ( x / y )
    toLLVM "Test4.ll" lcode4
    let lcode5 = mkModule "test5" $ ( negate y )
    toLLVM "Test5.ll" lcode5
    let lcode6 = mkModule "test6" $ ( sqrt z )
    toLLVM "Test6.ll" lcode6
    let lcode7 = mkModule "test7" $ sin a 
    toLLVM "Test7.ll" lcode7
    let lcode8 = mkModule "test8" $ ( cos a )
    toLLVM "Test8.ll" lcode8
    let lcode9 = mkModule "test9" $ ( tan a )
    toLLVM "Test9.ll" lcode9
    let lcode10 = mkModule "test10" $ ( sinh a )
    toLLVM "Test10.ll" lcode10
    let lcode11 = mkModule "test11" $ ( cosh a )
    toLLVM "Test11.ll" lcode11
    let lcode12 = mkModule "test12" $ ( tanh a )
    toLLVM "Test12.ll" lcode12
    let lcode13 = mkModule "test13" $ ( asin a )
    toLLVM "Test13.ll" lcode13
    let lcode14 = mkModule "test14" $ ( acos a )
    toLLVM "Test14.ll" lcode14
    let lcode15 = mkModule "test15" $ ( atan a )
    toLLVM "Test15.ll" lcode15
    let lcode16 = mkModule "test16" $ ( asinh a )
    toLLVM "Test16.ll" lcode16
    let lcode17 = mkModule "test17" $ ( acosh a )
    toLLVM "Test17.ll" lcode17
    let lcode18 = mkModule "test18" $ ( atanh a )
    toLLVM "Test18.ll" lcode18 -}
   
toLLVM :: String -> LLVM.AST.Module -> IO ()
toLLVM filename mod = withContext $ \ctx -> do
  llvm <- M.withModuleFromAST ctx mod M.moduleLLVMAssembly
  BS.putStrLn llvm
  BS.writeFile filename llvm





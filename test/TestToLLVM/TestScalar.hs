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
    let w = HashedExpression.variable "w"
    let z = constant 49.0
    let a = constant 1.0
    let b = constant 2.0
    let lcode1 = mkModule "test1" $ ( x + y )
    toLLVM "Test1.ll" lcode1
    let lcode2 = mkModule "test2" $ ( x - y )
    toLLVM "Test2.ll" lcode2
    let lcode3 = mkModule "test3" $ ( x * y )
    toLLVM "Test3.ll" lcode3
    let lcode4 = mkModule "test4" $ ( x / y )
    toLLVM "Test4.ll" lcode4
    let lcode5 = mkModule "test5" $ ( negate y )
    toLLVM "Test5.ll" lcode5
    let lcode6 = mkModule "test6" $ ( sqrt x )
    toLLVM "Test6.ll" lcode6
    let lcode7 = mkModule "test7" $ sin x 
    toLLVM "Test7.ll" lcode7
    let lcode8 = mkModule "test8" $ ( cos x)
    toLLVM "Test8.ll" lcode8
    let lcode9 = mkModule "test9" $ tan x
    toLLVM "Test9.ll" lcode9
    let lcode10 = mkModule "test10" $ ( sinh x )
    toLLVM "Test10.ll" lcode10
    let lcode11 = mkModule "test11" $ ( cosh x )
    toLLVM "Test11.ll" lcode11
    let lcode12 = mkModule "test12" $ ( tanh x )
    toLLVM "Test12.ll" lcode12
    let lcode13 = mkModule "test13" $ ( asin x )
    toLLVM "Test13.ll" lcode13
    let lcode14 = mkModule "test14" $ ( acos x )
    toLLVM "Test14.ll" lcode14
    let lcode15 = mkModule "test15" $ ( atan x )
    toLLVM "Test15.ll" lcode15
    let lcode16 = mkModule "test16" $ ( asinh x )
    toLLVM "Test16.ll" lcode16
    let lcode17 = mkModule "test17" $ ( acosh x )
    toLLVM "Test17.ll" lcode17
    let lcode18 = mkModule "test18" $ ( atanh x )
    toLLVM "Test18.ll" lcode18
    let lcode19 = mkModule "test19" $ ( exp x )
    toLLVM "Test19.ll" lcode19
    let lcode20 = mkModule "test20" $ ( log x )
    toLLVM "Test20.ll" lcode20
    let lcode21 = mkModule "test21" $ ( x HashedExpression.^ 2 )
    toLLVM "Test21.ll" lcode21
    let lcode22 = mkModule "test22" $ (w + x + y)
    toLLVM "Test22.ll" lcode22
    let lcode23 = mkModule "test23" $ (w + x * y)
    toLLVM "Test23.ll" lcode23
    let lcode24 = mkModule "test24" $ (w * x + y)
    toLLVM "Test24.ll" lcode24
    let lcode25 = mkModule "test25" $ (w + x / y)
    toLLVM "Test25.ll" lcode25
    let lcode26 = mkModule "test26" $ (w * x / y)
    toLLVM "Test26.ll" lcode26
    let lcode27 = mkModule "test27" $ (w * x * y)
    toLLVM "Test27.ll" lcode27
    let lcode28 = mkModule "test28" $ (w / x / y)
    toLLVM "Test28.ll" lcode28
    let lcode29 = mkModule "test29" $ (x + sin(y))
    toLLVM "Test29.ll" lcode29
    let lcode30 = mkModule "test30" $ (sin(x) - cos(y))
    toLLVM "Test30.ll" lcode30
    let lcode31 = mkModule "test31" $ ((x + y) HashedExpression.^ 2)
    toLLVM "Test31.ll" lcode31
    let lcode32 = mkModule "test32" $ (negate (negate x))
    toLLVM "Test32.ll" lcode32
    let lcode33 = mkModule "test33" $ (x + (negate y))
    toLLVM "Test33.ll" lcode33
    
   
toLLVM :: String -> LLVM.AST.Module -> IO ()
toLLVM filename mod = withContext $ \ctx -> do
  llvm <- M.withModuleFromAST ctx mod M.moduleLLVMAssembly
  BS.putStrLn llvm
  BS.writeFile filename llvm





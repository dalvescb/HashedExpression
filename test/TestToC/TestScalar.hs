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
import HashedExpression.Internal.ToC
import HashedExpression.Internal.Utils
import HashedExpression

main :: IO ()
main = do
    -- Encode and represent expressions
   let x = HashedExpression.variable "x"
   let y = HashedExpression.variable "y"
   let z = HashedExpression.variable "z"
   let a = HashedExpression.variable "a"
   let valMap = fromList [ ("x", VScalar 15.0)
                         , ("y", VScalar 3.0)
                         , ("z", VScalar 49.0)
                         , ("a", VScalar 1.0)
                         ]
   let cCode1  = intercalate "\n" $ singleExpressionCProgram valMap $ x + y
   writeFile "Test1.c" cCode1
   let cCode2 = intercalate "\n" $ singleExpressionCProgram valMap $ x - y 
   writeFile "Test2.c" cCode2
   let cCode3 = intercalate "\n" $ singleExpressionCProgram valMap $ x * y 
   writeFile "Test3.c" cCode3
   let cCode4 = intercalate "\n" $ singleExpressionCProgram valMap $ x / y
   writeFile "Test4.c" cCode4
   let cCode5 = intercalate "\n" $ singleExpressionCProgram valMap $ negate y
   writeFile "Test5.c" cCode5   
   let cCode6 = intercalate "\n" $ singleExpressionCProgram valMap $ sqrt z 
   writeFile "Test6.c" cCode6
   let cCode7 = intercalate "\n" $ singleExpressionCProgram valMap $ sin a
   writeFile "Test7.c" cCode7
   let cCode8 = intercalate "\n" $ singleExpressionCProgram valMap $ cos a
   writeFile "Test8.c" cCode8
   let cCode9 = intercalate "\n" $ singleExpressionCProgram valMap $ tan a
   writeFile "Test9.c" cCode9
   let cCode10 = intercalate "\n" $ singleExpressionCProgram valMap $ sinh a
   writeFile "Test10.c" cCode10
   let cCode11 = intercalate "\n" $ singleExpressionCProgram valMap $ cosh a
   writeFile "Test11.c" cCode11
   let cCode12 = intercalate "\n" $ singleExpressionCProgram valMap $ tanh a
   writeFile "Test12.c" cCode12
   let cCode13 = intercalate "\n" $ singleExpressionCProgram valMap $ asin a
   writeFile "Test13.c" cCode13
   let cCode14 = intercalate "\n" $ singleExpressionCProgram valMap $ acos a
   writeFile "Test14.c" cCode14
   let cCode15 = intercalate "\n" $ singleExpressionCProgram valMap $ atan a
   writeFile "Test15.c" cCode15
   let cCode16 = intercalate "\n" $ singleExpressionCProgram valMap $ asinh a
   writeFile "Test16.c" cCode16
   let cCode17 = intercalate "\n" $ singleExpressionCProgram valMap $ acosh a
   writeFile "Test17.c" cCode17
   let cCode18 = intercalate "\n" $ singleExpressionCProgram valMap $ atanh a
   writeFile "Test18.c" cCode18
   let cCode19 = intercalate "\n" $ singleExpressionCProgram valMap $ exp a
   writeFile "Test19.c" cCode19
   let cCode20 = intercalate "\n" $ singleExpressionCProgram valMap $ log a
   writeFile "Test20.c" cCode20
   let cCode21 = intercalate "\n" $ singleExpressionCProgram valMap $ y HashedExpression.^ 2
   writeFile "Test21.c" cCode21
   let cCode22 = intercalate "\n" $ singleExpressionCProgram valMap $ x + y + a
   writeFile "Test22.c" cCode22
   let cCode23 = intercalate "\n" $ singleExpressionCProgram valMap $ x + y * a
   writeFile "Test23.c" cCode23
   let cCode24 = intercalate "\n" $ singleExpressionCProgram valMap $ x * y + a
   writeFile "Test24.c" cCode24
   let cCode25 = intercalate "\n" $ singleExpressionCProgram valMap $ x + y / a
   writeFile "Test25.c" cCode25
   let cCode26 = intercalate "\n" $ singleExpressionCProgram valMap $ x * y / a
   writeFile "Test26.c" cCode26
   let cCode27 = intercalate "\n" $ singleExpressionCProgram valMap $ x * y * a
   writeFile "Test27.c" cCode27
   let cCode28 = intercalate "\n" $ singleExpressionCProgram valMap $ x / y / a
   writeFile "Test28.c" cCode28
   let cCode29 = intercalate "\n" $ singleExpressionCProgram valMap $ x + sin(a)
   writeFile "Test29.c" cCode29
   let cCode30 = intercalate "\n" $ singleExpressionCProgram valMap $ sin(a) - cos(a)
   writeFile "Test30.c" cCode30
   let cCode31 = intercalate "\n" $ singleExpressionCProgram valMap $ (x+y) HashedExpression.^ 2
   writeFile "Test31.c" cCode31
   let cCode32 = intercalate "\n" $ singleExpressionCProgram valMap $ negate (negate a)
   writeFile "Test32.c" cCode32
   let cCode33 = intercalate "\n" $ singleExpressionCProgram valMap $ x + (negate y)
   writeFile "Test33.c" cCode33


{-  
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
-}
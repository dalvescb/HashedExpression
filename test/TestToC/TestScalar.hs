{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeApplications #-}

module TestToC.TestScalar where

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
   let cCode1  = intercalate "\n" $ singleExpressionCProgram "test1" valMap $ x + y
   writeFile "Test1.c" cCode1
   let cCode2 = intercalate "\n" $ singleExpressionCProgram "test2" valMap $ x - y 
   writeFile "Test2.c" cCode2
   let cCode3 = intercalate "\n" $ singleExpressionCProgram "test3" valMap $ x * y 
   writeFile "Test3.c" cCode3
   let cCode4 = intercalate "\n" $ singleExpressionCProgram "test4" valMap $ x / y
   writeFile "Test4.c" cCode4
   let cCode5 = intercalate "\n" $ singleExpressionCProgram "test5" valMap $ negate y
   writeFile "Test5.c" cCode5   
   let cCode6 = intercalate "\n" $ singleExpressionCProgram "test6" valMap $ sqrt z 
   writeFile "Test6.c" cCode6
   let cCode7 = intercalate "\n" $ singleExpressionCProgram "test7" valMap $ sin a
   writeFile "Test7.c" cCode7
   let cCode8 = intercalate "\n" $ singleExpressionCProgram "test8" valMap $ cos a
   writeFile "Test8.c" cCode8
   let cCode9 = intercalate "\n" $ singleExpressionCProgram "test9" valMap $ tan a
   writeFile "Test9.c" cCode9
   let cCode10 = intercalate "\n" $ singleExpressionCProgram "test10"  valMap $ sinh a
   writeFile "Test10.c" cCode10
   let cCode11 = intercalate "\n" $ singleExpressionCProgram "test11"  valMap $ cosh a
   writeFile "Test11.c" cCode11
   let cCode12 = intercalate "\n" $ singleExpressionCProgram "test12"  valMap $ tanh a
   writeFile "Test12.c" cCode12
   let cCode13 = intercalate "\n" $ singleExpressionCProgram  "test13" valMap $ asin a
   writeFile "Test13.c" cCode13
   let cCode14 = intercalate "\n" $ singleExpressionCProgram "test14"  valMap $ acos a
   writeFile "Test14.c" cCode14
   let cCode15 = intercalate "\n" $ singleExpressionCProgram "test15"  valMap $ atan a
   writeFile "Test15.c" cCode15
   let cCode16 = intercalate "\n" $ singleExpressionCProgram "test16"  valMap $ asinh a
   writeFile "Test16.c" cCode16
   let cCode17 = intercalate "\n" $ singleExpressionCProgram "test17"  valMap $ acosh a
   writeFile "Test17.c" cCode17
   let cCode18 = intercalate "\n" $ singleExpressionCProgram "test18"  valMap $ atanh a
   writeFile "Test18.c" cCode18
   let cCode19 = intercalate "\n" $ singleExpressionCProgram "test19"  valMap $ exp a
   writeFile "Test19.c" cCode19
   let cCode20 = intercalate "\n" $ singleExpressionCProgram "test20"  valMap $ log a
   writeFile "Test20.c" cCode20
   let cCode21 = intercalate "\n" $ singleExpressionCProgram "test21"  valMap $ y HashedExpression.^ 2
   writeFile "Test21.c" cCode21
   let cCode22 = intercalate "\n" $ singleExpressionCProgram "test22"  valMap $ x + y + a
   writeFile "Test22.c" cCode22
   let cCode23 = intercalate "\n" $ singleExpressionCProgram "test23"  valMap $ x + y * a
   writeFile "Test23.c" cCode23
   let cCode24 = intercalate "\n" $ singleExpressionCProgram  "test24" valMap $ x * y + a
   writeFile "Test24.c" cCode24
   let cCode25 = intercalate "\n" $ singleExpressionCProgram "test25"  valMap $ x + y / a
   writeFile "Test25.c" cCode25
   let cCode26 = intercalate "\n" $ singleExpressionCProgram "test26"  valMap $ x * y / a
   writeFile "Test26.c" cCode26
   let cCode27 = intercalate "\n" $ singleExpressionCProgram "test27"  valMap $ x * y * a
   writeFile "Test27.c" cCode27
   let cCode28 = intercalate "\n" $ singleExpressionCProgram "test28"  valMap $ x / y / a
   writeFile "Test28.c" cCode28
   let cCode29 = intercalate "\n" $ singleExpressionCProgram "test29"  valMap $ x + sin(a)
   writeFile "Test29.c" cCode29
   let cCode30 = intercalate "\n" $ singleExpressionCProgram "test30"  valMap $ sin(a) - cos(a)
   writeFile "Test30.c" cCode30
   let cCode31 = intercalate "\n" $ singleExpressionCProgram "test31"  valMap $ (x+y) HashedExpression.^ 2
   writeFile "Test31.c" cCode31
   let cCode32 = intercalate "\n" $ singleExpressionCProgram  "test32" valMap $ negate (negate a)
   writeFile "Test32.c" cCode32
   let cCode33 = intercalate "\n" $ singleExpressionCProgram "test33"  valMap $ x + (negate y)
   writeFile "Test33.c" cCode33



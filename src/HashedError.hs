{-|
Module      : HashedError
Description : Responsible for calculating the calculation error amount of expressions based on Interval Analysis.
Maintainer  : ghaffh1@mcmaster.ca


The main idea behind implementation of HaheshError is to track down the amount of error generated based on different
mathematical operations. For this purpose we are going to use the simple idea of Interval Analyis.
-}

{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module HashedError where

import Data.Array
import Data.Complex
import qualified Data.IntMap.Strict as IM
import Data.Map (Map, fromList)
import qualified Data.Map as Map
import Debug.Trace (traceId, traceShowId)
import HashedExpression
    ( C
    , ET(..)
    , Expression(..)
    , ExpressionMap
    , Node(..)
    , One
    , R
    , Three
    , Two
    , Zero
    )
import HashedNode
import HashedPrettify (prettify, showExp)
import HashedUtils
import HashedErrorUtils


-- | Element wise adding the elements of List of Lists and create a new List
addLists :: Num a => [[a]] -> [a]
addLists [] = []
addLists (xs:[]) = xs
addLists (xs:xss) = zipWith (+) xs (addLists xss)

{--
    ============================
    ==  Interval Calculation  ==
    ============================
-}







-- | Calculate the Error Amount based the selected radius
--
class ErrorEvaluable d rc output where
  errorEval :: ValMaps -> Double -> Int -> Expression d rc -> output

-- |
--
instance ErrorEvaluable Zero R ErrorType where
    errorEval :: ValMaps -> Double -> Int -> Expression Zero R -> ErrorType
    errorEval valMap radius depth e@(Expression n mp)
        | [] <- retrieveShape n mp =
            case retrieveNode n mp of
                Var name ->
                    case Map.lookup name $ vm0 valMap of
                        Just val -> constantErorCalc radius depth val
                        _ -> error "no value associated with the variable"
                Const val -> (0,val,[val,val,val]) -- For constant value we are not going to calculate any error bound
                Sum R args -> intervalSum (map (errorEval valMap radius (depth + 1) . expZeroR mp) args :: [ErrorType]) depth
                Mul R args -> intervalProduct (map (errorEval valMap radius (depth + 1) . expZeroR mp) args :: [ErrorType]) depth
                Neg R arg -> intervalNeg (errorEval valMap radius (depth + 1) $ expZeroR mp arg :: ErrorType) depth
                Scale R arg1 arg2 ->
                  let intervalToCalc = [errorEval valMap radius (depth + 1) (expZeroR mp arg1) :: ErrorType
                                        , errorEval valMap radius (depth + 1) (expZeroR mp arg2) :: ErrorType]
                  in  intervalProduct intervalToCalc depth
                Power x arg ->
                  let expressionValue = errorEval valMap radius (depth + 1) (expZeroR mp arg) :: ErrorType
                  in intervalPower expressionValue x depth
--                Div arg1 arg2 ->
--                    eval valMap (expZeroR mp arg1) /
--                    eval valMap (expZeroR mp arg2)
--                Sqrt arg -> sqrt (eval valMap (expZeroR mp arg))
--                Sin arg -> sin (eval valMap (expZeroR mp arg))
--                Cos arg -> cos (eval valMap (expZeroR mp arg))
--                Tan arg -> tan (eval valMap (expZeroR mp arg))
--                Exp arg -> exp (eval valMap (expZeroR mp arg))
--                Log arg -> log (eval valMap (expZeroR mp arg))
--                Sinh arg -> sinh (eval valMap (expZeroR mp arg))
--                Cosh arg -> cosh (eval valMap (expZeroR mp arg))
--                Tanh arg -> tanh (eval valMap (expZeroR mp arg))
--                Asin arg -> asin (eval valMap (expZeroR mp arg))
--                Acos arg -> acos (eval valMap (expZeroR mp arg))
--                Atan arg -> atan (eval valMap (expZeroR mp arg))
--                Asinh arg -> asinh (eval valMap (expZeroR mp arg))
--                Acosh arg -> acosh (eval valMap (expZeroR mp arg))
--                Atanh arg -> atanh (eval valMap (expZeroR mp arg))
--                RealPart arg -> realPart (eval valMap (expZeroC mp arg))
--                ImagPart arg -> imagPart (eval valMap (expZeroC mp arg))
--                InnerProd R arg1 arg2 ->
--                    case retrieveShape arg1 mp of
--                        [] ->
--                            eval valMap (expZeroR mp arg1) *
--                            eval valMap (expZeroR mp arg2)
--                        [size] ->
--                            let res1 = eval valMap $ expOneR mp arg1
--                                res2 = eval valMap $ expOneR mp arg2
--                             in sum [ x * y
--                                    | i <- [0 .. size - 1]
--                                    , let x = res1 ! i
--                                    , let y = res2 ! i
--                                    ]
--                        [size1, size2] ->
--                            let res1 = eval valMap $ expTwoR mp arg1
--                                res2 = eval valMap $ expTwoR mp arg2
--                             in sum [ x * y
--                                    | i <- [0 .. size1 - 1]
--                                    , j <- [0 .. size2 - 1]
--                                    , let x = res1 ! (i, j)
--                                    , let y = res2 ! (i, j)
--                                    ]
--                        [size1, size2, size3] ->
--                            let res1 = eval valMap $ expThreeR mp arg1
--                                res2 = eval valMap $ expThreeR mp arg2
--                             in sum [ x * y
--                                    | i <- [0 .. size1 - 1]
--                                    , j <- [0 .. size2 - 1]
--                                    , k <- [0 .. size3 - 1]
--                                    , let x = res1 ! (i, j, k)
--                                    , let y = res2 ! (i, j, k)
--                                    ]
--                        _ -> error "4D shape?"
--                Piecewise marks conditionArg branchArgs ->
--                    let cdt = eval valMap $ expZeroR mp conditionArg
--                        branches = map (eval valMap . expZeroR mp) branchArgs
--                     in chooseBranch marks cdt branches
--                _ ->
--                    error
--                        ("expression structure Scalar R is wrong " ++ prettify e)
        | otherwise = error "one r but shape is not [] ??"



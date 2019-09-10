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
addLists [xs] = xs
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
                        Just val -> constantErorCalc depth radius  val
                        _ -> error "no value associated with the variable"
                Const val -> (0,val,[val,val]) -- For constant value we are not going to calculate any error bound
                Sum R args -> intervalSum (map (errorEval valMap radius (depth + 1) . expZeroR mp) args :: [ErrorType]) depth
                Mul R args -> intervalProduct (map (errorEval valMap radius (depth + 1) . expZeroR mp) args :: [ErrorType]) depth
                Neg R arg -> intervalNeg depth (errorEval valMap radius (depth + 1) $ expZeroR mp arg :: ErrorType)
                Scale R arg1 arg2 ->
                  let intervalToCalc = [errorEval valMap radius (depth + 1) (expZeroR mp arg1) :: ErrorType
                                        , errorEval valMap radius (depth + 1) (expZeroR mp arg2) :: ErrorType]
                  in  intervalProduct intervalToCalc depth
                Power x arg ->
                  let expressionValue = errorEval valMap radius (depth + 1) (expZeroR mp arg) :: ErrorType
                  in intervalPower depth x expressionValue
                Div arg1 arg2 ->
                  let divArg1 = errorEval valMap radius (depth + 1) (expZeroR mp arg1) :: ErrorType
                      divArg2 = errorEval valMap radius (depth + 1) (expZeroR mp arg2) :: ErrorType
                  in intervalDiv  depth divArg1 divArg2
                Sqrt arg -> intervalSqrt  depth (errorEval valMap radius (depth + 1) (expZeroR mp arg) :: ErrorType)
                Log arg -> intervalLog depth (errorEval valMap radius (depth + 1) (expZeroR mp arg) :: ErrorType)
--                Sin arg -> sin (eval valMap (expZeroR mp arg))
--                Cos arg -> cos (eval valMap (expZeroR mp arg))
--                Tan arg -> tan (eval valMap (expZeroR mp arg))
--                Exp arg -> exp (eval valMap (expZeroR mp arg))
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



-- | Error  Estimation for 1D arrays
--
instance ErrorEvaluable One R (Array Int ErrorType) where
    errorEval :: ValMaps -> Double -> Int -> Expression One R -> Array Int ErrorType
    errorEval valMap radius depth e@(Expression n mp)
        | [size] <- retrieveShape n mp =
            let fmapInterval :: (Int -> Double -> a -> b) -> Int -> Double -> Array Int a -> Array Int b
                fmapInterval f depth radius arr =
                    listArray
                      (0, size - 1)
                      [f depth radius x | i <- [0 .. size - 1], let x = arr ! i]
                fmap :: (Int -> a -> a) -> Int -> Array Int a -> Array Int a
                fmap f depth arr =
                    listArray
                        (0, size - 1)
                        [f depth x | i <- [0 .. size - 1], let x = arr ! i]
                fmapDiv :: (Int -> a -> a -> a) -> Int -> Array Int a -> Array Int a -> Array Int a
                fmapDiv f depth arr1 arr2 =
                    listArray
                        (0, size - 1)
                        [f depth x y | i <- [0 .. size - 1], let x = arr1 ! i, let y = arr2 ! i]
                fmapPower :: (Int -> Int -> a -> a) -> Int -> Int -> Array Int a -> Array Int a
                fmapPower f depth pwr arr =
                    listArray
                        (0, size - 1)
                        [f depth pwr x | i <- [0 .. size - 1], let x = arr ! i]
                zipWith ::
                       ([a] -> Int -> a)
                    -> Int
                    -> Array Int a
                    -> Array Int a
                    -> Array Int a
                zipWith f depth arr1 arr2 =
                    listArray
                        (0, size - 1)
                        [ f [x,y] depth
                        | i <- [0 .. size - 1]
                        , let x = arr1 ! i
                        , let y = arr2 ! i
                        ]
                foldl1' :: ([a] -> Int -> a) -> Int -> [Array Int a] -> Array Int a
                foldl1' f depth [x] = x
                foldl1' f depth (x:xs) = zipWith f depth x (foldl1' f depth xs)
             in case retrieveNode n mp of
                    Var name ->
                        case Map.lookup name $ vm1 valMap of
                            Just val -> fmapInterval constantErorCalc depth radius val
                            _ -> error "no value associated with the variable"
                    Const val -> listArray (0, size - 1) $ replicate size (0,val,[val,val])
                    Sum R args -> foldl1' intervalSum depth . map (errorEval valMap radius (depth +1) . expOneR mp) $ args
                    Mul R args -> foldl1' intervalProduct depth . map (errorEval valMap radius (depth +1) . expOneR mp) $ args
                    Neg R arg -> fmap intervalNeg depth (errorEval valMap radius (depth + 1) (expOneR mp arg))
                    Power x arg -> fmapPower intervalPower depth x  (errorEval valMap radius (depth + 1) (expOneR mp arg))
                    Scale R arg1 arg2 ->
                          let intervalToCalc = [errorEval valMap radius (depth + 1) (expOneR mp arg1)
                                , errorEval valMap radius (depth + 1) (expOneR mp arg2)]
                          in foldl1' intervalProduct depth intervalToCalc
                    Div arg1 arg2 ->
                          let divArg1 = errorEval valMap radius (depth + 1) (expOneR mp arg1) :: Array Int ErrorType
                              divArg2 = errorEval valMap radius (depth + 1) (expOneR mp arg2) :: Array Int ErrorType
                          in fmapDiv intervalDiv depth divArg1 divArg2
                    Sqrt arg -> fmap intervalSqrt depth (errorEval valMap radius (depth + 1) (expOneR mp arg))
                    Log arg -> fmap intervalLog depth (errorEval valMap radius (depth + 1) (expOneR mp arg))
--                    Sin arg -> fmap sin . eval valMap $ expOneR mp arg
--                    Cos arg -> fmap cos . eval valMap $ expOneR mp arg
--                    Tan arg -> fmap tan . eval valMap $ expOneR mp arg
--                    Exp arg -> fmap exp . eval valMap $ expOneR mp arg
--                    Sinh arg -> fmap sinh . eval valMap $ expOneR mp arg
--                    Cosh arg -> fmap cosh . eval valMap $ expOneR mp arg
--                    Tanh arg -> fmap tanh . eval valMap $ expOneR mp arg
--                    Asin arg -> fmap asin . eval valMap $ expOneR mp arg
--                    Acos arg -> fmap acos . eval valMap $ expOneR mp arg
--                    Atan arg -> fmap atan . eval valMap $ expOneR mp arg
--                    Asinh arg -> fmap asinh . eval valMap $ expOneR mp arg
--                    Acosh arg -> fmap acosh . eval valMap $ expOneR mp arg
--                    Atanh arg -> fmap atanh . eval valMap $ expOneR mp arg
--                    RealPart arg -> fmap realPart . eval valMap $ expOneC mp arg
--                    ImagPart arg -> fmap imagPart . eval valMap $ expOneC mp arg
--                    -- Rotate rA arg ->
--                    Piecewise marks conditionArg branchArgs ->
--                        let cdt = eval valMap $ expOneR mp conditionArg
--                            branches = map (eval valMap . expOneR mp) branchArgs
--                         in listArray
--                                (0, size - 1)
--                                [ chosen ! i
--                                | i <- [0 .. size - 1]
--                                , let chosen =
--                                          chooseBranch marks (cdt ! i) branches
--                                ]
--                    Rotate [amount] arg ->
--                        rotate1D size amount (eval valMap $ expOneR mp arg)
--                    _ -> error "expression structure One R is wrong"
        | otherwise = error "one r but shape is not [size] ??"



-- | Error  Estimation for 2D arrays
--
instance ErrorEvaluable Two R (Array (Int,Int) ErrorType) where
    errorEval :: ValMaps -> Double -> Int -> Expression Two R -> Array (Int,Int) ErrorType
    errorEval valMap radius depth e@(Expression n mp)
        | [size1,size2] <- retrieveShape n mp =
            let fmapInterval :: (Int -> Double -> a -> b) -> Int -> Double -> Array (Int, Int) a -> Array (Int, Int) b
                fmapInterval f depth radius arr =
                    listArray
                      ((0, 0), (size1 - 1, size2 -1))
                      [f depth radius x | i <- [0 .. size1 - 1], j <- [0 .. size2 - 1], let x = arr ! (i, j)]
                fmap :: (Int -> a -> a) -> Int -> Array (Int, Int) a -> Array (Int, Int) a
                fmap f depth arr =
                    listArray
                         ((0,0), (size1 - 1, size2 -1))
                         [f depth x | i <- [0 .. size1 - 1], j <- [0 .. size2 - 1], let x = arr ! (i,j)]
                fmapDiv :: (Int -> a -> a -> a) -> Int -> Array (Int,Int) a -> Array (Int,Int) a -> Array (Int,Int) a
                fmapDiv f depth arr1 arr2 =
                    listArray
                        ((0,0),(size1 -1 , size2 -1))
                        [f depth x y | i <- [0 .. size1 - 1] , j <-[0 .. size2 - 1], let x = arr1 ! (i,j), let y = arr2 ! (i,j)]
                fmapPower :: (Int -> Int -> a -> a) -> Int -> Int -> Array (Int,Int) a -> Array (Int,Int) a
                fmapPower f depth pwr arr =
                    listArray
                        ((0,0),(size1 - 1, size2 - 1))
                        [f depth pwr x | i <- [0 .. size1 - 1], j <- [0 .. size2 - 1], let x = arr ! (i,j)]
                zipWith ::
                       ([a] -> Int -> a)
                    -> Int
                    -> Array (Int,Int) a
                    -> Array (Int,Int) a
                    -> Array (Int,Int) a
                zipWith f depth arr1 arr2 =
                    listArray
                        ((0,0),(size1 - 1, size2 - 1))
                        [ f [x,y] depth
                        | i <- [0 .. size1 - 1], j <-[0 .. size2 - 1]
                        , let x = arr1 ! (i,j)
                        , let y = arr2 ! (i,j)
                        ]
                foldl1' :: ([a] -> Int -> a) -> Int -> [Array (Int,Int) a] -> Array (Int,Int) a
                foldl1' f depth [x] = x
                foldl1' f depth (x:xs) = zipWith f depth x (foldl1' f depth xs)
             in case retrieveNode n mp of
                    Var name ->
                        case Map.lookup name $ vm2 valMap of
                            Just val -> fmapInterval constantErorCalc (depth +1) radius val
                            _ -> error "no value associated with the variable"
                    Const val -> listArray ((0,0), (size1 - 1, size2 -1)) $ replicate (size1 * size2) (0,val,[val,val])
                    Sum R args -> foldl1' intervalSum depth (map (errorEval valMap radius (depth + 1) . expTwoR mp) args)
                    Mul R args -> foldl1' intervalProduct depth . map (errorEval valMap radius (depth +1) . expTwoR mp) $ args
                    Neg R arg -> fmap intervalNeg depth (errorEval valMap radius (depth + 1) (expTwoR mp arg))
                    Power x arg -> fmapPower intervalPower depth x  (errorEval valMap radius (depth + 1) (expTwoR mp arg))
                    Scale R arg1 arg2 ->
                         let intervalToCalc = [errorEval valMap radius (depth + 1) (expTwoR mp arg1)
                                , errorEval valMap radius (depth + 1) (expTwoR mp arg2)]
                          in foldl1' intervalProduct depth intervalToCalc
                    Div arg1 arg2 ->
                          let divArg1 = errorEval valMap radius (depth + 1) (expTwoR mp arg1) :: Array (Int,Int) ErrorType
                              divArg2 = errorEval valMap radius (depth + 1) (expTwoR mp arg2) :: Array (Int,Int) ErrorType
                          in fmapDiv intervalDiv depth divArg1 divArg2
                    Sqrt arg -> fmap intervalSqrt depth (errorEval valMap radius (depth + 1) (expTwoR mp arg))
                    Log arg -> fmap intervalLog depth (errorEval valMap radius (depth + 1) (expTwoR mp arg))
--                    Sin arg -> fmap sin . eval valMap $ expOneR mp arg
--                    Cos arg -> fmap cos . eval valMap $ expOneR mp arg
--                    Tan arg -> fmap tan . eval valMap $ expOneR mp arg
--                    Exp arg -> fmap exp . eval valMap $ expOneR mp arg
--                    Sinh arg -> fmap sinh . eval valMap $ expOneR mp arg
--                    Cosh arg -> fmap cosh . eval valMap $ expOneR mp arg
--                    Tanh arg -> fmap tanh . eval valMap $ expOneR mp arg
--                    Asin arg -> fmap asin . eval valMap $ expOneR mp arg
--                    Acos arg -> fmap acos . eval valMap $ expOneR mp arg
--                    Atan arg -> fmap atan . eval valMap $ expOneR mp arg
--                    Asinh arg -> fmap asinh . eval valMap $ expOneR mp arg
--                    Acosh arg -> fmap acosh . eval valMap $ expOneR mp arg
--                    Atanh arg -> fmap atanh . eval valMap $ expOneR mp arg
--                    RealPart arg -> fmap realPart . eval valMap $ expOneC mp arg
--                    ImagPart arg -> fmap imagPart . eval valMap $ expOneC mp arg
--                    -- Rotate rA arg ->
--                    Piecewise marks conditionArg branchArgs ->
--                        let cdt = eval valMap $ expOneR mp conditionArg
--                            branches = map (eval valMap . expOneR mp) branchArgs
--                         in listArray
--                                (0, size - 1)
--                                [ chosen ! i
--                                | i <- [0 .. size - 1]
--                                , let chosen =
--                                          chooseBranch marks (cdt ! i) branches
--                                ]
--                    Rotate [amount] arg ->
--                        rotate1D size amount (eval valMap $ expOneR mp arg)
--                    _ -> error "expression structure One R is wrong"
        | otherwise = error "one r but shape is not [size] ??"


-- | Error  Estimation for 3D arrays
--
instance ErrorEvaluable Three R (Array (Int, Int, Int) ErrorType) where
    errorEval :: ValMaps -> Double -> Int -> Expression Three R -> Array (Int, Int, Int) ErrorType
    errorEval valMap radius depth e@(Expression n mp)
        | [size1, size2, size3] <- retrieveShape n mp =
            let fmapInterval :: (Int -> Double -> a -> b) -> Int -> Double -> Array (Int, Int, Int) a -> Array (Int, Int, Int) b
                fmapInterval f depth radius arr =
                    listArray
                        ((0, 0, 0), (size1 - 1, size2 -1, size3 -1))
                        [f depth radius x | i <- [0 .. size1 - 1], j <- [0 .. size2 - 1], k <- [0 .. size3 - 1], let x = arr ! (i, j, k)]
                fmap :: (Int -> a -> a) -> Int -> Array (Int, Int, Int) a -> Array (Int, Int, Int) a
                fmap f depth arr =
                    listArray
                        ((0, 0, 0),(size1 - 1, size2 - 1, size3 - 1))
                        [f depth x | i <- [0 .. size1 - 1] , j <- [0 .. size2 - 1], k <-[0 .. size3 - 1], let x = arr ! (i, j, k)]
                fmapDiv :: (Int -> a -> a -> a) -> Int -> Array (Int, Int, Int) a -> Array (Int, Int, Int) a -> Array (Int, Int, Int) a
                fmapDiv f depth arr1 arr2 =
                    listArray
                        ((0, 0, 0),(size1 -1 , size2 -1, size3 - 1))
                        [f depth x y | i <- [0 .. size1 - 1] , j <-[0 .. size2 - 1], k <- [0 .. size3 - 1], let x = arr1 ! (i, j, k), let y = arr2 ! (i, j, k)]
                fmapPower :: (Int -> Int -> a -> a) -> Int -> Int -> Array (Int,Int, Int) a -> Array (Int,Int, Int) a
                fmapPower f depth pwr arr =
                    listArray
                        ((0, 0, 0),(size1 - 1, size2 - 1, size3 -1))
                        [f depth pwr x | i <- [0 .. size1 - 1], j <- [0 .. size2 - 1], k <- [0 .. size3 - 1], let x = arr ! (i, j, k)]
                zipWith ::
                       ([a] -> Int -> a)
                    -> Int
                    -> Array (Int, Int, Int) a
                    -> Array (Int, Int, Int) a
                    -> Array (Int, Int, Int) a
                zipWith f depth arr1 arr2 =
                    listArray
                        ((0, 0, 0),(size1 - 1, size2 - 1, size3 -1))
                        [ f [x,y] depth
                        | i <- [0 .. size1 - 1], j <-[0 .. size2 - 1], k <-[0 .. size3 - 1]
                        , let x = arr1 ! (i, j, k)
                        , let y = arr2 ! (i, j, k)
                        ]
                foldl1' :: ([a] -> Int -> a) -> Int -> [Array (Int, Int, Int) a] -> Array (Int, Int, Int) a
                foldl1' f depth [x] = x
                foldl1' f depth (x:xs) = zipWith f depth x (foldl1' f depth xs)
             in case retrieveNode n mp of
                    Var name ->
                        case Map.lookup name $ vm3 valMap of
                            Just val -> fmapInterval constantErorCalc (depth +1) radius val
                            _ -> error "no value associated with the variable"
                    Const val -> listArray ((0, 0, 0), (size1 - 1, size2 -1, size3 -1)) $ replicate (size1 * size2 * size3) (0,val,[val,val])
                    Sum R args -> foldl1' intervalSum depth (map (errorEval valMap radius (depth + 1) . expThreeR mp) args)
                    Mul R args -> foldl1' intervalProduct depth . map (errorEval valMap radius (depth +1) . expThreeR mp) $ args
                    Neg R arg -> fmap intervalNeg depth (errorEval valMap radius (depth + 1) (expThreeR mp arg))
                    Power x arg -> fmapPower intervalPower depth x  (errorEval valMap radius (depth + 1) (expThreeR mp arg))
                    Scale R arg1 arg2 ->
                         let intervalToCalc = [errorEval valMap radius (depth + 1) (expThreeR mp arg1)
                                , errorEval valMap radius (depth + 1) (expThreeR mp arg2)]
                          in foldl1' intervalProduct depth intervalToCalc
                    Div arg1 arg2 ->
                          let divArg1 = errorEval valMap radius (depth + 1) (expThreeR mp arg1) :: Array (Int,Int, Int) ErrorType
                              divArg2 = errorEval valMap radius (depth + 1) (expThreeR mp arg2) :: Array (Int,Int, Int) ErrorType
                          in fmapDiv intervalDiv depth divArg1 divArg2
                    Sqrt arg -> fmap intervalSqrt depth (errorEval valMap radius (depth + 1) (expThreeR mp arg))
                    Log arg -> fmap intervalLog depth (errorEval valMap radius (depth + 1) (expThreeR mp arg))
--                    Sin arg -> fmap sin . eval valMap $ expOneR mp arg
--                    Cos arg -> fmap cos . eval valMap $ expOneR mp arg
--                    Tan arg -> fmap tan . eval valMap $ expOneR mp arg
--                    Exp arg -> fmap exp . eval valMap $ expOneR mp arg
--                    Sinh arg -> fmap sinh . eval valMap $ expOneR mp arg
--                    Cosh arg -> fmap cosh . eval valMap $ expOneR mp arg
--                    Tanh arg -> fmap tanh . eval valMap $ expOneR mp arg
--                    Asin arg -> fmap asin . eval valMap $ expOneR mp arg
--                    Acos arg -> fmap acos . eval valMap $ expOneR mp arg
--                    Atan arg -> fmap atan . eval valMap $ expOneR mp arg
--                    Asinh arg -> fmap asinh . eval valMap $ expOneR mp arg
--                    Acosh arg -> fmap acosh . eval valMap $ expOneR mp arg
--                    Atanh arg -> fmap atanh . eval valMap $ expOneR mp arg
--                    RealPart arg -> fmap realPart . eval valMap $ expOneC mp arg
--                    ImagPart arg -> fmap imagPart . eval valMap $ expOneC mp arg
--                    -- Rotate rA arg ->
--                    Piecewise marks conditionArg branchArgs ->
--                        let cdt = eval valMap $ expOneR mp conditionArg
--                            branches = map (eval valMap . expOneR mp) branchArgs
--                         in listArray
--                                (0, size - 1)
--                                [ chosen ! i
--                                | i <- [0 .. size - 1]
--                                , let chosen =
--                                          chooseBranch marks (cdt ! i) branches
--                                ]
--                    Rotate [amount] arg ->
--                        rotate1D size amount (eval valMap $ expOneR mp arg)
--                    _ -> error "expression structure One R is wrong"
        | otherwise = error "one r but shape is not [size] ??"
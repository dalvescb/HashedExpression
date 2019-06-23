{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module HashedInterp where

import Data.Array as A
import Data.Complex as DC
import qualified Data.IntMap.Strict as IM
import Data.Map (Map, fromList)
import qualified Data.Map as Map
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
import HashedUtils

-- |
--
data ValMaps =
    ValMaps
        { vm0 :: Map String Double
        , vm1 :: Map String (Array Int Double)
        , vm2 :: Map String (Array (Int, Int) Double)
        , vm3 :: Map String (Array (Int, Int, Int) Double)
        }
    deriving (Eq, Show, Ord)

emptyVms :: ValMaps
emptyVms =
    ValMaps {vm0 = Map.empty, vm1 = Map.empty, vm2 = Map.empty, vm3 = Map.empty}

-- | Helpers so we can write things like
-- emptyVms |> withVm0 (..) |> withVm1 (..) |> withVM2 (..)
--
withVm0 :: Map String Double -> ValMaps -> ValMaps
withVm0 vm0 (ValMaps _ vm1 vm2 vm3) = ValMaps vm0 vm1 vm2 vm3

withVm1 :: Map String (Array Int Double) -> ValMaps -> ValMaps
withVm1 vm1 (ValMaps vm0 _ vm2 vm3) = ValMaps vm0 vm1 vm2 vm3

withVm2 :: Map String (Array (Int, Int) Double) -> ValMaps -> ValMaps
withVm2 vm2 (ValMaps vm0 vm1 _ vm3) = ValMaps vm0 vm1 vm2 vm3

withVm3 :: Map String (Array (Int, Int, Int) Double) -> ValMaps -> ValMaps
withVm3 vm3 (ValMaps vm0 vm1 vm2 _) = ValMaps vm0 vm1 vm2 vm3

-- | Turn expression to the right type
--
expZeroR :: ExpressionMap -> Int -> Expression Zero R
expZeroR = flip Expression

expOneR :: ExpressionMap -> Int -> Expression One R
expOneR = flip Expression

expTwoR :: ExpressionMap -> Int -> Expression Two R
expTwoR = flip Expression

expThreeR :: ExpressionMap -> Int -> Expression Three R
expThreeR = flip Expression

expZeroC :: ExpressionMap -> Int -> Expression Zero C
expZeroC = flip Expression

expOneC :: ExpressionMap -> Int -> Expression One C
expOneC = flip Expression

expTwoC :: ExpressionMap -> Int -> Expression Two C
expTwoC = flip Expression

expThreeC :: ExpressionMap -> Int -> Expression Three C
expThreeC = flip Expression

-- |
--
class Evaluable d rc output | d rc -> output where
    eval :: ValMaps -> Expression d rc -> output

-- |
--
instance Evaluable Zero R Double where
    eval :: ValMaps -> Expression Zero R -> Double
    eval valMap e@(Expression n mp)
        | [] <- retrieveShape n mp =
            case retrieveNode n mp of
                Var name ->
                    case Map.lookup name $ vm0 valMap of
                        Just val -> val
                        _ -> error "no value associated with the variable"
                Const val -> val
                Sum R args -> sum . map (eval valMap . expZeroR mp) $ args
                Mul R args -> product . map (eval valMap . expZeroR mp) $ args
                Neg R arg -> -(eval valMap $ expZeroR mp arg)
                Scale R arg1 arg2 ->
                    eval valMap (expZeroR mp arg1) *
                    eval valMap (expZeroR mp arg2)
                Div arg1 arg2 ->
                    eval valMap (expZeroR mp arg1) /
                    eval valMap (expZeroR mp arg2)
                Sqrt arg -> sqrt (eval valMap (expZeroR mp arg))
                Sin arg -> sin (eval valMap (expZeroR mp arg))
                Cos arg -> cos (eval valMap (expZeroR mp arg))
                Tan arg -> tan (eval valMap (expZeroR mp arg))
                Exp arg -> exp (eval valMap (expZeroR mp arg))
                Log arg -> log (eval valMap (expZeroR mp arg))
                Sinh arg -> sinh (eval valMap (expZeroR mp arg))
                Cosh arg -> cosh (eval valMap (expZeroR mp arg))
                Tanh arg -> tanh (eval valMap (expZeroR mp arg))
                Asin arg -> asin (eval valMap (expZeroR mp arg))
                Acos arg -> acos (eval valMap (expZeroR mp arg))
                Atan arg -> atan (eval valMap (expZeroR mp arg))
                Asinh arg -> asinh (eval valMap (expZeroR mp arg))
                Acosh arg -> acosh (eval valMap (expZeroR mp arg))
                Atanh arg -> atanh (eval valMap (expZeroR mp arg))
                RealPart arg -> DC.realPart (eval valMap (expZeroC mp arg))
                ImagPart arg -> DC.imagPart (eval valMap (expZeroC mp arg))
                InnerProd R arg1 arg2 ->
                    case retrieveShape arg1 mp of
                        [] ->
                            eval valMap (expZeroR mp arg1) *
                            eval valMap (expZeroR mp arg2)
                        [size] ->
                            let res1 = eval valMap $ expOneR mp arg1
                                res2 = eval valMap $ expOneR mp arg2
                             in sum [ x * y
                                    | i <- [0 .. size - 1]
                                    , let x = res1 ! i
                                    , let y = res2 ! i
                                    ]
                        [size1, size2] ->
                            let res1 = eval valMap $ expTwoR mp arg1
                                res2 = eval valMap $ expTwoR mp arg2
                             in sum [ x * y
                                    | i <- [0 .. size1 - 1]
                                    , j <- [0 .. size2 - 1]
                                    , let x = res1 ! (i, j)
                                    , let y = res2 ! (i, j)
                                    ]
                        [size1, size2, size3] ->
                            let res1 = eval valMap $ expThreeR mp arg1
                                res2 = eval valMap $ expThreeR mp arg2
                             in sum [ x * y
                                    | i <- [0 .. size1 - 1]
                                    , j <- [0 .. size2 - 1]
                                    , k <- [0 .. size3 - 1]
                                    , let x = res1 ! (i, j, k)
                                    , let y = res2 ! (i, j, k)
                                    ]
                _ -> error "expression structure Scalar R is wrong"
        | otherwise = error "one r but shape is not [] ??"

instance Evaluable Zero C (DC.Complex Double) where
    eval :: ValMaps -> Expression Zero C -> DC.Complex Double
    eval valMap e@(Expression n mp)
        | [] <- retrieveShape n mp =
            case retrieveNode n mp of
                Sum C args -> sum . map (eval valMap . expZeroC mp) $ args
                Mul C args -> product . map (eval valMap . expZeroC mp) $ args
                Neg C arg -> -(eval valMap $ expZeroC mp arg)
                Scale C arg1 arg2 ->
                    case retrieveElementType arg1 mp of
                        R ->
                            fromR (eval valMap (expZeroR mp arg1)) *
                            eval valMap (expZeroC mp arg2)
                        C ->
                            eval valMap (expZeroC mp arg1) *
                            eval valMap (expZeroC mp arg2)
                RealImag arg1 arg2 ->
                    eval valMap (expZeroR mp arg1) :+
                    eval valMap (expZeroR mp arg2)
                InnerProd C arg1 arg2 ->
                    case retrieveShape arg1 mp of
                        [] ->
                            eval valMap (expZeroC mp arg1) *
                            eval valMap (expZeroC mp arg2)
                        [size] ->
                            let res1 = eval valMap $ expOneC mp arg1
                                res2 = eval valMap $ expOneC mp arg2
                             in sum [ x * y
                                    | i <- [0 .. size - 1]
                                    , let x = res1 ! i
                                    , let y = res2 ! i
                                    ]
                        [size1, size2] ->
                            let res1 = eval valMap $ expTwoC mp arg1
                                res2 = eval valMap $ expTwoC mp arg2
                             in sum [ x * y
                                    | i <- [0 .. size1 - 1]
                                    , j <- [0 .. size2 - 1]
                                    , let x = res1 ! (i, j)
                                    , let y = res2 ! (i, j)
                                    ]
                        [size1, size2, size3] ->
                            let res1 = eval valMap $ expThreeC mp arg1
                                res2 = eval valMap $ expThreeC mp arg2
                             in sum [ x * y
                                    | i <- [0 .. size1 - 1]
                                    , j <- [0 .. size2 - 1]
                                    , k <- [0 .. size3 - 1]
                                    , let x = res1 ! (i, j, k)
                                    , let y = res2 ! (i, j, k)
                                    ]
                _ -> error "expression structure Scalar C is wrong"
        | otherwise = error "One C but shape is not [] ??"

-- |
--
instance Evaluable One R (Array Int Double) where
    eval :: ValMaps -> Expression One R -> Array Int Double
    eval valMap e@(Expression n mp)
        | [size] <- retrieveShape n mp =
            let fmap :: (a -> c) -> Array Int a -> Array Int c
                fmap f arr =
                    listArray
                        (0, size - 1)
                        [f x | i <- [0 .. size - 1], let x = arr ! i]
                zipWith ::
                       (a -> b -> c)
                    -> Array Int a
                    -> Array Int b
                    -> Array Int c
                zipWith f arr1 arr2 =
                    listArray
                        (0, size - 1)
                        [ f x y
                        | i <- [0 .. size - 1]
                        , let x = arr1 ! i
                        , let y = arr2 ! i
                        ]
                foldl1' :: (a -> a -> a) -> [Array Int a] -> Array Int a
                foldl1' f [x] = x
                foldl1' f (x:xs) = zipWith f x (foldl1' f xs)
             in case retrieveNode n mp of
                    Var name ->
                        case Map.lookup name $ vm1 valMap of
                            Just val -> val
                            _ -> error "no value associated with the variable"
                    Const val -> listArray (0, size - 1) $ replicate size val
                    Sum R args ->
                        foldl1' (+) . map (eval valMap . expOneR mp) $ args
                    Mul R args ->
                        foldl1' (+) . map (eval valMap . expOneR mp) $ args
                    Neg R arg -> fmap negate . eval valMap $ expOneR mp arg
                    Scale R arg1 arg2 ->
                        let scalar = eval valMap $ expZeroR mp arg1
                         in fmap (scalar *) . eval valMap $ expOneR mp arg2
                    Div arg1 arg2 ->
                        zipWith
                            (/)
                            (eval valMap $ expOneR mp arg2)
                            (eval valMap $ expOneR mp arg2)
                    Sqrt arg -> fmap sqrt . eval valMap $ expOneR mp arg
                    Sin arg -> fmap sin . eval valMap $ expOneR mp arg
                    Cos arg -> fmap cos . eval valMap $ expOneR mp arg
                    Tan arg -> fmap tan . eval valMap $ expOneR mp arg
                    Exp arg -> fmap exp . eval valMap $ expOneR mp arg
                    Log arg -> fmap log . eval valMap $ expOneR mp arg
                    Sinh arg -> fmap sinh . eval valMap $ expOneR mp arg
                    Cosh arg -> fmap cosh . eval valMap $ expOneR mp arg
                    Tanh arg -> fmap tanh . eval valMap $ expOneR mp arg
                    Asin arg -> fmap asin . eval valMap $ expOneR mp arg
                    Acos arg -> fmap acos . eval valMap $ expOneR mp arg
                    Atan arg -> fmap atan . eval valMap $ expOneR mp arg
                    Asinh arg -> fmap asinh . eval valMap $ expOneR mp arg
                    Acosh arg -> fmap acosh . eval valMap $ expOneR mp arg
                    Atanh arg -> fmap atanh . eval valMap $ expOneR mp arg
                    RealPart arg ->
                        fmap DC.realPart . eval valMap $ expOneC mp arg
                    ImagPart arg ->
                        fmap DC.imagPart . eval valMap $ expOneC mp arg
                    _ -> error "expression structure One R is wrong"
        | otherwise = error "one r but shape is not [size] ??"

-- |
--
instance Evaluable One C (Array Int (DC.Complex Double)) where
    eval :: ValMaps -> Expression One C -> Array Int (DC.Complex Double)
    eval valMap e@(Expression n mp)
        | [size] <- retrieveShape n mp =
            let fmap :: (a -> b) -> Array Int a -> Array Int b
                fmap f arr =
                    listArray
                        (0, size - 1)
                        [f x | i <- [0 .. size - 1], let x = arr ! i]
                zipWith ::
                       (a -> b -> c)
                    -> Array Int a
                    -> Array Int b
                    -> Array Int c
                zipWith f arr1 arr2 =
                    listArray
                        (0, size - 1)
                        [ f x y
                        | i <- [0 .. size - 1]
                        , let x = arr1 ! i
                        , let y = arr2 ! i
                        ]
                foldl1' :: (a -> a -> a) -> [Array Int a] -> Array Int a
                foldl1' f [x] = x
                foldl1' f (x:xs) = zipWith f x (foldl1' f xs)
             in case retrieveNode n mp of
                    Sum C args ->
                        foldl1' (+) . map (eval valMap . expOneC mp) $ args
                    Mul C args ->
                        foldl1' (+) . map (eval valMap . expOneC mp) $ args
                    Neg C arg -> fmap negate . eval valMap $ expOneC mp arg
                    Scale C arg1 arg2 ->
                        case retrieveElementType arg1 mp of
                            R ->
                                let scalar =
                                        fromR . eval valMap $ expZeroR mp arg1
                                 in fmap (scalar *) . eval valMap $
                                    expOneC mp arg2
                            C ->
                                let scalar = eval valMap $ expZeroC mp arg1
                                 in fmap (scalar *) . eval valMap $
                                    expOneC mp arg2
                    RealImag arg1 arg2 ->
                        zipWith
                            (:+)
                            (eval valMap $ expOneR mp arg1)
                            (eval valMap $ expOneR mp arg2)
                    _ -> error "expression structure One C is wrong"
        | otherwise = error "one C but shape is not [size] ??"

instance Evaluable Two R (Array (Int, Int) Double) where
    eval :: ValMaps -> Expression Two R -> Array (Int, Int) Double
    eval valMap e@(Expression n mp)
        | [size1, size2] <- retrieveShape n mp =
            let fmap :: (a -> c) -> Array (Int, Int) a -> Array (Int, Int) c
                fmap f arr =
                    listArray
                        ((0, 0), (size1 - 1, size2 - 1))
                        [ f x
                        | i <- [0 .. size1 - 1]
                        , j <- [0 .. size2 - 1]
                        , let x = arr ! (i, j)
                        ]
                zipWith ::
                       (a -> b -> c)
                    -> Array (Int, Int) a
                    -> Array (Int, Int) b
                    -> Array (Int, Int) c
                zipWith f arr1 arr2 =
                    listArray
                        ((0, 0), (size1 - 1, size2 - 1))
                        [ f x y
                        | i <- [0 .. size1 - 1]
                        , j <- [0 .. size2 - 1]
                        , let x = arr1 ! (i, j)
                        , let y = arr2 ! (i, j)
                        ]
                foldl1' ::
                       (a -> a -> a)
                    -> [Array (Int, Int) a]
                    -> Array (Int, Int) a
                foldl1' f [x] = x
                foldl1' f (x:xs) = zipWith f x (foldl1' f xs)
             in case retrieveNode n mp of
                    Var name ->
                        case Map.lookup name $ vm2 valMap of
                            Just val -> val
                            _ -> error "no value associated with the variable"
                    Const val ->
                        listArray ((0, 0), (size1 - 1, size2 - 1)) $
                        replicate (size1 * size2) val
                    Sum R args ->
                        foldl1' (+) . map (eval valMap . expTwoR mp) $ args
                    Mul R args ->
                        foldl1' (+) . map (eval valMap . expTwoR mp) $ args
                    Neg R arg -> fmap negate . eval valMap $ expTwoR mp arg
                    Scale R arg1 arg2 ->
                        let scalar = eval valMap $ expZeroR mp arg1
                         in fmap (scalar *) . eval valMap $ expTwoR mp arg2
                    Div arg1 arg2 ->
                        zipWith
                            (/)
                            (eval valMap $ expTwoR mp arg2)
                            (eval valMap $ expTwoR mp arg2)
                    Sqrt arg -> fmap sqrt . eval valMap $ expTwoR mp arg
                    Sin arg -> fmap sin . eval valMap $ expTwoR mp arg
                    Cos arg -> fmap cos . eval valMap $ expTwoR mp arg
                    Tan arg -> fmap tan . eval valMap $ expTwoR mp arg
                    Exp arg -> fmap exp . eval valMap $ expTwoR mp arg
                    Log arg -> fmap log . eval valMap $ expTwoR mp arg
                    Sinh arg -> fmap sinh . eval valMap $ expTwoR mp arg
                    Cosh arg -> fmap cosh . eval valMap $ expTwoR mp arg
                    Tanh arg -> fmap tanh . eval valMap $ expTwoR mp arg
                    Asin arg -> fmap asin . eval valMap $ expTwoR mp arg
                    Acos arg -> fmap acos . eval valMap $ expTwoR mp arg
                    Atan arg -> fmap atan . eval valMap $ expTwoR mp arg
                    Asinh arg -> fmap asinh . eval valMap $ expTwoR mp arg
                    Acosh arg -> fmap acosh . eval valMap $ expTwoR mp arg
                    Atanh arg -> fmap atanh . eval valMap $ expTwoR mp arg
                    RealPart arg ->
                        fmap DC.realPart . eval valMap $ expTwoC mp arg
                    ImagPart arg ->
                        fmap DC.imagPart . eval valMap $ expTwoC mp arg
                    _ -> error "expression structure Two R is wrong"
        | otherwise = error "Two r but shape is not [size1, size2] ??"

instance Evaluable Two C (Array (Int, Int) (DC.Complex Double)) where
    eval :: ValMaps -> Expression Two C -> Array (Int, Int) (DC.Complex Double)
    eval valMap e@(Expression n mp)
        | [size1, size2] <- retrieveShape n mp =
            let fmap :: (a -> c) -> Array (Int, Int) a -> Array (Int, Int) c
                fmap f arr =
                    listArray
                        ((0, 0), (size1 - 1, size2 - 1))
                        [ f x
                        | i <- [0 .. size1 - 1]
                        , j <- [0 .. size2 - 1]
                        , let x = arr ! (i, j)
                        ]
                zipWith ::
                       (a -> b -> c)
                    -> Array (Int, Int) a
                    -> Array (Int, Int) b
                    -> Array (Int, Int) c
                zipWith f arr1 arr2 =
                    listArray
                        ((0, 0), (size1 - 1, size2 - 1))
                        [ f x y
                        | i <- [0 .. size1 - 1]
                        , j <- [0 .. size2 - 1]
                        , let x = arr1 ! (i, j)
                        , let y = arr2 ! (i, j)
                        ]
                foldl1' ::
                       (a -> a -> a)
                    -> [Array (Int, Int) a]
                    -> Array (Int, Int) a
                foldl1' f [x] = x
                foldl1' f (x:xs) = zipWith f x (foldl1' f xs)
             in case retrieveNode n mp of
                    Sum C args ->
                        foldl1' (+) . map (eval valMap . expTwoC mp) $ args
                    Mul C args ->
                        foldl1' (+) . map (eval valMap . expTwoC mp) $ args
                    Neg C arg -> fmap negate . eval valMap $ expTwoC mp arg
                    Scale C arg1 arg2 ->
                        case retrieveElementType arg1 mp of
                            R ->
                                let scalar =
                                        fromR . eval valMap $ expZeroR mp arg1
                                 in fmap (scalar *) . eval valMap $
                                    expTwoC mp arg2
                            C ->
                                let scalar = eval valMap $ expZeroC mp arg1
                                 in fmap (scalar *) . eval valMap $
                                    expTwoC mp arg2
                    RealImag arg1 arg2 ->
                        zipWith
                            (:+)
                            (eval valMap $ expTwoR mp arg1)
                            (eval valMap $ expTwoR mp arg2)
                    _ -> error "expression structure Two C is wrong"
        | otherwise = error "Two C but shape is not [size1, size2] ??"

instance Evaluable Three R (Array (Int, Int, Int) Double) where
    eval :: ValMaps -> Expression Three R -> Array (Int, Int, Int) Double
    eval valMap e@(Expression n mp)
        | [size1, size2, size3] <- retrieveShape n mp =
            let fmap ::
                       (a -> c)
                    -> Array (Int, Int, Int) a
                    -> Array (Int, Int, Int) c
                fmap f arr =
                    listArray
                        ((0, 0, 0), (size1 - 1, size2 - 1, size3 - 1))
                        [ f x
                        | i <- [0 .. size1 - 1]
                        , j <- [0 .. size2 - 1]
                        , k <- [0 .. size3 - 1]
                        , let x = arr ! (i, j, k)
                        ]
                zipWith ::
                       (a -> b -> c)
                    -> Array (Int, Int, Int) a
                    -> Array (Int, Int, Int) b
                    -> Array (Int, Int, Int) c
                zipWith f arr1 arr2 =
                    listArray
                        ((0, 0, 0), (size1 - 1, size2 - 1, size3 - 1))
                        [ f x y
                        | i <- [0 .. size1 - 1]
                        , j <- [0 .. size2 - 1]
                        , k <- [0 .. size3 - 1]
                        , let x = arr1 ! (i, j, k)
                        , let y = arr2 ! (i, j, k)
                        ]
                foldl1' ::
                       (a -> a -> a)
                    -> [Array (Int, Int, Int) a]
                    -> Array (Int, Int, Int) a
                foldl1' f [x] = x
                foldl1' f (x:xs) = zipWith f x (foldl1' f xs)
             in case retrieveNode n mp of
                    Var name ->
                        case Map.lookup name $ vm3 valMap of
                            Just val -> val
                            _ -> error "no value associated with the variable"
                    Const val ->
                        listArray ((0, 0, 0), (size1 - 1, size2 - 1, size3 - 1)) $
                        replicate (size1 * size2 * size3) val
                    Sum R args ->
                        foldl1' (+) . map (eval valMap . expThreeR mp) $ args
                    Mul R args ->
                        foldl1' (+) . map (eval valMap . expThreeR mp) $ args
                    Neg R arg -> fmap negate . eval valMap $ expThreeR mp arg
                    Scale R arg1 arg2 ->
                        let scalar = eval valMap $ expZeroR mp arg1
                         in fmap (scalar *) . eval valMap $ expThreeR mp arg2
                    Div arg1 arg2 ->
                        zipWith
                            (/)
                            (eval valMap $ expThreeR mp arg2)
                            (eval valMap $ expThreeR mp arg2)
                    Sqrt arg -> fmap sqrt . eval valMap $ expThreeR mp arg
                    Sin arg -> fmap sin . eval valMap $ expThreeR mp arg
                    Cos arg -> fmap cos . eval valMap $ expThreeR mp arg
                    Tan arg -> fmap tan . eval valMap $ expThreeR mp arg
                    Exp arg -> fmap exp . eval valMap $ expThreeR mp arg
                    Log arg -> fmap log . eval valMap $ expThreeR mp arg
                    Sinh arg -> fmap sinh . eval valMap $ expThreeR mp arg
                    Cosh arg -> fmap cosh . eval valMap $ expThreeR mp arg
                    Tanh arg -> fmap tanh . eval valMap $ expThreeR mp arg
                    Asin arg -> fmap asin . eval valMap $ expThreeR mp arg
                    Acos arg -> fmap acos . eval valMap $ expThreeR mp arg
                    Atan arg -> fmap atan . eval valMap $ expThreeR mp arg
                    Asinh arg -> fmap asinh . eval valMap $ expThreeR mp arg
                    Acosh arg -> fmap acosh . eval valMap $ expThreeR mp arg
                    Atanh arg -> fmap atanh . eval valMap $ expThreeR mp arg
                    RealPart arg ->
                        fmap DC.realPart . eval valMap $ expThreeC mp arg
                    ImagPart arg ->
                        fmap DC.imagPart . eval valMap $ expThreeC mp arg
                    _ -> error "expression structure Two R is wrong"
        | otherwise = error "Two r but shape is not [size1, size2] ??"

instance Evaluable Three C (Array (Int, Int, Int) (DC.Complex Double)) where
    eval ::
           ValMaps
        -> Expression Three C
        -> Array (Int, Int, Int) (DC.Complex Double)
    eval valMap e@(Expression n mp) = undefined

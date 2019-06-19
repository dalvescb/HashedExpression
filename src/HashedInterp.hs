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
import HashedExpression hiding ((+), (-))

-- |
--
data ValMaps =
    ValMaps
        { vm0 :: Map String Double
        , vm1 :: Map String (Array Int Double)
        }
    deriving (Eq, Show, Ord)

subs :: [(String, Double)] -> [(String, Array Int Double)] -> ValMaps
subs vm0 vm1 = ValMaps (fromList vm0) (fromList vm1)

-- |
--
class Evaluable d rc output | d rc -> output where
    eval :: ValMaps -> Expression d rc -> output

-- |
--
instance Evaluable Zero R Double where
    eval :: ValMaps -> Expression Zero R -> Double
    eval valMap e@(Expression n mp) =
        case IM.lookup n mp of
            Just ([], Var name) ->
                case Map.lookup name $ vm0 valMap of
                    Just val -> val
                    _ -> error "no value associated with the variable"
            Just ([], Sum R [node1, node2]) ->
                let subExp1 = Expression node1 mp :: Expression Zero R
                    subExp2 = Expression node2 mp :: Expression Zero R
                 in eval valMap subExp1 + eval valMap subExp2
            _ -> error "expression structure Scalar R is wrong"

--            Just ([], Mul R [node1, node2]) ->
--                let subExp1 = Expression node1 mp :: Expression Scalar R
--                    subExp2 = Expression node2 mp :: Expression Scalar R
--                 in eval valMap subExp1 * eval valMap subExp2
--            Just ([], Scale R node1 node2) ->
--                let subExp1 = Expression node1 mp :: Expression Zero R
--                    subExp2 = Expression node2 mp :: Expression Zero R
--                 in eval valMap subExp1 * eval valMap subExp2
--            Just ([], InnerProd R node1 node2) ->
--                case IM.lookup node1 mp of
--                    Just ([], _) ->
--                        let subExp1 = Expression node1 mp :: Expression Scalar R -- shape is [], so must be Scalar R
--                            subExp2 = Expression node2 mp :: Expression Scalar R -- shape is [], so must be Scalar R
--                         in eval valMap subExp1 * eval valMap subExp2
--                    Just ([size], _) ->
--                        let subExp1 = Expression node1 mp :: Expression One R -- shape is [size], so must be One R
--                            subExp2 = Expression node2 mp :: Expression One R -- shape is [size], so must be One R
--                            lst1 = A.elems $ eval valMap subExp1
--                            lst2 = A.elems $ eval valMap subExp2
--                         in sum $ zipWith (*) lst1 lst2
-- |
--
instance Evaluable Zero C (DC.Complex Double) where
    eval :: ValMaps -> Expression Zero C -> DC.Complex Double
    eval valMap e@(Expression n mp) =
        case IM.lookup n mp of
            Just ([], Sum C [node1, node2]) ->
                let subExp1 = Expression node1 mp :: Expression Zero C
                    subExp2 = Expression node2 mp :: Expression Zero C
                 in eval valMap subExp1 + eval valMap subExp2
            Just ([], RealImag node1 node2) ->
                let subExp1 = Expression node1 mp :: Expression Zero R
                    subExp2 = Expression node2 mp :: Expression Zero R
                 in eval valMap subExp1 :+ eval valMap subExp2
            _ -> error "expression structure Scalar C is wrong"

--            Just ([], Mul C [node1, node2]) ->
--                let subExp1 = Expression node1 mp :: Expression Scalar C
--                    subExp2 = Expression node2 mp :: Expression Scalar C
--                 in eval valMap subExp1 * eval valMap subExp2
--            Just ([], Scale C node1 node2) ->
--                let subExp2 = Expression node2 mp :: Expression Zero C
--                    scale =
--                        case nodeElementType . retrieveNode mp $ node1 of
--                            R ->
--                                fromR . eval valMap $
--                                (Expression node1 mp :: Expression Zero R)
--                            C ->
--                                eval
--                                    valMap
--                                    (Expression node1 mp :: Expression Zero C)
--                 in scale * eval valMap subExp2
--            Just ([], InnerProd C node1 node2) ->
--                case IM.lookup node1 mp of
--                    Just ([], _) ->
--                        let subExp1 = Expression node1 mp :: Expression Scalar C -- shape is [], so must be Scalar C
--                            subExp2 = Expression node2 mp :: Expression Scalar C -- shape is [], so must be Scalar C
--                         in eval valMap subExp1 * eval valMap subExp2
--                    Just ([size], _) ->
--                        let subExp1 = Expression node1 mp :: Expression One C -- shape is [size], so must be One C
--                            subExp2 = Expression node2 mp :: Expression One C -- shape is [size], so must be One C
--                            lst1 = A.elems $ eval valMap subExp1
--                            lst2 = A.elems $ eval valMap subExp2
--                         in sum $ zipWith (*) lst1 lst2
-- |
--
instance Evaluable One R (Array Int Double) where
    eval :: ValMaps -> Expression One R -> Array Int Double
    eval valMap e@(Expression n mp) =
        case IM.lookup n mp of
            Just ([size], Var name) ->
                case Map.lookup name $ vm1 valMap of
                    Just val -> val
                    _ -> error "no value associated with the variable"
            Just ([size], Sum R [node1, node2]) ->
                let subExp1 = Expression node1 mp :: Expression One R
                    subExp2 = Expression node2 mp :: Expression One R
                    lst1 = A.elems $ eval valMap subExp1
                    lst2 = A.elems $ eval valMap subExp2
                    lstRes = zipWith (+) lst1 lst2
                 in A.listArray (0, size - 1) lstRes
            _ -> error "expression structure One R is wrong"

--            Just ([size], Mul R [node1, node2]) ->
--                let subExp1 = Expression node1 mp :: Expression One R
--                    subExp2 = Expression node2 mp :: Expression One R
--                    lst1 = A.elems $ eval valMap subExp1
--                    lst2 = A.elems $ eval valMap subExp2
--                    lstRes = zipWith (*) lst1 lst2
--                 in A.listArray (0, size - 1) lstRes
--            Just ([size], Scale R node1 node2) ->
--                let subExp1 = Expression node1 mp :: Expression Zero R
--                    subExp2 = Expression node2 mp :: Expression One R
--                    scale = eval valMap subExp1
--                 in fmap (* scale) $ eval valMap subExp2
-- |
--
instance Evaluable One C (Array Int (DC.Complex Double)) where
    eval :: ValMaps -> Expression One C -> Array Int (DC.Complex Double)
    eval valMap e@(Expression n mp) =
        case IM.lookup n mp of
            Just ([size], Sum C [node1, node2]) ->
                let subExp1 = Expression node1 mp :: Expression One C
                    subExp2 = Expression node2 mp :: Expression One C
                    lst1 = A.elems $ eval valMap subExp1
                    lst2 = A.elems $ eval valMap subExp2
                    lstRes = zipWith (+) lst1 lst2
                 in A.listArray (0, size - 1) lstRes
--            Just ([size], Mul C [node1, node2]) ->
--                let subExp1 = Expression node1 mp :: Expression One C
--                    subExp2 = Expression node2 mp :: Expression One C
--                    lst1 = A.elems $ eval valMap subExp1
--                    lst2 = A.elems $ eval valMap subExp2
--                    lstRes = zipWith (*) lst1 lst2
--                 in A.listArray (0, size - 1) lstRes
--            Just ([size], Scale C node1 node2) ->
--                let subExp2 = Expression node2 mp :: Expression One C
--                    lst = A.elems $ eval valMap subExp2
--                    scale =
--                        case nodeElementType . retrieveNode mp $ node1 of
--                            R ->
--                                fromR . eval valMap $
--                                (Expression node1 mp :: Expression Zero R)
--                            C ->
--                                eval
--                                    valMap
--                                    (Expression node1 mp :: Expression Zero C)
--                 in A.listArray (0, size - 1) $ map (* scale) lst
            Just ([size], RealImag node1 node2) ->
                let subExp1 = Expression node1 mp :: Expression One R
                    subExp2 = Expression node2 mp :: Expression One R
                    lst1 = A.elems $ eval valMap subExp1
                    lst2 = A.elems $ eval valMap subExp2
                    lstRes = zipWith (:+) lst1 lst2
                 in A.listArray (0, size - 1) lstRes
            _ -> error "expression structure One C is wrong"

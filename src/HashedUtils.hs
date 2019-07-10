module HashedUtils where

import qualified Data.IntMap.Strict as IM
import qualified Data.Set as Set
import HashedExpression
import HashedHash
import HashedNode

import Data.Complex
import Data.Maybe
import GHC.IO.Unsafe (unsafePerformIO)

-- | Forward pipe operator in Elm
--
(|>) :: a -> (a -> b) -> b
(|>) = flip ($)

infixl 1 |>

-- |
--
mapBoth :: (a -> b) -> (a, a) -> (b, b)
mapBoth f (x, y) = (f x, f y)

-- |
--
bringMaybeOut :: (Maybe a, Maybe b) -> Maybe (a, b)
bringMaybeOut (Just x, Just y) = Just (x, y)
bringMaybeOut _ = Nothing

-- | Check if all elements of the list is equal
--
allEqual :: (Eq a) => [a] -> Bool
allEqual xs = and $ zipWith (==) (safeTail xs) xs
  where
    safeTail [] = []
    safeTail (x:xs) = xs

fromR :: Double -> Complex Double
fromR x = x :+ 0

-- | This function is going to ensure that to input Expression has the same shape
ensureSameShape :: Expression d et1 -> Expression d et2 -> a -> a
-- Inputs e1= Expression one, e2= Expression two, after = Expression after shape change
-- output : after if the shape of e1 and e2 are same
ensureSameShape e1 e2 after
    -- Check if the Expression Shape are the same (See Expression Shape comments on HashedNode) and if the result is
    -- true return after (which is something) ?
    | expressionShape e1 == expressionShape e2 = after
    -- Show error if the result is not satisfactory
    | otherwise =
        error $
        "Ensure same shape failed " ++
        show (expressionShape e1) ++ " " ++ show (expressionShape e2)

ensureSameShapeList :: [Expression d et] -> a -> a
ensureSameShapeList es after
    | allEqual (map expressionShape es) = after
    | otherwise =
        error $ "Ensure same shape failed " ++ show (map expressionShape es)

constWithShape :: Shape -> Double -> Expression d R
constWithShape shape val = Expression h (IM.fromList [(h, node)])
  where
    node = (shape, Const val)
    h = hash node

pullConstant :: ExpressionMap -> Int -> Maybe (Shape, Double)
pullConstant mp n
    | (shape, Const c) <- retrieveInternal n mp = Just (shape, c)
    | otherwise = Nothing

pullConstants :: ExpressionMap -> [Int] -> Maybe (Shape, [Double])
pullConstants mp ns
    | xs@(x:_) <- mapMaybe (pullConstant mp) ns = Just (fst x, map snd xs)
    | otherwise = Nothing

isZero :: ExpressionMap -> Int -> Bool
isZero mp nId
    | Const 0 <- retrieveNode nId mp = True
    | otherwise = False

isOne :: ExpressionMap -> Int -> Bool
isOne mp nId
    | Const 1 <- retrieveNode nId mp = True
    | otherwise = False

isConstant :: ExpressionMap -> Int -> Bool
isConstant mp nId
    | Const _ <- retrieveNode nId mp = True
    | otherwise = False

pullSumOperands :: ExpressionMap -> Int -> [Int]
pullSumOperands mp nId
    | Sum _ operands <- retrieveNode nId mp = operands
    | otherwise = [nId]

pullProdOperands :: ExpressionMap -> Int -> [Int]
pullProdOperands mp nId
    | Mul _ operands <- retrieveNode nId mp = operands
    | otherwise = [nId]

aConst :: Shape -> Double -> (ExpressionMap, Int)
aConst shape val = (IM.fromList [(h, node)], h)
  where
    node = (shape, Const val)
    h = hash node

module HashedExpression.AD.Backward where

import qualified Data.IntMap.Strict as IM
import Data.Maybe (fromJust)
import HashedExpression.DAG
import HashedExpression.Internal.Expression
import HashedExpression.Internal.Inner
import HashedExpression.Operation
import HashedExpression.Internal.Node
import HashedExpression.Internal.Utils
import HashedExpression.Prettify
import Control.Monad (forM_)
import Debug.Trace (traceShowId, traceShow)

-- |
toDAG :: Expression d et -> DAG
toDAG e = IM.map (\(_, node) -> nodeArgs node) (exMap e)

-- | Backward
backwardAD :: Expression Scalar R -> IM.IntMap (ExpressionMap, NodeID) -- TODO: Change to (ExpressionMap, IntMap NodeID) to make it faster
backwardAD exp@(Expression rootID mp) =
  let rDAG = reverseDAG $ toDAG exp
      order = topoSort rDAG
      parents nID = fromJust $ IM.lookup nID rDAG
      computeBackward accMp nID
        | nID == rootID = aConst [] 1
        | otherwise = sumMany $ map each (parents nID)
        where
          lookupMemo nID = case IM.lookup nID accMp of
            Just res -> res
            Nothing -> error $ "Not found node" ++ show nID ++ " " ++ show accMp
          each pID = case retrieveNode pID mp of
            Sum {} -> lookupMemo pID
            Mul _ args ->
              let others = removeOnce (== nID) args
               in mulMany $ map (mp,) others ++ [lookupMemo pID]
            InnerProd _ aID bID
              | nID == aID -> apply (binaryET Scale ElementDefault) [lookupMemo pID, (mp, bID)]
              | nID == bID -> apply (binaryET Scale ElementDefault) [lookupMemo pID, (mp, aID)]

      f accMp nID = IM.insert nID (computeBackward accMp nID) accMp
   in foldl f IM.empty order

removeOnce :: (a -> Bool) -> [a] -> [a]
removeOnce p [] = []
removeOnce p (x:xs)
  | p x = xs
  | otherwise = x : removeOnce p xs

test :: IO ()
test = do
  let a = variable "a"
      b = variable "b"
      x = variable "x"
      x1 = variable1D @10 "x1"
      y1 = variable1D @10 "y1"
      f = (a * a) + (a + b) * (x1 <.> y1)
      res = backwardAD f
  let (Expression n mp) = f
  forM_ (IM.toList res) $ \(nID, ue) ->
    putStrLn $ "Partial derivative for " ++ debugPrint (mp, nID) ++ " is: " ++ debugPrint ue



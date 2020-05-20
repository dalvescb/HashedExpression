module HashedExpression.DAG where

import Control.Monad.ST.Strict
import Data.Array.MArray
import Data.Array.ST
import qualified Data.Array.Unboxed as UA
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.Maybe (fromJust)
import Data.STRef.Strict
import Control.Monad (forM_, unless)



-- | Graph as adjacent list, edges go from key -> values
type DAG = IntMap [Int]

-- | Reverse a DAG, i.e, change direction of all edges
reverseDAG :: DAG -> DAG
reverseDAG dag = IM.foldlWithKey updateAcc init dag
  where
    nodeIDs = IM.keys dag
    init = IM.fromList $ zip nodeIDs (repeat [])
    updateAcc :: DAG -> Int -> [Int] -> DAG
    updateAcc acc u =
      foldl
        (\dag v -> IM.insertWith (\_ old -> u : old) v [u] dag)
        acc

-- | Topological sort
topoSort :: DAG -> [Int]
topoSort dag = UA.elems topoOrder
  where
    nodeIDs = IM.keys dag
    len = length nodeIDs
    toPos nId =
      let n2Pos = IM.fromList $ zip nodeIDs [0 ..]
       in fromJust $ IM.lookup nId n2Pos
    adj u = fromJust $ IM.lookup u dag
    topoOrder =
      runSTUArray $ do
        marked <- newArray (0, len - 1) False :: ST s (STUArray s Int Bool)
        order <- newArray (0, len - 1) (-1) :: ST s (STUArray s Int Int)
        cnt <- newSTRef 0 :: ST s (STRef s Int)
        let dfs u = do
              let arrayPos = toPos u
              writeArray marked arrayPos True
              forM_ (adj u) $ \v -> do
                isMarked <- readArray marked (toPos v)
                unless isMarked $ dfs v
              cntVal <- readSTRef cnt
              writeArray order cntVal u
              writeSTRef cnt (cntVal + 1)
        forM_ nodeIDs $ \n -> do
          isMarked <- readArray marked (toPos n)
          unless isMarked $ dfs n
        return order

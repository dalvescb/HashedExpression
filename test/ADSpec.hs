module ADSpec where


import Commons
import HashedExpression.DAG
import HashedExpression.AD.Backward
import Test.QuickCheck
import HashedExpression.Internal.Expression
import HashedExpression.Internal.Node
import Data.IntMap.Strict as IM
import Test.HUnit
import Test.Hspec



prop_reverseDAGThenTopoSort :: ArbitraryExpresion -> Bool
prop_reverseDAGThenTopoSort (ArbitraryExpresion e) = exRootID e == last (topoSort $ toDAG e) 
  


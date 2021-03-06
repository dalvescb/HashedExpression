module NormalizeEval.ScalarCSpec where

import Commons
import Control.Monad (replicateM_)
import qualified Data.IntMap.Strict as IM
import Data.Map.Strict
import Data.Maybe (fromJust)
import Data.Typeable (Typeable)
import Debug.Trace (traceShow, traceShowId)
import GHC.IO.Unsafe (unsafePerformIO)
import HashedExpression.Internal.Expression
import HashedExpression.Internal.Normalize
import HashedExpression.Internal.Utils
import HashedExpression.Interp
import HashedExpression.Operation hiding (product, sum)
import qualified HashedExpression.Operation
import HashedExpression.Prettify
import Test.Hspec
import Test.QuickCheck
import Prelude hiding ((^))

-- |
prop_NormalizeThenEval :: SuiteScalarC -> Bool
prop_NormalizeThenEval (Suite exp valMaps) =
  eval valMaps exp ~= eval valMaps (normalize exp)

-- |
prop_Add :: SuiteScalarC -> SuiteScalarC -> (Bool, Bool, Bool) -> Bool
prop_Add (Suite exp1 valMaps1) (Suite exp2 valMaps2) (normalize1, normalize2, normalizeSum) =
  eval valMaps exp1' + eval valMaps exp2' ~= eval valMaps expSum'
  where
    valMaps = union valMaps1 valMaps2
    exp1'
      | normalize1 = normalize exp1
      | otherwise = exp1
    exp2'
      | normalize2 = normalize exp2
      | otherwise = exp2
    expSum'
      | normalizeSum = normalize (exp1 + exp2)
      | otherwise = exp1 + exp2

prop_Multiply :: SuiteScalarC -> SuiteScalarC -> (Bool, Bool, Bool) -> Bool
prop_Multiply (Suite exp1 valMaps1) (Suite exp2 valMaps2) x@(normalize1, normalize2, normalizeMul) =
  if eval valMaps exp1' * eval valMaps exp2' ~= eval valMaps expMul'
    then True
    else
      error $
        prettifyDebug exp1'
          ++ "\n-----------\n"
          ++ prettifyDebug exp2'
          ++ "\n-----------\n"
          ++ show valMaps
          ++ "\n-----------n"
          ++ show lhs
          ++ " not equals "
          ++ show rhs
  where
    lhs = eval valMaps exp1' * eval valMaps exp2'
    rhs = eval valMaps expMul'
    valMaps = union valMaps1 valMaps2
    exp1'
      | normalize1 = normalize exp1
      | otherwise = exp1
    exp2'
      | normalize2 = normalize exp2
      | otherwise = exp2
    expMul'
      | normalizeMul = normalize (exp1 * exp2)
      | otherwise = exp1 * exp2

prop_AddMultiply :: SuiteScalarC -> Bool
prop_AddMultiply (Suite exp valMaps) =
  eval valMaps (normalize (exp + exp))
    ~= eval valMaps (normalize (constant 2 *. exp))

spec :: Spec
spec =
  describe "normalize & eval property for Scalar C" $ do
    specify "evaluate must equals normalize then evaluate " $
      property prop_NormalizeThenEval
    specify "prop_Add" $ property prop_Add
    specify "prop_Multiply" $ property prop_Multiply
    specify "prop_AddMultiply" $ property prop_AddMultiply

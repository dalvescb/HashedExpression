{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeApplications #-}

module Main where

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
   let exp = x + y
   let valMap = fromList [("x", VScalar 5.0),
                         ("y", VScalar 3.0)]
   let cCode  = intercalate "\n" $ singleExpressionCProgram valMap exp
   putStrLn cCode

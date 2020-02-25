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
    let x = variable1D @10 "x"
    let y = variable1D @10 "y"
    let z = variable1D @10 "z"
    let exp = (x + y) <.> (x - y) + x <.> z
    print exp
    showExp exp
    -- Normalizing expressions
    let normalizedExp = normalize exp
    showExp normalizedExp
    -- Computing derivatives
    let dExp = derivativeAllVars exp
    showExp dExp
    -- Collect differentials
    let collectedDExp = collectDifferentials dExp
    showExp collectedDExp
    -- Convert to C
    let valMap = fromList [("x", V1D $ listArray (0, 9) [1..10]), 
         ("y", V1D $ listArray (0, 9) [1..10]),
         ("z", V1D $ listArray (0, 9) [1..10])]
    let cCode  = intercalate "\n" $ singleExpressionCProgram valMap normalizedExp
    putStrLn cCode



























